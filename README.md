# Gemini Webbrowser

# TODO
- Protocol
  - Parsers
    - General
      - [ ] Deal with use of `consumeRestOfLine` Bug: used on all gemtext lines, but will fail if there isnot a line break, but end of input instead
      - [ ] Rewrite tests using `compareResults`
      - [ ] Better fail messages -- using `fail`?
    - Request
      - [ ] Do I have to deal with U+FEFF ("Byte order mark")? (see gemini docs)
    - Response
      - [x] status codes: Optional parse-exection based on code?
      - [ ] What to do with `makeMime`, `pMime` & `Mime`
        - Currently: recovers for any mistake with default: text/gemini charset=utf-8. Should it fail instead of default? Should it only default if whole `<META>` is empty ("")?
      - [ ] If a MIME type begins with "text/" and no charset is explicitly given, the charset should be assumed to be UTF-8. Compliant clients MUST support UTF-8-encoded text/* responses. Clients MAY optionally support other encodings. Clients receiving a response in a charset they cannot decode SHOULD gracefully inform the user what happened instead of displaying garbage. If <META> is an empty string, the MIME type MUST default to "text/gemini; charset=utf-8". The text/gemini media type is defined in section 5.
      - [ ] Deal with charset
- TUI - Brick
  - [x] Create a outline for TUI
  - [x] Basic Controls: quit, send request, exit, scrolling
  - [x] Displaying a gemini-page --raw
  - [x] Search textfield (without fetching page)
  - [x] Better difference between heading levels: indent underline (border) as well?
  - [ ] Help page for controls (Ctrl-h)
  - [ ] Handler preformatted lines being cut off. Allow horizontal scrolling?
  - [ ] Handle navigating with links
  - [ ] Allow page/navigation history

TUI Goal:
|-----------Gemini-Browser----------|
|             Searchbar             |
|-----------------------------------|
|                                   |
|                                   |
|                                   |
|          Page contents            |
|                                   |
|                                   |
|                                   |
|-----------------------------------|
|            basic help             |
|-----------------------------------|


# Network - TCP & TLS
## TCP connection
C: Opens connection
S: Accepts connection
C/S: Complete TLS handshake (see section 4)
C: Validates server certificate (see 4.2)
C: Sends request (one CRLF terminated line) (see section 2)
S: Sends response header (one CRLF terminated line), closes connection under non-success conditions (see 3.1 and 3.2)
S: Sends response body (text or binary data) (see 3.3)
S: Closes connection (including TLS close_notify, see section 4)
C: Handles response (see 3.4)

## URI scheme
Mostly follows RFC 3986.
The port subcomponent is optional, with a default value of 1965.
An empty path is equivalent to a path consisting only of "/". Spaces in paths should be encoded as %20, not as +.
Clients SHOULD normalise URIs (as per section 6.2.3 of RFC 3986) before sending requests (see section 2) and servers SHOULD normalise received URIs before processing a request.

## Gemini Requests
Gemini requests are a single CRLF-terminated line with the following structure:

`<URL><CR><LF>`

<URL> is a UTF-8 encoded absolute URL, including a scheme, of maximum length 1024 bytes. The request MUST NOT begin with a U+FEFF byte order mark.

Sending an absolute URL instead of only a path or selector is effectively equivalent to building in a HTTP "Host" header. It permits virtual hosting of multiple Gemini domains on the same IP address. It also allows servers to optionally act as proxies. Including schemes other than "gemini" in requests allows servers to optionally act as protocol-translating gateways to e.g. fetch gopher resources over Gemini. Proxying is optional and the vast majority of servers are expected to only respond to requests for resources at their own domain(s).

Clients MUST NOT send anything after the first occurrence of <CR><LF> in a request, and servers MUST ignore anything sent after the first occurrence of a <CR><LF>

# Notes

## Response


### Statuscode BASED parsing

Focusing on what comes after `<STATUS><space>`.

#### 1X INPUT expected:

Here `<META>` is a prompt to be displayed to user.
Then to resend the request with the input inserted as a query.
  Reserved chars must be "percent encoded"

#### 2X SUCCESS

Here `<META>` is a MIME media type that applies to the response body.


#### 3X REDIRECT

There is no respone body.
Here `<META>` is a new url. Either relative or absolute. 
If relative -- must resolve with original request. But MUST NOT include query string (i.e. separated by `?`).

#### 4X TEMPORARY FAIL

no response body. 
Contents of `<META>` may provide information on the failure. Should be displayed to users.

#### 5X PERMANENT FAIL
The request has failed. There is no response body.
Contents of `<META>` may provide information on the failure. Should be displayed to users.


#### 6X Client certificate required
requested resource requires a client certificate to access.
If the request was made without a certificate, it should be repeated with one. 
If the request was made with a certificate, the server did not accept it and the request should be repeated with a different certificate. 
The contents of `<META>` (and/or the specific 6x code) may provide additional information on certificate requirements or the reason a certificate was rejected.

### MIME
- if
  - statuscode is not 2 digits
  - META is longer than 1024 bytes
  - THEN: Close connection & disregard response header & inform of error

 - Charset: Default = UTF-8
 - From MIME-parameters: Do not assume value of "lang". For simple clients: ignore:)
 - If a MIME type begins with "text/" and no charset is explicitly given, the charset should be assumed to be UTF-8. Compliant clients MUST support UTF-8-encoded text/* responses. Clients MAY optionally support other encodings. Clients receiving a response in a charset they cannot decode SHOULD gracefully inform the user what happened instead of displaying garbage.
