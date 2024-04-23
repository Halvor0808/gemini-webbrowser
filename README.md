# Gemini Webbrowser

# Questions -- I need help

- [ ]  `pGeminiUrl` -- I need to accept `gemini://`, and when no protocol is mentioned. But fail otherwise. How.
- [ ]  When rendering pages, the border is choppy
- [ ]  Encoding in brick. Encoding of emojis are all wrong



# TODO
- Protocol
  - Parsers
    - General
      - [ ] Deal with use of `consumeRestOfLine` Bug: used on all gemtext lines, but will fail if there is not a line break, but end of input instead
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
  - [ ] Handler preformatted lines being cut off.
  - [ ] Handle navigating with links
  - [ ] Allow page/navigation history

TUI Goal:
```
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
```

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
