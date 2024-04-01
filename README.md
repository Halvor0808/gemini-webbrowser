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
      - [ ] status codes: Optional parse-exection based on code?
      - [ ] What to do with `makeMime`, `pMime` & `Mime`
        - Currently: recovers for any mistake with default: text/gemini charset=utf-8. Should it fail instead of default? Should it only default if whole `<META>` is empty ("")?
      - [ ] If a MIME type begins with "text/" and no charset is explicitly given, the charset should be assumed to be UTF-8. Compliant clients MUST support UTF-8-encoded text/* responses. Clients MAY optionally support other encodings. Clients receiving a response in a charset they cannot decode SHOULD gracefully inform the user what happened instead of displaying garbage. If <META> is an empty string, the MIME type MUST default to "text/gemini; charset=utf-8". The text/gemini media type is defined in section 5.
      - [ ] Deal with charset
- TUI - Brick
  - Create a outline for TUI
  - Basic Controls outline
    - q = quit
    - enter (mode dependent)= enter search mode | trigger request on
    - esc = read mode
    - Arrow navigation
  - Displaying a gemini-page --raw
  - Scrolling on a page
  - Get a search field
    - Allow entering text (without fetching page)
  - Dynamic Padding: from min to a max value

TUI Goal:
Border                        Border
-----------Gemini-Browser-----------
|   |         Searchbar        |   |
------------------------------------
|   |                          |   |
|   |                          |   |
|   |                          |   |
|   |      Page contents       |   |
|   |                          |   |
|   |                          |   |
|   |                          |   |
------------------------------------


# Done
- [x] First Find status code, and then continue parsing based on STATUS

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


# Brick

Resources:
- https://hackage.haskell.org/package/brick
- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst

The process of writing an application using brick entails writing two important functions:

A **drawing function** that turns your application state into a specification of how your interface should be drawn, and
An **event handler** that takes your application state and an input event and decides whether to change the state or quit the program.

## Drawing function
Using primitives & combinators.
Place text on screen, set attributes (e.g. colur) and express layout (padding, centering, box layouts, scrolling viewports, ...)

Drawing functions are packaged into an `App` struct and handed off to `brick`'s main event loop.

Drawing functions can be found in `Brick.Widgets`. Mostly in `Btick.Widgets.Codre`.

## App
The three types in a App:
`s` = application state type, `e` = event type, `n` = resource name type

## Compiling!
Brick applications must be compiled with the threaded RTS using the GHC -threaded option.



