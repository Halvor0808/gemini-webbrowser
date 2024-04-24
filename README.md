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
  - [x] Help page for controls (Ctrl-e)
  - [ ] Handle preformatted lines being cut off.
  - [ ] Handle navigating with links
    - [x] Cursor and navigating with keyboard
    - [ ] Resolving relative URLs
  - [ ] Allow history, and history navigation

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

# Known issues
- TLS implementation does not work on windows for some servers
- Emojis and other "wide" charcaters are not well supported in Brick. Requires workaround in Vty. Which is outside the scope of "I can be bothered".