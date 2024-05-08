# Gemini Webbrowser

# TODO
- [ ] Change to LazyByteStrings
  - [ ] Update program 
    - [ ] Parser
    - [ ] TUI
  - [ ] Update tests

- Protocol
  - Parsers
    - General
      - [ ] Deal with use of `consumeRestOfLine` Bug: used on all gemtext lines, but will fail if there is not a line break, but end of input instead
    - Request
      - [ ] Do I have to deal with U+FEFF ("Byte order mark")? (see gemini docs)
    - Response
      - [ ] What to do with `makeMime`, `pMime` & `Mime`
        - Currently: recovers for any mistake with default: text/gemini charset=utf-8. Should it fail instead of default? Should it only default if whole `<META>` is empty ("")?
          - From Gemini's Docs: If a MIME type begins with "text/" and no charset is explicitly given, the charset should be assumed to be UTF-8. Compliant clients MUST support UTF-8-encoded text/* responses. Clients MAY optionally support other encodings. Clients receiving a response in a charset they cannot decode SHOULD gracefully inform the user what happened instead of displaying garbage. If <META> is an empty string, the MIME type MUST default to "text/gemini; charset=utf-8". The text/gemini media type is defined in section 5.
      - [ ] Deal with charset
- TUI - Brick
  - [x] Basic Controls: quit, send request, exit, scrolling
  - [x] Handle navigating with links
    - [x] Cursor and navigating with keyboard
    - [x] Resolving relative URLs
  - [x] Allow history, and history navigation
  - [ ] Handle preformatted lines being cut off.
  - [ ]  When rendering pages, the border is choppy

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
- Apprarently Emojis and other "wide" charcaters are not well supported in Brick. Since they affect the spacing pushing everything along. To fix this it requires a workaround in VTY. Since this is not crucial to my application, I deem it to be outside the scope of this project, and I cannot be bothered.
- Improve & update Help/control page
