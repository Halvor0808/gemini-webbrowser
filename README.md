# Gemini Webbrowser


# TODO
- Protocol
  - Parsers
    - General
      - [ ] Rewrite tests using `compareResults`
      - [ ] Better fail messages -- using `fail`?
    - Request
      - [ ] Do I have to deal with U+FEFF ("Byte order mark")? (see gemini docs)
    - Response
      - [ ] status codes: Optional parse-exection based on code?
      - [ ] What to do with `makeMime`
      - [ ] What to do with uses of `optional`
  - Gemtext
    - [ ] All of it