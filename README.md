# Gemini Webbrowser


# TODO
- Protocol
  - Parser
    - General
      - [ ] Rewrite tests using `compareResults`
      - [ ] Better fail messages -- using `fail`?
    - Request
      - [ ] Do I have to deal with U+FEFF ("Byte order mark")? (see gemini docs)
    - Response
      - [ ] Optional parse-exection based on status codes?
  - Gemtext
    - [ ] All of it