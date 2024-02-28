# Gemini Web Browser 
Assignment INF221 2024 - Halvor Brunt

[Gemini Project](https://geminiprotocol.net/) is unfamiliar, but exciting. Getting to know protocols, the terminal, and parsing.

Might get inspired by features or structure of [Amfora](https://github.com/makew0rld/amfora?tab=readme-ov-file)


## Description of the program:

The Project allows users to browse Geminiweb in a Terminal Environment. It uses the Gemini protocol and renders the response in the terminal.

## Overview of program design and libraries:

Libraries (from suggestions):
- Network ----------------- TCP-sockets
- Attoparsec -------------- parsing Gemini protocol responses
- Mega Parsec ------------- parsing data & user inputs
- Brick ------------------- Creating a TUI


File structure:
```
Main.hs                    -- Runs program
geminiResponseParser.hs    -- Parses protocol responses 
geminiRenderer.hs          -- Formats text ready to render
geminiTUI.hs               -- TUI
networkProtocolHandling.hs -- Handles networks connections & protocols
```

## Functional programming techniques:

With nothing specific in mind I will probably be utlizing:
- Pure functions (this is haskell after all)
- Immutable data structures
- Monads: State, IO, Either, ...
- Error handling: Either/ExceptT
- Pattern matching
    - Parsing
    - Render different Gemini Responses Types (text, image/pdf links, ...)

## What I expect to complete by the deadline:

A lack of experience leaves this as a wild guess:
An minial-browser with a search bar that can fetch and display raw documents in the terminal. Hopefully I have time to flesh it out with more features.

### Expected:
- [ ] Network
    - [ ] Implement TCP connection & Gemini protocol
    - [ ] Error handling and error messages
- [ ] Parsing
    - [ ] Parse gemini response messages
    - [ ] Render "gemtext" formatted text to terminal
- [ ] TUI
    - [ ] Single Tab browser with Search bar & raw text data
    - [ ] Link navigation
- [ ] Decent enough docs to allow usability

### Stretch goals
Hopefully I can implement some of these, and possibly extend it later:
- [ ] Extend TUI
    - [ ] Bookmarks & History
    - [ ] Keyboard and/or mouse navigation
    - [ ] Handle user input on pages. E.g. Comments, sign guest books, ...
    - [ ] Mouse clicks
    - [ ] Multiple tabs
