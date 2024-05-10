# INF221 Report – Gemini Web Browser
Submission by Halvor Brunt.

## 1 Project Idea

### 1.1	What Is Gemini
Gemini is a simple application-level client-server protocol (Gemini Network Protocol Specification, n.d.) for distributing various files, with emphasis on gemtext, a lightweight markup facilitating links between hosted files. It represents a minimalist web stack prioritizing user autonomy and privacy. The protocol and the markup are intentionally limited and built on standardised technologies like URIs, MIME media types and TLS. (Project Gemini FAQ, n.d.)

### 1.2 Why Make a TUI Gemini Web Browser
The project idea was a listed as a suggestion on mitt.uib. “Gemini Web Browser” as a project caught my eye because it sounded cool and touched upon various topics which I wanted to learn more about. Implementing an application-level protocol, parsing a language/markup (though incredibly simple syntax), and building a terminal user interface application. A buffet of interesting topics!

### 1.3 Original Intent of Program
Initially I had a naïve grand vision for the project. Naïve due to my lack of experience in all the project relevant fields, and the unknown number of libraries I would end up using.
The idea was to make a TUI browser using the gemini protocol to browse the “geminispace”, the collection of files available through gemini protocol (gemini-equivalent of www or gopherspace). Where the browser supported multiple tabs, global browser-history, and local tab-history, favourited gemini-pages, both keyboard and mouse navigation, redirecting non-gemini protocol links to the user’s default web browser and more. I wanted the gemini terminal equivalent of popular web browsers. Only a small subset of these got implemented, and to implement the others, it requires large restructuring. 

### 1.4 Build & Setup
Prerequisites : 

- Haskell is installed. 
- Cabal version = `3.10.2.1` is installed. 
- GHC version = `9.4.8` is installed.

Clone repo with preferred method

1. (SSH): `git clone git@github.com:Halvor0808/gemini-webbrowser.git`
2. (HTTPS): `git https://github.com/Halvor0808/gemini-webbrowser.git`

Change directory to the root of the project: `cd gemini-webbrowser`

Checkout to the git tag: `git checkout 221-submission`

There is a cabal.freeze file in the repository. So this will have the right dependencies.
Navigate to root of project and then run: `cabal update && cabal run`.

Upon running the project you are brought to the home page. It includes the keybindings for keyboard, and two useful links ( first one does not work on windows). When focus on the search field you can treat it like an input field. Press tab to switch between the search field and page content. Once focused on the page you can navigate vertically with the arrow keys. If the cursor is on a link press `<Enter>` to navigate to that link. Press `<Esc>` or `<Crtl>`   + `<q>` to exit the application.

### 1.5 Libraries
In the project proposal I mentioned: Network, Attoparsec, MegaParsec, and Brick.
Except for MegaParsec, all of these were used to some extent, in addition to some others, as seen below.

Base, mtl, attoparsec, brick, text. utf8-string, word-wrap, bytestring, vty, microlens, microlens-th, microlens-mtl, vector, tls, network, network-simple-tls, network-uri, data-default-class, QuickCheck.

## 2.	Functionality

### 2.1	What It Can and Cannot Be Used For
To reiterate, the gemini protocol aims to streamline the web experience by utilizing a minimalistic web stack, prioritizing user autonomy and privacy.
This program allows browsing most of geminispace. It can fetch all gemtext formatted pages but does not support other media types. If the response is expecting input (status code 10), the application prompts the user without providing an intuitive input field, limiting interactions to purely text-based content.
While the gemini protocol emphasizes privacy, the handling of TLS is scandalous. I merely ensure a working socket. In essence, accepting any sever certificate. Potentially compromising user privacy. 

### 2.2	Incomplete Implementations
This project currently has incomplete handling of TLS-sockets, TLS certificate, and input responses.

#### 2.2.1 Socket & TLS
The socket implementation, TLS use does not work as intended. It does protect user privacy and has TLS certificate issues. On Linux the TLS certificates are handled as expected. Whilst on Windows 11 there are inconsistencies in certificate handling, and I am not sure why. The application waits for a response the whole timeout duration, and then receives and displays a time out error. The requesting of a page is not forked, and thus the whole application freezes until the timeout error occurs. See `Socket.hs`.

#### 2.2.2 Input Response (Error Code 10-19)
The application does not handle Input Responses well. It should be prompting the user for input, automatically appending it as query to the URI, and then retry. Currently it prints out the input prompt in the application, and the user must manually enter the prompted query into the search field.

#### 2.2.3 Preformatted Lines
Preformatted lines are mostly handled well. Except for lines that are wider than the list widget that are cut off short. Not allowing the user to scroll to the right to see the rest of it. Limiting access to ASCII-art and other wide textual content.

## 3.	Functional Programming Techniques

### 3.1	Functional Techniques:
While I did not see the applications of a lot of techniques from lectures in this project, I have incorporated some of the basics. Pure functions, immutability, and pattern-matching, which are built into and enforced by the Haskell language, and Monads such as State, IO and Either, Parser. All of which I used.
`Maybe` is used to deal with optional values, such as MIME in responses, and interacting with other libraries. `IO` for communicating with the TCP/TLS sockets and reading local files. `Either` to recover from or deal with errors in `IO` operations, for unit tests for parsing, and for dealing with a failed or successful parse of a response.
`StateT` transformer in combination with `Parser` created a stateful parser that keeps track of a Boolean value. The Boolean indicates if it is within a preformatted line-block or not, toggled whenever it parses a line prefixed with a triple backtick (```).

```haskell
type StateParser a = StateT Bool Parser a
```

`State` was utilized through brick’s `EventM` which is the State monad parameterized over the representation of the application-state (`St` in this project).
I made two property-based tests with quick check, to see if the conversion between the two `Uri` and `Url` data types worked as intended. See section 4.1.2.
While not property-based, I have made a few unit tests for my parsers (see `Test.Protocol. Parser` folder). They will become property-based tests eventually.

## 3.2	Challenges

This project was a joyful, uphill battle, with many things to learn (different libraries, cabal, gemini protocol, network, byte string encoding, etc.). While fun it was also frustrating. The main challenges were properly understanding the emini protocol, handling Socket & TLS-certificates, and displaying and allowing user interaction with the gemtext format in brick.
While the gemini protocol is simple the amount of text and unfamiliar syntax it was expressed in proved demanding. Certain parts of the protocol only clicked into place once I started coding. Causing a loop of reading, coding, rereading and recoding.
Creating sockets with “gemini-servers” proved difficult to me. Not knowing where the issues lie and how to proceed. Parts of the issue was poor handling of TLS, which was evident once I got help from James Hobson. He provided a functional implementation I now use (Hobson, n.d.)

Working with `Brick` has been a labour of love, navigating the many options. The two main tasks were representing an input field (the search field) and representing a gemtext page while allowing keyboard navigation on that page. Eventually I ended up using the `Widget.Edit` to represent an input field, and a `Widget.List` with the page as a list of lines with line-based cursor support. Both of which have default event handling built in. While quite happy with the result there are some scrolling issues with how I am using the List widget.

## 4.	Evaluation

### 4.1 Unpolished Parts, Regrets and Mistakes
A natural consequence of doing it for the first time and “move fast and break things” is code that could be planned, implemented, and handled better.

#### 4.1.1 Lazy ByteStrings
Last week of the project I realized that I was using regular bytestrings instead of lazy bytestring. Considering the project reads and sends bytes to and from a socket, using lazy bytestrings is an obvious choice. This will be changed after submission.

#### 4.1.2 Url and URI
A browser needs URIs, so I made my own data type (`Url`). However, when looking into resolving relative URLs, I stumbled upon the RFC specification of URIs (Berners‐Lee et al., 2005). Implementing the specifications including edges cases would be a library. I searched and found that library (`Network.URI`) and used it immediately.
Instead of replacing my old data type, I instead transform between my data type `Url` and the library’s `URI` with the functions `uriToUrl` and `urlToUri`. This is bad practice, and I regret not replacing it completely. See `Protocol.Data.Response.hs`.
To justify the safety of the mess a tiny bit, I have made a QuickCheck test for the conversion between these two datatypes. (At time of submission I am working on making it part of a test suite). See `Test.MyQuickCheck.hs`.

#### 4.1.3 Scrolling and Line Visibility
The gemtext content is rendered in a brick `List` widget, which provides a line-based cursor. Upon rendering a new List, the top item is selected and made visible with a `visibility` request. But there is a bug when rendering the List upon fetching a new page. Sometimes it does not scroll the top-line completely into view, leaving the cursor out of view. Though, it can be scrolled into view by scrolling upwards.
I believe this is related to lists being widgets of unrestricted size. They must be the vertically limited, which makes it behave differently on different list size, and different screen window sizes. 

### 4.2 Evaluation
The result and the proposal idea are quite similar. I completed one stretch goal of adding history, but I did complete t the expected goal of good error messages. All other expected goals have been met.
I have made a TUI in brick where a user can enter URIs to fetch a resource using the gemini protocol through a TCP/TLS socket. The response header is then parsed, and based on the header an action is taken. If the status code is 10, 40, 50 or 60, it will feature meta string with an error message or a prompt.  If the status code in the header is 20 (success) then it will parse a meta string with mime-types and arguments, followed by parsing a response body. The parsed response body, error message, or input prompt is then displayed in the TUI. The user can also access a help-screen to see controls, and a history to see all visited pages in chronological order, where they can revisit said pages. I am happy with the “high-level” and visual result. I achieved what I set out to do, and I am happy about it. I did not use many different functional techniques, but I have utilized some basics.

The most valuable takeaway from this project is how to learn and use different Haskell libraries. A new skill, which lead to early on bad decisions (such as not using lazy bytestrings), but also lead to faster development and joy, eventually.

## 5.	Further Development
Given more time there are features I would add, and adjustments I would make. Here are some features listed in no particular order.
Tab history for easy forth and back navigation would be a nice addition to the chronological history stack. This could be represented as:
```haskell
type TabHistory = Cursor URI
data Cursor a = Cursor { cursorPrevious :: [a]
                       , cursorSelected ::  a
                       , cursorNext     :: [a]
}
```

**Bookmarks** for your frequently used sites. And then rendering them to the homepage. Bookmarks could be represented as a list, or an inductive rose tree.
```haskell
data Bookmarks a = Node a [Bookmarks a]
```

**Customizable homepage** allows a user to show case some personality and accommodate it to their own use. This is possible by loading their homepage into an `Edit` widget.

**Profile data type** to keep track of bookmarks, and custom homepages (and possibly more). Making both bookmarks, custom homepages, and possibly history persistent requires writing and reading from a file, and a parser.

**Command line arguments** to load specific profiles, optionally no profile, not saving history (incognito), and a debug argument to toggle the debug information displayed in the footer.

If I could start over, there are some things I would do differently. I would represent the TUI application state a bit differently. Currently I cannot have an `Edit` widget as content because it is restricted by the type of `_content` in `St`. I would change it to 
```haskell
data Content a = Line | Input | List List Name Line
data St = St  {_content ::  [Content], … }
```
This would address issues as not handling Input Response (Status Code 10), and otherwise accommodate for gemini resources that also allow user interaction (sign a guest book, leave a comment, etc.). It also addresses preformatted lines that are wider than the widget being cut off. Preformatted text blocks could be placed in a horizontally scrollable widget. Removing the List representation of content would require my own cursor implementation, similar to the one proposed further up on this page.
In conclusion, I am happy with this project and what functional techniques I have learned in the process. Even with its faults and possible improvements, I am proud of the outcome.

Halvor Brunt.

## References
Project Gemini FAQ. (n.d.). https://geminiprotocol.net/docs/faq.gmi#111-the-dense-jargony-answer-for-geeks-in-a-hurry

Gemini network protocol specification. (n.d.). https://geminiprotocol.net/docs/protocol-specification.gmi

Hobson, J. (n.d.). Discord - INF221 Channel. Discord. 

https://discord.com/channels/1206588530193600544/1206588530730598424/1229037136267972688

Berners‐Lee, T., Fielding, R., & Masinter, L. (2005). Uniform Resource Identifier (URI): Generic Syntax. https://doi.org/10.17487/rfc3986
