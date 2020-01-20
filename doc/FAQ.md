# REPL-Tooling and plug-ins FAQ

Some questions that come from time-to-time regarding Chlorine or Clover, mostly about why things are working the way they are, and some other design decisions about the plug-ins. As both use this library as base to connect to Clojure-like REPLs, I'm answering all here:

## Design
### Why not nREPL?
nREPL is a great tool, and it'll probably help solve lots of problems that I'm having right now while developing (mostly about "cutting the output" and "detecting evaluation results"). The problem with nREPL is that is Clojure-only. There are some implementations for other languages, but they are outdated, don't work, or need a specific dependency. This library uses the "bare minimum" to work on most implementations, so porting to a new runtime is really easy too.

### Will you support prepl?
Short answer: no.

Long answer: until prepl becomes available on most Clojure implementations, it'll not be supported. Even then, it'll probably not be the focus on the library. Again, the objective is to support all Clojure-like languages, so I'll focus on a way to work with the REPL outputs (and this will be where I'll spend most of my time). Also, prepl is **really confusing** on some implementations (for example, ClojureScript).

### When I try to connect using `lein` or `boot`, nothing happens!
When you start `lein` or `boot`, they'll give you by default a nREPL port to connect. **This is not the port you'll use!**. Instead, start a Socket REPL, and connect from there. You can start a socket REPL anytime by running on the REPL (assuming that you want it running on port 4444):

```clojure
(require 'clojure.core.server)
(clojure.core.server/start-server
  {:name "socket-repl"
   :port 4444
   :accept 'clojure.main/repl
   :address "localhost"}))
```

Beware that some implementations can vary a little bit.

### What is UNREPL?
UNREPL is a blob of Clojure code that you can send to the REPL. It'll make the Socket REPL more programmable (it'll output EDN and display what is an output, an error, or an evluation result). It'll also add support for "lazy fetching" of some code.

### Have you heard of / will you support LSP?
LSP does not support evaluation of arbitrary code. It is also not extensible, so it will not support things like "show schemas/specs for code", for example. Also, there are already similar projects that implement LSP for Clojure.

### What about Conjure?
Conjure is in Clojure. The focus is to make the tooling work for ClojureScript, so it can target more editor implementations.

## Similar projects
### What about pink-gorilla/clojupyter/gorilla-notebook/Gorilla REPL?
These are great projects, really. But what I want to do is a layer where you can develop inside your editor of choice, connected to a REPL, without adding any dependency on your project. Almost all of these projects need you to add another Clojure dependency, so they'll just work on Clojure or need to be ported between implementations

### Have you seen that editor where you add it as a dependency on your project and...
These editors again suffer from the same problem as above: only work on Clojure, or ClojureScript. REPL-Tooling compiles to Javascript, and so it is possible to use any editor that supports JS (Atom, VSCode, NeoVIM, Eclipse Theia/Che, GitPod, etc) or anyplace that have Node.JS if the editor does not.

## Implementations

### What Clojure implementations are supported?
For now, Clojure, ClojureScript, ClojureCLR, Joker, Babashka, and Clojerl.

### Why ClojureScript implementation doesn't support X feature?
There's a **big difference** from Clojure than other implementations: first, it does support UNREPL which makes scripting way easier. Second, that it's the most mature implementation. ClojureScript is also really mature, but it's incredibly different from Clojure that makes scripting over a CLJS REPL kinda difficult.

### Will you support "X"?
Probably, if it is close enought to Clojure. To be "close enough" means that:

1. Supports `let`
1. Quoting like `'tooling$eval-res` will return a symbol without the quote (Fennel and Hy don't support it)
1. Keywords render the same as Clojure (again, Fennel and Hy don't work)
1. It supports a socket REPL (Ferret don't even have a REPL. Babashka, on the first versions, didn't have a REPL but you could start a Socket REPL, so it works)