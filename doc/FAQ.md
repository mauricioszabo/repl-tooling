# REPL-Tooling and plug-ins FAQ

Some questions that come from time-to-time regarding Chlorine or Clover, mostly about why things are working the way they are, and some other design decisions about the plug-ins. As both use this library as base to connect to Clojure-like REPLs, I'm answering all here:

## Design
### What is a block / top-block?
A block is _almost_ the same as a form in Clojure. There are specifics for detecting what will be sent to the REPL for evaluation mostly for convenience. In these examples below, `|` will be used to simulate the cursor position, so it should be ignored. For example, if the cursor would be after the `2` in the `(+ 1 2 3)` code, it'll be identified as `(+ 1 2| 3)`:

1. Block will be where the cursor **is inside**:
* `(+ (- 1 2|) 3)` - the block is `(- 1 2)`

2. Because the way most editors work (highlighting the parenthesis) if the cursor is _outside_ a block but _before_ an open parens or _after_ a close parens, it'll behave differently:
* `(+ (- 1 2)| 3)` - the block is `(- 1 2)`
* `|(+ (- 1 2) 3)` - the block is `(+ (- 1 2) 3)`
* `(+ (- 1 2) 3)|` - same as above, the block is `(+ (- 1 2) 3)`

3. Top-block will always be a block that is releated to "root":
* `(+ 1 2|) (+ 3 4)` - the top-block is `(+ 1 2)`
* `(+ 1 2) (+ 3 4|)` - The top-block is `(+ 3 4)`
* `(+ 1 (- 2 |3) 4)` - The top-block is `(+ 1 (- 2 3) 4)`

4. Blocks will include reader tags:
* `#?(:cljs "clojure" :cljs |"script)` - will include the `#?` in the evaluation
* `#(+ 1 2|)` - will return an anonymous function
* `'(+ 1 2|`) - will include the quote
* `@(:an-atom my-map|)` - will include the deref
* `(deref (:an-atom my-map|))` - will NOT include the deref

5. Top-blocks will consider the parenthesis before they when evaluating:
* `(+ (- 2 3) 4)|` - will evaluate `(+ (- 2 3) 4)`

6. Comment-form `#_` is ignored always
* `#_(+ 1 2)|` - will evaluate `(+ 1 2)` for top-block, and nothing for block
* `#_(+ 1 2|)` - will evaluate `(+ 1 2)` for both top-block and block

### Why not nREPL?
nREPL is a great tool, and it'll probably help solve lots of problems that I'm having right now while developing (mostly about "cutting the output" and "detecting evaluation results"). The problem with nREPL is that is Clojure-only. There are some implementations for other languages, but they are outdated, don't work, or need a specific dependency. This library uses the "bare minimum" to work on most implementations, so porting to a new runtime is really easy too.

Now, Chlorine does have support for nREPL too, but again the target of the project is to connect to most Clojure implementations that's possible. Adding support for nREPL also adds supports for MORE languages, but it'll probably never be the target of this project to support custom middlewares or other advanced nREPL features

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
Conjure is written in Clojure. The focus is to make the tooling work for ClojureScript, so it can target more editor implementations.

## Similar projects
### What about pink-gorilla/clojupyter/gorilla-notebook/Gorilla REPL?
These are great projects, really. But what I want to do is a layer where you can develop inside your editor of choice, connected to a REPL, without adding any dependency on your project. Almost all of these projects need you to add another Clojure dependency, so they'll just work on Clojure or need to be ported between implementations.

### Have you seen that editor where you add it as a dependency on your project and...
These editors again suffer from the same problem as above: only work on Clojure, or ClojureScript. REPL-Tooling compiles to Javascript, and so it is possible to use any editor that supports JS (Atom, VSCode, NeoVIM, Eclipse Theia/Che, GitPod, etc) or anyplace that have Node.JS if the editor does not.

## Implementations

### What Clojure implementations are supported?
For now, Clojure, ClojureScript, ClojureCLR, Joker, Babashka, Lumo, Plank, and Clojerl.

### Why ClojureScript implementation doesn't support X feature?
There's a **big difference** from Clojure than other implementations: first, it does support UNREPL which makes scripting way easier. Second, that it's the most mature implementation. ClojureScript is also really mature, but it's incredibly different from Clojure that makes scripting over a CLJS REPL kinda difficult.

### Will you support "X"?
Probably, if it is close enought to Clojure. To be "close enough" means that:

1. Supports `let`
1. Quoting like `'tooling$eval-res` will return a symbol without the quote (Fennel and Hy don't support it)
1. Keywords render the same as Clojure (again, Fennel and Hy don't work)
1. It supports a socket REPL (Ferret don't even have a REPL. Babashka, on the first versions, didn't have a REPL but you could start a Socket REPL, so it works)

### When jack-in will be implemented?
I don't intend to implement "jack-in". The thing is, is difficult to cleanup external processes and this could complicate **a lot** the code. Also, there's the problem with environment variables (I had multiple problems when I used editors that used jack-in because env variables were not set, PATH was set to different places and so on), some sandboxing that some editors do, it makes docker difficult do run in this context and **also** makes it necessary to support `lein`, `boot`, `shadow-cljs`, `clj`, `clojure`, and the other specific implementations' CLI, when they exist.

## Bugs / Limitations

### Test output not appearing on the REPL, only on Chlorine
When you try to run `run-tests` or something that depends on `clojure.test`, sometimes the output will not appear. This is a limitation of `clojure.test` - the output of tests is redirected to `clojure.test/*test-out*`. The first time someone requires `clojure.test`, it'll bind to `*out*`.

This library tries its best to bind the right `*test-out*`, but if you find some trouble, you can try to reset with the following command:
```clojure
(alter-var-root #'clojure.test/*test-out* (constantly *out*))
```

### Output appearing on the REPL, but not on Chlorine
When you evaluate code that runs on other threads, Clojure does not redirect that STDOUT to the socket. So Chlorine have now way of capturing that output. This is a limitation on the way the JVM works, so nothing to do here, unfortunately

### When I evaluate multi-thread code, sometimes the result is wrong
Please, be **absolutely sure** that you're realizing the whole sequence. For example, if you evaluate these commands:

```clojure
(def a (atom 0))
(repeatedly 200
  (fn [] (future (swap! a inc))))
```

The result **will not** be 200 on Clojure - because Chlorine sometimes do not evaluate the full lazy sequence. If you want to be sure that you're getting the right results, wrap `repeatedly` in a `doall` call. Also, please notice that using lazy sequences for side-effects without making sure that it's fully realized **is considered a bug** - do not do it in production, or you will have really hard to debug issues.

### When I evaluate `*1`, the result is always `nil`
Chlorine / Clover does not support `*1`, `*2`, `*3`, nor `*e`. The reason is simple: when you have a `ns` form on your editor, the tooling will detect it and evaluate `in-ns` with that namespace, so that you'll always be on the right namespace when evaluating. This also means that if your editor does have a filename, it'll also send that filename as a command.

So, if you do have a filename and a namespace for (90% of the time you do), Chlorine will send:

`(in-ns 'the.current.namespace)` <- this binds `*1`

`(set-filename-and-row-cmd.....)` <- this re-binds `*1`

`(your code here)` <- this re-binds `*1`

As you can see, now the evaluation result is on `*1`, but when you issue another command, it'll re-bind `*1` two times, moving it to `*3` by the time it evaluates your code. So, effectively, `*3` works as `*1` and the others are just leaking abstractions - nothing to do here.

As for `*e`, it _does work_ only in Clojure - but again, it works "by coincidence". Exceptions are not really "serializable" so what this package do is to wrap the exception into an EDN and send it over the wire - so `*e` is never really bound to anything.
