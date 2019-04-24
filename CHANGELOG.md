# CHANGELOG

## TODO yet
- Documentation for multiple formats (IncompleteStr, LiteralRender)
- Port to ShadowCLJS
- Tests for Clojure and ClojureScript
- Add "inferior clojure" support (open up a REPL using processes)
- Autocomplete for Lumo
- Auto-detect of features
- Add support to `id` for Generic REPL
- Add support for more than one form eval on Clojure REPL

## Next Version
- Renderer for results and errors
- Autocomplete for clj with compliment
- Autocomplete for CLJS (only functions)
- Clojure socket REPL with UnREPL
- ClojureScript self-hosted REPL (run a command to start a CLJS REPL)
- Preliminary support for detecting forms/comments/namespaces

## 0.0.2
- Fixed a bug when we could interleave REPL commands
- Autocomplete for Lumo (the same that's used on the REPL)
- Add support for ..more.. in Clojure REPL
- Upgradable Clojure REPL (unrepl)
- BREAK on Clojure's REPL

## 0.0.1
- Evaluator, and eval commands
- Added Lumo support (_quasi-upgradable_)
- Added basic Socket REPL connection
- Added support for `id` to Lumo
