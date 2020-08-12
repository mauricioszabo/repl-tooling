# CHANGELOG

## 0.5.1
- Shadow-CLJS can call its own internal API by passing a specific command
- Support for resolving promises on Shadow Relay API
- `tap>` support for Shadow-CLJS Relay API
- Clickable stacktraces for Clojerl
- Better printer for Clojerl and nREPL
- Cutting some stdout messages from aux REPL

## 0.5.0
- Performance improvement while parsing Clojure code
- Runnable config
- Fix on `get-selection` for configs in ClojureScript
- `editor/eval-and-render` now resolves as promises
- Forward-finding namespaces if a NS form was not found before the cursor (fixes https://github.com/mauricioszabo/atom-chlorine/issues/193)
- Waiting for REPL results before forwarding new commands to the REPL (probably fixes https://github.com/mauricioszabo/atom-chlorine/issues/192)

## 0.4.4
- Interactive renderer support

## 0.4.1
- Fixed https://github.com/mauricioszabo/atom-chlorine/issues/150
- Alpha support for nREPL

## 0.4.0
- Add support for more than one form eval on Clojure REPL
- Fixed goto var definition on CLJS
- Removed LOTS of old code
- Adding support for Suitable autocomplete

## 0.2.1
- Fixed prompt appearing on ClojureScript
- Fixed `:pass` opts not being redirected to editor (fixes doc)
- Fixed exception messages
- Autocomplete crashing when prefix is null or empty

## 0.2.0
- Clojure socket REPL with UnREPL
- ClojureScript self-hosted REPL (run a command to start a CLJS REPL)
- Support for detecting forms/comments/namespaces
- AutoComplete on ClojureScript with Compliment
- Renderer for results and errors
- Autocomplete for clj with compliment
- Break on Clojure (https://github.com/mauricioszabo/repl-tooling/issues/6)
- Fixed "burst commands" (https://github.com/mauricioszabo/repl-tooling/issues/24)

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
