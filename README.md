# Clojure REPL's Tooling

This library's idea is to work as a base package for Clojure's editor tooling. As the idea is to work in multiple environments, it is written in ClojureScript so it'll work with Atom, VS Code, NeoVIM, and other editors that are able to run JS.

## What this library will do:

* [ ] UNREPL client (for Clojure's REPLs)
* [ ] Socket REPL client for Clojure
* [ ] Socket REPL client for self-hosted ClojureScript
* [ ] Socket REPL client for Lumo
* [ ] AutoComplete detector (Lumo, Clojure, ClojureScript)
* [ ] Detect beginning and ending of forms (for editor to evaluate)
* [ ] Detect NS of current file
* [ ] Detect features for refactoring, refresh, auto-import, organize namespace, and others

For evaluation, three events must be suported: `evaluate`, `break`, and detection of disconnect.

It is important that notice that this library needs to distinguish between output and result of commands - if not possible, result of commands should be priorized uniquely because it's essential for detection of features, autocompletes, and interaction with external libraries.
