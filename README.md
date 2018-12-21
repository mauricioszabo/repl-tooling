# Clojure REPL's Tooling

This library's idea is to work as a base package for Clojure's editor tooling. The idea is to work in multiple environments, it is written in ClojureScript so it'll work with Atom, VS Code, NeoVIM, and other editors that are able to run JS.

## Design decisions
This library is not made to be stand-alone, but to work as base for editor plug-ins (or web interfaces, etc). This means that there is no "main entry point" - there's only one entry on `shadow-cljs.edn` file to run tests.

This lib is still very crude: it's, right now, being developed together with Atom Chlorine (that's the reason is not yet published on Clojars or NPM). The best way to experiment with it right now is to add a submodule on your project and ask for Shadow-CLJS to add `repl-tooling/src` (or whatever the path you configured for your submodule) as a source-path. It would be great if we didn't add too many dependencies (for now, it just depends on core-async) for it to work, as probably specific editor's will pull their dependencies and they may conflict with this lib's deps (and also because of security issues).

This lib depends on `lein` because of unrepl blob. It is not clear how different editors will handle paths, so to sideload files (like, for example, the `unrepl` blob) I'm using the `resources` folder, and generating a macro. This means that when we compile a release version, the contents of the files will be inlined on the end code.

Also, there can't be specific editor's code inside repl-tooling. We can't priorize one editor from another, exactly because we need to find common ground between all editors. This have the huge advantage that, when we add a feature on repl-tooling, all editor's plug-ins will automatically have that new feature. Also, when we fix a bug, all editor's plug-ins will have the bug fixed.

## Plans for the future / documentation
All editors share a common ground: they need to evaluate code. When they can do it, we can use Compliment's autocomplete, external libraries, refactor-nrepl, cider, and other tools. So, the editor will need to ask this library for some informations:

1. How do I connect to a socket REPL on this specific host/port?
1. After connecting, how do I evaluate a specific code? What is the result of this specific evaluation?
1. How can I complete this symbol on this file/position?
1. What are the commands that this tooling supports?

For it to work seamlessly, this library will need to query the editor on some questions too:
1. We're connected, so use these REPLs to work with code (and update your UI to show the user)
1. We're disconnected, update your UI to show it to user
1. There's a stdout/stderr information, what do you want to do with it?
1. What is the content of the current editor?
1. What is the current position (begin-end) of the current editor?
1. Evaluation finished, here's the result. Update your UI with this information

Probably there will be other information that we will need. What I can think of is:
1. The editor's plug-in query for commands, and send together a map with information on how to get editor contents/position/selection/etc
1. This tooling will return the commands it supports (evaluate block, top-block, selection, autocomplete, etc) as a map of keywords/fns
1. This tooling will also return the configs it expects to work (for example, if we want to use full refresh with `tools.namespace`, or simple refresh with `require :reload`).
1. When we want to run a specific command we'll run the `fn` passing a fixed map with all configs that we need (commands that don't need specific config keys can simply ignore they)

This plug-in will also need to work around UNREPL. Unrepl is great to use, but sometimes it can be tricky: for example, when we have a big exception like `ex-info`, it'll not show `:cause` or `:trace` keys. This means that we'll need to lazy-fetch these infos until they are present

# A semi-organized TODO

* [x] UNREPL client (for Clojure's REPLs)
* [x] Socket REPL client for Clojure
* [x] Socket REPL client for self-hosted ClojureScript
* [x] Socket REPL client for Lumo
* [x] AutoComplete detector (Lumo, Clojure, ClojureScript)
* [ ] Detect beginning and ending of forms (for editor to evaluate)
* [ ] Detect NS of current file
* [ ] Detect features for refactoring, refresh, auto-import, organize namespace, and others

For evaluation, three events must be suported: `evaluate`, `break`, and detection of disconnect.

It is important that notice that this library needs to distinguish between output and result of commands - if not possible, result of commands should be priorized uniquely because it's essential for detection of features, autocompletes, and interaction with external libraries.
