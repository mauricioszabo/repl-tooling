# Organization of the Project

This project is meant to be consumed by an editor plug-in. But the developing of this project must follow some order, otherwise things get confusing __really fast__.

## Namespaces

Editors should consume from the `editor-integration` namespace. The `editor-integration` and its children is **the only namespace** allowed to depend on `commands` and `state`.

`state` is an Atom that defines the "current connection state/commands" of the editor. The `state` currently is composed by the following map:

```clojure
{:clj/aux a-repl-for-commands
 :clj/repl a-repl-for-evaluation
 :cljs/repl a-repl-for-evaluation
 :repl/info {:host string? :port integer?}
 :editor/commands {:evaluate-top-block {:name string?
                                        :description string?
                                        :command fn?}
                   ,,,}}
```

The `commands` is the huge map that's passed to `editor-integration.connection/connect!`, with callbacks on how to call the editor's internal state when it is needed.

The `editor-integration` namespace is responsible for "wiring things": to pick up an extreme example, see ClojureScript's autocomplete: it can work in three ways: if you're running Lumo, you want to use the internal APIs of Lumo to autocomplete things; if you're not running Lumo, you can be running another "self-hosted" or "bootstrapped" ClojureScript REPL like Plank or Shadow-CLJS' target `:bootstrap`, or you can be running Figwheel, Shadow-CLJS, etc (REPLs that run inside the JVM).

If you're into the last case, autocomplete can be done with the Compliment library, but it needs to be run inside **Clojure**, not ClojureScript. In this case, it needs to know the full state of the editor (and see if there's a `:clj/aux` REPL connected), otherwise things will not work. But if there's no `:clj/aux` REPL or if there's Compliment libary, it'll fallback to the "simple" autocomplete defined on `repl-tooling/features/autocomplete/simple-cljs`.

On `features` namespace, things should be simpler: you can't access the "full state" of the editor integration, but you shouldn't be limited on what parameters you can receive, considering that they **need to be** independent (so, no "maps with the same structure as `state`", for example). The reason for that is to not have a dependency between the _schema_ that `editor-integration` expects, because this schema **will change** over time, and it's better for the other parts of the code to not be aware of that.
