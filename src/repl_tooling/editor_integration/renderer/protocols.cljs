(ns repl-tooling.editor-integration.renderer.protocols)

(defprotocol Parseable
  (as-renderable [self repl editor-state]))

(defprotocol Renderable
  (as-html [this ratom root?])
  (as-text [this ratom root?]))
