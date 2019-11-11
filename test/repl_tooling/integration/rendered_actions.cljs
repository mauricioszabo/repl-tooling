(ns repl-tooling.integration.rendered-actions
  (:require [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.integration.fake-editor :refer [editor evaluate state connect!]]
            [repl-tooling.integration.ui-macros :as ui :include-macros true]
            [clojure.test :refer [async testing is] :include-macros true]
            [devcards.core :as cards :include-macros true]))

(cards/defcard-rg fake-editor
  editor
  state
  {:inspect-data true})
