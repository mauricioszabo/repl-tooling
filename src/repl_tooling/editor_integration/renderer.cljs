(ns repl-tooling.editor-integration.renderer
  (:require [reagent.core :as r]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol Renderable
  (tp [this])
  (as-string [this]))

(defrecord Literal [obj]
  Renderable
  (tp [_] (cond
            (string? obj) "string"
            (number? obj) "number"
            (boolean? obj) "bool"
            (nil? obj) "nil"
            :else "other"))
  (as-string [_] (pr-str obj)))

(defn parse-result
  "Will parse a result that comes from the REPL in a r/atom so that
it'll be suitable to be rendered with `view-for-result`"
  [evaluator result]
  (let [parsed (helpers/parse-result result)
        res (:result parsed)]
    (if res
      (r/atom (->Literal res))
      (with-meta (r/atom (->Literal (:error result))) {:error true}))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state]
  [:div {:class (tp @state)} (as-string @state)])
