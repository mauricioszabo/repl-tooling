(ns repl-tooling.editor-integration.renderer
  (:require [reagent.core :as r]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol Renderable
  (tp [this])
  (ellide-fn [this ratom])
  (children-fn [this])
  (as-string [this])
  (as-html [this ratom]))

#_
(type '(1 2))
#_
(type [1 2 3 4])
#_
(type #{})
#_
(type {:a 10 :b 20})

; (defn- coll-object)

(extend-protocol Renderable
  cljs.core/List
  (tp [_] "list")
  (as-string [obj] (pr-str (concat (butlast obj) '(...))))
  (as-html [obj ratom]
    [:div
     [:div {:class "list"}
      [:span {:class "delim"} "("]
      (doall (map (fn [e i]
                    [:<> {:key i}
                     (as-html e (r/cursor ratom i))
                     [:span {:class "space"} " "]])
                  (cond-> obj)
                          ; (if delim?))
                  (range)))
      [:span {:class "delim"} ")"]]])

  default
  (tp [obj] (cond
              (string? obj) "string"
              (number? obj) "number"
              (boolean? obj) "bool"
              (nil? obj) "nil"
              :else "other"))
  (as-string [obj] (pr-str obj))
  (as-html [obj _]
    (let [tp (cond
               (string? obj) "string"
               (number? obj) "number"
               (boolean? obj) "bool"
               (nil? obj) "nil"
               :else "other")]
      [:span {:class tp} (pr-str obj)])))

; (defrecord Literal [obj]
;   Renderable
;   (tp [_] (cond
;             (string? obj) "string"
;             (number? obj) "number"
;             (boolean? obj) "bool"
;             (nil? obj) "nil"
;             :else "other"))
;   (as-string [_] (pr-str obj)))

(defn parse-result
  "Will parse a result that comes from the REPL in a r/atom so that
it'll be suitable to be rendered with `view-for-result`"
  [evaluator result]
  (let [parsed (helpers/parse-result result)
        res (:result parsed)]
    (if res
      (r/atom res)
      (with-meta (r/atom (:error result)) {:error true}))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state]
  (as-html @state state))
  ; [:div {:class (tp @state)} (as-string @state)])
