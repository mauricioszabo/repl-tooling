(extend-protocol IPrintWithWriter
  js/Error
  (-pr-writer [ex writer _]
    (-write writer "#error ")
    (-write writer {:type (.-name ex)
                    :message (.-message ex)
                    :trace (->> ex
                                .-stack
                                clojure.string/split-lines)}))

  cljs.core/ExceptionInfo
  (-pr-writer [ex writer _]
    (-write writer "#error ")
    (-write writer {:type "cljs.core.ExceptionInfo"
                    :data (.-data ex)
                    :message (.-message ex)
                    :trace (->> ex
                                .-stack
                                clojure.string/split-lines)})))

(require '[reagent.ratom])
(when (.. js/goog -global -reagent -core)
  (extend-protocol IPrintWithWriter
    reagent.ratom/RAtom
    (-pr-writer [self writer _]
      (-write writer "#reagent.ratom.RAtom ")
      (-write writer (pr-str @self)))

    reagent.ratom/RCursor
    (-pr-writer [self writer _]
      (-write writer "#reagent.ratom.RCursor ")
      (-write writer (pr-str [(.-path self) @self])))))
