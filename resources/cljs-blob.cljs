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
