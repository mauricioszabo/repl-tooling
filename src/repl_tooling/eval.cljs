(ns repl-tooling.eval
  (:refer-clojure :exclude [eval])
  (:require [cljs.core.async :refer [put! <! >! chan] :refer-macros [go go-loop]]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol MoreData
  (without-ellision [self]
    "Return the object without the {:repl-tooling/... (more-fn)} key")

  (get-more-fn [self]
    "Returns a function that'll receive an Evaluator and a callback
will call the callback with the same kind of object with more data"))

(defprotocol Evaluator
  (evaluate [this command opts callback])
  (break [this repl]))

(defn evaluator
  ([in out on-line] (evaluator in out on-line identity))
  ([in out on-line on-unexpected]
   (let [pending-cmds (atom {})]
     (go-loop []
       (let [{:keys [id out result]} (<! out)]
         (on-line out)
         (when out
           (if-let [handler (get @pending-cmds id)]
             (handler result)
             (on-unexpected result))))
       (recur))
     {:pending-cmds pending-cmds
      :in in
      :out out})))

(defn eval [evaluator command opts callback]
  (let [id (str "eval" (gensym))]
    (swap! (:pending-cmds evaluator) assoc id callback)
    (go (>! (:in evaluator) [id command]))
    id))

(defn- without-ellision-list [lst]
  (cond-> lst (-> lst last :repl-tooling/...) butlast))

(defn get-more-fn-list [lst]
  (when-let [fun (-> lst last :repl-tooling/...)]
    (fn more
      ([repl callback] (more repl true callback))
      ([repl combine? callback]
       (evaluate repl fun {:ignore true}
                 #(let [res (-> % helpers/parse-result)]
                    (callback (cond->> (:result res)
                                       combine? (concat (without-ellision lst))))))))))

(defn- without-map [self]
  (dissoc self {:repl-tooling/... nil}))

(defn- get-more-map [self]
    (when-let [fun (get-in self [{:repl-tooling/... nil} :repl-tooling/...])]
      (fn more
        ([repl callback] (more repl true callback))
        ([repl combine? callback]
         (evaluate repl fun {:ignore true}
                   #(let [res (-> % helpers/parse-result)]
                      (callback (cond->> (:result res)
                                         combine? (merge self)))))))))

(extend-protocol MoreData
  helpers/IncompleteObj
  (without-ellision [_] nil)
  (get-more-fn [self] nil
    (fn more
      ([repl callback] (more repl true callback))
      ([repl _ callback]
       (evaluate repl
                 (:more-fn self)
                 {:ignore true}
                 #(let [parsed (helpers/parse-result %)
                        obj (:result parsed)
                        browsable (helpers/as-obj (cons nil obj))]
                    (callback browsable))))))
  helpers/Browseable
  (without-ellision [self]
    (:object self))

  (get-more-fn [self]
    (when-let [fun (or (:more-fn self) (get-more-fn (:attributes self)))]
      (fn more
        ([repl callback] (more repl true callback))
        ([repl combine? callback]
         (let [call #(let [not-ellided (without-ellision %)]
                       (callback (cond->> %
                                          combine? (assoc self
                                                          :more-fn nil
                                                          :attributes))))]
           (if (coll? fun)
             (evaluate repl fun {:ignore true} #(-> % helpers/parse-result :result call))
             (fun repl call)))))))

  helpers/WithTag
  (without-ellision [self]
    (helpers/WithTag. (without-ellision (helpers/obj self))
                      (helpers/tag self)))
  (get-more-fn [self]
    (when-let [fun (get-more-fn (helpers/obj self))]
      (fn more
        ([repl callback] (more repl true callback))
        ([repl combine? callback]
         (fun repl combine? #(helpers/WithTag. % (helpers/tag self)))))))

  helpers/IncompleteStr
  (without-ellision [self]
    (helpers/only-str self))
  (get-more-fn [self]
    (fn more
      ([repl callback] (more repl true callback))
      ([repl combine? callback]
       (let [fun (-> self meta :get-more)]
         (evaluate repl fun {:ignore true}
                   #(let [res (-> % helpers/parse-result)]
                      (callback (cond->> (:result res)
                                         combine? (helpers/concat-with self)))))))))

  cljs.core/PersistentHashMap
  (without-ellision [self] (without-map self))
  (get-more-fn [self] (get-more-map self))
  cljs.core/PersistentArrayMap
  (without-ellision [self] (without-map self))
  (get-more-fn [self] (get-more-map self))
  cljs.core/PersistentTreeMap
  (without-ellision [self] (without-map self))
  (get-more-fn [self] (get-more-map self))

  cljs.core/PersistentHashSet
  (without-ellision [self] (->> self (remove :repl-tooling/...) set))
  (get-more-fn [self]
    (when-let [fun (->> self (some :repl-tooling/...))]
      (fn more
        ([repl callback] (more repl true callback))
        ([repl combine? callback]
         (evaluate repl fun {:ignore true}
                   #(let [res (-> % helpers/parse-result)]
                      (callback (cond->> (:result res)
                                         combine? (into (without-ellision self))))))))))

  cljs.core/PersistentVector
  (without-ellision [self] (cond-> self
                                   (-> self last :repl-tooling/...) pop))
  (get-more-fn [self]
    (when-let [fun (-> self last :repl-tooling/...)]
      (fn more
        ([repl callback] (more repl true callback))
        ([repl combine? callback]
         (evaluate repl fun {:ignore true}
                   #(let [res (-> % helpers/parse-result)]
                      (callback (cond->> (:result res)
                                         combine? (into (without-ellision self))))))))))

  cljs.core/LazySeq
  (without-ellision [self] (without-ellision-list self))
  (get-more-fn [self] (get-more-fn-list self))

  cljs.core/List
  (without-ellision [self] (without-ellision-list self))
  (get-more-fn [self] (get-more-fn-list self))

  default
  (without-ellision [self] self)
  (get-more-fn [_] nil))
