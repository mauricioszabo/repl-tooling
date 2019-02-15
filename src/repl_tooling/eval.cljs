(ns repl-tooling.eval
  (:refer-clojure :exclude [eval])
  (:require [cljs.core.async :refer [<! >! chan] :refer-macros [go go-loop]]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol MoreData
  (without-ellision [self]
    "Return the object without the {:repl-tooling/... (more-fn)} key")

  (get-more-fn [self]
    "Returns a function that'll receive an Evaluator and a callback
will call the callback with the same kind of object with more data"))

(defprotocol Evaluator
  (evaluate [this command opts callback])
  (break [this id]))

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
    (fn [repl callback]
      (evaluate repl fun {:ignore? true}
                #(let [res (-> % helpers/parse-result)]
                   (callback (concat (without-ellision lst) (:result res))))))))

(defn- without-map [self]
  (dissoc self {:repl-tooling/... nil}))

(defn- get-more-map [self]
    (when-let [fun (get-in self [{:repl-tooling/... nil} :repl-tooling/...])]
      (prn fun)
      (fn [repl callback]
        (evaluate repl fun {:ignore? true}
                  #(let [res (-> % helpers/parse-result)]
                     (callback (merge self (:result res))))))))

(extend-protocol MoreData
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
      (fn [repl callback]
        (evaluate repl fun {:ignore? true}
                  #(let [res (-> % helpers/parse-result)]
                     (callback (into (without-ellision self) (:result res))))))))

  cljs.core/PersistentVector
  (without-ellision [self] (pop self))
  (get-more-fn [self]
    (when-let [fun (-> self last :repl-tooling/...)]
      (fn [repl callback]
        (evaluate repl fun {:ignore? true}
                  #(let [res (-> % helpers/parse-result)]
                     (callback (into (without-ellision self) (:result res))))))))

  cljs.core/LazySeq
  (without-ellision [self] (without-ellision-list self))
  (get-more-fn [self] (get-more-fn-list self))

  cljs.core/List
  (without-ellision [self] (without-ellision-list self))
  (get-more-fn [self] (get-more-fn-list self))

  default
  (without-ellision [self] self)
  (get-more-fn [_] nil))
