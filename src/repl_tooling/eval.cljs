(ns repl-tooling.eval
  (:refer-clojure :exclude [eval])
  (:require [cljs.core.async :refer [<! >! chan] :refer-macros [go go-loop]]))

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
