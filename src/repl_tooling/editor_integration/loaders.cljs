(ns repl-tooling.editor-integration.loaders
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as str]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.evaluation :as e-eval]))

(defn- eval-code [code {:keys [editor-data notify evaluate on-eval]}]
  (let [filename (:filename editor-data)]
    (set! helpers/*out-on-aux* true)
    (.. (evaluate {:text code :auto-opts true :aux true})
        (then #(notify {:type :info :title "Loaded file" :message filename}))
        (catch (fn [error]
                 (notify {:type :error :title "Error loading file" :message filename})
                 (on-eval {:id (gensym "load-file")
                           :repl nil ; FIXME: get the right REPL
                           :range [[0 0] [0 0]]
                           :editor-data editor-data
                           :result error})))
        (finally #(set! helpers/*out-on-aux* false)))))

(defn- do-load-file [{:keys [editor-data] :as options}]
  (let [filename (str/replace (:filename editor-data) "\\" "/")
        code (str "(do"
                  " (require 'clojure.string)"
                  " (println \"Loading\" \"" filename "\")"
                  " (try "
                  "  (let [path \"" filename "\""
                  ;; if target REPL is running on *nix-like O/S...
                  "        nix? (clojure.string/starts-with? (System/getProperty \"user.dir\") \"/\")"
                  ;; ...and the file path looks like Windows...
                  "        win? (clojure.string/starts-with? (subs path 1) \":/\")"
                  ;; ...extract the driver letter...
                  "        drv  (clojure.string/lower-case (subs path 0 1))"
                  ;; ...and map to a Windows Subsystem for Linux mount path:
                  "        path (if (and nix? win?) (str \"/mnt/\" drv (subs path 2)) path)]"
                  "   (clojure.core/load-file path))))")]
    (eval-code code options)))

(defn- do-load-file-simple [{:keys [editor-data] :as options}]
  (let [filename (str/replace (:filename editor-data) "\\" "/")
        code (str "(clojure.core/load-file \"" filename "\")")]
    (eval-code code options)))

(defn load-file [editor-data state]
  (let [{:keys [notify get-config on-eval]} (:editor/callbacks state)
        repl-kind (-> state :repl/info :kind)
        options {:notify notify
                 :evaluate (-> state :editor/features :eval)
                 :on-eval on-eval
                 :editor-data editor-data}]
    (if-let [filename (:filename editor-data)]
      (cond
        (e-eval/need-cljs? (get-config) filename)
        (notify {:type :error
                 :title "Can't load-file in a CLJS file"
                 :message "ClojureScript files are not supported to load file"})

        (= :clj repl-kind) (do-load-file options)

        :else (do-load-file-simple options))

      (notify {:type :error
               :title "No file to load"
               :message (str "Can't find a file to load. Please, ensure that "
                             "you're editing a saved file.")}))))
