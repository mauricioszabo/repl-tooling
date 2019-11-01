(ns repl-tooling.editor-integration.loaders
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.evaluation :as e-eval]))

(defn- do-load-file [filename repl notify]
  (let [filename (str/replace filename "\\" "/")
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
                  "   (load-file path))"
                  "  (catch Throwable t"
                  "   (doseq [e (:via (Throwable->map t))]"
                  "    (println (:message e))))))")]
    (eval/evaluate repl
                   code
                   {:namespace "user" :ignore true}
                   #(notify {:type :info :title "Loaded file" :message filename}))))

(defn load-file [{:keys [notify get-config] :as opts}
                 {:keys [repl-kind repl-name repl editor-data]}]
  (if-let [filename (:filename editor-data)]
    (cond
      (e-eval/need-cljs? (get-config) filename)
      (notify {:type :error
               :title "Can't load-file in a CLJS file"
               :message "ClojureScript files are not supported to load file"})

      (= :clj repl-kind)
      (do-load-file filename repl notify)

      :else
      (notify {:type :error
               :title "Can't load file for this kind of REPL"
               :message (str "Loading files on " repl-name " is not supported.")}))
    (notify {:type :error
             :title "No file to load"
             :message (str "Can't find a file to load. Please, ensure that "
                           "you're editing a saved file.")})))
