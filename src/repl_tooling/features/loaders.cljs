(ns repl-tooling.features.loaders
  (:refer-clojure :exclude [load-file])
  (:require [clojure.string :as str]))

(defn load-file [{:keys [notify]} editor-data]
  (if-let [file (:filename editor-data)]
    (prn :do-the-magic)
    (notify {:type :error
             :title "No file to load"
             :message (str "Can't find a file to load. Please, ensure that "
                           "you're editing a saved file.")}))
  #_
  (let [editor (atom/current-editor)
        file-name (.getPath editor)
        ;; canonicalize path separator for Java -- this avoids problems
        ;; with \ causing 'bad escape characters' in the strings below
        file-name (str/replace file-name "\\" "/")
        code (str "(do"
                  " (require 'clojure.string)"
                  " (println \"Loading\" \"" file-name "\")"
                  " (try "
                  "  (let [path \"" file-name "\""
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
    (evaluate-aux editor
                  (ns-for editor)
                  (.getFileName editor)
                  1
                  0
                  code
                  #(atom/info "Loaded file" file-name))))
