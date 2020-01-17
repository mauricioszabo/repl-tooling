(ns repl-tooling.features.definition
  (:require [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as editor-helpers]
            [promesa.core :as p]))

(defn- cmd-for-read-jar [jar-file-name]
  `(~'clojure.core/let [[jar# path#] (~'clojure.string/split ~jar-file-name #"!/" 2)
                        jar# (~'clojure.string/replace-first jar# #"file:" "")
                        jar-file# (java.util.jar.JarFile. jar#)
                        ba# (java.io.ByteArrayOutputStream.)
                        is# (.getInputStream jar-file# (.getJarEntry jar-file# path#))]
     (~'clojure.java.io/copy is# ba#)
     (java.lang.String. (.toByteArray ba#))))

(defn- wrap-contents [repl {:keys [file-name line]}]
  (when (string? file-name)
    (if (re-find #"\.jar!/" file-name)
      (p/let [{:keys [result]} (eval/eval repl (cmd-for-read-jar file-name))]
        {:file-name file-name :line (dec line) :contents result})
      {:file-name file-name :line (dec line)})))

(defn- classpath-meta->positions [clj-repl meta]
  (p/do!
    (eval/eval clj-repl "(clojure.core/require 'clojure.java.io)")
    (eval/eval clj-repl
               (str "(clojure.core/let [m '" meta "]"
                    "  (clojure.core/assoc m :file-name "
                    "                      (or (clojure.core/some->> m"
                    "                            :file"
                    "                            (.getResource (clojure.lang.RT/baseLoader))"
                    "                            .getPath)"
                    "                          (:file m))))"))))

(defn find-var-definition [cljs-repl clj-aux ns-name symbol-name]
  (p/let [cmd (str "(clojure.core/meta (clojure.core/resolve `" symbol-name "))")
          meta (eval/eval cljs-repl cmd {:namespace ns-name :ignore true})
          meta (select-keys (:result meta) [:name :file :status :column :line])
          pos (classpath-meta->positions clj-aux meta)
          with-contents (wrap-contents clj-aux (:result pos))]
    ; (prn :POS (keys with-contents))
    with-contents))
