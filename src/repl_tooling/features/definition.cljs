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

(defn- get-result [repl [file-name line]]
  (when (string? file-name)
    (if (re-find #"\.jar!/" file-name)
      (p/then (eval/eval repl (cmd-for-read-jar file-name))
              (fn [c] {:file-name file-name :line (dec line) :contents (:result c)}))
      {:file-name file-name :line (dec line)})))

(defn- cmd-for-filename [the-var]
  `(~'clojure.core/let [res# (~'clojure.core/meta (~'clojure.core/resolve (quote ~the-var)))]
     (~'clojure.core/require 'clojure.java.io)
     [(~'clojure.core/or (~'clojure.core/some->> res# :file
                           (.getResource (~'clojure.lang.RT/baseLoader))
                           .getPath)
                         (:file res#))
      (:line res#)]))

(defn find-var-definition [repl ns-name symbol-name]
  (p/let [fqn (eval/eval repl (str "`" symbol-name) {:namespace ns-name :ignore true})
          data (eval/eval repl (cmd-for-filename (:result fqn)))]
    (get-result repl (:result data))))
