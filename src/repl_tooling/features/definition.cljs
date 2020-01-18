(ns repl-tooling.features.definition
  (:require [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as editor-helpers]
            [promesa.core :as p]
            ["fs" :as fs]))

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

(defn- full-file-position [meta]
  (js/Promise.
   (fn [resolve]
     (fs/exists (:file meta) #(if %
                                (resolve (assoc meta
                                                :file-name (:file meta)
                                                :line (-> meta :line dec)))
                                (resolve nil))))))

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
(defn- from-classpath [clj-aux meta]
  (p/catch
   (p/let [pos (classpath-meta->positions clj-aux meta)]
     (wrap-contents clj-aux (:result pos)))
   (fn [_] nil)))

(defn- from-clr [clj-repl meta]
  (p/let [from-repl (eval/eval clj-repl
                               (str "(clojure.core/let [m '" meta "]"
                                    "  (clojure.core/some->> m"
                                    "    :file"
                                    "    (clojure.lang.RT/FindFile)"
                                    "    str))"))]
    {:file-name (:result from-repl) :line (-> meta :line dec)}))

(defn find-var-definition [cljs-repl clj-aux ns-name symbol-name]
  (p/let [cmd (str "(clojure.core/->> `" symbol-name " "
                   "clojure.core/resolve "
                   "clojure.core/meta "
                   "clojure.core/vec "
                   "(clojure.core/into {})"
                   ")")
          meta (eval/eval cljs-repl cmd {:namespace ns-name :ignore true})
          meta (select-keys (:result meta)
                            [:name :file :status :column :line])
          ; FIXME: use REPL detection here
          with-contents (full-file-position meta)
          with-contents (or with-contents (from-classpath clj-aux meta))
          with-contents (or with-contents (from-clr clj-aux meta))]
    with-contents))
