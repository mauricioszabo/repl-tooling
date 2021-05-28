(ns repl-tooling.features.definition
  (:require [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [promesa.core :as p]
            [com.wsscode.pathom.connect :as connect]
            [repl-tooling.template :as template]
            ["fs" :as fs]
            ["os" :refer [platform]]))

(defn- norm-result [file-name]
  (cond-> file-name
          (and (re-find #"^win\d+" (platform)))
          (str/replace-first #"^/" "")))

(defn- read-jar [clj jar-file-name]
  (let [[jar path] (str/split jar-file-name #"!/" 2)
        jar (clojure.string/replace-first jar #"file:" "")
        template
        `(let [jar-file# (java.util.jar.JarFile. jar)
               ba# (java.io.ByteArrayOutputStream.)
               is# (.getInputStream jar-file# (.getJarEntry jar-file# path))]
           (clojure.java.io/copy is# ba#)
           (java.lang.String. (.toByteArray ba#)))
        code (template/template template {:jar jar :path path})]
    (eval/eval clj code)))

(defn- classpath->full-position [clj meta]
  (p/let [code (template/template `(do
                                     (require 'clojure.java.io)
                                     (some->> file
                                              (.getResource (clojure.lang.RT/baseLoader))
                                              .getPath))
                                  {:file (:file meta)})
          {:keys [result]} (eval/eval clj code)]
    result))

(connect/defresolver file-from-classpath [{:keys [:repl/clj :var/meta]}]
  {::connect/output [:definition/file-contents :definition/filename]}
  (p/let [file-name (classpath->full-position clj meta)
          file-name (norm-result file-name)]
    (when (string? file-name)
      (if (re-find #"\.jar!/" file-name)
        (p/let [{:keys [result]} (read-jar clj file-name)]
          {:definition/filename file-name :definition/file-contents result})
        {:definition/filename file-name}))))

(connect/defresolver file-from-clr [{:keys [:repl/clj :var/meta]}]
  {::connect/output [:definition/filename]}
  (p/let [code (template/template `(some-> file
                                           clojure.lang.RT/FindFile
                                           str)
                                  {:file (:file meta)})
          {:keys [result]} (eval/eval clj code)]
    (when result
      {:definition/filename (norm-result result)}))
  (constantly nil))

(defn- fs-exists? [file]
  (new js/Promise (fn [resolve] (fs/exists file resolve))))

(connect/defresolver existing-filename [{:keys [:var/meta]}]
  {::connect/output [:definition/filename]}

  (p/then (fs-exists? (:file meta))
          #(when % {:definition/filename (-> meta :file norm-result)})))

(connect/defresolver position-resolver [{:keys [:var/meta]}]
  {::connect/output [:definition/row :definition/col]}

  (when-let [line (:line meta)]
    (cond-> {:definition/row (-> meta :line dec)}
            (:column meta) (assoc :definition/col (-> meta :column dec)))))

(connect/defresolver resolver-for-ns-only [{:keys [:repl/clj :var/fqn]}]
  {::connect/output [:var/meta :definition/row]}

  (when (-> fqn namespace nil?)
    (p/let [code (template/template `(some-> (find-ns 'namespace-sym)
                                             ns-interns
                                             first
                                             second
                                             meta)
                                  {:namespace-sym fqn})
            {:keys [result]} (eval/eval clj code)]
      (when result
        {:var/meta result
         :definition/row 0}))))

(connect/defresolver resolver-for-stacktrace [{:repl/keys [clj]
                                               :ex/keys [function-name filename row]}]
  {::connect/output [:var/meta :definition/row]}
  (p/let [ns-name (-> function-name (str/split #"/") first)
          code (template/template `(let [n# (find-ns 'namespace-sym)]
                                     (->> n#
                                          ns-interns
                                          (some (fn [[_# res#]]
                                                  (let [meta# (meta res#)
                                                        file# (-> meta# :file str)]
                                                    (and (clojure.string/ends-with?
                                                          file# file-name)
                                                         meta#))))))
                                  {:namespace-sym (symbol ns-name)
                                   :file-name filename})
          {:keys [result]} (eval/eval clj code)]
    {:var/meta result
     :definition/row (dec row)}))

(def resolvers [position-resolver existing-filename file-from-clr file-from-classpath
                resolver-for-ns-only resolver-for-stacktrace])
