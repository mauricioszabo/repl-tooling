#!/usr/bin/env bb

(defn- get-project! []
  (-> "project.clj"
      slurp
      edn/read-string
      (nth 1)))

(defn- get-version! []
  (-> "project.clj"
      slurp
      edn/read-string
      (nth 2)))

(defn- can-deploy? []
  (let [curr-version (get-version!)
        status (:status (curl/get (str "https://clojars.org/" (get-project!)
                                       "/versions/" (get-version!))
                                  {:throw false}))]
    (= 404 status)))

(defn- tag-name [] (System/getenv "CIRCLE_TAG"))

(defn- decode-base64 [string]
  (-> java.util.Base64
      .getDecoder
      (.decode string)))

(defn- import-gpg! []
  (let [secret (System/getenv "GPG_SECRET_KEYS")
        ownertrust (System/getenv "GPG_OWNERTRUST")]
    (when-not (and secret ownertrust) (throw (ex-info "Can't find GPG keys!" {})))
    (shell/sh "gpg" "--import" :in (decode-base64 secret))
    (shell/sh "gpg" "--import-ownertrust" :in (decode-base64 ownertrust))))

(defn deploy! []
  (let [tag (tag-name)]
    (when-not (can-deploy?)
      (throw (ex-info "Can't deploy this version - release version already exist on clojars"
                      {:version (get-version!)})))

    (when (some-> tag (str/replace-first #"v" "") (not= (get-version!)))
      (throw (ex-info "Tag version mismatches with project.clj"
                      {:tag-name tag
                       :version (get-version!)})))

    (if tag
      (do
        (import-gpg!)
        (println "Deploying a release version"))
      (do
        (println "Deploying a snapshot version")
        (shell/sh "lein" "change" "version" "str" "\"-SNAPSHOT\"")))

    (shell/sh "lein" "change" ":deploy-repositories" "concat"
              (pr-str [["releases" {:url "https://clojars.org/repo/"
                                    :username :env/clojars_login
                                    :password :env/clojars_password}]]))
    (shell/sh "lein" "deploy" "releases")))

(deploy!)
