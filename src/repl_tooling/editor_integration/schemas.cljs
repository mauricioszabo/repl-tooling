(ns repl-tooling.editor-integration.schemas
  (:require [clojure.spec.alpha :as s]))

(s/def ::contents string?)
(s/def ::filename string?)
(s/def ::row-col (s/tuple int? int?))
(s/def ::range (s/tuple ::row-col ::row-col))
(s/def ::editor-result (s/keys :req-un [::contents ::filename ::range]))

(s/def ::id symbol?)
(s/def ::eval-data (s/keys :req-un [::id ::editor-result ::range]))
(s/def ::eval-result (s/keys :req-un [::id ::editor-result ::range ::result]))

(s/def ::on-start-eval (s/fspec :args (s/cat :eval-data ::eval-data)))
(s/def ::on-eval (s/fspec :args (s/cat :eval-data ::eval-result)))
(s/def ::editor-data (s/fspec :ret ::editor-result))

(s/def ::type #{:info :warning :error})
(s/def ::title string?)
(s/def ::message string?)
(s/def ::notify (s/fspec :args (s/keys :req-un [::type ::title ::message])))

(s/def ::project-paths (s/coll-of string?))
(s/def ::eval-mode #{:clj :cljs :prefer-clj :prefer-cljs})
(s/def ::configs (s/keys :req-un [::project-paths ::eval-mode]))
(s/def ::get-config (s/fspec :ret ::configs))

(s/def ::prompt (s/fspec :ret #(instance? js/Promise %)))

(s/def ::on-stdout (s/fspec :args (s/cat :txt string?)))
(s/def ::on-stderr (s/fspec :args (s/cat :txt string?)))
(s/def ::on-result (s/fspec :args (s/cat :parsed-result any?)))
(s/def ::on-disconnect fn?)

(s/def ::callbacks (s/keys :req-un [::on-start-eval
                                    ::on-eval
                                    ::editor-data
                                    ::notify
                                    ::get-config
                                    ::prompt
                                    ::on-stdout
                                    ::on-stdout
                                    ::on-stderr
                                    ::on-result
                                    ::on-disconnect]))
