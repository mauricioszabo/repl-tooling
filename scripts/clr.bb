(println "Starting the process...")
(require '[babashka.process :as p])

(println "Running Socket REPL")
(def p
  (p/process
   [
    "/root/.dotnet/tools/Clojure.Main" "-e"
    (pr-str (quote (do (clojure.core.server/start-server {:name "test"
                                                          :port 4444
                                                          :accept (quote clojure.core.server/repl)})
                     (System.Threading.Thread/Sleep 60000))))]
   {:inherit true
    :shutdown p/destroy-tree}))

(println "Waiting...")
(when-not (babashka.wait/wait-for-port "127.0.0.1" 4444 {:timeout 60000})
  (println "Somehow, things didn't start...")
  @p
  (System/exit 1))

(println "Running Tests")
@(p/process ["node" "target/fixture.js" "somefile.clj"]
            {:inherit true
             :shutdown p/destroy-tree})
