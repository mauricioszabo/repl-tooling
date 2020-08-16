(ns repl-tooling.nrepl.nrepl-test
  (:require [devcards.core :as cards]
            [clojure.test :refer [testing is]]
            [check.core :refer [check] :as c]
            [clojure.core.async :as async]
            [check.async :refer [async-test await!]]
            [repl-tooling.nrepl.bencode :as bencode]
            [repl-tooling.integration.fake-editor :as editor]
            [repl-tooling.editor-integration.connection :as conn]))

(cards/deftest bencode
  (testing "encode numbers"
    (check (bencode/encode 210) => "i210e"))

  (testing "encode strings"
    (check (bencode/encode "foo") => "3:foo")
    (check (bencode/encode "foo\n") => "4:foo\n")
    (check (bencode/encode "á") => "2:á")
    (check (bencode/encode "Здравей, Свят!") => "25:Здравей, Свят!"))

  (testing "encode keywords"
    (check (bencode/encode :foo) => "3:foo")
    (check (bencode/encode :foo/bar) => "7:foo/bar"))

  (testing "encode symbols"
    (check (bencode/encode 'foo) => "3:foo")
    (check (bencode/encode 'foo/bar) => "7:foo/bar"))

  (testing "encode lists"
    (check (bencode/encode ["a" "b"]) => "l1:a1:be")
    (check (bencode/encode '("a" "b")) => "l1:a1:be"))

  (testing "encode maps"
    (check (bencode/encode {"a" "b"}) => "d1:a1:be")))

(cards/deftest decode
  (let [decode! (bencode/decoder)]
    (check (decode! "") => [])
    (testing "decode numbers"
      (check (decode! "i210e") => [210])
      (check (decode! "i-210e") => [-210])
      (check (decode! "i21ei20e") => [21 20]))

    (testing "decode partially"
      (check (decode! "i2") => [])
      (check (decode! "1") => [])
      (check (decode! "0ei20e") => [210 20]))

    (testing "decode strings"
      (check (decode! "0:") => [""])
      (check (decode! "3:foo") => ["foo"])

      (check (decode! "0") => [])
      (check (decode! ":") => [""])

      (check (decode! "3") => [])
      (check (decode! ":") => [])
      (check (decode! "fo") => [])
      (check (decode! "o") => ["foo"])

      (check (decode! "4:foo\n") => ["foo\n"])

      (check (decode! "1:") => [])
      (check (decode! "i") => ["i"])
      (check (decode! "1:") => [])
      (check (decode! "i") => ["i"]))

    (testing "decode multi-byte strings"
      (check (decode! "2:á3:lé") => ["á" "lé"]))))

(cards/deftest decode-lists
  (let [decode! (bencode/decoder)]
    (testing "full lists"
      (check (decode! "le") => [[]])
      (check (decode! "llee") => [[[]]])
      (check (decode! "llei10ee") => [[[] 10]]))

    (testing "multiple lists"
      (check (decode! "llei10eeli12ee") => [[[] 10] [12]]))

    (testing "lazily decode lists"
      (check (decode! "l") => [])
      (check (decode! "e") => [[]])

      (check (decode! "li0") => [])
      (check (decode! "ei") => [])
      (check (decode! "2ee") => [[0 2]]))

    (testing "lazily decode multiple lists"
      (check (decode! "li0ei2") => [])
      (check (decode! "eelli10eee") => [[0 2] [[10]]]))

    (testing "decode maps"
      (check (decode! "de") => [{}])
      (check (decode! "di10ei20eli11eedi99ei98eee") => [{10 20, [11] {99 98}}]))

    (testing "decode maps"
      (check (decode! "d1:a1:be") => [{"a" "b"}]))

    (testing "decode nested data"
      (check (decode! "d1:a1:bi0eli0ei2eee") => [{"a" "b", 0 [0 2]}]))))

(cards/defcard-rg fake-editor
  editor/editor
  editor/state)

(cards/deftest nrepl-connection
  (let [out (async/chan)]
    (async-test "connecting to a nREPL REPL" {:timeout 8000
                                              :teardown (do
                                                          (swap! editor/state assoc
                                                                 :port 2233)
                                                          (async/close! out)
                                                          (editor/disconnect!))}
      (swap! editor/state assoc :port 3322)
      (await! (editor/connect! {:on-stderr #(swap! editor/state update :stdout
                                                   (fn [e] (str e "ERR: " %)))}))
      (await! (editor/wait-for #(-> @editor/state :repls :eval)))

      (testing "evaluation works"
        (editor/type-and-eval "(+ 2 3)")
        (check (await! (editor/change-result)) => "5")
        (is (not (re-find #"=>" (editor/txt-for-selector "#stdout")))))

      (testing "exception works"
        (editor/type-and-eval "(/ 10 0)")
        (check (await! (editor/change-result)) => #"java.lang.ArithmeticException"))

      (testing "STDOUT works"
        (editor/type-and-eval "(prn :some-message)")
        (check (await! (editor/change-stdout)) => #":some-message"))

      (testing "STDERR works"
        (editor/type-and-eval "(binding [*out* *err*] (prn :some-error))")
        (check (await! (editor/change-stdout)) => #"ERR: :some-error"))

      (testing "break works"
        (editor/type-and-eval "(Thread/sleep 1000)")
        ; TODO - remove this delay
        (async/<! (async/timeout 200))
        ((-> @editor/state :commands :break-evaluation :command))
        (check (await! (editor/change-result)) => #"Interrupted")))))
