(ns repl-tooling.nrepl.nrepl-test
  (:require [devcards.core :as cards]
            [clojure.test :refer [testing is]]
            [check.core :refer [check] :as c]
            [clojure.core.async :as async]
            [check.async :refer [async-test await!]]
            [repl-tooling.nrepl.bencode :as bencode]))

(cards/deftest bencode
  (testing "encode numbers"
    (check (bencode/encode 210) => "i210e"))

  (testing "encode strings"
    (check (bencode/encode "foo") => "3:foo")
    (check (bencode/encode "á") => "2:á"))

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

    (testing "decode numbers"
      (check (decode! "i210e") => [210])
      (check (decode! "i-210e") => [-210])
      (check (decode! "i21ei20e") => [21 20]))

    (testing "decode partially"
      (check (decode! "i21") => [])
      (check (decode! "0ei20e") => [210 20]))

    (testing "decode strings"
      (check (decode! "3:fo") => [])
      (check (decode! "o") => ["foo"])

      (check (decode! "1:") => [])
      (check (decode! "i") => ["i"]))

    (testing "decode lists"
      (check (decode! "li0ei2ee") => [[0 2]]))

    (testing "decode maps"
      (check (decode! "d1:a1:be") => [{"a" "b"}]))

    (testing "decode nested data"
      (check (decode! "d1:a1:bi0eli0ei2eee") => [{"a" "b", 0 [0 2]}]))))
