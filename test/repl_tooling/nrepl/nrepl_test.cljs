(ns repl-tooling.nrepl.nrepl-test
  (:require [devcards.core :as cards]
            [clojure.test :refer [testing]]
            [check.core :refer [check] :as c]
            [clojure.core.async :as async]
            [check.async :refer [async-test await!]]
            [repl-tooling.nrepl.bencode :as bencode]))

(cards/deftest bencode
  (testing "encode booleans"
    (check (bencode/encode true) => "1")
    (check (bencode/encode false) => "0"))

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
  (let [results (atom [])
        dec! (bencode/decoder #(swap! results conj %))
        decode! (fn [frag]
                  (reset! results [])
                  (dec! frag))]

    (testing "decode booleans"
      (decode! "10")
      (check @results => [true])
      (decode! "1")
      (check @results => [false]))

    (testing "decode numbers"
      (decode! "i") ;trailing `true`
      (decode! "210e")
      (check @results => [210])
      (decode! "i21ei20e")
      (check @results => [21 20]))

    (testing "decode partially"
      (decode! "i21")
      (check @results => [])
      (decode! "0e01")
      (check @results => [210 false])
      (decode! "i0e")) ;trailing `true`

    (testing "decode strings"
      (decode! "3:fo")
      (check @results => [])
      (decode! "o")
      (check @results => ["foo"])

      (decode! "1")
      (check @results => [])
      (decode! ":a")
      (check @results => ["a"]))))

  ; (testing "encode keywords"
  ;   (check (bencode/encode :foo) => "3:foo")
  ;   (check (bencode/encode :foo/bar) => "7:foo/bar"))
  ;
  ; (testing "encode lists"
  ;   (check (bencode/encode ["a" "b"]) => "l1:a1:be")
  ;   (check (bencode/encode '("a" "b")) => "l1:a1:be"))
  ;
  ; (testing "encode maps"
  ;   (check (bencode/encode {"a" "b"}) => "d1:a1:be")))
