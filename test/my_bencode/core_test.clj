(ns my-bencode.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [my-bencode.core :as my-ben :refer [gen-byte-seq
                                                read-netstring
                                                write-netstring
                                                write-bencode
                                                read-bencode]]))

(def ^{:private true} hello "Hello, world!")
(def ^{:private true} ham "Ham, Cheese & Bread")

;; read-netstring tests

(deftest read-netstring-tests
  (testing "read-netstring with empty string"
    (is (= (read-netstring "0:,") "")))
  (testing "read-netstring with hello"
    (is (= (read-netstring "13:Hello, world!,") hello)))
  (testing "read-netstring with ham"
    (is (= (read-netstring "19:Ham, Cheese & Bread,") ham)))
  (testing "read-netstring using the :buffer-size argument"
    (is (= (read-netstring "13:Hello, world!," :buffer-size 13) hello))))

;; write-netstring tests

(deftest write-netstring-tests
  (testing "write-netstring with empty string"
    (is (= (write-netstring (gen-byte-seq "")) "0:,")))
  (testing "write-netstring with hello"
    (is (= (write-netstring (gen-byte-seq hello)) "13:Hello, world!,")))
  (testing "write-netstring with ham"
    (is (= (write-netstring (gen-byte-seq ham)) "19:Ham, Cheese & Bread,"))))

;; write-bencode tests

(deftest write-bencode-tests
  (testing "write-bencode for byte strings"
    (is (= (write-bencode (gen-byte-seq "")) "0:"))
    (is (= (write-bencode (gen-byte-seq hello)) "13:Hello, world!"))
    (is (= (write-bencode (gen-byte-seq ham)) "19:Ham, Cheese & Bread")))
  (testing "write-bencode for strings"
    (is (= (write-bencode "") "0:"))
    (is (= (write-bencode hello) "13:Hello, world!"))
    (is (= (write-bencode ham) "19:Ham, Cheese & Bread")))
  (testing "write-bencode for integers"
    (is (= (write-bencode 42) "i42e"))
    (is (= (write-bencode 0) "i0e"))
    (is (= (write-bencode -8) "i-8e")))
  (testing "write-bencode for lists"
    (is (= (write-bencode nil) "le"))
    (is (= (write-bencode []) "le"))
    (is (= (write-bencode [1 2 3]) "li1ei2ei3ee"))
    (is (= (write-bencode '("see" "hear" "do")) "l3:see4:hear2:doe"))
    (is (= (write-bencode [1 "see" {:foo "bar"}]) "li1e3:seed3:foo3:baree")))
  (testing "write-bencode for maps"
    (is (= (write-bencode {}) "de"))
    (is (= (write-bencode {:foo 42 :bar "spam"}) "d3:bar4:spam3:fooi42ee"))
    (is (= (write-bencode {:foo 12 :bar "spam" :xyz [1 2 3] :abc {:pi 3}})
           "d3:abcd2:pii3ee3:bar4:spam3:fooi12e3:xyzli1ei2ei3eee"))))

;; read-bencode tests

(deftest read-bencode-tests
  (testing "read-bencode for numbers (integers)"
    (is (= (read-bencode "i0e") 0))
    (is (= (read-bencode "i42e") 42))
    (is (= (read-bencode "i-8e") -8)))
  (testing "read-bencode for strings"
    (is (= (read-bencode "0:") ""))
    (is (= (read-bencode "13:Hello, world!") hello))
    (is (= (read-bencode "19:Ham, Cheese & Bread") ham)))
  (testing "read-bencode for list"
    )
  (testing "read-bencode for map"
    ))
