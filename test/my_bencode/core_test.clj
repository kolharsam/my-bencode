(ns my-bencode.core-test
  (:require [clojure.test :refer [deftest is are testing]]
            [my-bencode.core :as my-ben :refer [gen-byte-seq read-netstring write-netstring]]))

(def ^{:private true} hello "Hello, world!")
(def ^{:private true} ham "Ham, Cheese & Bread")

;; gen-byte-seq tests

(deftest helper-tests
  (testing "gen-byte-seq method"
    (is (.getBytes hello "UTF-8") (my-ben/gen-byte-seq hello))
    (is (.getBytes ham "UTF-8") (my-ben/gen-byte-seq ham))))

;; read-netstring tests


;; write-netstring tests
