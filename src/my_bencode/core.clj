(ns my-bencode.core
  (:require [clojure.string :as str])
  (:import clojure.lang.RT))

;; TODO: Use of InputStreams and OutputStreams

(def ^{:private true} comma ",")
(def ^{:private true} colon ":")

(defn- is-digit?
  "Returns true if c is a valid digit and is a positive number"
  [c]
  (let [num (try (Integer/parseInt c)
                 (catch Exception e
                   (str "Errored Input!")))]
    (pos-int? num)))

(defn- to-number
  "collapses the input seq into a number"
  [num-seq]
  (let [num (str/join num-seq)
        to-num (Integer/parseInt num)]
    to-num))

;; I could've resolved the whole thing by using
;; regex perhaps. But it may be a source of bugs
(defn- read-length
  "Helper to read the `length` part of a netstring"
  [input]
  (loop [in (str/split input #"")
         nums []]
    (if (empty? in)
      (throw (Exception. "Errored Input!"))             ;; couldn't find a colon at all
      (let [[current & other] in]
        (if (= current colon)
          {:number (to-number nums) :rest-string (apply str (rest in))}
          (if (is-digit? current)
            (recur other (conj nums current))
            (throw (Exception. "Errored Input!")))))))) ;; an invalid character found before first colon

(defn- comma-present?
  "Returns true if the input has a comma at the end"
  [^String input]
  (let [rev-str (str/reverse input)
        last (subs rev-str 0 1)]
    (= comma last)))

;; The idea is that we can use it for reading both netstrings and bencode
(defn- read-netstring*
  "Returns the string without the comma"
  [^String input]
  (let [rev-input (str/reverse input)
        str-comma (subs rev-input 0 1)
        rest (subs rev-input 1)]
    (if (= str-comma comma)
      (str/reverse (str/join #"" rest))
      input)))

;; Similar to the idea applied above, can be used for writing
(defn- write-netstring*
  "Returns the netstring without the comma at the end"
  [^String input]
  (if-not (bytes? input)
    (throw (Exception. "Byte Strings only!"))
    (let [byte-count (alength input)
          input-str (String. input "UTF-8")
          final-str (str byte-count colon input-str)]
      final-str)))

;; Helper

(defn gen-byte-seq
  "A helper function that is exposed to help convert to byte sequence"
  [^String input]
  (.getBytes input "UTF-8"))

;; Netstring

(defn write-netstring
  "Returns a netstring for the given byte sequence"
  [input]
  (let [net-str (write-netstring* input)
        final-str (str net-str comma)]
    final-str))

(defn read-netstring
  "Returns the original string input, you could pass an optional key
  for the validating the netstring against a given buffer size"
  ([input]
   (read-netstring input :buffer-size nil))
  ([input & {:keys [buffer-size]}]
   (let [len-info (read-length input)
         net-str-len (:number len-info)
         mod-input (:rest-string len-info)]
     (when-not (nil? buffer-size)
       (when-not (> net-str-len buffer-size)
         (throw (Exception. "Buffer overflow."))))
     (if-not (comma-present? mod-input)
       (throw (Exception. "Not a valid netstring."))
       (let [netstring (read-netstring* mod-input)
             netstring-len (.length netstring)]
         (if-not (= net-str-len netstring-len)
           (throw (Exception. "Not a valid netstring. Check length."))
           netstring))))))

;; Bencode

;; Allowed expressions/symbols are:
;; 1. Integers
;; 2. Lists
;; 3. Byte Strings
;; 4. Maps / Dictionaries

(defmulti write-bencode
  (fn [input]
    (cond
      (instance? (RT/classForName "[B") input) :bytes
      (integer? input) :integer
      (string? input)  :string
      ;; (nil? input) :list
      ;; (map? input) :map
      ;; (or (list? input) (coll? input) (.isArray (class input))) :list
      :else (type input))))

(defmethod write-bencode :default
  [input]
  (throw (IllegalArgumentException. (str "Cannot write value of type: " (class input)))))

(defmethod write-bencode :bytes
  [input]
  (write-netstring* input))

(defmethod write-bencode :integer
  [input]
  (str "i" input "e"))

(defmethod write-bencode :string
  [input]
  (write-netstring* (gen-byte-seq input)))


(comment
  ;; (write-bencode (format "%o" 34))
  (write-bencode (gen-byte-seq "Hello"))
  ;; Another option for testing for class [B

  (defn- is-bytestring?
    "Returns true if input is a bytestring"
    [input]
    (-> input
        class
        .getComponentType
        (= Byte/TYPE)))
  )
