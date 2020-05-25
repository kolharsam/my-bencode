(ns my-bencode.core
  (:require [clojure.string :as str]))

;; TODO: Use of InputStreams and OutputStreams

(def ^{:private true} comma ",")
(def ^{:private true} colon ":")
(def ^{:private true} re-colon #":")

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
  (let [num (apply str num-seq)
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

(defn gen-byte-seq
  "A helper function that is exposed to help convert to byte sequence"
  [^String input]
  (.getBytes input "UTF-8"))

(defn write-netstring
  "Returns a netstring for the given byte sequence"
  [input]
  (if-not (bytes? input)
    (throw (Exception. "Incorrect Input!"))
    (let [byte-count (alength input)
          input-str (String. input "UTF-8")
          final-str (str byte-count colon input-str comma)]
      final-str)))

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

;; TODO: read-bencode, write-bencode
;; will have to use multi-methods for this

(comment
  ;; (write-netstring (gen-byte-seq "hello world"))
  ;; (read-length "1:Hello:world,")
  ;; (read-netstring "11:hello world,")
  )
