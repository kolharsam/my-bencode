;; NOTE - This is a mere representation of the encoding & decoding
;; To get to the actual implementation we would have to use byte array's / streams
;;
;; Check the nrepl/bencode repo for the actual implementation.
;; It's battle tested and has a marvelously simple API to use it
;;
;; TODO: make it use-able with InputStreams and OutputStreams
;; IDEA: Probably can make this a web app with CLJS, just to give
;; people and idea of how bencoding works

(ns my-bencode.core
  (:require [clojure.string :as str])
  (:import clojure.lang.RT))

(def ^{:private true} comma ",")
(def ^{:private true} colon ":")
(def ^{:private true} l "l")
(def ^{:private true} e "e")
(def ^{:private true} d "d")
(def ^{:private true} i "i")
(def ^{:private true} char-e \e)

(defn- is-digit?
  "Returns true if c is a valid digit and is a positive number"
  [c]
  (let [num (try (Integer/parseInt c)
                 (catch Exception e
                   (str "Errored Input: " e)))]
    (<= 0 num)))

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
          {:value (to-number nums) :rest (apply str (rest in))}
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

;; For simplicity Imma assume that all input is valid!
;; please don't hate me
(defn- map-maker
  "Returns a map from a list of values"
  [input]
  (let [ks (take-nth 2 input)
        vs (take-nth 2 (rest input))
        mks (map keyword ks)
        mps (interleave mks vs)]
    (apply hash-map mps)))

;; Helper

(defn gen-byte-seq
  "A helper function that is exposed to help convert to byte sequence"
  [^String input]
  (.getBytes input "UTF-8"))

;; Netstring

;; write-netstring

(defn write-netstring
  "Returns a netstring for the given byte sequence"
  [input]
  (let [net-str (write-netstring* input)
        final-str (str net-str comma)]
    final-str))

;; read-netstring

(defn read-netstring
  "Returns the original string input, you could pass an optional key
  for the validating the netstring against a given buffer size"
  ([input]
   (read-netstring input :buffer-size nil))
  ([input & {:keys [buffer-size]}]
   (let [len-info (read-length input)
         net-str-len (:value len-info)
         mod-input (:rest len-info)]
     (when-not (nil? buffer-size)
       (when-not (<= net-str-len buffer-size)
         (throw (Exception. "Buffer overflow."))))
     (if-not (comma-present? mod-input)
       (throw (Exception. "Not a valid netstring."))
       (let [netstring (read-netstring* mod-input)
             netstring-len (.length netstring)]
         (if-not (= net-str-len netstring-len)
           (throw (Exception. "Not a valid netstring. Check length."))
           netstring))))))

;; Bencode (pronounced b-encode)

;; Allowed expressions are:
;; 1. Integers
;; 2. Lists
;; 3. Byte Strings
;; 4. Maps / Dictionaries

;; write-bencode

(declare lexicographical-sort)

(defmulti write-bencode
  "Returns a bencode for the allowed expressions"
  (fn [input]
    (cond
      (instance? (RT/classForName "[B") input) :bytes
      (integer? input) :integer
      (string? input)  :string
      (nil? input) :list
      (or (list? input) (.isArray (class input)) (vector? input)) :list
      (map? input) :map
      :else (type input))))

(defmethod write-bencode :default
  [input]
  (throw
   (IllegalArgumentException.
    (str "Cannot write value of type: " (class input)))))

(defmethod write-bencode :bytes
  [input]
  (write-netstring* input))

(defmethod write-bencode :integer
  [input]
  (str i input e))

(defmethod write-bencode :string
  [input]
  (write-netstring* (gen-byte-seq input)))

(defmethod write-bencode :list
  [input]
  (if (nil? input)
    (str l e)
    (let [vec-input (vec input)
          elem-bencode (map write-bencode vec-input)]
      (str l (str/join elem-bencode) e))))

(defmethod write-bencode :map
  [input]
  (let [mp-keys (keys input)
        mp-str-keys (map name mp-keys)
        new-mp (lexicographical-sort mp-str-keys input)
        new-mp-ben (map write-bencode new-mp)]
    (str d (str/join new-mp-ben) e)))

;; write-bencode helpers

(defn- lexicographical-sort
  "Returns the keys of a map arranged in a lexicographical order,
  the final result is a list with the sorted keys & values interleaved"
  [mp-keys orig-map]
  (let [sorted-keys (sort mp-keys)
        rearranged-vals (reduce
                         (fn [ord-vals current-key]
                           (conj ord-vals
                                 ((keyword current-key) orig-map))) [] sorted-keys)]
    (interleave (vec sorted-keys) rearranged-vals)))

;; read-bencode

;; this is perhaps not the best code I've written, there are repetitions
;; and this all feels make-shifty. I'll make it better soon, with just binary code

(defmulti read-bencode
  "Returns the original data that was encoded"
  (fn [input]
    (let [first-byte (subs input 0 1)
          par-equal (partial = first-byte)]
      (cond
        (par-equal i) :integer
        (par-equal l) :list
        (par-equal d) :map
        (is-digit? first-byte) :string))))

(defmethod read-bencode :default
  [input]
  (throw
   (IllegalArgumentException.
    "Cannot identify of type bencode of type: " input)))

(defmethod read-bencode :integer
  [input]
  (loop [input-split (str/split (subs input 1) #"")
         nums []]
    (if-not (= e (first input-split))
      (recur (rest input-split) (conj nums (first input-split)))
      (if (= (count input-split) 1)
        (Integer/parseInt (str/join nums))
        {:value (Integer/parseInt (str/join nums)) :rest (str/join (rest input-split))}))))

;; no buffer-size for this unlike I had an option for netstrings
(defmethod read-bencode :string
  [input]
  (let [len-info (read-length input)
        len-value (:value len-info)
        rem-str (:rest len-info)
        sub-main (subs rem-str 0 len-value)
        rest-str (subs rem-str len-value)]
    (if (= rest-str "")
      sub-main
      {:value sub-main :rest rest-str})))

(defmethod read-bencode :list
  [input]
  (when-not (or (= e input) (= 1 (.length input)))
    (let [edit-str (subs input 1)]
      (loop [current-str edit-str
             values []]
        (if (= e (subs current-str 0 1))
          (if (= (subs current-str 1) "")
            values
            {:value values :rest (subs current-str 1)})
          (let [{value :value, rest-str :rest} (read-bencode current-str)]
            (when-not (or (nil? value) (nil? rest-str))
              (recur rest-str (conj values value)))))))))

(defmethod read-bencode :map
  [input]
  (when-not (or (= e input) (= 1 (.length input)))
    (let [edit-str (subs input 1)]
      (loop [current-str edit-str
             values []]
        (if (= e (subs current-str 0 1))
          (if (= (subs current-str 1) "")
            (map-maker values)
            {:value (map-maker values) :rest (subs current-str 1)})
          (let [{value :value, rest-str :rest} (read-bencode current-str)]
            (when-not (or (nil? value) (nil? rest-str))
              (recur rest-str (conj values value)))))))))

(comment
  ;; Some Notes

  ;; (write-bencode (format "%o" 34))

  ;; Another option for testing for class [B
  ;; (-> (gen-byte-seq "String") class .getComponentType (= Byte/TYPE))

  ;; (coll? {}) - returns true since it implements IPersistentCollection
  )
