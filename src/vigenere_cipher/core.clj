(ns vigenere-cipher.core)

;; alphabet size + space character
(def alphabet-size 27)

;; encode a character as an integer
(defn encode-char [c]
  (let [ascii (int c)]
    (if (and (>= ascii (int \a))
             (<= ascii (int \z)))
      (- ascii (int \a))
      (if (= c \space)
        26))))

;; decode an integer as a character
(defn decode-char [e]
  (if (and (>= e 0)
           (<= e 25))
    (char (+ e (int \a)))
    (if (= e 26)
      \space )))

;; encode the whole string as a list of integers
(defn encode-string [s]
  (map encode-char s))

;;test
(encode-string "hello")
;; => (7 4 11 11 14)

;; decode a list of integers into a string
(defn decode-string [s]
  (apply str (map decode-char s)))

;; test
(decode-string (encode-string "she sells sea shells on the sea shore"))
;; => "she sells sea shells on the sea shore"

;; make an infinite list of characters encoded as integers fo a key
(defn encode-key [key]
  (cycle (encode-string key)))

;; make an infinite list of string as characters encoded as integers
(defn infinite-encrypt [s k]
  (map (fn [x y] (mod (+ x y) alphabet-size)) (encode-string s) (encode-key k)))

;; make a correct length string out of infinite list of integers
(defn encrypt [s k]
  (decode-string (take (count s) (infinite-encrypt s k))))

;; make an inversed key to decrypt a string
(defn inverse-key [k]
  (decode-string (map (fn [x] (mod (- alphabet-size x) alphabet-size)) (encode-string k))))

;; decrypt the encrypted string by the same key
(defn decrypt [s k]
  (encrypt s (inverse-key k)))

;; tests
(encrypt "hello" "ab")
;; => "hflmo"
(decrypt "hflmo" "ab")
;; => "hello"
(let [k "a b"
      s "she sells sea shell on the sea shore"]
  (decrypt (encrypt s k) k))
;; => "she sells sea shell on the sea shore"
