(ns csvfromvcf.base64)

(def ^{:doc "Map base64 character to 6-bit integer for decoding."
       :private true}
  c2i
  (let [ks "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"]
    (zipmap ks (range 0 (count ks)))))

(defn quad
  "Convert sequence s into a sequence of sequences of up to 4 characters."
  [s]
  (partition-all 4 (filter c2i s)))

(defn decode-chunk
  "Decode a chunk of 2 to 4 base64 characters."
  [chunk]
  (let [n (count chunk)
        i (reduce (fn [i c] (bit-or (bit-shift-left i 6) (c2i c))) 0 chunk)]
    (cond (= n 4)
          [(bit-and 0xff (bit-shift-right i 16))
           (bit-and 0xff (bit-shift-right i  8))
           (bit-and 0xff (bit-shift-right i  0))]
          (= n 3)
          [(bit-and 0xff (bit-shift-right i 10))
           (bit-and 0xff (bit-shift-right i  2))]
          (= n 2)
          [(bit-and 0xff (bit-shift-right i  4))])))

(defn base64->bytes
  "Decode the sequence s of base64 characters into a byte sequence."
  [s]
  (map byte (flatten (map decode-chunk (quad s)))))

(defn base64->string
  "Decode the sequence s of base64 characters into a string."
  [s]
  (apply str (map char (flatten (map decode-chunk (quad s))))))

;; 2.00 oz spiced rum (Captain Morgan's)
;; 1.00 oz Cointreau
;; 0.33 oz lemon juice
;; dust with cinnamon
;; and garnish with lemon twist
(def recipe
  "Mi4wMCBveiBzcGljZWQgcnVtIChDYXB0YWluIE1vcmdhbidzKQ0xLjAwIG96IENvaW50cmVhdQ0wLjMzIG96IGxlbW9uIGp1aWNlDWR1c3Qgd2l0aCBjaW5uYW1vbg1hbmQgZ2FybmlzaCB3aXRoIGxlbW9uIHR3aXN0")

(def n "SmFtZXMgJiBLYXJlbiA=")

;; (base64->string recipe)
;; (base64->string n)
;; (base64-decode recipe)
;; (base64-decode n)
