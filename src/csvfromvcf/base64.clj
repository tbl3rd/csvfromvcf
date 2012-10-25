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
        i (reduce (fn [i c] (bit-or (bit-shift-left i 6) (c2i c))) 0 chunk)
        x (fn [r] #(bit-and 0xff (bit-shift-right % r)))]
    (cond (= n 4) [((x 16) i) ((x 8) i) ((x 0) i)]
          (= n 3) [((x 10) i) ((x 2) i)]
          (= n 2) [((x  4) i)])))

(defn base64->bytes
  "Decode the sequence s of base64 characters into a byte sequence."
  [s]
  (map byte (flatten (map decode-chunk (quad s)))))

(defn base64->string
  "Decode the sequence s of base64 characters into a string."
  [s]
  (apply str (map char (flatten (map decode-chunk (quad s))))))
