(ns csvfromvcf.base64)

(defn base64-decode
  "Return the string base64-decoded from s."
  [s]
  (let [ks "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        c2i (into {} (map (fn [k v] [k v]) ks (range 0 (count ks))))
        unpack4
        (fn [b cs]
          (if (first cs)
            (recur (bit-or (bit-shift-left b 6) (c2i (first cs))) (next cs))
            (.toByteArray (BigInteger/valueOf b))))]
    (apply str (map #(apply str (map char (unpack4 0 %))) (partition 4 4 "" s)))))

;; 2.00 oz spiced rum (Captain Morgan's)
;; 1.00 oz Cointreau
;; 0.33 oz lemon juice
;; dust with cinnamon
;; and garnish with lemon twist
(def test
  "Mi4wMCBveiBzcGljZWQgcnVtIChDYXB0YWluIE1vcmdhbidzKQ0xLjAwIG96IENvaW50cmVhdQ0wLjMzIG96IGxlbW9uIGp1aWNlDWR1c3Qgd2l0aCBjaW5uYW1vbg1hbmQgZ2FybmlzaCB3aXRoIGxlbW9uIHR3aXN0")

;; (base64-decode test)
