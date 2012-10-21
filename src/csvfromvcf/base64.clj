(ns csvfromvcf.base64)

(defn base64-decode
  "Return the string base64-decoded from s."
  [s]
  (let [ks "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        c2i (zipmap ks (range 0 (count ks)))
        bivo #(BigInteger/valueOf %)]
    (apply str
           (map char
                (.toByteArray
                 (reduce (fn [bi c] (.or (.shiftLeft bi 6) (bivo (c2i c))))
                         (bivo 0) s))))))

;; 2.00 oz spiced rum (Captain Morgan's)
;; 1.00 oz Cointreau
;; 0.33 oz lemon juice
;; dust with cinnamon
;; and garnish with lemon twist
(def test
  "Mi4wMCBveiBzcGljZWQgcnVtIChDYXB0YWluIE1vcmdhbidzKQ0xLjAwIG96IENvaW50cmVhdQ0wLjMzIG96IGxlbW9uIGp1aWNlDWR1c3Qgd2l0aCBjaW5uYW1vbg1hbmQgZ2FybmlzaCB3aXRoIGxlbW9uIHR3aXN0")

;; (base64-decode test)
