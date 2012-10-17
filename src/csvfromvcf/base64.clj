(ns csvfromvcf.base64)

(defn base64-decode
  "Return the string base64-decoded from s."
  [s]
  (let [ks "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        c2i (into {} (map (fn [k v] [k v]) ks (range 0 (count ks))))]))
