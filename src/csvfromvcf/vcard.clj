(ns csvfromvcf.vcard
  (:require [clojure.java.io])
  (:use [csvfromvcf.base64]))

(defn linify
  "Join multiple lines from lines on CRLFSP sequences."
  ([lines] (linify lines ""))
  ([lines sofar]
     (lazy-seq
      (if-let [line (first lines)]
        (if (empty? sofar)
          (linify (next lines) line)
          (let [trimmed (clojure.string/trim line)]
            (if (= line trimmed)
              (cons sofar (linify lines))
              (linify (next lines) (str sofar trimmed)))))
        (cons sofar ())))))

(defn parsify
  "Split line at leftmost : and then at ;s returning a vector of
  vectors.  The first vector is of property name and parameters.
  The second vector is of property values."
  [line]
  (into []
        (map #(clojure.string/split % #";")
             (clojure.string/split line #":" 2))))

(defn classify
  "Classify the vCard properties props according to contact field."
  [props]
  (let [tag (first props)]
    (cond (= tag "BEGIN") :begin
          (= tag "END") :end
          (= tag "BDAY") :birthday
          (= tag "FN") :formal          ; formal name
          (= tag "N") :name
          (= tag "NICKNAME") :nickname
          (= tag "ADR") :address
          (= tag "EMAIL") :email
          (= tag "NOTE") :note
          (= tag "URL") :url
          (= tag "VERSION") :version
          (= tag "X-WV-ID") :userid
          (= tag "TEL")
          (cond (some #{"CELL"} props) :mobile
                (some #{"HOME"} props) :home
                (some #{"WORK"} props) :work
                :else :ignore)
          :else :ignore)))

(def base64? #{:note})

(defn pairify
  "Turn line into a pair vector with PropertyName key and
  PropertyValue value."
  [line]
  (let [[parameters values] (parsify line)
        k (classify parameters)
        decode (if (base64? k) base64-decode identity)]
    [k (clojure.string/join
        "\n"
        (map decode (remove clojure.string/blank? values)))]))

(defn mapify
  "Return a map of PropertyName to vector of PropertyValues
  representing the vCard file cf."
  [cf]
  (println "mapify" cf)
  (with-open [in (clojure.java.io/reader cf)]
    (reduce (fn [m [k v]] (assoc m k (conj (m k []) v)))
            {} (map pairify (linify (line-seq in))))))

(defn cardify
  [dir]
  (let [d (clojure.java.io/file dir)
        ds (filter #(re-matches #"\d\d*\.vcf$" (.getName %)) (file-seq d))]
    (map mapify ds)))

(defn csvify
  [c]
  (prn "name" ","
       "mobile phone" ","
       "home phone" ","
       "work phone" ","
       "e-mail address" ","
       "home address" ","
       "note"))

(defn -main
  [& args]
  (doseq [arg args] (prn (cardify arg))))

;; (-main "/Users/tbl/Nokia/contacts")