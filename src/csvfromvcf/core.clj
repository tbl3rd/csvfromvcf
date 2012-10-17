(ns csvfromvcf.core
  (:require [clojure.java.io]))

(defn linify
  "Join multiple lines from ls on CRLFSP sequences."
  ([lines] (linify lines ""))
  ([lines sofar]
     (if-let [line (first lines)]
       (if (empty? sofar)
         (recur (rest lines) (str line))
         (let [trimmed (clojure.string/trim line)]
           (if (= line trimmed) (cons sofar (linify lines))
               (recur (rest lines) (str sofar trimmed)))))
       (list sofar))))

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

(defn pairify
  "Turn line into a pair vector with PropertyName key and
  PropertyValue value."
  [line]
  (let [[parameters values] (parsify line)]
    [(classify parameters)
     (clojure.string/join "\n" (remove clojure.string/blank? values))]))

(defn mapify
  "Return a map of PropertyName to vector of PropertyValues
  representing the vCard file cf."
  [cf]
  (with-open [in (clojure.java.io/reader cf)]
    (reduce (fn [m [k v]] (assoc m k (conj (or (m k) []) v)))
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
