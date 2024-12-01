(ns aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn tuples [& parsers]
  (fn [infile]
    (for [line (line-seq infile)]
      (let [parts (re-seq #"\w+" line)]
        (assert (= (count parts) (count parsers)) line)
        ((first parsers) (first parts))
        (into [] (map #(%1 %2) parsers parts))))))

(defn read-input [year day & {:keys [as]}]
  (with-open [infile (io/reader (format "input/%d/%02d.txt" year day))]
    (into [] (as infile))))
    
  
