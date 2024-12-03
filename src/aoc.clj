(ns aoc
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def int (s/conformer parse-long))

(defn choice [& keywords]
  (s/& (s/conformer keyword) (set keywords)))

(defmacro specish [& parts]
  (loop [spec []
         n 0
         [part & parts] parts]
    (cond
      (nil? part) `(s/cat ~@spec)
      (keyword? part) (recur (-> spec (conj part) (conj (first parts)))
                             n
                             (next parts))
      (string? part) (recur (-> spec
                                (conj (-> n str keyword))
                                (conj `(set (vector ~part))))
                            (inc n)
                            parts)
      true (recur (-> spec (conj (-> n str keyword)) (conj part)) (inc n) parts))))

(defn specs [spec]
  (fn [data]
    (let [conformed (s/conform spec data)]
      (if (s/invalid? conformed)
        (s/explain spec data)
        (walk/prewalk (fn [x]
                        (if (map? x)
                          (let [all-ks (keys x)
                                ks (remove #(parse-long (name %)) all-ks)]
                            (select-keys x ks))
                          x))
                      conformed)))))

(defn tuples [& parsers]
  (fn [data]
    (let [parts data]
      (assert (= (count parts) (count parsers)) data)
      (into [] (map #(%1 %2) parsers parts)))))

(defn- map-parsers [parsers data]
  (cond
    (fn? parsers) (parsers data)
    true  (mapv #(map-parsers %1 %2)
                (concat parsers (repeat (last parsers))) data)))

(defn- paragraphs [aseq]
  (letfn [(break? [x] (and (= 1 (count x)) (= nil (first x))))]
    (->> aseq (partition-by empty?) (remove break?))))

(defn read-input [year day & {:keys [lines-as data-as paragraphs-as word-regex]
                              :or {word-regex #"\w+"}}]
  (let [path (format "input/%d/%02d.txt" year day)]
    (if data-as
      (data-as (slurp path))
      (with-open [infile (io/reader path)]
        (let [lines (->> infile line-seq (map #(re-seq word-regex %)))]
          (cond
            lines-as (map-parsers lines-as lines)
            paragraphs-as (->> lines paragraphs (map-parsers paragraphs-as))))))))
    
  
