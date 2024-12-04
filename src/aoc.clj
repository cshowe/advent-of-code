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

(defn apply-specish [spec data]
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

(defn grid [lines]
  (into {} (for [[row line] (map-indexed vector lines)
                 [col char] (map-indexed vector line)]
             [[row col] char])))

(defn- map-parsers [parsers data]
  (cond
    (fn? parsers) (parsers data)
    (vector? parsers) (mapv #(map-parsers %1 %2)
                            (concat parsers (repeat (last parsers))) data)
    true (apply-specish parsers data)))

(defn read-input [year day & {:keys [lines-as data-as paragraphs-as word-regex]
                              :or {word-regex #"\w+"}}]
  (with-open [infile (io/reader (format "input/%d/%02d.txt" year day))]
    (cond->> (line-seq infile)
      word-regex (map #(re-seq word-regex %))
      data-as (apply concat)
      paragraphs-as (partition-by empty?)
      paragraphs-as (remove #(= % [nil]))
      true (map-parsers (or data-as lines-as paragraphs-as)))))
