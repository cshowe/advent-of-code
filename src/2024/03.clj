(ns aoc.2024.03
  (:require [aoc :as aoc]))

(defn rf [[total on] [match x y]]
  (cond
    (= match "do()") [total true]
    (= match "don't()") [total false]
    (not on) [total on]
    true [(+ total (* (parse-long x) (parse-long y))) on]))

(defn do-part [input re]
 (->> (re-seq re input)
      (reduce rf [0 true])
      first
      println))
  
(let [data (aoc/read-input 2024 3 :data-as str)]
  (do-part data #"mul\((\d+),(\d+)\)")
  (do-part data #"do\(\)|don't\(\)|mul\((\d+),(\d+)\)"))
