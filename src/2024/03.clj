(ns aoc.2024.03
  (:require [aoc :as aoc]))

(defn rf [[total on] [match x y]]
  (cond
    (= match "do()") [total true]
    (= match "don't()") [total false]
    (not on) [total on]
    true [(+ total (* (parse-long x) (parse-long y))) on]))
  
(let [word #"do\(\)|don't\(\)|mul\((\d+),(\d+)\)"
      data (aoc/read-input 2024 3 :word-regex word :data-as [vec])]
  (println (->> data (remove #(-> % second nil?)) (reduce rf [0 true]) first)
           (->> data  (reduce rf [0 true]) first)))
