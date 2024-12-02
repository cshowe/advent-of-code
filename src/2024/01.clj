(ns aoc.2024.01
  (:require [aoc :as aoc]))

(let [data (aoc/read-input 2024 1 :lines-as [(aoc/tuples parse-long parse-long)])
      [l1 l2] (apply mapv vector data)
      l2-counts (frequencies l2)]
  (println (reduce + (map (comp abs -) (sort l1) (sort l2)))
           (transduce (map #(* % (l2-counts % 0))) + l1)))
