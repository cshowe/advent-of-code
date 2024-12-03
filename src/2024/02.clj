(ns aoc.2024.02
  (:require [aoc :as aoc]))

(defn safe? [x]
  (let [ds (mapv #(apply - %) (partition 2 1 x))
        max-d (reduce max ds)
        min-d (reduce min ds)]
    (or (and (>= min-d 1) (<= max-d 3))
        (and (>= min-d -3) (<= max-d -1)))))

(defn damped [pred]
  #(some pred (for [n (range (count %))] (concat (take n %) (drop (inc n) %)))))

(let [data (aoc/read-input 2024 2 :lines-as [[parse-long]])]
  (println (->> data (filter safe?) count)
           (->> data (filter (damped safe?)) count)))
