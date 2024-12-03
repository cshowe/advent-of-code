(ns aoc.2024.02
  (:require [aoc :as aoc]))

(defn is-safe? [x]
  (let [ds (mapv #(apply - %) (partition 2 1 x))
        max-d (reduce max ds)
        min-d (reduce min ds)]
    (or (and (>= min-d 1) (<= max-d 3))
        (and (>= min-d -3) (<= max-d -1)))))

(defn damped-safe? [x]
  (some is-safe? (for [n (range (count x))] (concat (take n x) (drop (inc n) x)))))

(let [data (aoc/read-input 2024 2 :lines-as [[parse-long]])]
  (println (->> data (filter is-safe?) count)
           (->> data (filter damped-safe?) count)))
