(ns aoc.2024.04
  (:require [aoc :as aoc]
            [clojure.set :as s]))

(def grid8 [[-1 -1][-1 0][-1 1]
            [0 -1][0 1]
            [1 -1][1 0][1 1]])

(defn word-search [letter-locs deltas [start & target]]
  (loop [[l & target] target
         [d & deltas] deltas
         locations (letter-locs start)]
    (if (nil? l) locations
        (let [new-letter-locs (letter-locs l)
              xform (comp (map #(mapv + d %)) (filter new-letter-locs))]
          (recur target deltas (sequence xform locations))))))
  
(let [raw (aoc/read-input 2024 4 :word-regex nil :lines-as aoc/grid)
      data (-> (group-by second raw) (update-vals #(into #{} (map first) %)))]
  (->> (for [d grid8]
         (->> (word-search data (repeat 3 d) "XMAS") count))
       (reduce +)
       println)
  (->> (for [d1 [[-1 -1][-1 1][1 -1][1 1]]
             a2 [[0 1] [1 0]]]
         (let [s2 (mapv * d1 a2 [2 2])
               path [d1 (mapv #(* -2 %) d1) s2 (mapv * (mapv - d1 s2) [2 2])]]
           (->> (word-search data path "AMSMS") count)))
       (reduce +)
       println))
