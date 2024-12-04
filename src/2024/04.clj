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
    (if (nil? l) (set locations)
        (let [new-letter-locs (letter-locs l)
              xform (comp (map #(mapv + d %)) (filter new-letter-locs))]
          (recur target deltas (sequence xform locations))))))
  
(let [raw (aoc/read-input 2024 4 :word-regex nil :lines-as aoc/grid)
      data (-> (group-by second raw) (update-vals #(into #{} (map first) %)))
      find-xmas #(word-search data (repeat 3 %) "XMAS")
      find-mas (fn [x] (word-search data [(mapv #(* -2 %) x) x] "SMA"))]
  (println (->> grid8 (pmap (comp count find-xmas)) (reduce +))
           (let [[a b c d] (pmap find-mas [[-1 -1] [1 1] [-1 1] [1 -1]])]
             (count (s/intersection (s/union a b) (s/union c d))))))
