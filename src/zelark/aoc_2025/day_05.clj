(ns zelark.aoc-2025.day-05
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.range :as r]))

;; --- Day 5: Cafeteria ---
;; https://adventofcode.com/2025/day/5

(def input (aoc/get-input 2025 05))

(defn parse-ranges [input]
  (->> (re-seq #"(\d+)-(\d+)" input)
       (mapv (fn [[_ start end]]
               (r/rangee (parse-long start)
                         (inc (parse-long end)))))))

(defn parse-input [input]
  (let [[ranges ids] (aoc/split-on-blankline input)]
    {:ranges (parse-ranges ranges)
     :ids (aoc/parse-longs ids)}))

;; part 1 (10.056682 msecs)
(defn in-any-range? [id ranges]
  (some #(r/contains? % id) ranges))

(let [{:keys [ids ranges]} (parse-input input)]
  (->> (filter #(in-any-range? % ranges) ids)
       (count))) ; 698

;; part 2 (1.695834 msecs)
(let [{:keys [ranges]} (parse-input input)]
  (->> (apply r/merge ranges)
       (aoc/sum r/length))) ; 352807801032167