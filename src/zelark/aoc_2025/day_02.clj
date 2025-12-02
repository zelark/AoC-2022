(ns zelark.aoc-2025.day-02
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 2: Gift Shop ---
;; https://adventofcode.com/2025/day/2

(def input (aoc/get-input 2025 2))

(defn parse-input [input]
  (->> (re-seq #"(\d+)-(\d+)" input)
       (mapv (fn [[_ start end]]
               [(parse-long start)
                (parse-long end)]))))

(defn solve [input pattern]
  (aoc/sum (for [[start end] (parse-input input)
                 num (range start (inc end))
                 :when (re-matches pattern (str num))]
             num)))

;; part 1 (414.583925 msecs)
(solve input #"^(.+)\1$") ; 28846518423

;; part 2 (412.718297 msecs)
(solve input #"^(.+)\1+$") ; 31578210022