(ns zelark.aoc-2025.day-04
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [clojure.set :as set]))

;; --- Day 4: Printing Department ---
;; https://adventofcode.com/2025/day/4

(def input (aoc/get-input 2025 04))

(defn parse-input [input]
  (-> (g2/parse input #{\@}) keys set))

(defn accessable? [rolls roll]
  (let [neighbors (filter rolls (g2/all-neighbors roll))]
    (< (count neighbors) 4)))

(defn step [rolls]
  (reduce (fn [acc roll] (if (accessable? rolls roll) (disj acc roll) acc))
          rolls
          rolls))

(defn solve [input remove-fn]
  (let [rolls (parse-input input)
        left  (remove-fn rolls)]
    (count (set/difference rolls left))))

;; part 1 (115.75031 msecs)
(solve input step) ; 1486

;; part 2 (1834.912761 msecs)
(solve input #(aoc/fix-point step %)) ; 9024