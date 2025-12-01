(ns zelark.aoc-2025.day-01
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 1: Secret Entrance ---
;; https://adventofcode.com/2025/day/1

(def input (aoc/get-input 2025 1))

(defn parse-input [input]
  (->> (re-seq #"(R|L)(\d+)" input)
       (mapv (fn [[_ d n]] [(keyword d) (parse-long n)]))))

(defn solve [input count]
  (->> (parse-input input)
       (reductions (fn [[dial _] [direction distance]]
                     (let [dial* (case direction
                                   :L (- dial distance)
                                   :R (+ dial distance))]
                       [(mod dial* 100)
                        (count dial dial*)]))
                   [50 0])
       (aoc/sum second)))

;; part 1 (6.711541 msecs)
(solve input #(if (zero? (mod %2 100)) 1 0)) ; 1011

;; part 2 (7.161612 msecs)
(solve input
       (fn [dial dial*]
         (cond-> (abs (quot dial* 100))
           (and (<= dial* 0) (pos? dial)) inc))) ; 5937
