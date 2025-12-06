(ns zelark.aoc-2025.day-06
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 6: Trash Compactor ---
;; https://adventofcode.com/2025/day/6

(def input (aoc/get-input 2025 06))

(defn parse-input [input parse-numbers]
  (let [lines (str/split-lines input)]
    [(re-seq #"\+|\*" (peek lines))
     (parse-numbers (butlast lines))]))

(defn do-math [op numbers]
  (apply (-> op symbol resolve) numbers))

(defn solve [input parse-numbers]
  (->> (parse-input input parse-numbers)
       (apply mapv do-math)
       (aoc/sum)))

;; part 1 (7.225912 msecs)
(defn parse-numbers-p1 [lines]
  (-> (mapv aoc/parse-longs lines)
      (aoc/transpose)))

(solve input parse-numbers-p1) ; 3261038365331

;; part 2 (26.655757 msecs)
(defn parse-numbers-p2 [lines]
  (->> (apply mapv (fn [& digits]
                     (->> (remove #{\space} digits)
                          (str/join)
                          (parse-long)))
              lines)
       (partition-by nil?)
       (remove (comp nil? first))))

(solve input parse-numbers-p2) ; 8342588849093