(ns zelark.aoc-2025.day-07
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 7: Laboratories ---
;; https://adventofcode.com/2025/day/7

(def input (aoc/get-input 2025 07))

(defn find-start [grid]
  (some (fn [[loc ch]] (when (= ch \S) loc)) grid))

(defn parse-input [input]
  (let [grid (g2/parse input)]
    {:grid  (g2/parse input)
     :start (find-start grid)}))

;; part 1 (36.834373 msecs)
(defn step-p1 [grid beams n]
  (reduce (fn [[acc n] [x y]]
            (let [y' (inc y)]
              (case (grid [x y'])
                \. [(conj acc [x y']) n]
                \^ [(conj acc [(dec x) y'] [(inc x) y'])
                    (inc n)])))
          [#{} n]
          beams))

(let [{:keys [grid start]} (parse-input input)]
  (->> (iterate #(step-p1 grid (first %) (second %)) [#{start} 0])
       (drop-while #(grid (update (ffirst %) 1 inc)))
       (first)
       (second))) ; 1667

;; part 2 (43.261896 msecs)
(defn step-p2 [grid beams]
  (reduce-kv (fn [acc [x y] n]
               (let [y' (inc y)]
                 (case (grid [x y'])
                   \. (update acc [x y'] (fnil + 0) n)
                   \^ (-> acc
                          (update [(dec x) y'] (fnil + 0) n)
                          (update [(inc x) y'] (fnil + 0) n)))))
             {}
             beams))

(let [{:keys [grid start]} (parse-input input)]
  (->> (iterate #(step-p2 grid %) {start 1})
       (drop-while #(grid (update (first (keys %)) 1 inc)))
       (first)
       (aoc/sum val))) ; 62943905501815