(ns zelark.aoc-2025.day-03
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 3: Lobby ---
;; https://adventofcode.com/2025/day/3

(def input (aoc/get-input 2025 03))

(defn parse-input [input]
  (str/split-lines input))

(defn index-of-max [bank from]
  (some #(str/index-of bank % from) ["9" "8" "7" "6" "5" "4" "3" "2" "1"]))

(defn max-joltage [bank len]
  (->> (reduce (fn [indexes n]
                 (let [last-index (or (peek indexes) -1)
                       next-index (index-of-max (subs bank 0 (- (count bank) n))
                                                (inc last-index))]
                   (conj indexes next-index)))
               []
               (reverse (range 0 len)))
       (map #(nth bank %))
       (apply str)
       (parse-long)))

;; part 1 (1.596993 msecs)
(->> (parse-input input)
     (aoc/sum #(max-joltage % 2))) ; 17031

;; part 2 (2.823817 msecs)
(->> (parse-input input)
     (aoc/sum #(max-joltage % 12))) ; 168575096286051
