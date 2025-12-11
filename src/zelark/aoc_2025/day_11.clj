(ns zelark.aoc-2025.day-11
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.graph :as g]
            [clojure.string :as str]
            [medley.core :as mdl]))

;; --- Day 11: Reactor ---
;; https://adventofcode.com/2025/day/11

(def input (aoc/get-input 2025 11))

(defn parse-device [m line]
  (let [[device & outputs] (re-seq #"[a-z]+" line)]
    (assoc m device (set outputs))))

(defn parse-input [input]
  (->> (str/split-lines input)
       (reduce parse-device {"out" #{}})))

(defn ways [g from to]
  (let [dp (->> (g/topo-sort g)
                (reverse)
                (drop-while (complement #{from}))
                (mdl/take-upto #{to})
                (reduce (fn [m from]
                          (reduce (fn [m to] (update m to (fnil + 0) (m from 0))) m (g from)))
                        {from 1}))]
    (dp to)))

;; part 1 (21.705046 msecs)
(let [g (parse-input input)]
  (ways g "you" "out")) ; 543

;; part 2 (59.201407 msecs)
(let [g (parse-input input)]
  (* (ways g "svr" "fft")
     (ways g "fft" "dac")
     (ways g "dac" "out"))) ; 479511112939968