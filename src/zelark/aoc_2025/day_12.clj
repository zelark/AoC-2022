(ns zelark.aoc-2025.day-12
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 12: Christmas Tree Farm ---
;; https://adventofcode.com/2025/day/12

(def input (aoc/get-input 2025 12))

(defn parse-present [lines]
  (let [[idx & shape] (str/split-lines lines)]
    {:id    (first (aoc/parse-longs idx))
     :shape (vec shape)
     :n     (aoc/sum #(aoc/cnt % \#) shape)}))

(defn parse-region [line]
  (let [[size qts] (str/split line #":")]
    {:size (aoc/parse-longs size)
     :qts  (zipmap (range) (aoc/parse-longs qts))}))

(defn parse-input [input]
  (let [lines (aoc/split-on-blankline input)]
    {:presents (->> (butlast lines)
                    (reduce (fn [m p]
                              (let [p (parse-present p)]
                                (assoc m (p :id) p)))
                            {}))
     :regions  (->> (str/split-lines (last lines))
                    (mapv parse-region))}))

;; part 1 (11.096173 msecs)
(defn quick-check [region presents]
  (let [{:keys [qts size]} region
        region-area (apply * size)
        required (reduce-kv (fn [acc idx quantity]
                              (+ acc
                                 (* (:n (presents idx))
                                    quantity)))
                            0
                            qts)]
    (<= required region-area)))

(let [{:keys [regions presents]} (parse-input input)]
  (-> (filter #(quick-check % presents) regions)
      (count))) ; 474

;; part 2 ()
;; As always!
