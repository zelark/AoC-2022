(ns zelark.aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.edn :as edn]
            [clojure.data.priority-map :refer [priority-map-keyfn]])
  (:import [clojure.lang PersistentQueue]))

(defn get-input
  ([year day & {:keys [fetch?]}]
   (let [path-to-file (format "%d/input_%02d.txt" year day)
         load-input #(-> % io/resource slurp str/trim-newline)]
     (if (or (not (io/resource path-to-file))
             fetch?)
       (do (println "Downloading" (str path-to-file))
           (shell/sh "./bin/fetch-input" (str year) (str day))
           (load-input path-to-file))
       (load-input path-to-file)))))

(defn queue [& args]
  (into PersistentQueue/EMPTY args))

(defn index-by
  [f coll]
  (persistent! (reduce #(assoc! %1 (f %2) %2) (transient {}) coll)))

;; Parsing
(defn parse-longs [s]
  (->> (re-seq #"-?\d+" s)
       (mapv parse-long)))

(defn- parse-cline [line re f]
  (let [[id chunks] (str/split line #":")
        id (parse-long (re-find #"\d+" id))
        chunks (str/split chunks re)]
    [id (mapv (comp f str/trim) chunks)]))

(defn parse-clines
  "Parses lines with colon, which looks something like:
  Card ID: item1 `sep` item2 ...

  Splits line's items with `re`, and apply `f` to each item.

  Returns a vector of vectors: [[id1 items1] [id2 items2] ...]."
  [input re f]
  (->> (str/split-lines input)
       (mapv #(parse-cline % re f))))

(defn parse-bin [s]
  (Long/parseLong s 2))

(defn split-on-blankline [input]
  (str/split input #"\R\R"))

(defn parse-asm-code [input]
  (->> (str/split-lines input)
       (mapv #(str "[" % "]"))
       (mapv edn/read-string)
       (mapv (fn [x] (mapv #(cond-> % (symbol? %) keyword) x)))))

;; Math
(defn mod-1
  "Returns the 1-based modulus `base` of `n`"
  [n base]
  (inc (mod (dec n) base)))

(defn one?
  "Returns true if `n` is zero, else false."
  [n]
  (== n 1))

(== 1.0 1)

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm
  ([] 1)
  ([a] a)
  ([a b] (/ (* a b) (gcd a b)))
  ([a b & rst] (reduce lcm (lcm a b) rst)))

(defn rangex
  ([] ())
  ([^long start ^long end]
   (if (<= start end)
     (range start (inc end))
     (range start (dec end) -1))))

(defn sum [xs]
  (reduce + 0 xs))

(defn mul [xs]
  (reduce * 1 xs))

(defn fix-point [f x]
   (let [x' (f x)]
    (if (= x x') x (recur f x'))))

(defn transpose [v]
  (if (string? (first v))
    (apply mapv str v)
    (apply mapv vector v)))

(defn rotate
  "Rotates `v` clockwise."
  [v]
  (let [h (count v)
        w (count (first v))]
    (->> (for [i (range w) j (range h)]
           (get-in v [(- h j 1) i]))
         (partition h)
         (mapv #(apply str %)))))

;; Grids
(defn grid-get [grid [x y]]
  (get-in grid [y x]))

(defn empty-grid [w h]
  (vec (repeat h (vec (repeat w \.)))))

(defn print-grid [grid]
  (->> (map str/join grid)
       (str/join \newline)
       (println)))

(defn mark-point [grid [x y] v]
  (assoc-in grid [y x] v))

(defn print-points [points]
  (let [max-x (apply max (map first points))
        max-y (apply max (map second points))]
    (-> (reduce #(mark-point %1 %2 \#)
                (empty-grid (inc max-x) (inc max-y))
                points)
        (print-grid))))

(defn print-points-2 [loc->ch]
  (let [points (keys loc->ch)
        max-x  (apply max (map first points))
        max-y  (apply max (map second points))
        min-x  (apply min (map first points))
        min-y  (apply min (map second points))
        off-x  min-x
        off-y  min-y
        grid   (empty-grid (count (range min-x (inc max-x)))
                           (count (range min-y (inc max-y))))]
    ;; (prn :min-x min-x :max-x max-x)
    ;; (prn :min-y min-y :max-y max-y)
    ;; (prn :off-x off-x :off-y off-y)
    ;; (print-grid grid)
    (-> (reduce-kv (fn [g [x y] ch]
                     (let [normed-loc [(+ x (- off-x)) (+ y (- off-y))]]
                       (mark-point g normed-loc ch)))
                   grid
                   loc->ch)
        (print-grid))))

(defn manhattan-distance [[^long x1 ^long y1] [^long x2 ^long y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(set! *warn-on-reflection* true)

;; path finding
(defn- generate-route [node came-from]
  (loop [route ()
         node node]
    (if (came-from node)
      (recur (cons node route) (came-from node))
      route)))

(defn bfs [graph start goal?]
  (loop [seen  {start nil}
         queue (queue start)]
    (if-let [current (peek queue)]
      (if (goal? current)
        {:status :found
         :route  (generate-route current seen)}
        (let [[seen queue] (reduce (fn [[seen queue :as acc] node]
                                     (if (contains? seen node)
                                       acc
                                       [(assoc seen node current) (conj queue node)]))
                                   [seen (pop queue)]
                                   (graph current))]
          (recur seen queue)))
      {:status :not-found
       :seen   (set (keys seen))})))

;; It's a copy from https://github.com/arttuka/astar with a few improvements.
(defn astar
  "Finds the shortest route from start to goal in a graph.
  
  Params:
  * `graph` — a function (eg. a map) from nodes to a collection of adjacent nodes.
  * `dist` — a function from two nodes to the distance (as a number) from the first node to the second.
  * `h` — a function from a node to the heuristic distance from that node to the goal.
    It should never overestimate the distance.
  * `start` — a start node.
  * `goal?` — a predicate function, if returns `true` the goal is found.
  * `score?` (optional) if `true` returns `score` if the goal is found.
  
  By default returns a list of nodes on the route, excluding the start node and including the goal node.
  If a route can't be found, returns nil."
  [graph dist h start goal? & {:keys [score?]}]
  (loop [visited {}
         queue (priority-map-keyfn first start [0 0 nil])]
    (when (seq queue)
      (let [[current [_ current-score previous]] (peek queue)
            visited (assoc visited current previous)]
        (if (goal? current)
          (if score? current-score (generate-route current visited))
          (recur visited (reduce (fn [queue node]
                                   (let [score (+ current-score (dist current node))]
                                     (if (and (not (contains? visited node))
                                              (or (not (contains? queue node))
                                                  (< score (get-in queue [node 1]))))
                                       (assoc queue node [(+ score (h node)) score current])
                                       queue)))
                                 (pop queue)
                                 (graph current))))))))

;; nubmers and digits
(defn ch->digit [c]
  (- (int c) (int \0)))

(defn add-decit [n d]
  (+ (* n 10) d))

(defn nlen
  ([num] (nlen num 0))
  ([num n]
   (let [q (quot num 10)]
     (if (zero? q)
       (inc n)
       (recur q (inc n))))))
