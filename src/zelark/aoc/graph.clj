(ns zelark.aoc.graph
  (:require [clojure.set :as set])
  (:import [clojure.lang PersistentQueue]))

(defn shortest-paths
  "Floyd–Warshall algorithm"
  ([g] (shortest-paths g (constantly 1)))
  ([g w]
   (let [init (into (reduce (fn [m v] (assoc m [v v] 0)) {} (keys g))
                    (for [[v1 vs] g, v2 vs] [[v1 v2] (w [v1 v2])]))]
     (reduce (fn [dist [k i j]]
               (assoc dist [i j]
                      (min (get dist [i j] ##Inf)
                           (+ (get dist [i k] ##Inf)
                              (get dist [k j] ##Inf)))))
             init
             (for [k (keys g), i (keys g), j (keys g)] [k i j])))))

(defn- bfs-seq
  ([neighbors start]
   (bfs-seq neighbors (conj PersistentQueue/EMPTY [start]) #{start}))
  ([neighbors queue seen]
   (when-let [current (peek queue)]
     (cons current
           (lazy-seq
            (let [[queue seen]
                  (->> (neighbors (peek current))
                       (reduce (fn [[q s] node]
                                 (if (seen node)
                                   [q s]
                                   [(conj q (conj current node)) (conj s node)]))
                               [(pop queue) seen]))]
              (bfs-seq neighbors queue seen)))))))

(defn bfs
  ([g start] (bfs-seq g start))
  ([g start goal]
   (let [goal? (if (or (fn? goal) (set? goal)) goal #{goal})]
     (first (filter #(goal? (peek %)) (bfs-seq g start))))))

(defn connected-group [neighbors v]
  (reduce #(conj % (peek %2)) #{} (bfs neighbors v)))

(defn connected-groups
  ([g] (connected-groups g nil))
  ([g neighbors]
   (let [neighbors (or neighbors g)]
     (reduce (fn [groups v]
               (if (some #(% v) groups)
                 groups
                 (conj groups (connected-group neighbors v))))
             []
             (if (map? g) (keys g) g)))))

(defn- bk
  "Performs Bron-Kerbosch algorithm using a pivot vertex.

  Pseudocode:
  ```
  algorithm BronKerbosch2(R, P, X) is
      if P and X are both empty then
          report R as a maximal clique
      choose a pivot vertex u in P ⋃ X
      for each vertex v in P \\ N(u) do
          BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
          P := P \\ {v}
          X := X ⋃ {v}
  ```
  "
  ([graph]
   (let [nodes (set (keys graph))]
     (bk graph #{} nodes #{})))
  ([grpah r p x]
   (if (and (empty? p)
            (empty? x))
     [r]
     (let [u (first (set/union p x))]
       (loop [pwnu (remove (grpah u) p)
              p    p
              x    x
              result []]
         (if (empty? pwnu)
           result
           (let [v      (first pwnu)
                 result (into result
                              (bk grpah
                                  (conj r v)
                                  (set/intersection p (grpah v))
                                  (set/intersection x (grpah v))))]
             (recur (rest pwnu) (disj p v) (conj x v) result))))))))

(defn maximal-cliques
  "Returns a vector of the maximal cliques using Bron-Kerbosch."
  [graph]
  (bk graph))

