(ns zelark.aoc.range
  (:refer-clojure :exclude [merge range contains?]))

(defrecord Range [start end len]
  Object
  (toString [_] (str [start end])))

(defmethod print-method Range
  [object writer]
  (.append writer (format "[%s %s]" (.start object) (.end object))))

(defn rangee
  "Creates a range from start (inclusive) to end."
  ([range]
   (cond
     (sequential? range)
     (rangee (first range) (second range))

     (instance? Range range)
     range

     :else
     (throw (IllegalArgumentException. (format "Don't know how to crate a %s from %s" Range range)))))
  ([start end]
   (assert (< start end) "start must be less then end.")
   (map->Range {:start start
                :end   end
                :len   (- end start)})))

(defn rangel
  "Creates a range with start at start and length len."
  [start len]
  (rangee start (+ start len)))

;; It would be nice if we can override Clojure `count` for `Range` record.
;; But seems it is not allowed.
(defn length
  "Returns `length` of `range`."
  [range]
  (:len range))

(defn shift
  "Returns a new range shifted by n."
  [range n]
  (rangel (+ (:start range) n) (:len range)))

(defn contains?
  "Returns true if range contains x."
  [range x]
  (and (<= (:start range) x)
       (< x (:end range))))

(defn split
  "Splits range at n."
  [range n]
  (when (contains? range n)
    (let [{:keys [start end]} range]
      [(rangee start n) (rangee n end)])))

(defn intersection
  "Returns a new range, which is an intersection between r1 and r2.
  Otherwise returns nil."
  [r1 r2]
  (let [start (max (:start r1) (:start r2))
        end   (min (:end r1) (:end r2))]
    (when (< start end)
      (rangee start end))))

(defn- exclude-one [r1 r2]
  (let [left  (when (< (:start r1) (:start r2))
                (rangee (:start r1) (min (:end r1) (:start r2))))
        right (when (< (:end r2) (:end r1))
                (rangee (max (:start r1) (:end r2)) (:end r1)))]
    [left right]))

(defn exclude
  "Returns a result of exclusion ranges from range.

  Example: result for (exclude [1 10] [2 7]) is [[1 2]] [7 10]]."
  [range & ranges]
  (loop [r1     range
         ranges (sort-by :start ranges)
         result []]
    (if (and r1 (first ranges))
      (let [[prefix suffix] (exclude-one r1 (first ranges))]
        (recur suffix
               (rest ranges)
               (cond-> result prefix (conj prefix))))
      (cond-> result r1 (conj r1)))))

(defn- merge-two [r1 r2]
  (when (intersection r1 r2)
    (rangee (min (:start r1) (:start r2))
            (max (:end r1) (:end r2)))))

(defn merge
  "Merges ranges if there are intersections between.

  Example: result for (merge [3 6] [10 15] [16 21] [12 19]) is [[3 6] [10 21]]"
  ([& ranges]
   (when-let [ranges (seq (sort-by :start < ranges))]
     (reduce (fn [acc current]
               (let [last (peek acc)]
                 (if-let [merged (merge-two current last)]
                   (conj (pop acc) merged)
                   (conj acc current))))
             (vector (first ranges))
             (rest ranges)))))

(comment
  (rangel 1 10) ; => [1 10]
  (rangee 10 1) ; => AssertionError

  (shift (rangel 1 4000) 10) ; => [11 4011]

  (contains? (rangel 1 10) 10) ; => true
  (contains? (rangee 1 10) 10) ; => false

  (length (rangel 1 4000)) ; => 4000

  (split (rangel 1 4000) 1000) ; => [[1 1000] [1000 4001]]
  (length (first (split (rangel 1 4000) 4000))) ; => 3999

  (intersection (rangel 1 10) (rangel 5 12)) ; => [5 11]

  (exclude-one (rangel 1 9) (rangel -3 3)) ; => [nil [1 10]]
  (exclude-one (rangel 1 9) (rangel -2 5)) ; => [nil [3 10]]
  (exclude-one (rangel 1 9) (rangel 0 10)) ; => [nil nil]
  (exclude-one (rangel 1 9) (rangel 2 5))  ; => [[1 2] [7 10]]
  (exclude-one (rangel 1 9) (rangel 6 7))  ; => [[1 6] nil]
  (exclude-one (rangel 1 9) (rangel 11 4)) ; => [[1 10] nil]

  (exclude (rangel 1 9) (rangel -3 3))  ; => [[1 10]]
  (exclude (rangel 1 9) (rangel -2 5))  ; => [[3 10]]
  (exclude (rangel 1 9) (rangel 0 10))  ; => []
  (exclude (rangel 1 9) (rangel 2 5))   ; => [[1 2] [7 10]]
  (exclude (rangel 1 9) (rangel 6 7))   ; => [[1 6]]
  (exclude (rangel 1 9) (rangel 11 4))  ; => [[1 10]]

  (merge) ; => nil
  (merge (rangee 3 6)
         (rangee 10 15)
         (rangee 16 21)
         (rangee 12 19)) ; => [[3 6] [10 21]]
  )
