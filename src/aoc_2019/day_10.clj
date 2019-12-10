(ns aoc-2019.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (-> "day_10.txt" io/resource slurp))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn mag
  "The magnitude of a vector"
  [v]
  (->> v (map #(* % %)) (reduce +) Math/sqrt))

(defn normalize
  "A normalized unit vector point in the same direction"
  [v]
  (let [m (mag v)]
    ;; NOTE: i start to get some bad results past ~12 digit precision
    (map #(round2 10 (/ % m)) v)))

(defn asteroid-locs
  [input]
  (->> input
       s/split-lines
       (map-indexed
        (fn [y row]
          (->> (s/split row #"")
               (map-indexed (fn [x col] [[x, y] col])))))
       (reduce concat)
       (filter (fn [[[x y] col]] (= col "#")))
       (map first)
       (into #{})))

(defn trajectory-data
  [point asteroid]
  (let [d   (map - asteroid point)
        m   (mag d)
        u   (normalize d)]
    {:point asteroid :delta d :mag m :unit u}))

(defn asteroids-visible
  [asteroid-set point]
  (let [others (into [] (set/difference asteroid-set #{point}))
        diffs  (map #(map - %1 point) others)
        mags   (map mag diffs)
        norms  (map normalize diffs)
        all    (partition 4 (interleave others diffs mags norms))
        sorted (sort-by (fn [coll] (nth coll 2)) > all)]
    (->> sorted
         (reduce (fn [m [loc diff mag norm]]
                   (merge m {norm loc}))
                 {})
         count)))

(defn part-one []
  (let [asteroids (asteroid-locs input)]
    (->> asteroids
         (map (fn [loc] [loc (asteroids-visible asteroids loc)]))
         (sort-by second >)
         first
         second)))

(println (part-one))
