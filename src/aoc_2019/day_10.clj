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

(defn angle
  "Get the angle in degrees of a unit vector relative to the negative y axis"
  [[x y]]
  (let [deg (Math/toDegrees (Math/atan2 x (- y)))]
    (if (neg? deg) (+ deg 360) deg)))

(defn asteroid-points
  "Get the location of all the asteroids"
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
  "Get relevant trajectory data for an asteroid relative to a point"
  [point asteroid]
  (let [d   (map - asteroid point)
        m   (mag d)
        u   (normalize d)]
    {:point asteroid :delta d :mag m :unit u}))

(defn all-trajectories
  "Get relevant trajectory data for all the asteroids relative to a point"
  [asteroid-set point]
  (let [others       (into [] (set/difference asteroid-set #{point}))]
    (map #(trajectory-data point %) others)))

(defn asteroids-visible
  "Get the number of asteroids visible from a point"
  [asteroid-set point]
  (->> point
       (all-trajectories asteroid-set)
       (sort-by :mag >)
       (reduce (fn [m {p :point u :unit}] (merge m {u p})) {})
       count))

(defn find-monitoring-station
  "Find the best location for a monitoring station and how many asteroids it sees"
  [input]
  (let [asteroids (asteroid-points input)]
    (->> asteroids
         (map (fn [point] [point (asteroids-visible asteroids point)]))
         (sort-by second >)
         first)))

(def test-input ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##")

(def more-test-input ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(defn vaporization-order
  "Determine the best order in which to vaporize all the asteroids"
  [asteroid-set point]
  (let [rots-by-angle (->> point
                           (all-trajectories asteroid-set)
                           (map #(assoc % :angle (angle (:unit %))))
                           (group-by :angle)
                           (sort-by first)
                           (map (fn [[u xs]] [u (sort-by :mag xs)]))
                           (map second))
        num-rots (->> rots-by-angle (map count) (apply max))]
    (->> (range 0 num-rots)
         (mapcat (fn [i] (map #(nth % i nil) rots-by-angle)))
         (map :point)
         (filter #(not (nil? %))))))

(defn part-one []
  (second (find-monitoring-station input)))

(defn part-two []
  (as-> input $
       (find-monitoring-station $)
       (first $)
       (vaporization-order (asteroid-locs input) $)
       (nth $ 199)
       ((fn [[x y]] (+ (* x 100) y)) $)))

(do (println (str (part-one) "\n" (part-two))))
