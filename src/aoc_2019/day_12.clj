(ns aoc-2019.day-12
  (:require [clojure.java.io :as io]))

(def input (-> "day_12.txt" io/resource io/reader line-seq))

(def test-input ["<x=-1, y=0, z=2>"
                 "<x=2, y=-10, z=-7>"
                 "<x=4, y=-8, z=8>"
                 "<x=3, y=5, z=-1>"])

(defn read-vec [vec-str]
  (->> vec-str
       (re-matches #"<x=(.+) y=(.+) z=(.+)>")
       rest
       (mapv read-string)))

(defn moon [[x y z]]
  {:pos [x y z]
   :vel [0 0 0]})

(defn moons [input]
  (mapv (comp moon read-vec) input))

(defn gravity-delta [n1 n2]
  (cond (= n1 n2) 0
        (< n1 n2) 1
        (> n1 n2) -1))

(defn transpose [m]
  (apply mapv vector m))

(defn next-vel-value [ns n v]
  (reduce #(+ %1 (gravity-delta n %2)) v ns))

(defn next-vel-component [pos-component vel-component]
  (map-indexed (fn [i v] (next-vel-value pos-component (pos-component i) v))
               vel-component))

(defn next-moons [moons]
  (let [pos-components (transpose (map :pos moons))
        vel-components (transpose (map :vel moons))
        new-vel-comps  (map next-vel-component pos-components vel-components)
        new-vels       (transpose new-vel-comps)
        new-ps         (mapv #(mapv + %1 %2) (map :pos moons) new-vels)
        next-moon      (fn [moon pos vel] (merge moon {:pos pos :vel vel}))]
    (mapv (partial apply next-moon)
          (partition 3 (interleave moons new-ps new-vels)))))

(defn abs-sum [v]
  (->> v
       (map #(Math/abs %))
       (reduce +)))

(defn total-energy [moons]
  (let [pot (map (comp abs-sum :pos) moons)
        kin (map (comp abs-sum :vel) moons)
        tot (map * pot kin)]
    (reduce + tot)))

(defn part-one []
  (->> input
       moons
       (iterate next-moons)
       (take 1001)
       last
       total-energy
       println))

;; TODO find out how long it takes for one moon to do its orbit and reach its original position ;; TODO if that works then do it for the other moons
;; TODO find the least common multiple of their respective cycles
;; TODO that number should be the total amount of the time for the cycle of the overall system

(do (part-one))
