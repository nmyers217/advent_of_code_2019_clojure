(ns aoc-2019.day-12
  (:require [clojure.java.io :as io]))

(def input (-> "day_12.txt" io/resource io/reader line-seq))

(def test-input ["<x=-8, y=-10, z=0>"
                 "<x=5, y=5, z=10>"
                 "<x=2, y=-7, z=3>"
                 "<x=9, y=-8, z=-3>"])

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

(defn next-velocity-component [n v ns]
  (reduce #(+ %1 (gravity-delta n %2)) v ns))

(defn abs-sum [v]
  (->> v
       (map #(Math/abs %))
       (reduce +)))

;; FIXME i feel like there was a way more elegant way to do this, but i couldn't figure it out
(defn next-moons [moons]
  (let [get-component  (fn [c] (mapv #((:pos %) c) moons))
        components     (mapv get-component (range 0 3))
        gravity-deltas (map-indexed
                        (fn [vel-c comps]
                          (map-indexed
                           (fn [moon-i c]
                             (next-velocity-component c (get-in moons [moon-i :vel vel-c]) comps))
                           comps))
                        components)
        positions      (mapv :pos moons)
        new-velocities (mapv (fn [moon-i] (mapv #(nth % moon-i) gravity-deltas))
                             (range (count moons)))
        new-positions  (mapv #(mapv + %1 %2) positions new-velocities)
        ]
    (mapv merge
          (mapv #(hash-map :pos %) new-positions)
          (mapv #(hash-map :vel %) new-velocities))))

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

(do (part-one))
