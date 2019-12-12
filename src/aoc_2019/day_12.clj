(ns aoc-2019.day-12
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (-> "day_12.txt" io/resource io/reader line-seq))


(defn read-vec
  "Read a 3d vector from a string"
  [vec-str]
  (->> vec-str
       (re-matches #"<x=(.+) y=(.+) z=(.+)>")
       rest
       (mapv read-string)))

(defn moon
  "Get the initial state of a moon"
  [[x y z]]
  {:pos [x y z] :vel [0 0 0]})

(defn moons
  "Get the initial state of all the moons"
  [input]
  (mapv (comp moon read-vec) input))

(defn gravity-delta
  "Get the delta to adjust `n1` by for `n2`"
  [n1 n2]
  (cond (= n1 n2) 0
        (< n1 n2) 1
        (> n1 n2) -1))

(defn next-vel-value
  "Given a vec of either x,y, or z values, a starting velocity, and one of the values in the vec
  calculate what the new velocity should be"
  [ns n v]
  (reduce #(+ %1 (gravity-delta n %2)) v ns))

(defn next-vel-component
  "Calculate what an entire component of velocity values should update to"
  [pos-component vel-component]
  (map-indexed (fn [i v] (next-vel-value pos-component (pos-component i) v))
               vel-component))

(defn transpose [m]
  (apply mapv vector m))

(defn next-moons
  "Advance the moons forward one slice of time"
  [moons]
  (let [pos-components (transpose (map :pos moons))
        vel-components (transpose (map :vel moons))
        new-vel-comps  (map next-vel-component pos-components vel-components)
        new-vels       (transpose new-vel-comps)
        new-ps         (mapv #(mapv + %1 %2) (map :pos moons) new-vels)
        next-moon      (fn [moon pos vel] (merge moon {:pos pos :vel vel}))]
    (mapv (partial apply next-moon)
          (partition 3 (interleave moons new-ps new-vels)))))

(defn abs-sum
  "Sum the absolute vlaues of a vector"
  [v]
  (->> v (map #(Math/abs %)) (reduce +)))

(defn total-energy
  "Calcualte the total energy of a system of moons"
  [moons]
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

;; FIXME this is super slow, but still clocks in at under 30 secs on my machine
(defn axis-cycle-count
  "Determine how long it takes each component/axis of the system to repeat
  this will return the count of time slices that each axis needs to cycle back"
  [axis moons]
  (let [axes (fn [ms]
               (let [ps     (transpose (map :pos ms))
                     vs     (transpose (map :vel ms))]
                 {:p-axis (get ps axis) :v-axis (get vs axis)}))

        seen-already? (fn [s m]
                        (let [a (axes m)]
                          (if (get s a) (reduced nil) (set/union s #{a}))))]
    (->> moons
         (iterate next-moons)
         (reductions seen-already? #{})
         (drop 2)
         (take-while (comp not nil?))
         count
         inc)))

(defn gcd
  "Greatest common divisor"
  [a b]
  (if (zero? b) a (recur b, (mod a b))))

(defn lcm
  "Least common multiple"
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v]
  (reduce lcm v))

(defn part-two []
  (let [moons      (moons input)
        cycles     (pmap #(axis-cycle-count % moons) (range 0 3))
        full-cycle (apply lcmv cycles)]
    (println full-cycle)))

(do (part-one) (part-two))
