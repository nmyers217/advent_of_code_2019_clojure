(ns aoc-2019.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def baby-input "R8,U5,L5,D3\nU7,R6,D4,L4")

(def test-input "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")

(def input (->> "day_03.txt" io/resource slurp))

(defn read-move [move-str]
  {:dir (first move-str)
   :mag (read-string (subs move-str 1))})

(defn read-wire [wire-str]
  {:loc [0 0] ; a location is just [x y]
   :moves (map read-move (s/split wire-str #","))
   :path '() ; the locations the wire traveled in order
   :path-index {} ; steps taken for each location in path
   :steps 0 ; the steps taken so far
   })

(defn read-input [input]
  (->> input s/split-lines (map read-wire)))

(defn locs-for-move
  "Given a starting loc and a move, get all the locs that comprise the move"
  [[start-x start-y] {d :dir m :mag}]
  (when-let [dir-vec
             (case d \U [0 -1] \D [0 1] \L [-1 0] \R [1 0])]
    (let [scalars (range 1 (inc m))
          deltas (map (fn [[x y] s] [(* x s) (* y s)])
                      (repeat (count scalars) dir-vec)
                      scalars)]
      (map #(map + %1 %2)
           (repeat (count deltas) [start-x start-y])
           deltas))))

(defn next-wire
  "Advance a wire to its next state"
  [wire]
  (when-let [move (first (:moves wire))]
    (let [new-locs (locs-for-move (:loc wire) move)
          num-locs (count new-locs)
          new-indexes (zipmap
                       new-locs
                       (drop (inc (:steps wire)) (range)))]
      (merge
       wire
       {:loc (last new-locs)
        :moves (rest (:moves wire))
        :path (concat (:path wire) new-locs)
        :path-index (merge new-indexes (:path-index wire))
        :steps (+ (:steps wire) num-locs)}))))

(defn trace-wire
  "Fully trace a wire to completion"
  [wire]
  (if (empty? (:moves wire))
    wire
    (trace-wire (next-wire wire))))

(defn intersections
  "Get a list of all the locs where some wires crossed"
  [wires]
  (into [] (apply set/intersection
                  (map #(into #{} (:path %)) wires))))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn part-one []
  (->> input
       read-input
       (map trace-wire)
       intersections
       (map (partial manhattan [0 0]))
       sort
       first))

(defn combined-steps
  "Get the total steps all wires needed to reach a location"
  [wires loc]
  (reduce + (map #(get (:path-index %) loc) wires)))

(defn part-two []
  (let [traced (->> input read-input (map trace-wire))
        intersects (intersections traced)
        total-steps (map #(combined-steps traced %)
                         intersects)]
    (-> total-steps sort first)))
