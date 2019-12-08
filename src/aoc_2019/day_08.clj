(ns aoc-2019.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (-> "day_08.txt" io/resource io/reader line-seq first))
(def test-input "0222112222120000")

(defn data [input]
  (as-> input $
    (s/split $ #"")
    (map read-string $)))

(defn layers [width height coll]
  (partition (* width height) coll))

(defn count-digit [d coll]
  (->> coll (filter #(= % d)) count))

(defn has-less-than [d a b]
  (if (< (count-digit d a) (count-digit d b))
    a b))

(defn image->str [width image]
  (->> image
       (partition width)
       (map (partial apply str))
       (s/join "\n")
       (map #(if (= % \0) " " %))))

(defn part-one []
  (->> input
       data
       (layers 25 6)
       (reduce (partial has-less-than 0))
       (#(* (count-digit 1 %)
            (count-digit 2 %)))))

(defn mask-pixel [p1 p2]
  (if (< p2 2) p2 p1))

(defn mask-layer [l1 l2]
  (->> (interleave l1 l2)
       (partition 2)
       (map (partial apply mask-pixel))))

(defn part-two []
  (->> input
       data
       (layers 25 6)
       reverse
       (reduce mask-layer)
       (image->str 25)
       println))

(do (println (part-one) "\n" (part-two)))
