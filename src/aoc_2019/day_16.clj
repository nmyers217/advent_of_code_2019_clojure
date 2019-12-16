(ns aoc-2019.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (-> "day_16.txt"
      io/resource
      io/reader
      line-seq
      first))

(defn data [input]
  (mapv read-string (s/split input #"")))

(defn pattern [n]
  (->> [0 1 0 -1]
       (mapcat #(repeat n %))
       cycle
       (drop 1)))

(defn last-digit [n]
  (-> n str last str read-string))

(defn combine [input-signal pattern]
  (->> pattern
       (interleave input-signal)
       (partition 2)
       (map (partial apply *))
       (reduce +)
       last-digit))

(defn output-signal [input-signal]
  (->> (range 1 (inc (count input-signal)))
       (map #(combine input-signal (pattern %)))))

(->>  input
      data
      (iterate output-signal)
      (take 101)
      last
      (take 8)
      (apply str)
      println)
