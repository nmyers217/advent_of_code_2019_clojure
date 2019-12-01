(ns aoc-2019.day-01
  (:require [clojure.java.io :as io]))

(def input
  (->> "day_01.txt" io/resource io/reader))

(def data
  (->> input line-seq (map read-string)))

(defn get-fuel
  "Calculates the required fuel for a given mass"
  [mass]
  (-> mass (/ 3) (Math/floor) (- 2) (int)))

(defn part-one []
  (reduce + (map get-fuel data)))

(defn get-fuel-recursive
  "Calculates the required fuel for a given mass
  as well as the fuel for its fuel"
  [mass]
  (let [fuel (get-fuel mass)]
    (if (<= fuel 0)
      0
      (+ fuel (get-fuel-recursive fuel)))))

(defn part-two []
  (reduce + (map get-fuel-recursive data)))

(do
  (println
   (str (part-one) "\n" (part-two))))
