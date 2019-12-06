(ns aoc-2019.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input
  (-> "day_06.txt" io/resource slurp))

(defn data [input]
  (->> input
       s/split-lines
       (map #(s/split % #"\)"))))

(defn graph [data]
  (reduce
   (fn [graph [orbited by]]
     (update graph orbited #(set/union % #{by})))
   {}
   data))

(defn total-graph
  ([node graph]
   (total-graph 1 node graph))
  ([acc node graph]
   (let [orbits (get graph node)
         cur-total (* acc (count orbits))]
     (+ cur-total
        (reduce
         +
         (map #(total-graph (inc acc) % graph) orbits))))))

(defn part-one []
  (->> input data graph (total-graph "COM")))

(defn find-planet-orbited-by [node graph]
  (->> graph
       (filter #(get (second %) node))
       first
       first))

(defn find-path
  ([graph start end]
   (find-path [] #{start} graph start end))
  ([path visited graph cur end]
   (let [orbited-by (get graph cur)
         is-dead-end? (or (= cur "COM") (= 0 (count orbited-by)))
         path-found? (= cur end)]
     (cond
       path-found? (conj path cur)
       is-dead-end? nil
       true (let [orbiting (find-planet-orbited-by cur graph)
                  checks (filter
                          #(not (contains? visited %))
                          (conj orbited-by orbiting))]
              (->>
               checks
               (map #(find-path (conj path cur)
                                (conj orbited-by cur)
                                graph % end))
               (filter #(not (nil? %)))
               (sort-by count)
               first))))))

(defn part-two []
  (let [graph (-> input data graph)
        start (find-planet-orbited-by "YOU" graph)
        end (find-planet-orbited-by "SAN" graph)]
    (-> graph
        (find-path start end)
        count
        dec)))

(do (println (str (part-one) "\n" (part-two))))
