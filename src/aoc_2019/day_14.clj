(ns aoc-2019.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "day_14.txt" io/resource slurp))

(defn read-entry [entry]
  (->> (s/split entry #"=>")
       (map (comp (partial into {})
               (partial map (fn [[_ qty chem]] [chem (read-string qty)]))
               (partial re-seq #"(\d+) ([A-Z]+)")))
       (zipmap [:in :out])))

(defn data [input]
  (->> input s/split-lines (map read-entry)))

(defn find-first-in [pred coll]
  (some #(if (pred %) % nil) coll))

(defn find-reaction [data chemical]
  (let [event?   (fn [reaction]
                   (find-first-in (fn [[c]] (= c chemical))
                                  (:out reaction)))
        reaction (find-first-in event? data)]
    {:reaction reaction
     :result   (second (event? reaction))}))

(defn map-mul [m x]
  (->> m (map (fn [[k v]] [k (* x v)])) (into {})))

(defn calc-cost [reactions requirements]
  (loop [requirements requirements]
    ;;(println requirements)
    (if-let [[chemical amount]
             (find-first-in (fn [[chemical amount]]
                              (and (pos-int? amount)
                                   (not= "ORE" chemical)))
                            requirements)]
      (let [{:keys [reaction result]} (find-reaction reactions chemical)
            multiple      (+ (quot amount result)
                             (if-not (zero? (mod amount result)) 1 0))
            requirements' (merge-with + requirements
                                      (map-mul (:out reaction) (* -1 multiple))
                                      (map-mul (:in reaction) multiple))]
        (recur requirements'))
      requirements)))

(defn part-one []
  (-> input
      data
      (calc-cost {"FUEL" 1})
      (get "ORE")
      println))

(defn part-two []
  (let [data         (data input)
        ore-supply   1000000000000
        cost         (fn [n]
                       (-> data (calc-cost {"FUEL" n}) (get "ORE")))
        ore-per-fuel (cost 1)
        start-fuel   (quot ore-supply ore-per-fuel)]
    (loop [prev 0
           fuel start-fuel]
      (let [$ (cost fuel)]
        ;;(println $)
        (if (>= $ ore-supply)
          (println prev)
          (let [ore-left      (- ore-supply $)
                est-fuel-left (quot ore-left ore-per-fuel)]
            (recur fuel
                   (+ fuel (if (pos? est-fuel-left)
                             est-fuel-left
                             1)))))))))

(part-one)
(part-two)

