(ns aoc-2019.day-11
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.core.async :as async]
            [aoc-2019.intcode :as ic]
            [clojure.string :as s]))


(defn robot []
  {:dir [0 -1]
   :pos [0 0]
   :action :paint
   :whites #{}
   :blacks #{}})

(defn next-dir [input [x y]]
  (if (not (zero? input))
    [(- y)    x]
    [   y  (- x)]))

(defn process-input
  [{:keys [dir pos action whites blacks]} input]
  (let [paint? (= action :paint)
        move?  (= action :move)
        n-dir (if move? (next-dir input dir) dir)]
    {:dir n-dir
     :pos (if move? (map + pos n-dir) pos)
     :action (if paint? :move :paint)
     :whites (cond
               (and paint? (= input 1)) (set/union whites #{pos})
               (and paint? (= input 0)) (set/difference whites #{pos})
               true whites)
     :blacks (cond
               (and paint? (= input 0)) (set/union blacks #{pos})
               (and paint? (= input 1)) (set/difference blacks #{pos})
               true blacks)}))


(defn process-output
  [{:keys [pos whites blacks]}]
  (cond (get whites pos) 1
        (get blacks pos) 0
        true 0))

(def hull-robot-program (ic/read-intcode-file "day_11.txt"))

(defn robot-controller
  [robot program]
  (let [r    (atom robot)
        i-fn (fn [] (process-output @r))
        o-fn (fn [v] (swap! r process-input v))]
    (do (ic/intcode program i-fn o-fn)
        @r)))

(defn part-one []
  (let [{w :whites b :blacks} (robot-controller (robot) hull-robot-program)
        ps (set/union w b)]
    (count ps)))

(defn part-two []
  (let [r                (merge (robot) {:whites #{[0 0]}})
        {whites :whites} (robot-controller r hull-robot-program)
        find-max-dim     (fn [key-fn coll] (apply max (map key-fn coll)))
        height           (+ 2 (find-max-dim second whites))
        width            (+ 2 (find-max-dim first whites))
        grid             (into [] (repeat height (into [] (repeat width " "))))
        paint-cell-white (fn [grid [x y]] (update-in grid [y x] (fn [v] \#)))
        painted-grid     (reduce paint-cell-white grid whites)]
    (doseq [row painted-grid]
      (println (s/join #"" row)))))

(do (println (part-one))
    (part-two))
