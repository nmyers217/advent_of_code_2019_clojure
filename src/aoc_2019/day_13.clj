(ns aoc-2019.day-13
  (:require [clojure.string :as s]
            [aoc-2019.intcode :as ic]))

(def id->tile {0 :empty 1 :wall 2 :block 3 :paddle 4 :ball})

(def tile->c  {:empty " " :wall "$" :block "#" :paddle "=" :ball "*"})

(defn game-output [file]
  (let [q      (atom [])
        memory (ic/read-intcode-file file)
        i-fn   (comp read-string read-line)
        o-fn   (fn [v] (swap! q conj v))]
    (do (ic/intcode memory i-fn o-fn)
        @q)))

(defn read-instruction [[x y id]]
  {:x x :y y :t (id->tile id)})

(defn read-instructions [output]
  (->> output
       (partition 3)
       (map read-instruction)))

(defn play [instructions]
  (let [width  (inc (apply max (map :x instructions)))
        height (inc (apply max (map :y instructions)))
        build  (fn [n v] (into [] (repeat n v)))
        screen (build height (build width (tile->c :empty)))
        paint  (fn [s x y t] (update-in s [y x] (fn [v] (tile->c t))))]
    (reduce (fn [s {:keys [x y t]}] (paint s x y t))
            screen instructions)))

(defn print-screen [game]
  (do
    (doseq [row game]
      (println (s/join #"" row)))
    game))

(defn count-tile [tile game]
  (->> game
       flatten
       (filter #(= (tile->c tile) %))
       count))

(defn part-one []
  (->> "day_13.txt"
       game-output
       read-instructions
       play
       print-screen
       (count-tile :block)
       println))

(do (part-one))
