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
  {:x x :y y :id id :t (id->tile id)})

(defn read-instructions [output]
  (->> output
       (partition 3)
       (map read-instruction)))

;; TODO handle tracking the score somehow
(defn play [instructions]
  (let [width  (inc (apply max (map :x instructions)))
        height (inc (apply max (map :y instructions)))
        build  (fn [n v] (into [] (repeat n v)))
        screen (build height (build width (tile->c :empty)))
        paint  (fn [s x y t id]
                 (if (= [x y] [-1 0])
                   (do (println (str "Score: " id))
                       s)
                   (update-in s [y x] (fn [v] (tile->c t)))))]
    (reduce (fn [s {:keys [x y t id]}] (paint s x y t id))
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

;; TODO pass in a sequence of inputs to make
;; TODO track score somehow
;; TODO returns nil if not all inputs exhausted, otherwise it returns the score
(defn play-with-joystick [file]
  (let [q      (atom [])
        p      (fn [] (->> @q read-instructions play print-screen))
        memory (assoc (ic/read-intcode-file file) 0 2)
        i-fn   (fn [] (p) ((comp read-string read-line)))
        o-fn   (fn [v] (swap! q conj v))]
    (do (ic/intcode memory i-fn o-fn)
        @q)))

(play-with-joystick "day_13.txt")

;; TODO create this tree structure that accounts for every possible input sequence
;; TODO investigate using a zipper for this one
;;         -1                     0                     1
;;  -1      0      1      -1      0      1       -1     0      1
;;-1 0 1 -1 0 1 -1 0 1  -1 0 1 -1 0 1 -1 0 1  -1 0 1 -1 0 1 -1 0 1
;; TODO for every possible path we try to play that sequence of inputs
;; TODO we map to a score if we get one, if we can't play the amount of moves then nil
;; TODO trace all the paths to a nil
;; TODO find the path that leads to the biggest score, then that's the answer
