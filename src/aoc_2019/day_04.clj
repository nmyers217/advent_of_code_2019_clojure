(ns aoc-2019.day-04
  (:require [clojure.string :as s]))

(def input "273025-767253")

(defn read-range [input]
  (let [[beg end] (map read-string (s/split input #"-"))]
    (range beg end)))

(defn has-adjacent?
  "Check a string two see if a character occurs twice in a row"
  [str]
  (->> str (re-find #"(\d)\1") some?))

(defn not-descending?
  "Check if the digits in a string do not descend"
  [str]
  (apply <= (map int str)))

(defn has-adjacent-pair?
  "This time we check for a pair of EXACTLY 2 chars"
  [str]
  (:status
   (reduce
    (fn [state c]
      (let [same (= c (:last state))
            prev-n (:num-so-far state)
            n (if same (inc prev-n) 1)]
        (if (and (not same) (= prev-n 2))
          (reduced state)
          (merge state
                 {:last c :num-so-far n :status (= n 2)}))))
    {:last nil :num-so-far 0 :status nil}
    str)))

(defn get-answer [& validators]
  (->> input
       read-range
       (map str)
       (filter (apply every-pred validators))
       count))

(defn part-one []
  (get-answer has-adjacent? not-descending?))
(defn part-two []
  (get-answer has-adjacent-pair? not-descending?))

(do (println (str (part-one) "\n" (part-two))))
