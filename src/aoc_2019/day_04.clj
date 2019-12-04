(ns aoc-2019.day-04
  (:require [clojure.string :as s]))

(def input "273025-767253")

(defn read-range [input]
  (let [[beg end] (map read-string (s/split input #"-"))]
    (range beg end)))

(defn has-adjacent?
  "Check a string two see if a character occurs twice in a row"
  [str]
  (true?
   (reduce (fn [a b]
             (if (= a b) (reduced true) b))
           str)))

(defn not-descending?
  "Check if the digits in a string do not descend"
  [str]
  (not
   (false?
    (reduce (fn [a b]
              (if (< (int b) (int a))
                (reduced false)
                b))
            str))))

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

(defn is-valid? [num]
  (let [s (str num)]
    (and (has-adjacent? s) (not-descending? s))))

(defn is-more-valid? [num]
  (let [s (str num)]
    (and (not-descending? s) (has-adjacent-pair? s))))

(defn get-answer [validator]
  (->> input
       read-range
       (filter validator)
       count))

(defn part-one [] (get-answer is-valid?))
(defn part-two [] (get-answer is-more-valid?))
(do (println (str (part-one) "\n" (part-two))))
