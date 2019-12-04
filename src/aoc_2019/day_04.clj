(ns aoc-2019.day-04
  (:require [clojure.string :as s]))

(def input "273025-767253")

(defn read-range [input]
  (let [[beg end] (map read-string (s/split input #"-"))]
    (range beg end)))

(defn has-adjacent? [str]
  (true?
   (reduce (fn [a b]
             (if (= a b) (reduced true) b))
           str)))

(defn is-ascending? [str]
  (not
   (false?
    (reduce (fn [a b]
              (if (< (int b) (int a))
                (reduced false)
                b))
            str))))

(defn is-valid? [num]
  (let [s (str num)]
    (and (has-adjacent? s) (is-ascending? s))))

(defn part-one []
  (->> input
       read-range
       (filter is-valid?)
       count))

(defn has-adjacent-pair? [str]
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


(defn is-more-valid? [num]
  (let [s (str num)]
    (and (is-ascending? s) (has-adjacent-pair? s))))


(defn part-two []
  (->> input
       read-range
       (filter is-more-valid?)
       count))

(do
  (println (str (part-one) "\n" (part-two))))
