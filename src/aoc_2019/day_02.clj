(ns aoc-2019.day-02
  (:require [clojure.java.io :as io]))

(def data
  (-> "day_02.txt"
      io/resource
      slurp
      (#(str "[" % "]"))
      read-string))

(defn get-state
  "Get the inital state for a program"
  [data]
  {:ip 0
   :data data
   :ops {1 + 2 *}})

(defn get-next-state
  "Advance program state to its next state"
  [state]
  (let [ip (:ip state)
        data (:data state)
        op-code (data ip)]
    (if (= op-code 99)
      nil
      (let [op (get (:ops state) op-code)
            input-1 (data (data (+ ip 1)))
            input-2 (data (data (+ ip 2)))
            result-loc (data (+ ip 3))
            result (op input-1 input-2)]
        {:ip (+ ip 4)
         :data (assoc data result-loc result)
         :ops (:ops state)}))))

(defn prime-data
  "Prime program data with a noun ad verb"
  [noun verb data]
  (-> data
      (assoc 1 noun)
      (assoc 2 verb)))

(defn run-program
  "Advance a program's state until completion"
  [prev-state state]
  (if (nil? state)
    prev-state
    (run-program state (get-next-state state))))

(defn run-and-get-output
  "Given a noun verb and some data,
  run a full program and extract its output"
  [noun verb data]
  (let [state (->>
               data
               (prime-data noun verb)
               get-state)
        end-state (run-program nil state)]
    ((:data end-state) 0)))

(defn part-one []
  (run-and-get-output 12 2 data))

(defn part-two []
  (reduce
   (fn [res [noun verb]]
     (let [output
           (run-and-get-output noun verb data)]
       (if (= output 19690720)
         (reduced (+ (* 100 noun) verb))
         output)))
   (for [noun (range 0 100)
         verb (range 0 100)]
     [noun verb])))

(do
  (println (str (part-one) "\n" (part-two))))
