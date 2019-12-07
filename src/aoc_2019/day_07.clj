(ns aoc-2019.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def data
  (-> "day_07.txt"
      io/resource
      slurp
      (#(str "[" % "]"))
      read-string))

(defn initial-state
  "Get a map that represents the state of the computer"
  ([data ip input-fn output-fn]
   {:ip ip ; The instruction pointer
    :memory data ; the program in memory
    ;; How different modes access their data
    ;; NOTE: this is only for reads since writes are always mode 0
    :modes {0 (fn [memory ip] (memory (memory ip)))
            1 (fn [memory ip] (memory ip))}
    ;; All the operations we support
    :ops {1 {:arity 2 :write? :memory :fn +}
          2 {:arity 2 :write? :memory :fn *}
          3 {:arity 0 :write? :memory :fn input-fn}
          4 {:arity 1 :write? nil :fn output-fn}
          5 {:arity 1 :write? :ip :fn #(not= % 0)}
          6 {:arity 1 :write? :ip :fn #(= % 0)}
          7 {:arity 2 :write? :memory :fn #(if (< %1 %2) 1 0)}
          8 {:arity 2 :write? :memory :fn #(if (= %1 %2) 1 0)}
          99 nil}}))

(defn num->op-code [num]
  (let [temp (str num)
        op (as-> temp $
             (count $)
             (repeat (- 5 $) "0")
             (s/join $)
             (str $ temp))
        code-str (subs op 3)
        code (if (s/starts-with? code-str "0")
               (-> code-str last str read-string)
               code-str)
        modes (as-> op $
                (subs $ 0 3)
                (s/split $ #"")
                (map read-string $)
                (reverse $))]
    {:code code :param-modes modes}))

(defn next-state
  "Advance program state to its next state"
  [{:keys [ip memory modes ops] :as state}]
  ;;(println (str memory " @ " ip))
  (let [{:keys [code param-modes]} (num->op-code (memory ip))
        ;; Helper that uses given mode to look something up in memory
        ;; at the given offset from the instruction pointer
        param-lookup (fn [[off mode]]
                       (let [mode-fn (get modes mode)]
                         (mode-fn memory (+ ip off))))]
    (when (and (not= code 99) (get ops code))
      (let [{:keys [arity fn write?] :as op} (get ops code)
            ip-offsets (take arity (drop 1 (range)))
            offsets-with-mode (zipmap ip-offsets param-modes)
            params (map param-lookup offsets-with-mode)
            result (apply fn params)
            write-offset (+ arity 1)]
        (merge state
               {:memory
                (if (= write? :memory)
                  (assoc memory (param-lookup [write-offset 1]) result)
                  memory)
                :ip
                (if (and (= write? :ip) result)
                  (param-lookup [write-offset (first (drop arity param-modes))])
                  (+ ip 1 arity (if write? 1 0)))})))))

(defn run-program
  "Advance a program's state until completion"
  [prev-state state]
  (if state
    (run-program state (next-state state))
    prev-state))

(defn amplifiers [phases]
  [{:phase (phases 0) :input 0   :output nil}
   {:phase (phases 1) :input nil :output nil}
   {:phase (phases 2) :input nil :output nil}
   {:phase (phases 3) :input nil :output nil}
   {:phase (phases 4) :input nil :output nil}])

(defn run-amplifier-controller-software
  [data phases]
  (let [amps (atom (amplifiers phases))
        amp-idx (atom 0)]
    (do
      (while (< @amp-idx 5)
        (let [std-in-src (atom '(:phase :input))
              std-in
              (fn []
                (let [source (first @std-in-src)
                      res (source (@amps @amp-idx))]
                  (do
                    (swap! std-in-src rest)
                    res)))
              std-out
              (fn [new]
                (do
                  (swap! amps
                         #(update-in % [@amp-idx :output]
                                     (fn [old] new)))
                  (when (< (inc @amp-idx) 5)
                    (swap! amps
                           #(update-in % [(inc @amp-idx) :input]
                                       (fn [old] new))))))
              program (initial-state data 0 std-in std-out)]
          (do
            ;;(println @amps)
            (run-program nil program)
            (swap! amp-idx inc))))
      (->> @amps last :output))))

(defn part-one []
  (->>
   (combo/permuted-combinations (range 0 5) 5)
   (map
    (comp
     (partial run-amplifier-controller-software data)
     vec))
   (apply max)))

(do
  (println
   (str (part-one) "\n")))
