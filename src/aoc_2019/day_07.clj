(ns aoc-2019.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as async]))

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

;; TODO: pass in the io-mapping
(defn io-state [phases]
  (let [s {0 {:std-in (async/chan) :std-out 1}
           1 {:std-in (async/chan) :std-out 2}
           2 {:std-in (async/chan) :std-out 3}
           3 {:std-in (async/chan) :std-out 4}
           4 {:std-in (async/chan) :std-out (async/chan)}}]
    ;; Prime the first channel with phase and input 0
    (async/onto-chan (:std-in (get s 0)) [(phases 0) 0] nil)
    ;; Send rest of the phases to their channels
    (doseq [i (range 1 (count phases))]
      (async/onto-chan (:std-in (get s i)) [(phases i)] nil))
    s))

;; Thread safe logging for debugging stuff
(def log-lock (Object.))
(defn log [& args]
  (locking log-lock
    (apply println args)))

;; TODO: pass in the io-mapping
(defn run-amplifier-controller-software
  [data io-data]
  (let [run (fn [amp]
              (let [io-d (get io-data amp)
                    i (fn []
                        (let [res (async/<!! (:std-in io-d))]
                          ;;(log amp "<=" res)
                          res))
                    o (fn [v]
                        (let [out-amp (:std-out io-d)
                              std-out (if (int? out-amp)
                                        (:std-in (get io-data out-amp))
                                        out-amp)]
                          ;;(log amp "=>" v)
                          (async/go (async/>! std-out v))))
                    s (initial-state data 0 i o)]
                (future (run-program nil s))))
        result (->> (range 0 (count (keys io-data)))
                    (map run) ; run all the amps
                    (map deref) ; wait for them all to finish
                    (interleave (vals io-data)) ; pair them up with data
                    (partition 2)
                    last ; get the last amp
                    first ; get its io data
                    :std-out ; get its std-out
                    (#(if (int? %) (get io-data %) %)) ; indirection
                    async/<!!) ; wait for the output
        ]
    result))

(defn part-one [data]
  (->>
   (combo/permuted-combinations (range 0 5) 5)
   (map
    (comp
     (partial run-amplifier-controller-software data)
     io-state
     vec))
   (apply max)))

(defn part-two [data]
  (->>
   (combo/permuted-combinations (range 5 10) 5)))

(run-amplifier-controller-software
 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
 (io-state [9 8 7 6 5]))

;;(part-two [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
;;(do (println (part-one data) "\n"))
