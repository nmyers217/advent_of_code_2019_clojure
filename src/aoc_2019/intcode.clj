(ns aoc-2019.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(defn read-intcode-file
  "Reads an intcode program from a give resource file name"
  [file-name]
  (->> file-name io/resource slurp (#(str "[" % "]")) read-string))


(def ^:dynamic *intcode-debug* false)

(defn read-op-code
  "Parses an op-code into its code and a list of modes for its params"
  [op-code]
  (let [code (as-> op-code $
               (str $) (reverse $) (take 2 $) (reverse $)
               (drop-while #(= % \0) $) (s/join #"" $) (read-string $))
        param-modes (as-> op-code $
                      (str $) (reverse $) (drop 2 $)
                      (map (comp read-string str) $)
                      (concat $ (repeat 0)) (take 3 $))]
    {:code code :param-modes param-modes}))

(defn memory-helpers
  "Get some helper closures for working with memory"
  [memory ip rb]
  (let [ip+             (fn [delta] (+ ip delta))
        mem-index       (fn [i] (memory i))
        mem-index-rel   (comp mem-index ip+)
        mem-deref       (fn [i] (memory (memory i)))
        mem-deref-rel   (comp mem-deref ip+)
        mem-set         (fn [i v] (assoc memory i v))
        mem-set-ptr     (fn [ptr-i v] (mem-set (mem-index ptr-i) v))
        mem-set-ptr-rel (fn [ptr-delta v] (mem-set-ptr (ip+ ptr-delta) v))

        with-modes    (fn [ip-deltas modes]
                        (partition 2 (interleave ip-deltas modes)))
        read  (fn [ip-delta mode]
                (case mode
                  0 (mem-deref-rel ip-delta)
                  1 (mem-index-rel ip-delta)
                  2 (-> ip-delta mem-index-rel (+ rb) mem-index)))
        write (fn [ip-delta mode v]
                (case mode
                  0 (mem-set-ptr-rel ip-delta v)
                  2 (-> ip-delta mem-index-rel (+ rb) (mem-set v))))]
    {:read read
     :write write
     :with-modes with-modes}))

(defn next-memory
  "Calculates the next state of the memory and performs IO"
  [memory input-fn output-fn ip rb]
  (let [{:keys [read write with-modes]} (memory-helpers memory ip rb)
        {c :code pm :param-modes}       (read-op-code (read 0 1))]
    (when *intcode-debug* (println c pm))
    (case c
      1 (write 3 (last pm)
               (apply + (map (partial apply read) (with-modes [1 2] pm))))
      2 (write 3 (last pm)
               (apply * (map (partial apply read) (with-modes [1 2] pm))))
      3 (write 1 (first pm) (input-fn))
      4 (do (-> (read 1 (first pm)) output-fn)
            memory)
      5 memory
      6 memory
      7 (write 3 (last pm)
               (apply #(if (< %1 %2) 1 0)
                      (map (partial apply read) (with-modes [1 2] pm))))
      8 (write 3 (last pm)
               (apply #(if (= %1 %2) 1 0)
                      (map (partial apply read) (with-modes [1 2] pm))))
      9 memory)))

(defn next-ip
  "Calcualtes the next ip given some memory, the current ip, and rb"
  [memory ip rb]
  (let [{:keys [read write with-modes]} (memory-helpers memory ip rb)
        {c :code pm :param-modes}       (read-op-code (read 0 1))]
    (case c
      1 (+ ip 4)
      2 (+ ip 4)
      3 (+ ip 2)
      4 (+ ip 2)
      5 (if (not= 0 (read 1 (first pm)))
          (read 2 (second pm))
          (+ ip 3))
      6 (if (= 0 (read 1 (first pm)))
          (read 2 (second pm))
          (+ ip 3))
      7 (+ ip 4)
      8 (+ ip 4)
      9 (+ ip 2))))

(defn next-rb
  "Calculates the next rb given some memory, the ip, and the current rb"
  [memory ip rb]
  (let [{:keys [read write with-modes]} (memory-helpers memory ip rb)
        {c :code pm :param-modes}       (read-op-code (read 0 1))]
    (if (= c 9)
      (+ rb (read 1 (first pm)))
      rb)))

(defn intcode
  "Takes the current parameters of an intcode computer and completes the program"
  ([memory]
   (intcode memory (comp read-string read-line) println))

  ([memory input-fn output-fn]
   (intcode memory input-fn output-fn nil))

  ([memory input-fn output-fn memory-output-fn]
   (let [padded-memory (vec (concat memory (repeat 1024 0)))]
     (intcode padded-memory input-fn output-fn memory-output-fn 0 0)))

  ([memory input-fn output-fn memory-output-fn ip rb]
   (when *intcode-debug* (println ip "@" memory))
   (if (not= (memory ip) 99)
     (recur (next-memory memory input-fn output-fn ip rb)
            input-fn
            output-fn
            memory-output-fn
            (next-ip memory ip rb)
            (next-rb memory ip rb))
     (when (not (nil? memory-output-fn))
       (do (memory-output-fn memory)
           nil)))))
