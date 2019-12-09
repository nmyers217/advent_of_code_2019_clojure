(ns aoc-2019.day-09
  (:require [aoc-2019.intcode :as ic]))


(def boost-program (ic/read-intcode-file "day_09.txt"))


(ic/intcode boost-program #(read-string "1") println)
(ic/intcode boost-program #(read-string "2") println)
