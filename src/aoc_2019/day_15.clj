(ns aoc-2019.day-15
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as s]
            [aoc-2019.intcode :as ic]))

(def dir->input {:north 1 :south 2 :west 3 :east 4})
(def input->dir {1 :north 2 :south 3 :west 4 :east})
(def delta->dir {[0 -1] :north [0 1] :south [-1 0] :west [1 0] :east})
(def dir->opposite {:north :south :south :north :west :east :east :west})

(def output->status {0 :hit-wall 1 :moved 2 :found})
(def status->output {:hit-wall 0 :moved 1 :found 2})

(defn droid []
  {:pos [0 0]
   :path []
   :visited #{}
   :state :looking
   :tried #{}
   :trying :north
   :oxygen-pos nil})

(defn droid-reset [d]
  (merge d {:state :looking :tried #{} :trying :north}))

(defn next-pos [[x y] dir]
  (case dir
    :north [x (dec y)]
    :south [x (inc y)]
    :west  [(dec x) y]
    :east  [(inc x) y]))

(defn droid-move [{:keys [pos path visited] :as d} new-pos]
  (merge d {:pos     new-pos
            :path    (conj path pos)
            :visited (conj visited pos)}))

(defn next-valid-dirs [{:keys [pos visited tried trying]}]
  (let [all-dirs (into #{} (keys dir->input))]
    (if (= tried all-dirs)
      []
      (let [untried (set/difference all-dirs tried)]
        (->> untried
             (filter #(nil? (get visited (next-pos pos %))))
             (sort-by #(get dir->input %)))))))

(defn wall-hit [{:keys [trying tried] :as d}]
  (merge d {:tried (conj tried trying)}))

(defn turn-around [{:keys [pos path visited] :as d}]
  (merge d {:state   :turn-around
            :pos     (last path)
            :visited (conj visited pos)
            :path    (pop path)
            :tried   #{:north :south :east :west}
            :trying  (let [d (map - pos (last path))]
                       (-> d delta->dir dir->opposite))
            }))

(defn reorient [d]
  (let [next-dirs (next-valid-dirs d)]
    (if (empty? next-dirs)
      (turn-around d)
      (merge d {:trying (first next-dirs)
                :tried  (set/difference #{:north :south :west :east}
                                        (into #{} next-dirs))}))))

(defn next-droid
  [{:keys [pos path visited state trying oxygen-pos] :as d} status]
  (case status
    :moved    (if (= state :looking)
                (if (and (= pos [0 0]) (empty? (next-valid-dirs d)))
                  (merge d {:done true})
                  (-> d
                      (droid-move (next-pos pos trying))
                      droid-reset
                      reorient))
                ((comp reorient droid-reset) d))
    :hit-wall (if (= state :looking)
                (let [d'        (wall-hit d)
                      next-dirs (next-valid-dirs d')]
                  (if (empty? next-dirs)
                    (if (= pos [0 0])
                      (merge d {:done true})
                      (turn-around d'))
                    (reorient d')))
                (throw (Exception. "I hit a wall at a point I already visisted! :(")))
    :found    (if (= state :looking)
                (let [p (next-pos pos trying)]
                  (-> d
                      (merge {:oxygen-pos p})
                      (droid-move p)
                      reorient))
                ((comp reorient droid-reset) d))))

(defn droid->input
  [{:keys [state trying] :as d}]
  (when (nil? (dir->input trying))
    (println d))
  (cond
    (:done d) 42 ;; this input will terminate the program
    true      (dir->input trying)))

(defn print-droid [{:keys [pos visited oxygen-pos] :as d}]
  (if (empty? visited)
    (println "D")
    (let [get-dim  (fn [dim]
                     (fn [e coll] (->> coll (map e) (apply dim))))
          all      (conj visited pos)
          biggest  (get-dim max)
          smallest (get-dim min)
          left     (smallest first all)
          right    (biggest first all)
          top      (smallest second all)
          bot      (biggest second all)
          width    (inc (- right left))
          height   (inc (- bot top))
          pad      8
          grid     (into [] (repeat (+ height pad)
                                    (into [] (repeat (+ width pad) "#"))))
          trans    (fn [[x y]]
                     [(+ x (inc (quot width  2)) (quot pad 2))
                      (+ y (inc (quot height 2)) (quot pad 2))])
          place    (fn [[x y] c grid]
                     (update-in grid [y x] (fn [v] c)))
          path     (reduce (fn [g p] (place (trans p) "." g)) grid visited)
          d        (place (trans pos) "D" path)
          final    (if (nil? oxygen-pos)
                     d
                     (place (trans oxygen-pos) "*" d))]
      (doseq [line (map #(s/join #"" %1) final)]
        (println line)))))

(defn explore []
  (let [memory (ic/read-intcode-file "day_15.txt")
        d    (atom (droid))
        i-fn (fn []
               (let [i (droid->input @d)]
                 (do ;;(println "Input:" (input->dir i) (:state @d))
                   i)))
        o-fn (fn [v]
               (do ;;(println "Output:" (output->status v))
                 (swap! d next-droid (output->status v))))]
    (do (ic/intcode memory i-fn o-fn)
        @d)))

(defn find-shortest-path
  ([points pos dest]
   (find-shortest-path points pos dest [] #{}))
  ([points pos dest path visited]
   (if (= pos dest)
     (drop 1 (conj path dest))
     (let [next-points (->> #{:north :south :east :west}
                            (map #(next-pos pos %))
                            (filter #(not (nil? (get points %))))
                            (filter #(nil? (get visited %))))
           paths       (->> next-points
                            (map
                             #(find-shortest-path
                               points % dest (conj path pos) (conj visited pos)))
                            (filter #(not (empty? %)))
                            (sort-by count))]
       (first paths)))))

(defn spread-oxygen
  ([points pos]
   (spread-oxygen points pos [] #{}))
  ([points pos path visited]
   (let [next-points (->> #{:north :south :east :west}
                          (map #(next-pos pos %))
                          (filter #(not (nil? (get points %))))
                          (filter #(nil? (get visited %))))]
     (if (or (nil? next-points) (empty? next-points))
       (do ;;(println "Dead end" path)
           (into [] (drop 1 (conj path pos))))
       (let [paths (->> next-points
                       (map
                        #(spread-oxygen
                          points % (conj path pos) (conj visited pos)))
                       (sort-by count >))]
         (first paths))))))

(let [{:keys [visited oxygen-pos] :as d} (explore)]
  (do
    (print-droid d)
    (println (count (find-shortest-path visited [0 0] oxygen-pos)))
    (println (count (spread-oxygen visited oxygen-pos)))))
