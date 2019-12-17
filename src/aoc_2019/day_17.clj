(ns aoc-2019.day-16
  (:require [clojure.string :as str]
            [aoc-2019.intcode :as ic]))

(defn camera-image []
  (let [mem   (ic/read-intcode-file "day_17.txt")
        image (atom "")
        i-fn  (comp read-string read-line)
        o-fn  (fn [v] (swap! image str (char v)))]
    (do (ic/intcode mem i-fn o-fn)
        @image)))

(defn image->grid [image]
  (->> image
       str/split-lines
       (map #(str/split % #""))
       (into [])))

(defn grid->dims [grid]
  {:height (count grid)
   :width  (count (first grid))})

(defn in-bounds? [[x y] grid]
  (let [{:keys [width height]} (grid->dims grid)]
    (and (>= y 0) (< y height) (>= x 0) (< x width))))

(defn index-grid [[x y] grid]
  (get-in grid [y x] nil))

(defn map-grid [f grid]
  (let [{:keys [width height]} (grid->dims grid)
        coords (for [y (range 0 height)
                     x (range 0 width)]
                 [x y])]
    (map #(f % (index-grid % grid)) coords)))

(defn neighbors [pos grid]
  (->> [[0 -1] [-1 0] [1 0] [0 1]]
       (mapv #(mapv + pos %))
       (mapv #(index-grid % grid))
       (filterv #(not (nil? %)))))

(defn part-one []
  (let [image     (camera-image)
        grid      (image->grid image)
        scaffold? (fn [[x y] c] (when (= c "#") [x y]))
        points    (->> grid
                       (map-grid scaffold?)
                       (filter #(not (nil? %))))
        all-scaffold? (fn [points] (every? #(= % "#") points))
        intersection? (fn [pos] (all-scaffold? (neighbors pos grid)))]
    (->> points
         (filterv intersection?)
         (mapv (partial apply *))
         (reduce +))))

(println (part-one))
