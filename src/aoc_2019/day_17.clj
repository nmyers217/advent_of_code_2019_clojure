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
  {:height (count grid) :width  (count (first grid))})

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
    (println image)
    (->> points
         (filterv intersection?)
         (mapv (partial apply *))
         (reduce +)
         println)))

(def droid->dir {"^" [0 -1] "v" [0 1] "<" [-1 0] ">" [1 0]})
(defn turn-right  [[x y]] [(- y)    x ])
(defn turn-left [[x y]] [    y (- x)])

(defn droid-status [grid]
  (->> grid
       (map-grid (fn [pos c]
                   (when (get #{"^" "<" ">" "v"} c)
                     {:pos pos :dir (droid->dir c) :path []})))
       (filter #(not (nil? %)))
       first))

(defn travel [grid droid]
  (loop [grid grid
         {:keys [pos dir path] :as d} droid]
    (let [scaffold? #(= "#" (index-grid % grid))
          next-pos  (mapv + pos dir)]
      (if-not (scaffold? next-pos)
        ;; We can't go forward, we need to turn
        (let [left     (turn-left dir)
              right    (turn-right dir)
              next-dir (->> [left right]
                            (filter #(scaffold? (mapv + pos %)))
                            first)
              c        (if (= next-dir left) "L" "R")]
          (if (nil? next-dir)
            path ;; Base case, nowhere left to turn to
            ;; Make the turn
            (recur grid (merge d {:dir next-dir
                                  :path (conj path [c 0])}))))
        ;; Keep going forward
        (recur grid (merge d {:pos next-pos
                              :path (conj (pop path)
                                          (update (last path) 1 inc))}))))))

(defn scaffold-path []
  (let [image (camera-image)
        grid  (image->grid image)
        droid (droid-status grid)]
    (->> (travel grid droid)
         flatten
         (str/join #",")
         println)))

;; I just did this next part by hand in a couple minutes
;;
;; My over all instructions from scaffold-path above were:
;; R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,L,10,L,12,R,10,R,6,L,12,L,10
;;
;; Which I broke down into:
;; A => R,6,L,10,R,10,R,10
;; B => L,10,L,12,R,10
;; A => R,6,L,10,R,10,R,10
;; B => L,10,L,12,R,10
;; A => R,6,L,10,R,10,R,10
;; C => R,6,L,12,L,10
;; A => R,6,L,10,R,10,R,10
;; C => R,6,L,12,L,10
;; B => L,10,L,12,R,10
;; C => R,6,L,12,L,10

(defn str->ascii [s]
  (map (comp int char) s))

(defn intcode-inputs [video?]
  (->> ["A,B,A,B,A,C,A,C,B,C\n"  ;; main routine
        "R,6,L,10,R,10,R,10\n"   ;; routine A
        "L,10,L,12,R,10\n"       ;; routine B
        "R,6,L,12,L,10\n"        ;; routine C
        (if video? "y\n" "n\n")] ;; video feed
       (map str->ascii)
       flatten
       (into [])))

(defn part-two []
  (let [mem   (assoc (ic/read-intcode-file "day_17.txt") 0 2)
        i-q   (atom (intcode-inputs false))
        i-fn  (fn []
                (let [i (first @i-q)]
                  (do (swap! i-q rest)
                      i)))
        o-fn  println]
    (do (ic/intcode mem i-fn o-fn))))

(do (part-one) (part-two))
