(ns aoc-2019.day-13
  (:require [clojure.string :as s]
            [aoc-2019.intcode :as ic]))

(def id->tile {0 :empty 1 :wall 2 :block 3 :paddle 4 :ball})

(def tile->c  {:empty " " :wall "$" :block "#" :paddle "=" :ball "*"})

(defn game-output [file]
  (let [q      (atom [])
        memory (ic/read-intcode-file file)
        i-fn   (comp read-string read-line)
        o-fn   (fn [v] (swap! q conj v))]
    (do (ic/intcode memory i-fn o-fn)
        @q)))

(defn read-instruction [[x y id]]
  {:x x :y y :id id :t (id->tile id)})

(defn read-instructions [output]
  (->> output
       (partition 3)
       (map read-instruction)))

(defn play [instructions]
  (let [width  (inc (apply max (map :x instructions)))
        height (inc (apply max (map :y instructions)))
        build  (fn [n v] (into [] (repeat n v)))
        screen (build height (build width (tile->c :empty)))

        paint  (fn [s x y t]
                 (if (= [x y] [-1 0])
                   s
                   (update-in s [y x] (fn [v] (tile->c t)))))

        score  (fn [s x y id]
                 (if (and (= [x y] [-1 0]) (> id s)) id s))]
    {:screen (reduce (fn [s {:keys [x y t]}] (paint s x y t))
                     screen instructions)
     :score  (reduce (fn [s {:keys [x y id]}] (score s x y id))
                     0 instructions)}))

(defn print-screen [screen]
  (do
    (doseq [row screen]
      (println (s/join #"" row)))
    screen))

(defn print-game [{:keys [screen score] :as game}]
  (do (println (str "Score: " score))
      (print-screen screen)
      game))

(defn get-tile-location [screen tile]
  (let [pred    (fn [i row] (when (> (.indexOf row (tile->c tile)) -1)
                          [i row]))
        [y row] (first (keep-indexed pred screen))
        x       (when row (.indexOf row (tile->c tile)))
        ]
    (if (and x y)
      [x y]
      nil)))

(defn dir-to-move-paddle [screen]
  (let [[bx _] (get-tile-location screen :ball)
        [px _] (get-tile-location screen :paddle)]
    (when (and bx px)
      (cond (> (- bx px) 2) :right
            (> (- px bx) 2) :left
            true :any))))

(defn count-tile [tile {screen :screen}]
  (->> screen
       flatten
       (filter #(= (tile->c tile) %))
       count))

(defn part-one []
  (->> "day_13.txt"
       game-output
       read-instructions
       play
       print-game
       (count-tile :block)
       println))

(defn play-with-joystick [program inputs]
  (let [memory (assoc program 0 2)

        o-q    (atom [])
        i-q    (atom (if (empty? inputs) [0] inputs))
        mem    (atom memory)

        i-fn   (fn [] (let [i (first @i-q)]
                       (do
                         (swap! i-q #(if (empty? (rest %))
                                       [0] ; Always have an extra move ready
                                       (into [] (rest %))))
                         i)))
        o-fn   (fn [v] (swap! o-q conj v))

        mo-fn  (fn [new] (swap! mem (fn [old] new)))]
    (do (ic/intcode memory i-fn o-fn mo-fn)
        (when (= @i-q [0]) ; Return nil if too many inputs were given
          (->> @o-q
               read-instructions
               play
               (hash-map :memory @mem :game))))))

(defn play-all
  ([file]
   (play-all (ic/read-intcode-file file) 0 []))
  ([program best-score inputs]
   ;;(println best-score " @ " inputs)
   (let [game  (:game (play-with-joystick program inputs))
         score (:score game)]
     (if (nil? score)
       best-score
       (let [paddle-dir (dir-to-move-paddle (:screen game))
             score'     (if (> score best-score)
                          (do (println "SCORE: " score) score)
                          best-score)]
         ;; TODO we cant check every single possibility, and 99% of them dont make sense
         ;; TODO we are just going to have to do some kind of ball prediction ai thing
         (->> (cond (empty? inputs) [-1 0 1]
                    (= paddle-dir :left) [-1]
                    (= paddle-dir :right) [1]
                    true [-1 0 1])
              (map #(play-all program score' (conj inputs %)))
              (apply max)))))))

;;(->> [0 1 -1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 1 -1 -1 -1 -1 -1]
;;     (play-with-joystick (ic/read-intcode-file "day_13.txt"))
;;     :game
;;     print-game)

(println (play-all "day_13.txt"))

;;(do (part-one))
