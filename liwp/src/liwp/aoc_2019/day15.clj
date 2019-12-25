(ns liwp.aoc-2019.day15
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :refer [join]]
            [liwp.aoc-2019.intcode :as ic]))

(def input (read-string (str "[" (slurp "day15-input.txt") "]")))

(def tiles
  {0 :wall
   1 :empty
   2 :oxygen})

(def render-tile
  {nil \space
   :wall \#
   :empty \.
   :oxygen \O
   :start \S
   :cursor \x})

(def directions
  {:north 1
   :south 2
   :west 3
   :east 4})

(defn screen-dimensions [screen]
  (let [x-coords (map ffirst screen)
        y-coords (map (comp second first) screen)
        min-x (reduce min x-coords)
        max-x (reduce max x-coords)
        min-y (reduce min y-coords)
        max-y (reduce max y-coords)]
    {:min-x min-x :max-x max-x :min-y min-y :max-y max-y}))

(defn render-line [screen y min-x max-x]
  (apply str (for [x (range min-x (inc max-x))
                   :let [tile (get screen [x y])]]
               (render-tile tile))))

(defn render-screen [screen coords]
  (let [screen (assoc screen coords :cursor)
        {:keys [min-x max-x min-y max-y]} (screen-dimensions screen)]
    (join
     \newline
     (for [y (range max-y (dec min-y) -1)]
       (render-line screen y min-x max-x)))))

(defn run-cpu-with-input [cpu input]
  (-> cpu
      (update :input conj input)
      (assoc :output clojure.lang.PersistentQueue/EMPTY)
      ic/run-cpu))

(defn move [[x y] direction]
  (case direction
    :north [x (inc y)]
    :south [x (dec y)]
    :west [(dec x) y]
    :east [(inc x) y]))

(defn opposite-direction [direction]
  (case direction
      :north :south
      :south :north
      :west :east
      :east :west))

(defn add-new-work [screen queue coords direction]

  (if (or (get screen coords) (get queue coords))
    nil
    (->> directions
         keys
         (remove #{(opposite-direction direction)})
         vec
         (vector coords)
         vector)))

(defn find-oxygen [input]
  (loop [cpu (ic/new-cpu input)
         screen {[0 0] :start}
         queue {[0 0] [:north :south :west :east]}
         coords [0 0]
         path []]
    (let [pending-directions (get queue coords [])
          next-direction (first pending-directions)]
      (if (not next-direction)
        ;; no more moves - backtrack!
        (let [next-direction (opposite-direction (peek path))
              path (pop path)
              cpu (run-cpu-with-input cpu (directions next-direction))
              new-coords (move coords next-direction)]
          (recur cpu screen queue new-coords path))
        ;; execute next move
        (let [queue (assoc queue coords (rest pending-directions))
              cpu (run-cpu-with-input cpu (directions next-direction))
              tile (-> cpu :output peek tiles)
              new-coords (move coords next-direction)
              new-work (add-new-work screen queue new-coords next-direction)
              screen (assoc screen new-coords tile)]
          (case tile
            :wall (recur cpu screen queue coords path)
            :empty (recur cpu screen
                          (into queue new-work)
                          new-coords
                          (conj path next-direction))
            :oxygen (conj path next-direction)))))))

(defn exhaustive-map [input]
  (loop [cpu (ic/new-cpu input)
         screen {[0 0] :start}
         queue {[0 0] [:north :south :west :east]}
         coords [0 0]
         path []]
    (let [pending-directions (get queue coords [])
          next-direction (first pending-directions)]
      (if (every? #(-> % second seq nil?) queue)
        screen
        (if (not next-direction)
               ;; no more moves - backtrack!
               (let [next-direction (opposite-direction (peek path))
                     path (pop path)
                     cpu (run-cpu-with-input cpu (directions next-direction))
                     new-coords (move coords next-direction)]
                 (recur cpu screen (dissoc queue coords) new-coords path))
               ;; execute next move
               (let [queue (assoc queue coords (rest pending-directions))
                     cpu (run-cpu-with-input cpu (directions next-direction))
                     tile (-> cpu :output peek tiles)
                     new-coords (move coords next-direction)
                     new-work (add-new-work screen queue new-coords next-direction)
                     screen (assoc screen new-coords tile)]
                 (case tile
                   :wall (recur cpu screen queue coords path)
                   :empty (recur cpu screen
                                 (into queue new-work)
                                 new-coords
                                 (conj path next-direction))
                   :oxygen (recur cpu screen
                                  (into queue new-work)
                                  new-coords
                                  (conj path next-direction)))))))))

(defn neighbouring-coords [screen [x y]]
  (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
       (filter #(-> % screen (= :empty)))
       vec))

(defn walk-from-oxygen [screen]
  (let [oxygen-coords ((map-invert screen) :oxygen)]
    (loop [screen screen
           next-coords (neighbouring-coords screen oxygen-coords)
           steps 0]
      (let [next-neighbours (->> next-coords
                                 (mapcat #(neighbouring-coords screen %))
                                 (into #{}))
            screen (reduce #(assoc %1 %2 :oxygen) screen next-coords)]
        (if (empty? next-coords)
          steps
          (recur screen next-neighbours (inc steps)))))))

(defn part-1 []
  (let [path (find-oxygen input)]
    (count path)))

(defn part-2 []
  (let [screen (exhaustive-map input)]
    (walk-from-oxygen screen)))

(defn run []
  (println "Day 15")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
