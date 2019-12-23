(ns liwp.aoc-2019.day11
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :as string]
            [liwp.aoc-2019.intcode :as ic]))

(def input [3,8,1005,8,284,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,28,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,50,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,72,1006,0,24,1,1106,12,10,1006,0,96,1,1008,15,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,108,1006,0,54,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,134,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,155,1006,0,60,1006,0,64,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,183,1006,0,6,1006,0,62,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,211,1,108,0,10,2,1002,15,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,242,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,263,101,1,9,9,1007,9,1010,10,1005,10,15,99,109,606,104,0,104,1,21101,0,666526126996,1,21101,301,0,0,1105,1,405,21101,846138811028,0,1,21101,312,0,0,1106,0,405,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,248129978391,1,21101,359,0,0,1105,1,405,21101,97751403560,0,1,21102,1,370,0,1106,0,405,3,10,104,0,104,0,3,10,104,0,104,0,21101,988753585000,0,1,21101,393,0,0,1105,1,405,21102,867961709324,1,1,21102,404,1,0,1106,0,405,99,109,2,22102,1,-1,1,21102,40,1,2,21101,436,0,3,21102,1,426,0,1105,1,469,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,431,432,447,4,0,1001,431,1,431,108,4,431,10,1006,10,463,1102,0,1,431,109,-2,2106,0,0,0,109,4,1202,-1,1,468,1207,-3,0,10,1006,10,486,21102,1,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21101,505,0,0,1106,0,510,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,533,2207,-4,-2,10,1006,10,533,22101,0,-4,-4,1105,1,601,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,552,0,1105,1,510,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,571,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,593,21202,-1,1,1,21102,1,593,0,106,0,468,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0])

(defn run-cpu-with-input [cpu input]
  (-> cpu
      (update :input conj input)
      ic/run-cpu))

(def ^:const black 0)
(def ^:const white 1)

(def left-turns
  {:up :left
   :left :down
   :down :right
   :right :up})

(def right-turns (map-invert left-turns))

(def turns
  {0 left-turns
   1 right-turns})

(defn move [[x y] dir]
  (case dir
    :up [x (inc y)]
    :left [(dec x) y]
    :down [x (dec y)]
    :right [(inc x) y]))

(defn run-system [cpu start-panel]
  (loop [cpu cpu dir :up trace {[0 0] start-panel} coordinates [0 0]]
    (let [input (get trace coordinates black)
          cpu (run-cpu-with-input cpu input)
          {:keys [blocked? output]} cpu]
      (if (not blocked?)
        trace
        (let [color (first output)
              ;; paint current panel
              trace (assoc trace coordinates color)
              ;; turn the robot
              turn (second output)
              dir (-> turn turns dir)
              ;; move the robot
              coordinates (move coordinates dir)
              cpu (-> cpu (update :output pop) (update :output pop))]
          (recur cpu dir trace coordinates))))))

(defn part-1 []
  (let [cpu (ic/new-cpu input)
        trace (run-system cpu black)]
    (count trace)))

(defn trace-dimensions [trace]
  (let [x-coords (map ffirst trace)
        y-coords (map (comp second first) trace)
        min-x (reduce min x-coords)
        max-x (reduce max x-coords)
        min-y (reduce min y-coords)
        max-y (reduce max y-coords)]
    {:min-x min-x :max-x max-x :min-y min-y :max-y max-y}))

(defn render-line [trace y min-x max-x]
  (apply str (for [x (range min-x (inc max-x))
                   :let [color (get trace [x y] black)]]
               (if (= black color) \space \X))))

(defn paint-registration []
  (let [cpu (ic/new-cpu input)
        trace (run-system cpu white)
        {:keys [min-x max-x min-y max-y]} (trace-dimensions trace)]
    (string/join
     \newline
     (for [y (range max-y (dec min-y) -1)]
       (render-line trace y min-x max-x)))))

(defn part-2 []
  (println (paint-registration)))

(defn run []
  (println "Day 11")
  (println "Part 1:" (part-1))
  (println "Part 2:")
  (part-2))
