(ns liwp.aoc-2019.day13
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :as string]
            [liwp.aoc-2019.intcode :as ic]))

(def input (read-string (str "[" (slurp "day13-input.txt") "]")))

(def tiles
  {0 :empty
   1 :wall
   2 :block
   3 :paddle
   4 :ball})

(defn count-tiles [tile screen]
  (count (filter #(-> % second (= tile)) screen)))

(defn collect-output [output]
  (loop [screen {} ball nil paddle nil output output]
    (let [[x y t] (take 3 output)
          tile (get tiles t t)
          ball (or ball (when (= :ball tile) [x y]))
          paddle (or paddle (when (= :paddle tile) [x y]))]
      (if (nil? x)
        {:ball ball :paddle paddle :score (get screen [-1 0]) :screen screen}
        (recur (assoc screen [x y] tile) ball paddle (drop 3 output))))))

(defn part-1 []
  (->> input
       ic/new-cpu
       ic/run-cpu
       :output
       collect-output
       :screen
       (count-tiles :block)))

(defn run-cpu-with-input [cpu input]
  (-> cpu
      (update :input conj input)
      (assoc :output clojure.lang.PersistentQueue/EMPTY)
      ic/run-cpu))

(defn part-2 []
  (let [tape (assoc input 0 2)
        cpu (ic/run-cpu (ic/new-cpu tape))]
    (loop [cpu (ic/run-cpu cpu)]
      (let [{:keys [halted? output]} cpu
            {:keys [ball paddle score screen]} (collect-output output)
            move (compare (first ball) (first paddle))]
        (if halted?
          score
          (recur (run-cpu-with-input cpu move)))))))

(defn run []
  (println "Day 13")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
