(ns liwp.aoc-2019.day02
  (:require [liwp.aoc-2019.intcode :as ic]))

(def input [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,13,23,27,1,6,27,31,1,31,10,35,1,35,6,39,1,39,13,43,2,10,43,47,1,47,6,51,2,6,51,55,1,5,55,59,2,13,59,63,2,63,9,67,1,5,67,71,2,13,71,75,1,75,5,79,1,10,79,83,2,6,83,87,2,13,87,91,1,9,91,95,1,9,95,99,2,99,9,103,1,5,103,107,2,9,107,111,1,5,111,115,1,115,2,119,1,9,119,0,99,2,0,14,0])

(defn run-with-noun-and-verb [noun verb]
  (let [state {:tape (-> input
                         (assoc 1 noun)
                         (assoc 2 verb))
               :pc 0}]
    (-> state
         ic/run-cpu
         :tape
         (nth 0))))

(defn part-1 []
  (run-with-noun-and-verb 12 2))

(defn part-2 []
  (let [candidates (for [noun (range 100)
                         verb (range 100)
                         :let [res (run-with-noun-and-verb noun verb)]
                         :when (= 19690720 res)]
                     [noun verb])
        [noun verb] (first candidates)]
    (+ (* 100 noun) verb)))

(defn run []
  (println "Day 02")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
