(ns liwp.aoc-2019.day10
  (:require [clojure.string :refer [split-lines]]
            [liwp.aoc-2019.intcode :as ic]))

(def input (slurp "day10-input.txt"))

(defn parse-line [s]
  (into #{} (filter identity (map-indexed #(if (= %2 \#) %1 nil) s))))

(defn parse-map [s]
  (->> s
       split-lines
       (map-indexed (fn [y l] [y (parse-line l)]))
       (filter (fn [[_ xs]] (not= xs #{})))
       (into {})))

(defn new-interceptor [x1 y1 x2 y2]
  (let [[x1 x2] (if (< x1 x2) [x1 x2] [x2 x1])
        [y1 y2] (if (< y1 y2) [y1 y2] [y2 y1])]
    (if (= x1 x2)
      (fn [x y] (and (= x x1) (< y1 y y2)))
      (let [slope (/ (- y2 y1) (- x2 x1))
            intercept (- y1 (* slope x1))]
        (fn [x y]
          (and (= (+ (* slope x) intercept) y)
               (< x1 x x2)
               (<= y1 y y2)))))))

(defn interceptors-for-meteor [m x1 y1]
  (for [y2 (keys m)
        x2 (get m y2)]
    (new-interceptor x1 y1 x2 y2)))

(defn interceptors-for-map [m]
  (into {} (for [y (keys m)
                 x (get m y)]
             [[x y] (interceptors-for-meteor m x y)])))

(defn run-meteor-interceptor [m interceptor]
  (into #{} (for [y (keys m)
                  x (get m y)
                  :when (interceptor x y)]
              [x y])))

(defn run-meteor-interceptors [m interceptors]
  (reduce into (map #(run-meteor-interceptor m %) interceptors)))

(defn run-on-map [m]
  (let [meteor-count (reduce + (map count (vals m)))
        meteors (into
                 {}
                 (for [[meteor interceptors] (interceptors-for-map m)]
                   [meteor (run-meteor-interceptors m interceptors)]))]
    meteors)
  #_(let [meteor-count (reduce + (map count (vals m)))
        meteors (into
                 {}
                 (for [[meteor interceptors] (interceptors-for-map m)]
                   [meteor (- meteor-count
                              1
                              (count (run-meteor-interceptors m interceptors)))]))]
    (apply max-key second meteors)))

(defn part-1 []
  (let [m (parse-map input)]
    (run-on-map m)))

;; example
;; {0 #{0 3 5 7 8 9 11 15 16 18 19},
;;  1 #{1 3 4 5 6 7 9 11 13 14 20 21 23},
;;  2 #{0 1 4 6 7 8 11 12 13 16 17 18 19 20 23},
;;  3 #{0 1 2 3 5 7 10 15 18 19 21 22},
;;  4 #{1 2 3 4 5 6 7 9 10 11 12 13 17 19 20 21},
;;  5 #{1 2 6 8 10 11 12 15 16 17 19 21 23},
;;  6 #{1 2 3 4 5 6 12 14 15 16 19},
;;  7 #{1 2 5 6 8 11 12 13 14 15 19 20 21 23},
;;  8 #{0 1 2 3 4 5 6 8 11 12 13 14 15 18 20 22},
;;  9 #{1 2 3 5 6 7 11 12 14 15 20 21 23},
;;  10 #{0 1 3 4 5 7 8 10 12 15 16 17 18},
;;  11 #{0 2 5 6 9 12 14 17 18 19 20 21 23},
;;  12 #{0 1 2 3 4 6 7 9 11 13 15 17 19 22 23},
;;  13 #{0 4 5 7 8 10 11 12 14 15 17 19 20 21},
;;  14 #{0 1 2 3 5 6 8 10 12 13 14 15 17 18 19 20 21 23},
;;  15 #{1 4 5 9 10 13 14 17 19 21 22},
;;  16 #{0 1 2 6 7 8 9 11 12 13 15 17 18 19 21 23},
;;  17 #{2 3 4 5 7 8 9 10 11 14 15 16 17 18 20 22 23},
;;  18 #{2 3 4 7 8 9 12 15 16 20 22},
;;  19 #{0 1 3 4 5 6 10 11 16 17 18 19 21 22},
;;  20 #{0 1 2 3 6 9 10 12 14 19 22},
;;  21 #{1 12 15 17 19 20 21 22},
;;  22 #{0 1 2 5 6 7 9 10 11 13 15 17 22 23},
;;  23 #{0 1 2 3 4 5 6 7 9 10 11 12 13 14 15 17 19 20 22 23}}

(defn part-2 [])

(defn run []
  (println "Day 10")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
