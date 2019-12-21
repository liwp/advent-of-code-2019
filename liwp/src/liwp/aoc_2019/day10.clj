(ns liwp.aoc-2019.day10
  (:require [clojure.string :refer [split-lines]]))

(def input (slurp "day10-input.txt"))

(defn parse-line [s]
  (filter identity (map-indexed #(if (= %2 \#) %1 nil) s)))

(defn parse-map [s]
  (->> s
       split-lines
       (map-indexed (fn [y l] [y (parse-line l)]))
       (filter #(-> % second seq))
       (mapcat (fn [[y xs]] (map #(vector % y) xs)))))

(defn new-interceptor [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    (fn [x y] (and (= x x1) (or (< y1 y y2) (< y2 y y1))))
    (let [slope (/ (- y2 y1) (- x2 x1))
          intercept (- y1 (* slope x1))]
      (fn [x y]
        (and (= (+ (* slope x) intercept) y)
             (or (< x1 x x2) (< x2 x x1))
             (or (<= y1 y y2) (<= y2 y y1)))))))

(defn occludes? [interceptor [x y]]
  (interceptor x y))

(defn any-occludes? [start end m]
  (let [interceptor (new-interceptor start end)]
    (or (= start end)
        (some #(occludes? interceptor %) m))))

(defn visible-meteors-for [start m]
  (filter #(not (any-occludes? start % m)) m))

(defn visible-meteors [m]
  (map #(vector % (visible-meteors-for % m)) m))

(defn run-on-map [m]
  (->> m
       visible-meteors
       (map (fn [[k v]] [k (count v)]))
       (apply max-key second)))

(defn meteor->polar [[x1 y1] [x2 y2]]
  (let [x (- x2 x1)
        y (- y2 y1)
        r (Math/sqrt (+ (* x x) (* y y)))
        theta (Math/atan2 y x)]
    [[theta r] [x2 y2]]))

(defn sort-meteors [origin m]
  (let [split-point (- (/ Math/PI 2))
        [as bs] (->> m
                     (filter #(not= origin %))
                     (map #(meteor->polar origin %))
                     (sort-by first)
                     (split-with #(< (ffirst %) split-point)))]
    (concat bs as)))

(defn laser-meteors
  [origin ms & [last-theta]]
  (let [[m & ms] ms
        ms (vec ms)
        new-theta (ffirst m)]
    (cond
      (nil? m) nil
      (nil? (seq ms)) m
      (= new-theta last-theta) (laser-meteors origin (conj ms m) last-theta)
      :else (lazy-seq (cons m (laser-meteors origin ms new-theta))))))

(defn laser-map [origin ms]
  (->> ms
       (sort-meteors origin)
       (laser-meteors origin)))

(defn part-1 []
  (let [m (parse-map input)]
    (run-on-map m)))

(defn part-2 []
  (let [m (parse-map input)
        [_ [x y]] (nth (laser-map [20 21] m) 199)]
    (+ (* x 100) y)))

(defn run []
  (println "Day 10")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
