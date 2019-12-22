(ns liwp.aoc-2019.day12
  (:require [clojure.string :refer [split-lines]]))

(def input (slurp "day12-input.txt"))

(def moon-re #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")

(defn create-moon [re-match]
  (let [[_ x y z] re-match]
    {:x (read-string x)
     :y (read-string y)
     :z (read-string z)
     :vel {:x 0 :y 0 :z 0}}))

(defn parse-moons [string]
  (->> string
       (re-seq moon-re)
       (mapv create-moon)))

(defn apply-gravity-to-pair-on-axis [axis [a b]]
  (case (compare (get a axis) (get b axis))
    -1 [(update-in a [:vel axis] inc) (update-in b [:vel axis] dec)]
    1 [(update-in a [:vel axis] dec) (update-in b [:vel axis] inc)]
    0 [a b]))

(defn reduce-moons-on-axis [axis acc x]
  (let [m (peek acc)
        xs (pop acc)
        [x m] (apply-gravity-to-pair-on-axis axis [x m])]
    (conj xs x m)))

(defn apply-gravity-on-axis [ms axis]
  (if (= 2 (count ms))
    (apply-gravity-to-pair-on-axis axis ms)
    (let [m (peek ms)
          ms (pop ms)]
      (reduce #(reduce-moons-on-axis axis %1 %2) [m] (apply-gravity-on-axis ms axis)))))

(defn apply-velocity-to-moon-on-axis [moon axis]
  (update moon axis + (-> moon :vel axis)))

(defn apply-velocity-on-axis [ms axis]
  (mapv #(apply-velocity-to-moon-on-axis % axis) ms))

(defn run-step-on-axis [ms axis]
  (-> ms
      (apply-gravity-on-axis axis)
      (apply-velocity-on-axis axis)))

(defn run-step [ms]
  (-> ms
      (run-step-on-axis :x)
      (run-step-on-axis :y)
      (run-step-on-axis :z)))

(def xyz-fn (juxt :x :y :z))

(defn energy-for-moon [moon]
  (let [pot (apply + (map #(Math/abs %) (xyz-fn moon)))
        kin (apply + (map #(Math/abs %) (xyz-fn (:vel moon))))]
    (* pot kin)))

(defn energy-for-moons [moons]
  (reduce + (mapv energy-for-moon moons)))

(defn run-steps [moons n]
  (nth (iterate run-step moons) n))

(defn part-1 []
  (let [moons (parse-moons input)]
    (energy-for-moons (run-steps moons 1000))))

(defn find-cycle-on-axis [axis moons]
  (loop [history #{} moons moons count 0]
    (if (history moons)
      count
      (recur (conj history moons) (run-step-on-axis moons axis) (inc count)))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn find-cycle [moons]
  (let [cycles (map #(find-cycle-on-axis % moons) [:x :y :z])]
    (reduce lcm cycles)))

(defn part-2 []
  (let [moons (parse-moons input)]
    (find-cycle moons)))

(defn run []
  (println "Day 12")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
