(ns liwp.aoc-2019.day14
  (:require [clojure.string :refer [split split-lines]]))

(def input (slurp "day14-input.txt"))

(defn parse-pair [s]
  (let [[n id](split s #" ")]
    [id (read-string n)]))

(defn parse-reaction [s]
  (let [[sources result] (split s #" => ")
        sources (split sources #", ")
        sources (map parse-pair sources)
        [id n] (parse-pair result)]
    [id {:amount n :src (into {} sources)}]))

(defn parse-reactions [s]
  (->> s
       split-lines
       (map parse-reaction)
       (into {})))

(defn fuel->ore [rs fuel]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY ["FUEL" fuel]) spares {} ore 0]
    (let [[id n] (peek queue)
          queue (pop queue)]
      (if (nil? id)
        ore
        (if (= "ORE" id)
          (recur queue spares (+ ore n))
          (let [spare (get spares id 0)
                [need spare] (if (< n spare) [0 (- spare n)] [(- n spare) 0])
                {:keys [amount src]} (get rs id)
                reaction-multiple (long (Math/ceil (/ need amount)))
                add-to-queue (for [[k v] src] [k (* v reaction-multiple)])
                diff (- (* reaction-multiple amount) need)
                spares (assoc spares id (+ spare diff))]
            (recur (apply conj queue add-to-queue) spares ore)))))))

(defn part-1 []
  (fuel->ore (parse-reactions input) 1))

(defn ore->fuel [rs target-ore]
  (let [one-fuel (fuel->ore rs 1)
        start (long (/ target-ore one-fuel))
        end (* 2 start)
        current (quot (+ start end) 2)]
    (loop [start start current current end end]
      (let [ore (fuel->ore rs current)]
        (cond
          (or (= start current) (= current end)) current
          (< ore target-ore) (recur current (quot (+ current end) 2) end)
          :else (recur start (quot (+ start current) 2) current))))))

(defn part-2 []
  (ore->fuel (parse-reactions input) 1000000000000))

(defn run []
  (println "Day 14")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
