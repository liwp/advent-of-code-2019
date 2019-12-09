(ns liwp.aoc-2019
  (:require [clojure.string :as str]
            [liwp.aoc-2019.day01]
            [liwp.aoc-2019.day02]
            [liwp.aoc-2019.day03]
            [liwp.aoc-2019.day04]
            [liwp.aoc-2019.day05]
            [liwp.aoc-2019.day06]))

(def current-day 6)

(defn parse-args [args]
  (->> args
       (map read-string)
       (filter int?)
       (filter #(<= 1 % current-day))
       seq))


(defn day->fn [day]
  (let [ns (format "liwp.aoc-2019.day%02d" day)]
    (resolve (symbol ns "run"))))

(defn -main
  [& args]
  (let [days (if (seq args)
               (parse-args args)
               (range 1 (inc current-day)))]
    (if days
      (do
        (println "Executing puzzles for days:" (str/join ", " days))
        (dorun (map #(time ((day->fn %))) days)))
      (println "Invalid arguments:" (str/join " " args)))))
