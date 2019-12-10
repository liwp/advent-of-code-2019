(ns liwp.aoc-2019
  (:require [clojure.string :as str]
            [liwp.aoc-2019.day01]
            [liwp.aoc-2019.day02]
            [liwp.aoc-2019.day03]
            [liwp.aoc-2019.day04]
            [liwp.aoc-2019.day05]
            [liwp.aoc-2019.day06]
            [liwp.aoc-2019.day07]))

(def day-ns-re #"liwp\.aoc-2019\.day(\d{2})")

(defn day-ns? [ns]
  (->> ns
       ns-name
       str
       (re-matches day-ns-re)))

(defn all-day-ns []
  (sort-by str (filter day-ns? (all-ns))))

(defn ns->day [ns]
  (->> ns
       ns-name
       str
       (re-find day-ns-re)
       second
       read-string))

(defn day-ns->fn [ns]
  (ns-resolve ns (symbol "run")))

(defn args->set [args]
  (->> args
       (map read-string)
       (filter int?)
       (into #{})))

(defn -main
  [& args]
  (let [days (if (seq args)
               (let [include-day? (args->set args)]
                 (filter #(-> % ns->day include-day?) (all-day-ns)))
               (all-day-ns))]
    (if (seq days)
      (dorun (map #(time ((day-ns->fn %))) days))
      (println "Invalid arguments:" (str/join " " args)))))
