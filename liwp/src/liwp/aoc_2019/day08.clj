(ns liwp.aoc-2019.day08
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :refer [trim]]))

(defn parse-input [input]
  (map #(-> % str read-string) input))

(def input (->> "day08-input.txt" slurp trim parse-input))

(def width 25)
(def height 6)

(defn parse-layers [input w h]
  (partition (* w h) input))

(defn count-digits [digit layer]
  (count (filter #(= digit %) layer)))

(defn layer-with-min-zeros [layers]
  (apply min-key #(count-digits 0 %) layers))

(defn calculate-checksum [data w h]
  (let [layers (parse-layers data w h)
        layer (layer-with-min-zeros layers)
        ones (count-digits 1 layer)
        twos (count-digits 2 layer)]
    (* ones twos)))

(defn part-1 []
  (calculate-checksum input width height))

(defn collapse-pixel [px]
  (->> px
       (drop-while #(= 2 %))
       first))

(defn render-image [data w h]
  (let [layers (parse-layers data w h)
        pixels (apply map vector layers)
        pixels (map collapse-pixel pixels)]
    pixels))

(defn part-2 []
  (partition width (render-image input width height)))

(defn run []
  (println "Day 08")
  (println "Part 1:" (part-1))
  (println "Part 2:")
  (pprint (part-2)))
