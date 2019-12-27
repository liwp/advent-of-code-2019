(ns liwp.aoc-2019.day17
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :as s]
            [liwp.aoc-2019.intcode :as ic]))

(def input (read-string (str "[" (slurp "day17-input.txt") "]")))

(def ccw
  {:north :west
   :west :south
   :south :east
   :east :north})

(def cw (map-invert ccw))

(defn output->scaffold [output]
  (loop [coords #{} start nil dir nil y 0 x 0 output (seq output)]
    (if (nil? output)
      {:coords coords :start start :dir dir}
      (let [[o & output] output]
        (case (char o)
          \. (recur coords start dir y (inc x) output)
          \# (recur (conj coords [x y]) start dir y (inc x) output)
          \^ (recur (conj coords [x y]) [x y] :north y (inc x) output)
          \v (recur (conj coords [x y]) [x y] :south y (inc x) output)
          \< (recur (conj coords [x y]) [x y] :west y (inc x) output)
          \> (recur (conj coords [x y]) [x y] :east y (inc x) output)
          \newline (recur coords start dir (inc y) 0 output))))))

(defn move [[x y] dir]
  (case dir
    :north [x (dec y)]
    :west  [(dec x) y]
    :south [x (inc y)]
    :east  [(inc x) y]))

(defn opposite-direction [direction]
  (case direction
    :north :south
    :south :north
    :west :east
    :east :west))

(defn next-dirs [dir]
  ;; start with all directions
  (-> (into #{} (keys cw))
      ;; remove fwd and bwd
      (disj  dir (opposite-direction dir))
      seq
      ;; prepend the current direction
      (conj dir)))

(defn collect-intersections [{:keys [coords dir start]}]
  (loop [intersections #{} path [] done #{} pos start dirs (keys cw)]
    (if (nil? (seq dirs))
      {:intersections intersections :path (conj path pos) :dir dir}
      (let [[dir & dirs] dirs
            next-pos (move pos dir)]
        (if (coords next-pos)
          ;; we're still on the scaffolding
          (if (done next-pos)
            ;; intersection!
            (recur (conj intersections next-pos) (conj path pos) done next-pos (next-dirs dir))
            ;; next block on scaffolding - turn ccw
            (recur intersections (conj path pos) (conj done next-pos) next-pos (next-dirs dir)))
          ;; we're not on the scaffolding - turn cw
          (recur intersections path done pos dirs))))))

(def run-cpu-to-collect-intersections
  (memoize
   (fn [tape]
     (let [cpu (ic/run-cpu (ic/new-cpu tape))
           scaffold (output->scaffold (:output cpu))]
       (collect-intersections scaffold)))))

(defn part-1 []
  (let [{:keys [intersections]} (run-cpu-to-collect-intersections input)]
    (reduce + (map #(apply * %) intersections))))

(defn coords->dir [[xa ya] [xb yb]]
  (let [dx (- xb xa)
        dy (- yb ya)]
    (case [dx dy]
      [0 -1] :north
      [0  1] :south
      [-1 0] :west
      [ 1 0] :east)))

(defn dirs->turn [da db]
  (cond
    (= db (ccw da)) :left
    (= db (cw  da)) :right))

(defn path->instructions [path dir]
  (loop [instructions [] path path dir dir]
    (let [[pos & path] path]
      (if (nil? path)
        instructions
        (let [next-pos (first path)
              required-dir (coords->dir pos next-pos)
              required-turn (dirs->turn dir required-dir)]
          (if (= required-dir dir)
            (recur (conj instructions 1) path dir)
            (recur (conj instructions required-turn) (cons pos path) required-dir)))))))

(defn encode-instructions [instructions]
  (s/join
   ","
   (loop [acc [] instructions (partition-by identity instructions)]
     (let [[instruction & instructions] instructions]
       (if (nil? instruction)
         acc
         (let [c (case (first instruction)
                   :left \L
                   :right \R
                   (count instruction))]
           (recur (conj acc c) instructions)))))))

(defn compress-command [command debug]
  (let [a ["L,6,R,12,L,6,L,8,L,8" "A"]
        b ["L,6,R,12,R,8,L,8" "B"]
        c ["L,4,L,4,L,6" "C"]
        debug (if debug "y" "n")
        main (reduce #(apply s/replace %1 %2) command [a b c])]
    (map int (s/join \newline (cons main (map first [a b c debug nil]))))))

(defn build-command [tape debug]
  (let [{:keys [dir path]} (run-cpu-to-collect-intersections tape)
        instructions (path->instructions path dir)
        command (encode-instructions instructions)]
    (compress-command command debug)))

(defn part-2 []
  (let [cmd (build-command input false)
        tape (assoc input 0 2)
        cpu (merge
             (ic/new-cpu tape)
             {:input (into clojure.lang.PersistentQueue/EMPTY cmd)})
        cpu (ic/run-cpu cpu)]
    (last (:output cpu))))

(defn run []
  (println "Day 16")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
