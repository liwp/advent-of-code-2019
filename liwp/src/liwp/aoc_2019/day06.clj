(ns liwp.aoc-2019.day06)

(def input (slurp "day06-input.txt"))

(defn parse-orbits [s]
  (->> s
       (re-seq #"(.+)\)(.+)\n?")
       (map (fn [[_ oee oer]]  [oer oee]))
       (into {})))

(def orbits (parse-orbits input))

(def nested-orbits-for
  (memoize (fn [orbits orbiter]
             (if (= orbiter "COM")
               nil
               (let [orbitee (get orbits orbiter)]
                 (conj (nested-orbits-for orbits orbitee) orbiter))))))

(defn sum-all-orbits [orbits]
  (let [orbiters (keys orbits)]
    (->> orbiters
         (map #(nested-orbits-for orbits %))
         (map count)
         (reduce +))))

(defn remove-common-seq [seq-a seq-b]
  (let [[a & as] seq-a
        [b & bs] seq-b]
    (if (and a b (= a b))
      (recur as bs)
      [seq-a seq-b])))

(defn orbital-transfers [orbits src dst]
  (let [src-path (reverse (nested-orbits-for orbits src))
        dst-path (reverse (nested-orbits-for orbits dst))
        paths (remove-common-seq src-path dst-path)]
    (- (reduce + (map count paths)) 2)))

(defn part-1 []
  (sum-all-orbits orbits))

(defn part-2 []
  (orbital-transfers orbits "YOU" "SAN"))

(defn run []
  (println "Day 06")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
