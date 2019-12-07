(ns liwp.aoc-2019.day04)

(def input {:min 234208 :max 765869})

(defn int->vec [i]
  (loop [acc '() i i]
    (if (zero? i)
      (into [] acc)
      (recur (conj acc (mod i 10)) (quot i 10)))))

(defn repeated-digits? [pw]
  (not= (count pw) (count (into #{} pw))))

(defn is-sorted? [pw]
  (= (sort pw) pw))

(defn has-pair? [pw]
  (->> pw
       frequencies
       vals
       (some #{2})))

(defn run-with-pred [pred {:keys [min max]}]
  (->> (range min (inc max))
       (map int->vec)
       (filter pred)
       (count)))

(defn is-valid-for-part-1 [pw]
  (and (is-sorted? pw)
       (repeated-digits? pw)))

(defn part-1 []
  (run-with-pred is-valid-for-part-1 input))

(defn is-valid-for-part-2 [pw]
  (and (is-sorted? pw)
       (has-pair? pw)))

(defn part-2 []
  (run-with-pred is-valid-for-part-2 input))

(defn run []
  (println "Day 04")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
