(ns liwp.aoc-2019.day16)

(def input (map #(Character/digit % 10) "59701570675760924481468012067902478812377492613954566256863227624090726538076719031827110112218902371664052147572491882858008242937287936208269895771400070570886260309124375604950837194732536769939795802273457678297869571088110112173204448277260003249726055332223066278888042125728226850149451127485319630564652511440121971260468295567189053611247498748385879836383604705613231419155730182489686270150648290445732753180836698378460890488307204523285294726263982377557287840630275524509386476100231552157293345748976554661695889479304833182708265881051804444659263862174484931386853109358406272868928125418931982642538301207634051202072657901464169114"))

(defn multipliers [pos]
  (->> [0 1 0 -1]
       (mapcat #(repeat pos %))
       cycle
       rest))

(defn multiply [is ms]
  (map #(* %1 %2) is ms))

(defn compute-digit [is pos]
  (mod (Math/abs (reduce + (multiply is (multipliers (inc pos))))) 10))

(defn run-phase-1 [is]
  (map #(compute-digit is %) (range (count is))))

(defn run-phases-1 [is n]
  (first (drop n (iterate run-phase-1 is))))

(defn run-part-1 [is]
  (take 8 (run-phases-1 is 100)))

(defn part-1 []
  (apply str (run-part-1 input)))

(defn run-phase-2 [is]
  (loop [acc (list (peek is)) is (pop is)]
    (let [a (peek is)]
      (if (nil? a)
        (vec acc)
        (let [b (first acc)]
          (recur (conj acc (mod (+ a b) 10)) (pop is)))))))

(defn run-phases-2 [is n]
  (first (drop n (iterate run-phase-2 is))))

(defn run-part-2 [is]
  (let [offset (read-string (apply str (drop-while zero? (take 7 is))))
        is (vec (apply concat (repeat 10000 is)))
        res (run-phases-2 is 100)]
    (take 8 (drop offset res))))

(defn part-2 []
  (apply str (run-part-2 input)))

(defn run []
  (println "Day 16")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
