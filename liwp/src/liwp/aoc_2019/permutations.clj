(ns liwp.aoc-2019.permutations)

(defn permutations [xs]
  (if (= 1 (count xs))
    (list xs)
    (for [x xs
          tail (permutations (disj (set xs) x))]
      (cons x tail))))
