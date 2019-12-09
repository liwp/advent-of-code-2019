(ns liwp.aoc-2019.day06-test
  (:require [liwp.aoc-2019.day06 :as sut]
            [clojure.test :as t]))

(def demo (sut/parse-orbits "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"))

(t/deftest test-parse-orbits
  (t/is (= {"K" "J"
            "L" "K"
            "G" "B"
            "J" "E"
            "H" "G"
            "E" "D"
            "C" "B"
            "F" "E"
            "B" "COM"
            "I" "D"
            "D" "C"} demo)))

(t/deftest test-sum-all-orbits
  (t/is (= 42 (sut/sum-all-orbits demo))))

(t/deftest test-part-1
  (t/is (= 270768 (sut/part-1))))

(def orbital-transder-demo {"K" "J"
                            "L" "K"
                            "G" "B"
                            "J" "E"
                            "H" "G"
                            "E" "D"
                            "C" "B"
                            "F" "E"
                            "B" "COM"
                            "I" "D"
                            "D" "C"
                            "YOU" "K"
                            "SAN" "I"})

(t/deftest test-orbital-transdfers
  (t/is (= 4 (sut/orbital-transfers orbital-transder-demo "SAN" "YOU"))))

(t/deftest test-part-2
  (t/is (= 451 (sut/part-2))))
