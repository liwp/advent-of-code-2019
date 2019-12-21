(ns liwp.aoc-2019.day11-test
  (:require [liwp.aoc-2019.day11 :as sut]
            [clojure.test :as t]))

(t/deftest test-part-1
  (t/is (= 2539 (sut/part-1))))

(t/deftest test-part-2
  (let [output (with-out-str (sut/part-2))
        expected " XXXX X    XXXX XXX  X  X   XX XXX   XX    \n    X X    X    X  X X X     X X  X X  X   \n   X  X    XXX  XXX  XX      X X  X X  X   \n  X   X    X    X  X X X     X XXX  XXXX   \n X    X    X    X  X X X  X  X X X  X  X   \n XXXX XXXX XXXX XXX  X  X  XX  X  X X  X   \n"]
    (t/is (= expected output))))
