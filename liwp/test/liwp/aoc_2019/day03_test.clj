(ns liwp.aoc-2019.day03-test
  (:require [liwp.aoc-2019.day03 :as sut]
            [clojure.test :as t]))

(defn parse [s]
  (-> s sut/parse-wire sut/wire->segments))

(def wires-1 (map parse ["R8,U5,L5,D3"
                         "U7,R6,D4,L4"]))

(def wires-2 (map parse ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                         "U62,R66,U55,R34,D71,R55,D58,R83"]))

(def wires-3 (map parse ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]))

(t/deftest part-1
  (t/testing "demo 1"
    (t/is (= 6 (sut/run-part-1 wires-1))))

  (t/testing "demo 2"
    (t/is (= 159 (sut/run-part-1 wires-2))))

  (t/testing "demo 3"
    (t/is (= 135 (sut/run-part-1 wires-3)))))

(t/deftest part-2
  (t/testing "demo 1"
    (t/is (= 30 (sut/run-part-2 wires-1))))

  (t/testing "demo 2"
    (t/is (= 610 (sut/run-part-2 wires-2))))

  (t/testing "demo 3"
    (t/is (= 410 (sut/run-part-2 wires-3)))))
