(ns liwp.aoc-2019.day16-test
  (:require [liwp.aoc-2019.day16 :as sut]
            [clojure.test :as t]))

(t/deftest test-after-100-phases
  (let [is [8 0 8 7 1 2 2 4 5 8 5 9 1 4 5 4 6 6 1 9 0 8 3 2 1 8 6 4 5 5 9 5]]
    (t/is (= [2 4 1 7 6 1 7 6] (sut/run-part-1 is))))

  (let [is [1 9 6 1 7 8 0 4 2 0 7 2 0 2 2 0 9 1 4 4 9 1 6 0 4 4 1 8 9 9 1 7]]
    (t/is (= [7 3 7 4 5 4 1 8] (sut/run-part-1 is))))

  (let [is [6 9 3 1 7 1 6 3 4 9 2 9 4 8 6 0 6 3 3 5 9 9 5 9 2 4 3 1 9 8 7 3]]
    (t/is (= [5 2 4 3 2 1 3 3] (sut/run-part-1 is)))))

(t/deftest test-part-1
  (t/is (= "61149209" (sut/part-1))))

(t/deftest test-after-10000-repeats-and-100-phases
  (let [is [0 3 0 3 6 7 3 2 5 7 7 2 1 2 9 4 4 0 6 3 4 9 1 5 6 5 4 7 4 6 6 4]]
    (t/is (= [8 4 4 6 2 0 2 6] (sut/run-part-2 is))))

  (let [is [0 2 9 3 5 1 0 9 6 9 9 9 4 0 8 0 7 4 0 7 5 8 5 4 4 7 0 3 4 3 2 3]]
    (t/is (= [7 8 7 2 5 2 7 0] (sut/run-part-2 is))))

  (let [is [0 3 0 8 1 7 7 0 8 8 4 9 2 1 9 5 9 7 3 1 1 6 5 4 4 6 8 5 0 5 1 7]]
    (t/is (= [5 3 5 5 3 7 3 1] (sut/run-part-2 is)))))

(t/deftest test-part-2
  (t/is (= "16178430" (sut/part-2))))
