(ns liwp.aoc-2019.day17-test
  (:require [liwp.aoc-2019.day17 :as sut]
            [clojure.test :as t]))

(t/deftest test-part-1
  (t/is (= 7404 (sut/part-1))))

(t/deftest test-part-2
  (t/is (= 929045 (sut/part-2))))
