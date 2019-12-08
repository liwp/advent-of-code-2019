(ns liwp.aoc-2019.day05-test
  (:require [liwp.aoc-2019.day05 :as sut]
            [clojure.test :as t]))

(t/deftest test-part-1
  (t/is (= 13087969 (sut/part-1))))

(t/deftest test-part-2
  (t/is (= 14110739 (sut/part-2))))
