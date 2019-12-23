(ns liwp.aoc-2019.day13-test
  (:require [liwp.aoc-2019.day13 :as sut]
            [clojure.test :as t]))

(t/deftest test-part-1
  (t/is (= 420 (sut/part-1))))

(t/deftest test-part-2
  (t/is (= 21651 (sut/part-2))))
