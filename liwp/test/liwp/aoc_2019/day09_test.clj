(ns liwp.aoc-2019.day09-test
  (:require [liwp.aoc-2019.day09 :as sut]
            [clojure.test :as t]))

(t/deftest test-part-1
  (t/is (= 3429606717 (sut/part-1))))

(t/deftest test-part-2
  (t/is (= 33679 (sut/part-2))))
