(ns liwp.aoc-2019.day02-test
  (:require [liwp.aoc-2019.day02 :as sut]
            [clojure.test :as t]))

(t/deftest part-1
  (t/is (= 3706713 (sut/part-1))))

(t/deftest part-2
  (t/is (= 8609 (sut/part-2))))
