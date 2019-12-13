(ns liwp.aoc-2019.day08-test
  (:require [liwp.aoc-2019.day08 :as sut]
            [clojure.test :as t]))

(t/deftest test-part-1
  (t/is (= 1820 (sut/part-1))))

(def demo (sut/parse-input "0222112222120000"))

(t/deftest test-part-2-demo
  (t/is (= [0 1 1 0] (sut/render-image demo 2 2))))

(def expected-image [[1 1 1 1 0 1 0 0 1 0 1 0 0 1 0 0 1 1 0 0 0 0 1 1 0]
                     [0 0 0 1 0 1 0 0 1 0 1 0 1 0 0 1 0 0 1 0 0 0 0 1 0]
                     [0 0 1 0 0 1 0 0 1 0 1 1 0 0 0 1 0 0 0 0 0 0 0 1 0]
                     [0 1 0 0 0 1 0 0 1 0 1 0 1 0 0 1 0 0 0 0 0 0 0 1 0]
                     [1 0 0 0 0 1 0 0 1 0 1 0 1 0 0 1 0 0 1 0 1 0 0 1 0]
                     [1 1 1 1 0 0 1 1 0 0 1 0 0 1 0 0 1 1 0 0 0 1 1 0 0]])

(t/deftest test-part-2
  (t/is (= expected-image (sut/part-2))))
