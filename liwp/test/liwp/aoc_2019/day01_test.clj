(ns liwp.aoc-2019.day01-test
  (:require [liwp.aoc-2019.day01 :as sut]
            [clojure.test :as t]))

(t/deftest mass->fuel
  (t/is (= (sut/mass->fuel 12) 2))
  (t/is (= (sut/mass->fuel 14) 2))
  (t/is (= (sut/mass->fuel 1969) 654))
  (t/is (= (sut/mass->fuel 100756) 33583)))

(t/deftest mass->fuel+
  (t/is (= (sut/mass->fuel+ 14) 2))
  (t/is (= (sut/mass->fuel+ 1969) 966))
  (t/is (= (sut/mass->fuel+ 100756) 50346)))
