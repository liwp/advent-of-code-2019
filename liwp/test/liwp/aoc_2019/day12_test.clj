(ns liwp.aoc-2019.day12-test
  (:require [liwp.aoc-2019.day12 :as sut]
            [clojure.test :as t]))

(def demo-1 "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")

(t/deftest test-demo-1
  (let [moons (sut/parse-moons demo-1)]
    (t/testing "step 1"
      (t/is (= [{:x 2, :y -1, :z 1, :vel {:x 3, :y -1, :z -1}}
                {:x 3, :y -7, :z -4, :vel {:x 1, :y 3, :z 3}}
                {:x 1, :y -7, :z 5, :vel {:x -3, :y 1, :z -3}}
                {:x 2, :y 2, :z 0, :vel {:x -1, :y -3, :z 1}}]
               (sut/run-step moons))))

    (t/testing "step 10"
      (t/is (= [{:x 2, :y 1, :z -3, :vel {:x -3, :y -2, :z 1}}
                {:x 1, :y -8, :z 0, :vel {:x -1, :y 1, :z 3}}
                {:x 3, :y -6, :z 1, :vel {:x 3, :y 2, :z -3}}
                {:x 2, :y 0, :z 4, :vel {:x 1, :y -1, :z -1}}]
               (sut/run-steps moons 10))))

    (t/testing "energy after step 10"
      (t/is (= 179 (sut/energy-for-moons (sut/run-steps moons 10)))))))

(t/deftest test-part-1
  (t/is (= 8044 (sut/part-1))))

(t/deftest test-find-cycle
  (let [moons (sut/parse-moons demo-1)]
    (t/is (= 2772 (sut/find-cycle moons)))))

(def demo-2 "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")

(t/deftest test-find-cycle-2
  (let [moons (sut/parse-moons demo-2)]
    (t/is (= 4686774924 (sut/find-cycle moons)))))

(t/deftest test-part-2
  (t/is (= 362375881472136 (sut/part-2))))
