(ns liwp.aoc-2019.day14-test
  (:require [liwp.aoc-2019.day14 :as sut]
            [clojure.test :as t]
            [clojure.string :as string]))

(def demo-0 (sut/parse-reactions "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"))

(def demo-1 (sut/parse-reactions "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"))

(def demo-2 (sut/parse-reactions "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF"))

(def demo-3 (sut/parse-reactions "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX"))

(t/deftest test-parse-reaction
  (t/is (= ["D" {:amount 2 :src {"A" 1 "B" 2 "C" 3}}]
           (sut/parse-reaction "1 A, 2 B, 3 C => 2 D"))))

(t/deftest test-part-1-demo-0
  (t/is (= 31 (sut/fuel->ore demo-0 1))))

(t/deftest test-part-1-demo-1
  (t/is (= 13312 (sut/fuel->ore demo-1 1))))

(t/deftest test-part-1-demo-2
  (t/is (= 180697 (sut/fuel->ore demo-2 1))))

(t/deftest test-part-1-demo-3
  (t/is (= 2210736 (sut/fuel->ore demo-3 1))))

(t/deftest test-part-1
  (t/is (= 720484 (sut/part-1))))

(t/deftest test-part-2-demo-1
  (t/is (= 82892753 (sut/ore->fuel demo-1 1000000000000))))

(t/deftest test-part-2-demo-2
  (t/is (= 5586022 (sut/ore->fuel demo-2 1000000000000))))

(t/deftest test-part-2-demo-3
  (t/is (= 460664 (sut/ore->fuel demo-3 1000000000000))))

(t/deftest test-part-2-input
  (t/is (= 1993284 (sut/ore->fuel (sut/parse-reactions sut/input) 1000000000000))))

(t/deftest test-part-2
  (t/is (= 1993284 (sut/part-2))))
