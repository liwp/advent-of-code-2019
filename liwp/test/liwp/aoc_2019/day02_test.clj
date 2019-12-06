(ns liwp.aoc-2019.day02-test
  (:require [liwp.aoc-2019.day02 :as sut]
            [clojure.test :as t]))

(t/deftest Add
  (t/testing "accept"
    (t/is (sut/accept (sut/->Add) 1))
    (t/is (not (sut/accept (sut/->Add) 2)))
    (t/is (not (sut/accept (sut/->Add) 99))))

  (t/testing "execute"
    (let [add (sut/->Add)
          pc 0
          tape [1,9,10,3,2,3,11,0,99,30,40,50]
          new-state (sut/execute add {:pc pc :tape tape})]
      (t/is (= 4 (:pc new-state)))
      (t/is (= [1,9,10,70,2,3,11,0,99,30,40,50] (:tape new-state))))))

(t/deftest Mul
  (t/testing "accept"
    (t/is (not (sut/accept (sut/->Mul) 1)))
    (t/is (sut/accept (sut/->Mul) 2))
    (t/is (not (sut/accept (sut/->Mul) 99))))

  (t/testing "execute"
    (let [mul (sut/->Mul)
          pc 4
          tape [1,9,10,70,2,3,11,0,99,30,40,50]
          new-state (sut/execute mul {:pc pc :tape tape})]
      (t/is (= 8 (:pc new-state)))
      (t/is (= [3500,9,10,70,2,3,11,0,99,30,40,50] (:tape new-state))))))

(t/deftest Halt
  (t/testing "accept"
    (t/is (not (sut/accept (sut/->Halt) 1)))
    (t/is (not (sut/accept (sut/->Halt) 2)))
    (t/is (sut/accept (sut/->Halt) 99)))

  (t/testing "execute"
    (let [halt (sut/->Halt)
          pc 8
          tape [3500,9,10,70,2,3,11,0,99,30,40,50]
          new-state (sut/execute halt {:pc pc :tape tape})]
      (t/is (= nil (:pc new-state)))
      (t/is (= tape (:tape new-state))))))

(t/deftest run-cpu
  (t/is (= [3500,9,10,70,2,3,11,0,99,30,40,50]
           (sut/run-cpu [1,9,10,3,2,3,11,0,99,30,40,50])))
  (t/is (= [2,0,0,0,99]
           (sut/run-cpu [1,0,0,0,99])))
  (t/is (= [2,3,0,6,99]
           (sut/run-cpu [2,3,0,3,99])))
  (t/is (= [2,4,4,5,99,9801]
         (sut/run-cpu [2,4,4,5,99,0])))
  (t/is (= [30,1,1,4,2,5,6,0,99]
           (sut/run-cpu [1,1,1,4,99,5,6,0,99]))))

(t/deftest part-1
  (t/is (= 3706713 (sut/part-1))))

(t/deftest part-2
  (t/is (= 8609 (sut/part-2))))
