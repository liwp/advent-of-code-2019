(ns liwp.aoc-2019.intcode-test
  (:require [liwp.aoc-2019.intcode :as sut]
            [clojure.test :as t]))

(defn queue [& args]
  (into clojure.lang.PersistentQueue/EMPTY args))

(t/deftest Add
  (t/testing "opcode"
    (t/is (= 1 (sut/opcode (sut/->Add)))))

  (t/testing "execute"
    (let [add (sut/->Add)
          pc 0
          tape [1,9,10,3,2,3,11,0,99,30,40,50]
          new-state (sut/execute add {:pc pc :tape tape})]
      (t/is (= 4 (:pc new-state)))
      (t/is (= [1,9,10,70,2,3,11,0,99,30,40,50] (:tape new-state))))))

(t/deftest Input
  (t/testing "opcode"
    (t/is (= 3 (sut/opcode (sut/->Input)))))

  (t/testing "execute"
    (let [in (sut/->Input)
          pc 0
          tape [3,3,99,0]
          new-state (sut/execute in {:input (queue 12) :pc pc :tape tape})]
      (t/is (= [] (:input new-state)))
      (t/is (= 2 (:pc new-state)))
      (t/is (= [3,3,99,12] (:tape new-state))))

    (t/testing "multiple inputs"
      (let [in (sut/->Input)
            pc 0
            tape [3,3,99,0]
            new-state (sut/execute in {:input (queue 11 22) :pc pc :tape tape})]
        (t/is (= [22] (:input new-state)))
        (t/is (= 2 (:pc new-state)))
        (t/is (= [3,3,99,11] (:tape new-state)))))

    (t/testing "block on missing"
      (let [in (sut/->Input)
            pc 0
            tape [3,3,99,0]
            new-state (sut/execute in {:input (queue) :pc pc :tape tape})]
        (t/is (:blocked? new-state))
        (t/is (= 0 (:pc new-state)))
        (t/is (= [3,3,99,0] (:tape new-state)))))))

(t/deftest Mul
  (t/testing "opcode"
    (t/is (= 2 (sut/opcode (sut/->Mul)))))

  (t/testing "execute"
    (let [mul (sut/->Mul)
          pc 4
          tape [1,9,10,70,2,3,11,0,99,30,40,50]
          new-state (sut/execute mul {:pc pc :tape tape})]
      (t/is (= 8 (:pc new-state)))
      (t/is (= [3500,9,10,70,2,3,11,0,99,30,40,50] (:tape new-state))))))

(t/deftest Output
  (t/testing "opcode"
    (t/is (= 4 (sut/opcode (sut/->Output)))))

  (t/testing "execute"
    (let [state (sut/execute (sut/->Output) {:pc 0 :tape [4,3,99,12]})]
      (t/is (= 2 (:pc state)))
      (t/is (= [4,3,99,12] (:tape state)))
      (t/is (= [12] (:output state))))

    (let [new-state (sut/execute (sut/->Output) {:base 1 :pc 0 :tape [204,1,99]})]
      (t/is (= 2 (:pc new-state)))
      (t/is (= [204,1,99] (:tape new-state)))
      (t/is (= [99] (:output new-state))))))

(t/deftest AdjustBase
  (t/testing "opcode"
    (t/is (= 9 (sut/opcode (sut/->AdjustBase)))))

  (t/testing "execute"
    (t/testing "relative mode"
      (let [state (sut/execute (sut/->AdjustBase) {:base 0 :pc 0 :tape [209,2,99]})]
        (t/is (= 2 (:pc state)))
        (t/is (= 99 (:base state)))))

    (t/testing "immediate mode"
      (let [state (sut/execute (sut/->AdjustBase) {:base 2000 :pc 0 :tape [109,19]})]
        (t/is (= 2 (:pc state)))
        (t/is (= 2019 (:base state)))))

    (t/testing "negative immediate"
      (let [state (sut/execute (sut/->AdjustBase) {:base 2000 :pc 0 :tape [109,-19]})]
        (t/is (= 2 (:pc state)))
        (t/is (= 1981 (:base state)))))))

(t/deftest inst->opcode
  (t/is (= 2 (sut/inst->opcode 1002)))
  (t/is (= 9 (sut/inst->opcode 109))))

(t/deftest inst->modes
  (t/is (= [0] (sut/inst->modes 3 1)))
  (t/is (= [1] (sut/inst->modes 103 1)))
  (t/is (= [1 1 0] (sut/inst->modes 1102 3)))
  (t/is (= [0 1 0] (sut/inst->modes 1002 3)))
  (t/is (= [0 0 0] (sut/inst->modes 2 3)))
  (t/is (= [2] (sut/inst->modes 204 1))))

(t/deftest tape-read
  (t/testing "position mode"
    (t/is (= 11 (sut/tape-read {:tape [10 11 12]} sut/POS_MODE 1))))
  (t/testing "immediate mode"
    (t/is (= 1 (sut/tape-read {:tape [10 11 12]} sut/IMM_MODE 1))))
  (t/testing "relative mode"
    (t/is (= 12 (sut/tape-read {:base 1 :tape [10 11 12]} sut/REL_MODE 1))))
  (t/testing "read past tape"
    (t/is (= 0 (sut/tape-read {:tape [10]} sut/POS_MODE 1000)))))

(t/deftest tape-write
  (t/testing "position mode"
    (t/is (= [11 99] (sut/tape-write {:tape [11 22]} sut/POS_MODE 1 99))))
  (t/testing "immediate mode"
    (t/is (thrown?
           java.lang.IllegalArgumentException
           (sut/tape-write {:tape [11 22]} sut/IMM_MODE 1 99))))
  (t/testing "relative mode"
    (t/is (= [11 22 99] (sut/tape-write {:base 1 :tape [11 22 33]} sut/REL_MODE 1 99))))
  (t/testing "write past tape"
    (let [tape (sut/tape-write {:tape [10]} sut/POS_MODE 3 99)]
      (t/is (= 4 (count tape)))
      (t/is (= [10 0 0 99] tape)))))

(defn new-state [tape & [input]]
  {:base 0 :input (queue input) :pc 0 :tape tape})

(defn run [tape & [input]]
  (let [state (new-state tape input)]
    (sut/run-cpu state)))

(t/deftest run-cpu
  (t/is (= [3500,9,10,70,2,3,11,0,99,30,40,50]
           (:tape (sut/run-cpu (new-state [1,9,10,3,2,3,11,0,99,30,40,50])))))
  (t/is (= [2,0,0,0,99]
           (:tape (sut/run-cpu (new-state [1,0,0,0,99])))))
  (t/is (= [2,3,0,6,99]
           (:tape (sut/run-cpu (new-state [2,3,0,3,99])))))
  (t/is (= [2,4,4,5,99,9801]
           (:tape (sut/run-cpu (new-state [2,4,4,5,99,0])))))
  (t/is (= [30,1,1,4,2,5,6,0,99]
           (:tape (sut/run-cpu (new-state [1,1,1,4,99,5,6,0,99])))))
  (t/testing "input-output"
    (let [state (-> [3,0,4,0,99]
                    new-state
                    (assoc :input (queue 123)))
          {:keys [output tape]} (sut/run-cpu state)]
      (t/is (= [123,0,4,0,99] tape))
      (t/is (= 123 (peek output)))))

  (t/testing "addressing modes"
    (let [state (-> [1002,4,3,4,33]
                    new-state)
          {:keys [tape]} (sut/run-cpu state)]
      (t/is (= [1002,4,3,4,99] tape)))))

(t/deftest run-cpu-comparisons
  (t/testing "position mode"
    (t/testing "equals"
      (let [tape [3,9,8,9,10,9,4,9,99,-1,8]]
        (t/testing "8 == 8"
          (let [{:keys [output tape]} (run tape 8)]
            (t/is (= 1 (peek output)))
            (t/is (= [3,9,8,9,10,9,4,9,99,1,8] tape))))
        (t/testing "1 == 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 0 (peek output)))
            (t/is (= [3,9,8,9,10,9,4,9,99,0,8] tape))))))

    (t/testing "less-than"
      (let [tape [3,9,7,9,10,9,4,9,99,-1,8]]
        (t/testing "8 < 8"
          (let [{:keys [output tape]} (run tape 8)]
            (t/is (= 0 (peek output)))
            (t/is (= [3,9,7,9,10,9,4,9,99,0,8] tape))))
        (t/testing "1 < 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 1 (peek output)))
            (t/is (= [3,9,7,9,10,9,4,9,99,1,8] tape)))))))

  (t/testing "immediate mode"
    (t/testing "equals"
      (let [tape [3,3,1108,-1,8,3,4,3,99]]
        (t/testing "8 == 8"
          (let [{:keys [output tape]} (run tape 8)]
            (t/is (= 1 (peek output)))
            (t/is (= [3,3,1108,1,8,3,4,3,99] tape))))
        (t/testing "1 == 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 0 (peek output)))
            (t/is (= [3,3,1108,0,8,3,4,3,99] tape))))))

    (t/testing "less-than"
      (let [tape [3,3,1107,-1,8,3,4,3,99]]
        (t/testing "8 < 8"
          (let [{:keys [output tape]} (run tape 8)]
            (t/is (= 0 (peek output)))
            (t/is (= [3,3,1107,0,8,3,4,3,99] tape))))
        (t/testing "1 < 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 1 (peek output)))
            (t/is (= [3,3,1107,1,8,3,4,3,99] tape))))))))

(t/deftest run-cpu-jumps
  (t/testing "position mode"
    (let [tape [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]]
      (t/testing "8 == 0"
        (let [{:keys [output tape]} (run tape 8)]
          (t/is (= 1 (peek output)))
          (t/is (= [3,12,6,12,15,1,13,14,13,4,13,99,8,1,1,9] tape))))
      (t/testing "0 == 0"
        (let [{:keys [output tape]} (run tape 0)]
          (t/is (= 0 (peek output))))
        (t/is (= [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] tape)))))

  (t/testing "immediate mode"
    (let [tape [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]]
      (t/testing "8 == 0"
        (let [{:keys [output tape]} (run tape 8)]
          (t/is (= 1 (peek output)))
          (t/is (= [3,3,1105,8,9,1101,0,0,12,4,12,99,1] tape))))
      (t/testing "0 == 0"
        (let [{:keys [output tape]} (run tape 0)]
          (t/is (= 0 (peek output)))
          (t/is (= [3,3,1105,0,9,1101,0,0,12,4,12,99,0] tape)))))))


(t/deftest run-cpu-conditionals
  (let [tape [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]]
    (t/testing "input < 8"
      (let [{:keys [output tape]} (run tape 1)]
        (t/is (= 999 (peek output)))))
    (t/testing "input == 8"
      (let [{:keys [output tape]} (run tape 8)]
        (t/is (= 1000 (peek output)))))
    (t/testing "input >8"
      (let [{:keys [output tape]} (run tape 10)]
        (t/is (= 1001 (peek output)))))))

(t/deftest test-demo-1
  (t/testing "write outside of tape"
    (let [tape [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
      (t/is (= [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
               (-> tape run :output seq))))))

(t/deftest test-large-numbers
  (t/testing "large multiplication result"
    (let [tape [1102,34915192,34915192,7,4,7,99,0]]
      (t/is (= (* 34915192 34915192) (-> tape run :output peek)))))

  (t/testing "large literal"
    (let [tape [104,1125899906842624,99]]
      (t/is (= (long 1125899906842624) (-> tape run :output peek))))))
