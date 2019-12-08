(ns liwp.aoc-2019.intcode-test
  (:require [liwp.aoc-2019.intcode :as sut]
            [clojure.test :as t]))

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
          new-state (sut/execute in {:input 12 :pc pc :tape tape})]
      (t/is (= 2 (:pc new-state)))
      (t/is (= [3,3,99,12] (:tape new-state))))))

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
    (let [out (sut/->Output)
          pc 0
          tape [4,3,99,12]
          new-state (sut/execute out {:pc pc :tape tape})]
      (t/is (= 2 (:pc new-state)))
      (t/is (= [4,3,99,12] (:tape new-state)))
      (t/is (= 12 (:output new-state))))))

(t/deftest inst->opcode
  (t/is (= 2 (sut/inst->opcode 1002))))

(t/deftest inst->modes
  (t/is (= [0] (sut/inst->modes 3 1)))
  (t/is (= [1] (sut/inst->modes 103 1)))
  (t/is (= [1 1 0] (sut/inst->modes 1102 3)))
  (t/is (= [0 1 0] (sut/inst->modes 1002 3)))
  (t/is (= [0 0 0] (sut/inst->modes 2 3))))

(defn new-state [tape & [input]]
  {:input input :pc 0 :tape tape})

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
                    (assoc :input 123))
          {:keys [output tape]} (sut/run-cpu state)]
      (t/is (= [123,0,4,0,99] tape))
      (t/is (= 123 output))))

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
            (t/is (= 1 output))
            (t/is (= [3,9,8,9,10,9,4,9,99,1,8] tape))))
        (t/testing "1 == 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 0 output))
            (t/is (= [3,9,8,9,10,9,4,9,99,0,8] tape))))))

    (t/testing "less-than"
      (let [tape [3,9,7,9,10,9,4,9,99,-1,8]]
        (t/testing "8 < 8"
          (let [{:keys [output tape]} (run tape 8)]
            (t/is (= 0 output))
            (t/is (= [3,9,7,9,10,9,4,9,99,0,8] tape))))
        (t/testing "1 < 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 1 output))
            (t/is (= [3,9,7,9,10,9,4,9,99,1,8] tape)))))))

  (t/testing "immediate mode"
    (t/testing "equals"
      (let [tape [3,3,1108,-1,8,3,4,3,99]]
        (t/testing "8 == 8"
          (let [{:keys [output tape]} (run tape 8)]
            (t/is (= 1 output))
            (t/is (= [3,3,1108,1,8,3,4,3,99] tape))))
        (t/testing "1 == 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 0 output))
            (t/is (= [3,3,1108,0,8,3,4,3,99] tape))))))

    (t/testing "less-than"
      (let [tape [3,3,1107,-1,8,3,4,3,99]]
        (t/testing "8 < 8"
          (let [{:keys [output tape]} (run tape 8)]
            (t/is (= 0 output))
            (t/is (= [3,3,1107,0,8,3,4,3,99] tape))))
        (t/testing "1 < 8"
          (let [{:keys [output tape]} (run tape 1)]
            (t/is (= 1 output))
            (t/is (= [3,3,1107,1,8,3,4,3,99] tape))))))))

(t/deftest run-cpu-jumps
  (t/testing "position mode"
    (let [tape [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]]
      (t/testing "8 == 0"
        (let [{:keys [output tape]} (run tape 8)]
          (t/is (= 1 output))
          (t/is (= [3,12,6,12,15,1,13,14,13,4,13,99,8,1,1,9] tape))))
      (t/testing "0 == 0"
        (let [{:keys [output tape]} (run tape 0)]
          (t/is (= 0 output)))
        (t/is (= [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] tape)))))

  (t/testing "immediate mode"
    (let [tape [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]]
      (t/testing "8 == 0"
        (let [{:keys [output tape]} (run tape 8)]
          (t/is (= 1 output))
          (t/is (= [3,3,1105,8,9,1101,0,0,12,4,12,99,1] tape))))
      (t/testing "0 == 0"
        (let [{:keys [output tape]} (run tape 0)]
          (t/is (= 0 output))
          (t/is (= [3,3,1105,0,9,1101,0,0,12,4,12,99,0] tape)))))))


(t/deftest run-cpu-conditionals
  (let [tape [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]]
    (t/testing "input < 8"
      (let [{:keys [output tape]} (run tape 1)]
        (t/is (= 999 output))))
    (t/testing "input == 8"
      (let [{:keys [output tape]} (run tape 8)]
        (t/is (= 1000 output))))
    (t/testing "input >8"
      (let [{:keys [output tape]} (run tape 10)]
        (t/is (= 1001 output))))))
