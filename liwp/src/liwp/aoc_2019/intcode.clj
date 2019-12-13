(ns liwp.aoc-2019.intcode)

(defn inst->opcode [inst]
  (mod inst 100))

(defn int->vec [i]
  (loop [acc '() i i]
    (if (zero? i)
      (into [] acc)
      (recur (conj acc (mod i 10)) (quot i 10)))))

(defn inst->modes [inst argc]
  (let [inst (quot inst 100)
        modes (reverse (int->vec inst))
        pad (repeat 0)]
    (vec (take argc (concat modes pad)))))

(defn tape-write [tape addr value]
  (assoc tape addr value))

(defn tape-read [tape addr mode]
  (case mode
    0 (nth tape addr)
    1 addr))

(defn binary-op [state f]
  (let [{:keys [pc tape]} state
        [encoded-op a-src b-src dst] (subvec tape pc)
        modes (inst->modes encoded-op 3)
        a (tape-read tape a-src (first modes))
        b (tape-read tape b-src (second modes))
        res (f a b)]
    (merge state {:pc (+ pc 4)
                  :tape (tape-write tape dst res)})))

(defprotocol AInstruction
  (execute [this state])
  (opcode [this]))

(defrecord Add []
  AInstruction
  (execute [this state]
    (binary-op state +))
  (opcode [this] 1))

(defrecord Mul []
  AInstruction
  (execute [this state]
    (binary-op state *))
  (opcode [this] 2))

(defrecord Input []
  AInstruction
  (execute [this state]
    (let [{:keys [input pc tape]} state
          [_ addr] (subvec tape pc)]
      (if (seq input)
        (merge state {:input (vec (rest input))
                      :pc (+ pc 2)
                      :tape (tape-write tape addr (first input))})
        (assoc state :blocked? true))))
  (opcode [this] 3))

(defrecord Output []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op addr] (subvec tape pc)
          modes (inst->modes encoded-op 1)
          output (tape-read tape addr (first modes))]
      (merge state {:output output :pc (+ pc 2)})))
  (opcode [this] 4))

(defrecord JumpIfTrue []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op test-addr pc-addr] (subvec tape pc)
          modes (inst->modes encoded-op 2)
          test (not (zero? (tape-read tape test-addr (first modes))))
          pc (if test
               (tape-read tape pc-addr (second modes))
               (+ pc 3))]
      (merge state {:pc pc})))
  (opcode [this] 5))

(defrecord JumpIfFalse []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op test-addr pc-addr] (subvec tape pc)
          modes (inst->modes encoded-op 2)
          test (zero? (tape-read tape test-addr (first modes)))
          pc (if test
               (tape-read tape pc-addr (second modes))
               (+ pc 3))]
      (merge state {:pc pc})))
  (opcode [this] 6))

(defrecord LessThan []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op a-addr b-addr res-addr] (subvec tape pc)
          modes (inst->modes encoded-op 3)
          a (tape-read tape a-addr (first modes))
          b (tape-read tape b-addr (second modes))
          res (if (< a b) 1 0)]
      (merge state {:pc (+ pc 4) :tape (tape-write tape res-addr res)})))
  (opcode [this] 7))

(defrecord Equals []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op a-addr b-addr res-addr] (subvec tape pc)
          modes (inst->modes encoded-op 3)
          a (tape-read tape a-addr (first modes))
          b (tape-read tape b-addr (second modes))
          res (if (= a b) 1 0)]
      (merge state {:pc (+ pc 4) :tape (tape-write tape res-addr res)})))
  (opcode [this] 8))

(defrecord Halt []
  AInstruction
  (execute [this state]
    (merge state {:halted? true}))
  (opcode [this] 99))

(defn run-cpu
  [state]
  (let [instructions (into {} (map #(vector (opcode %) %)) [(->Add)
                                                            (->Input)
                                                            (->Mul)
                                                            (->Output)
                                                            (->JumpIfFalse)
                                                            (->JumpIfTrue)
                                                            (->LessThan)
                                                            (->Equals)
                                                            (->Halt)])]
    (loop [state (dissoc state :blocked?)]
      (let [{:keys [blocked? halted? output pc tape]} state
            encoded-op (get tape pc)
            op (inst->opcode encoded-op)]
        (if (or blocked? halted?)
          state
          (let [inst (get instructions op)
                state (execute inst state)]
            (recur state)))))))

