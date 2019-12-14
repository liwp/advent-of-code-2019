(ns liwp.aoc-2019.intcode)

(def ^:const POS_MODE 0)
(def ^:const IMM_MODE 1)
(def ^:const REL_MODE 2)

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

(defn tape-write [{:keys [base tape]} mode addr value]
  (let [addr (case mode
               ;; position mode
               0 addr
               ;; relative mode
               2 (+ base addr))
        new-length (max (count tape) addr)
        tape (vec (take new-length (concat tape (repeat 0))))]
    (assoc tape addr value)))

(defn tape-read [{:keys [base tape]} mode addr]
  (case mode
    ;; position mode
    0 (nth tape addr 0)
    ;; immediate mode
    1 addr
    ;; relative mode
    2 (nth tape (+ base addr) 0)))

(defn binary-op [state f]
  (let [{:keys [pc tape]} state
        [encoded-op a-src b-src dst] (subvec tape pc)
        [a-mode b-mode dst-mode] (inst->modes encoded-op 3)
        a (tape-read state a-mode a-src)
        b (tape-read state b-mode b-src)
        res (f a b)]
    (merge state {:pc (+ pc 4)
                  :tape (tape-write state dst-mode dst res)})))

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
          [encoded-op addr] (subvec tape pc)
          [mode] (inst->modes encoded-op 1)]
      (if (seq input)
        (merge state {:input (vec (rest input))
                      :pc (+ pc 2)
                      :tape (tape-write state mode addr (first input))})
        (assoc state :blocked? true))))
  (opcode [this] 3))

(defrecord Output []
  AInstruction
  (execute [this state]
    (let [{:keys [output pc tape]} state
          [encoded-op addr] (subvec tape pc)
          [mode] (inst->modes encoded-op 1)
          output (conj output (tape-read state mode addr))]
      (merge state {:output output :pc (+ pc 2)})))
  (opcode [this] 4))

(defrecord JumpIfTrue []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op test-addr pc-addr] (subvec tape pc)
          modes (inst->modes encoded-op 2)
          test (not (zero? (tape-read state (first modes) test-addr)))
          pc (if test
               (tape-read state (second modes) pc-addr)
               (+ pc 3))]
      (merge state {:pc pc})))
  (opcode [this] 5))

(defrecord JumpIfFalse []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op test-addr pc-addr] (subvec tape pc)
          modes (inst->modes encoded-op 2)
          test (zero? (tape-read state (first modes) test-addr))
          pc (if test
               (tape-read state (second modes) pc-addr)
               (+ pc 3))]
      (merge state {:pc pc})))
  (opcode [this] 6))

(defrecord LessThan []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op a-addr b-addr dst-addr] (subvec tape pc)
          [a-mode b-mode dst-mode] (inst->modes encoded-op 3)
          a (tape-read state a-mode a-addr)
          b (tape-read state b-mode b-addr)
          res (if (< a b) 1 0)]
      (merge state {:pc (+ pc 4) :tape (tape-write state dst-mode dst-addr res)})))
  (opcode [this] 7))

(defrecord Equals []
  AInstruction
  (execute [this state]
    (let [{:keys [pc tape]} state
          [encoded-op a-addr b-addr dst-addr] (subvec tape pc)
          [a-mode b-mode dst-mode] (inst->modes encoded-op 3)
          a (tape-read state a-mode a-addr)
          b (tape-read state b-mode b-addr)
          res (if (= a b) 1 0)]
      (merge state {:pc (+ pc 4) :tape (tape-write state dst-mode dst-addr res)})))
  (opcode [this] 8))

(defrecord AdjustBase []
  AInstruction
  (execute [this state]
    (let [{:keys [base pc tape]} state
          [encoded-op addr] (subvec tape pc)
          [mode] (inst->modes encoded-op 1)
          adjustment (tape-read state mode addr)
          base (+ base adjustment)]
      (merge state {:pc (+ pc 2) :base base})))
  (opcode [this] 9))

(defrecord Halt []
  AInstruction
  (execute [this state]
    (merge state {:halted? true}))
  (opcode [this] 99))

(def default-state
  {:base 0
   :blocked? false
   :halted? false
   :output clojure.lang.PersistentQueue/EMPTY
   :pc 0})

(defn should-return? [state]
  (or (:blocked? state) (:halted? state)))

(defn next-opcode [{:keys [pc tape]}]
  (let [inst (get tape pc)]
    (inst->opcode inst)))

(defn run-cpu
  [state]
  (let [state (merge default-state state {:blocked? false :halted? false})
        instructions (into {} (map #(vector (opcode %) %)) [(->Add)
                                                            (->Input)
                                                            (->Mul)
                                                            (->Output)
                                                            (->JumpIfFalse)
                                                            (->JumpIfTrue)
                                                            (->LessThan)
                                                            (->Equals)
                                                            (->AdjustBase)
                                                            (->Halt)])]
    (loop [state state]
      (if (should-return? state)
        state
        (let [inst (get instructions (next-opcode state))]
          (recur (execute inst state)))))))

