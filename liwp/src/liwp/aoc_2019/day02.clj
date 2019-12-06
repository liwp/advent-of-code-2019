(ns liwp.aoc-2019.day02)

(def input [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,13,23,27,1,6,27,31,1,31,10,35,1,35,6,39,1,39,13,43,2,10,43,47,1,47,6,51,2,6,51,55,1,5,55,59,2,13,59,63,2,63,9,67,1,5,67,71,2,13,71,75,1,75,5,79,1,10,79,83,2,6,83,87,2,13,87,91,1,9,91,95,1,9,95,99,2,99,9,103,1,5,103,107,2,9,107,111,1,5,111,115,1,115,2,119,1,9,119,0,99,2,0,14,0])

(defn binary-op [{:keys [pc tape]} op]
  (let [[_ a-src b-src dst] (subvec tape pc)
        a (nth tape a-src)
        b (nth tape b-src)
        res (op a b)]
    {:pc (+ pc 4)
     :tape (assoc tape dst res)}))

(defprotocol AInstruction
  (accept [this opcode])
  (execute [this state]))

(defrecord Add []
  AInstruction
  (accept [this opcode] (= opcode 1))
  (execute [this state]
    (binary-op state +)))

(defrecord Mul []
  AInstruction
  (accept [this opcode] (= opcode 2))
  (execute [this state]
    (binary-op state *)))

(defrecord Halt []
  AInstruction
  (accept [this opcode] (= opcode 99))
  (execute [this state]
    (assoc state :pc nil)))

(defn run-cpu [tape noun verb]
  (let [tape (-> tape
                 (assoc 1 noun)
                 (assoc 2 verb))
        add (Add.)
        mul (Mul.)
        hal (Halt.)]
    (loop [state {:pc 0 :tape tape}]
      (let [{:keys [pc tape]} state
            opcode (get tape pc)]
        (if (nil? pc)
          (nth tape 0)
          (recur (cond
                   (accept add opcode) (execute add state)
                   (accept mul opcode) (execute mul state)
                   (accept hal opcode) (execute hal state)
                   :else {:pc nil :tape tape})))))))

(defn part-1 []
  (run-cpu input 12 2))

(defn part-2 []
  (let [[noun verb] (first
                     (for [noun (range 100)
                           verb (range 100)
                           :when (= 19690720 (run-cpu input noun verb))]
                       [noun verb]))]
    (+ (* 100 noun) verb)))

(defn run []
  (println "Day 02")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
