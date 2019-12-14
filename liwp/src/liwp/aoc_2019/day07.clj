(ns liwp.aoc-2019.day07
  (:require [liwp.aoc-2019.intcode :as ic])
  (:require [liwp.aoc-2019.permutations :refer [permutations]]))

(def input [3,8,1001,8,10,8,105,1,0,0,21,46,59,80,105,122,203,284,365,446,99999,3,9,102,3,9,9,1001,9,5,9,102,2,9,9,1001,9,3,9,102,4,9,9,4,9,99,3,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,101,5,9,9,1002,9,3,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,4,9,1001,9,2,9,102,4,9,9,101,3,9,9,102,2,9,9,4,9,99,3,9,102,5,9,9,101,4,9,9,102,3,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99])

(defn new-cpu [tape input]
  {:input (conj clojure.lang.PersistentQueue/EMPTY input)
   :pc 0
   :tape tape})

(defn run-cpu-with-input [state input]
  (-> state
      (update :input conj input)
      ic/run-cpu))

(defn run-cpus [cpus input]
  (loop [new-cpus [] [cpu & cpus] cpus input input]
    (if (nil? cpu)
      {:cpus new-cpus :output input}
      (let [cpu (run-cpu-with-input cpu input)
            output (:output cpu)
            cpu (merge cpu {:output (pop output)})]
        (recur (conj new-cpus cpu) cpus (peek output))))))

(defn run-system [tape phases]
  (loop [cpus (map #(new-cpu tape %) phases) input 0]
    (let [{:keys [cpus output]} (run-cpus cpus input)
          {:keys [blocked?]} (last cpus)]
      (if blocked?
        (recur cpus output)
        output))))

(defn find-max-output [phases]
  (->> phases
       permutations
       (map #(run-system input %))
       (apply max)))

(defn part-1 []
  (find-max-output (range 5)))

(defn part-2 []
  (find-max-output (range 5 10)))

(defn run []
  (println "Day 07")
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
