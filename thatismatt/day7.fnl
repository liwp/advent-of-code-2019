(local c (require :day5))

(local m {})

(fn m.map [f t]
  (let [r []]
    (each [_ v (pairs t)]
      (table.insert r (f v)))
    r))

(fn m.all-uniq [t]
  (var dupe false)
  (let [r {}]
    (each [_ v (pairs t)]
      (set dupe (or dupe (. r v)))
      (tset r v true)))
  (not dupe))

(fn m.amplify [make-program pss]
  (var signal 0)
  (while (> (length pss) 0)
    (set signal
         (-> (c.run (make-program) [(table.remove pss 1) signal]) (. 2) c.last)))
  signal)

(fn m.pss-range [from to]
  (let [t []]
    (for [h from to]
      (for [i from to]
        (for [j from to]
          (for [k from to]
            (for [l from to]
              (when (m.all-uniq [h i j k l])
                (table.insert t [h i j k l])))))))
    t))

(fn m.max-signal [make-program pss-from pss-to amplify]
  (var max-signal 0)
  (var max-pss nil)
  (each [_ pss (pairs (m.pss-range pss-from pss-to))]
    (let [signal (amplify
                  make-program
                  [(table.unpack pss)])]
      (when (> signal max-signal)
        (set max-signal signal)
        (set max-pss pss))))
  [max-signal max-pss])

(fn m.amplify+feedback-loop [states-0]
  (var signal 0)
  (var states states-0)
  (while (-> states (. 1) (. 1) (not= :halt))
    (set states
         (m.map
          (fn [[status xs ip in out]]
            (let [state (c.run-loop [:ok xs ip [signal] out])]
              (set signal (-> state c.last c.last))
              state))
          states)))
  states)

(fn m.amplify+feedback [make-program pss]
  (let [states (m.map
                (fn [ps] (c.run-loop [:ok (c.zero-base (make-program)) 0 [ps] []]))
                pss)]
    (->> pss
         (m.map (fn [ps] (c.run-loop [:ok (c.zero-base (make-program)) 0 [ps] []])))
         (m.amplify+feedback-loop states)
         c.last c.last c.last)))

(fn m.make-program []
  [3 8 1001 8 10 8 105 1 0 0 21 34 59 76 101 114 195 276 357 438 99999 3 9 1001 9 4 9 1002 9 4 9 4 9 99 3 9 102 4 9 9 101 2 9 9 102 4 9 9 1001 9 3 9 102 2 9 9 4 9 99 3 9 101 4 9 9 102 5 9 9 101 5 9 9 4 9 99 3 9 102 2 9 9 1001 9 4 9 102 4 9 9 1001 9 4 9 1002 9 3 9 4 9 99 3 9 101 2 9 9 1002 9 3 9 4 9 99 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 99 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 99])

(print (.. "Day 7 / Part 1: "
           (. (m.max-signal
               m.make-program
               0 4
               m.amplify) 1)))

(print (.. "Day 7 / Part 2: "
           (. (m.max-signal
               m.make-program
               5 9
               m.amplify+feedback) 1)))

(comment

 (global m (require :day7))
 (global c (require :day5))

 (m.all-uniq [0 1 2 3 4])

 (m.all-uniq [1 2 3 4 1])

 (m.amplify (fn [] [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])
            [4 3 2 1 0])
 (m.amplify (fn [] [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0])
            [0 1 2 3 4])

 (m.max-signal
  (fn [] [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])
  0 4
  m.amplify)

 (m.max-signal
  (fn [] [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0])
  0 4
  m.amplify)

 (m.max-signal
  (fn [] [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5])
  5 9
  m.amplify+feedback)

 (m.max-signal
  (fn [] [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10])
  5 9
  m.amplify+feedback)

 )

m
