(fn map-keys [f t]
  (let [r []]
    (each [k v (pairs t)]
      (tset r (f k) v))
    r))

(fn zero-base [t]
  (map-keys #(- $ 1) t))

(fn one-base [t]
  (map-keys #(+ $ 1) t))

(fn make-input []
  [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 1 6 19 1 19 6 23 2 23 6 27 2 6 27 31 2 13 31 35 1 9 35 39 2 10
   39 43 1 6 43 47 1 13 47 51 2 6 51 55 2 55 6 59 1 59 5 63 2 9 63 67 1 5 67 71 2 10 71 75 1 6 75 79
   1 79 5 83 2 83 10 87 1 9 87 91 1 5 91 95 1 95 6 99 2 10 99 103 1 5 103 107 1 107 6 111 1 5 111 115
   2 115 6 119 1 119 6 123 1 123 10 127 1 127 13 131 1 131 2 135 1 135 5 0 99 2 14 0 0])

(fn store [xs loc val]
  (tset xs loc val)
  xs)

(fn fetch [xs loc]
  (. xs (. xs loc)))

(fn step [xs ip]
  (let [x0 (. xs ip)
        x1 (fetch xs (+ 1 ip))
        x2 (fetch xs (+ 2 ip))
        x3 (. xs (+ 3 ip))]
    (match x0
      1  [:ok (store xs x3 (+ x1 x2)) (+ 4 ip)]
      2  [:ok (store xs x3 (* x1 x2)) (+ 4 ip)]
      99 [:halt xs ip]
      _  (error (.. "Invalid instruction: " (tostring x0) " at: " (tostring ip))))))

(fn run-loop [state]
  (let [[ok/halt xs ip] state]
    (match ok/halt
      :ok   (run-loop (step xs ip))
      :halt xs)))

(fn run [xs]
  (one-base (run-loop [:ok (zero-base xs) 0])))

(fn restore-noun+verb [xs noun verb]
  (store xs 2 noun)
  (store xs 3 verb)
  xs)

(fn restore+run [xs noun verb]
  (-> xs
      (restore-noun+verb noun verb)
      run
      (. 1)))

;; (run [1 0 0 0 99])          ;; [2 0 0 0 99]
;; (run [2 3 0 3 99])          ;; [2 3 0 3 99]
;; (run [2 4 4 5 99 0])        ;; [2 4 4 5 99 0]
;; (run [1 1 1 4 99 5 6 0 99]) ;; [30 1 1 4 2 5 6 0 99]

(print (.. "Day 2 / Part 1: "
           (restore+run (make-input) 12 2)))
