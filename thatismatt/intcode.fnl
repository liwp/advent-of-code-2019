(local c {})

(fn c.last [t]
  (. t (length t)))

(fn c.map-keys [f t]
  (let [r []]
    (each [k v (pairs t)]
      (tset r (f k) v))
    r))

(fn c.zero-base [t]
  (c.map-keys #(- $ 1) t))

(fn c.one-base [t]
  (c.map-keys #(+ $ 1) t))

(fn c.store [xs loc val]
  (tset xs loc val)
  xs)

(fn c.parse-param-modes [code]
  (let [param-modes []]
    (for [i 2 4]
      (table.insert param-modes
                    (-> (/ code (math.pow 10 i)) math.floor (% 10))))
    param-modes))

(fn c.parse-opcode [code]
  (let [op (% code 100)]
    [op (c.parse-param-modes code)]))

(fn c.get-param [xs ip rb param-modes i]
  (match (. param-modes i)
    ;; position mode
    0 (or (. xs (. xs (+ i ip))) 0)
    ;; immediate mode
    1 (. xs (+ i ip))
    ;; relative mode
    2 (or (. xs (+ rb (. xs (+ i ip)))) 0)))

(fn c.get-locat [xs ip rb param-modes i]
  (match (. param-modes i)
    0 (. xs (+ i ip))
    1 (error "Invalid param mode: 1")
    2 (+ rb (. xs (+ i ip)))))

(set c.ops {})

(fn c.ops.add [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)
        x2 (c.get-param xs ip rb param-modes 2)
        x3 (c.get-locat xs ip rb param-modes 3)]
    [:ok (c.store xs x3 (+ x1 x2)) (+ 4 ip) rb in out]))

(fn c.ops.mul [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)
        x2 (c.get-param xs ip rb param-modes 2)
        x3 (c.get-locat xs ip rb param-modes 3)]
    [:ok (c.store xs x3 (* x1 x2)) (+ 4 ip) rb in out]))

(fn c.ops.input [xs ip rb param-modes in out]
  (let [x (c.get-locat xs ip rb param-modes 1)
        i (table.remove in 1)]
    (if (= nil i)
        [:pause-input xs ip rb in out]
        [:ok (c.store xs x i) (+ 2 ip) rb in out])))

(fn c.ops.output [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)]
    (table.insert out x1)
    [:ok xs (+ 2 ip) rb in out]))

(fn c.ops.jump-true [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)
        x2 (c.get-param xs ip rb param-modes 2)]
    ;; true - aka non-zero
    [:ok xs (if (= x1 0) (+ ip 3) x2) rb in out]))

(fn c.ops.jump-false [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)
        x2 (c.get-param xs ip rb param-modes 2)]
    ;; false - aka zero
    [:ok xs (if (= x1 0) x2 (+ ip 3)) rb in out]))

(fn c.ops.less [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)
        x2 (c.get-param xs ip rb param-modes 2)
        x3 (c.get-locat xs ip rb param-modes 3)]
    [:ok (c.store xs x3 (if (< x1 x2) 1 0)) (+ 4 ip) rb in out]))

(fn c.ops.equal [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)
        x2 (c.get-param xs ip rb param-modes 2)
        x3 (c.get-locat xs ip rb param-modes 3)]
    [:ok (c.store xs x3 (if (= x1 x2) 1 0)) (+ 4 ip) rb in out]))

(fn c.ops.relative-base [xs ip rb param-modes in out]
  (let [x1 (c.get-param xs ip rb param-modes 1)]
    [:ok xs (+ 2 ip) (+ x1 rb) in out]))

(fn c.step [xs ip rb in out]
  (let [opcode (. xs ip)
        [op param-modes] (c.parse-opcode opcode)]
    (match op
      1  (c.ops.add           xs ip rb param-modes in out)
      2  (c.ops.mul           xs ip rb param-modes in out)
      3  (c.ops.input         xs ip rb param-modes in out)
      4  (c.ops.output        xs ip rb param-modes in out)
      5  (c.ops.jump-true     xs ip rb param-modes in out)
      6  (c.ops.jump-false    xs ip rb param-modes in out)
      7  (c.ops.less          xs ip rb param-modes in out)
      8  (c.ops.equal         xs ip rb param-modes in out)
      9  (c.ops.relative-base xs ip rb param-modes in out)
      99 [:halt xs ip rb in out]
      _  (error (.. "Invalid op: " (tostring opcode) " at: " (tostring ip))))))

(fn c.run-loop [state]
  (let [[status xs ip rb in out] state]
    (if (= status :ok)
        (c.run-loop (c.step xs ip rb in out))
        state)))

(fn c.run [xs in]
  (let [state (c.run-loop [:ok (c.zero-base xs) 0 0 in []])
        [status xs ip rb in out] state]
    (if (= status :halt)
        [(c.one-base xs) out]
        state)))

(comment

 (global fennel (require :fennel))
 (global fv (fennel.dofile "/home/matt/code/fennel/fennelview.fnl"))
 (global c (require :intcode))

 (fn c.run-tests [tests]
   (print)
   (var tests-count 0)
   (var passes-count 0)
   (each [_ [xs in e out] (pairs tests)]
     (let [(ok? result) (pcall c.run xs in)
           pass? (and ok?
                      (= (fv result) (fv [e out])))]
       (print (if pass? (.. "✓ - " (fv [xs in]))
                  ok?   (.. "✗ - expected: " (fv [e out]) " actual: " (fv result))
                  :else (.. "✗ - " (fv [xs in]) " - " result)))
       (set tests-count (+ tests-count 1))
       (when pass?
         (set passes-count (+ passes-count 1)))))
   (print (.. (fv passes-count) "/" (fv tests-count))))

 (c.run-tests
  [[[1 0 0 0 99]          []  [2 0 0 0 99]          []]
   [[2 3 0 3 99]          []  [2 3 0 6 99]          []]
   [[2 3 0 1 99]          []  [2 2 0 1 99]          []]
   [[2 4 4 5 99 0]        []  [2 4 4 5 99 9801]     []]
   [[1 1 1 4 99 5 6 0 99] []  [30 1 1 4 2 5 6 0 99] []]
   [[3 0 4 0 99]          [7] [7 0 4 0 99]          [7]]
   [[1101 3 7 0 99]       []  [10 3 7 0 99]         []]
   [[1002 4 3 4 33]       []  [1002 4 3 4 99]       []]
   [[1101 100 -1 4 0]     []  [1101 100 -1 4 99]    []]])

 (c.run-tests
  [[[3 9 8 9 10 9 4 9 99 -1 8] [8] [3 9 8 9 10 9 4 9 99 1 8] [1]]
   [[3 9 8 9 10 9 4 9 99 -1 8] [4] [3 9 8 9 10 9 4 9 99 0 8] [0]]
   [[3 9 7 9 10 9 4 9 99 -1 8] [4] [3 9 7 9 10 9 4 9 99 1 8] [1]]
   [[3 9 7 9 10 9 4 9 99 -1 8] [8] [3 9 7 9 10 9 4 9 99 0 8] [0]]
   [[3 3 1108 -1 8 3 4 3 99]   [8] [3 3 1108 1 8 3 4 3 99] [1]]
   [[3 3 1108 -1 8 3 4 3 99]   [4] [3 3 1108 0 8 3 4 3 99] [0]]
   [[3 3 1107 -1 8 3 4 3 99]   [4] [3 3 1107 1 8 3 4 3 99] [1]]
   [[3 3 1107 -1 8 3 4 3 99]   [8] [3 3 1107 0 8 3 4 3 99] [0]]
   [[3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [1] [3 12 6 12 15 1 13 14 13 4 13 99 1 1 1 9] [1]]
   [[3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [0] [3 12 6 12 15 1 13 14 13 4 13 99 0 0 1 9] [0]]
   [[3 3 1105 -1 9 1101 0 0 12 4 12 99 1]      [1] [3 3 1105 1 9 1101 0 0 12 4 12 99 1]      [1]]
   [[3 3 1105 -1 9 1101 0 0 12 4 12 99 1]      [0] [3 3 1105 0 9 1101 0 0 12 4 12 99 0]      [0]]])

 (let [program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
   (print)
   (each [_ i (pairs [4 8 16])]
     (print (.. (fv i) " " (fv (. (c.run program [i]) 2))))))

 (c.run [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99] [])

 (c.run [1102 34915192 34915192 7 4 7 99 0] [])

 (c.run [104 1125899906842624 99] [])

 )

c
