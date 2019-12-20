(local m {})

(fn m.last [t]
  (. t (length t)))

(fn m.map-keys [f t]
  (let [r []]
    (each [k v (pairs t)]
      (tset r (f k) v))
    r))

(fn m.zero-base [t]
  (m.map-keys #(- $ 1) t))

(fn m.one-base [t]
  (m.map-keys #(+ $ 1) t))

(fn m.store [xs loc val]
  (tset xs loc val)
  xs)

(fn m.fetch [xs loc]
  (. xs (. xs loc)))

(fn m.parse-param-modes [code]
  (let [param-modes []]
    (for [i 2 4]
      (table.insert param-modes
                    (-> (/ code (math.pow 10 i)) math.floor (% 10))))
    param-modes))

(fn m.parse-opcode [code]
  (let [op (% code 100)]
    [op (m.parse-param-modes code)]))

(fn m.get-param [xs ip param-modes i]
  (let [pm (. param-modes i)]
    (match pm
      0 (m.fetch xs (+ i ip))
      1 (. xs (+ i ip)))))

(set m.ops {})

(fn m.ops.add [xs ip param-modes in out]
  (let [x1 (m.get-param xs ip param-modes 1)
        x2 (m.get-param xs ip param-modes 2)
        x3 (. xs (+ 3 ip))]
    [:ok (m.store xs x3 (+ x1 x2)) (+ 4 ip) in out]))

(fn m.ops.mul [xs ip param-modes in out]
  (let [x1 (m.get-param xs ip param-modes 1)
        x2 (m.get-param xs ip param-modes 2)
        x3 (. xs (+ 3 ip))]
    [:ok (m.store xs x3 (* x1 x2)) (+ 4 ip) in out]))

(fn m.ops.input [xs ip param-modes in out]
  (let [x (. xs (+ 1 ip))
        i (table.remove in 1)]
    [:ok (m.store xs x i) (+ 2 ip) in out]))

(fn m.ops.output [xs ip param-modes in out]
  (let [x1 (m.get-param xs ip param-modes 1)]
    (table.insert out x1)
    [:ok xs (+ 2 ip) in out]))

(fn m.ops.jump-true [xs ip param-modes in out]
  (let [x1 (m.get-param xs ip param-modes 1)
        x2 (m.get-param xs ip param-modes 2)]
    ;; true - aka non-zero
    [:ok xs (if (= x1 0) (+ ip 3) x2) in out]))

(fn m.ops.jump-false [xs ip param-modes in out]
  (let [x1 (m.get-param xs ip param-modes 1)
        x2 (m.get-param xs ip param-modes 2)]
    ;; false - aka zero
    [:ok xs (if (= x1 0) x2 (+ ip 3)) in out]))

(fn m.ops.less [xs ip param-modes in out]
  (let [x1 (m.get-param xs ip param-modes 1)
        x2 (m.get-param xs ip param-modes 2)
        x3 (. xs (+ 3 ip))]
    [:ok (m.store xs x3 (if (< x1 x2) 1 0)) (+ 4 ip) in out]))

(fn m.ops.equal [xs ip param-modes in out]
  (let [x1 (m.get-param xs ip param-modes 1)
        x2 (m.get-param xs ip param-modes 2)
        x3 (. xs (+ 3 ip))]
    [:ok (m.store xs x3 (if (= x1 x2) 1 0)) (+ 4 ip) in out]))

(fn m.step [xs ip in out]
  (let [opcode (. xs ip)
        [op param-modes] (m.parse-opcode opcode)]
    (match op
      1  (m.ops.add        xs ip param-modes in out)
      2  (m.ops.mul        xs ip param-modes in out)
      3  (m.ops.input      xs ip param-modes in out)
      4  (m.ops.output     xs ip param-modes in out)
      5  (m.ops.jump-true  xs ip param-modes in out)
      6  (m.ops.jump-false xs ip param-modes in out)
      7  (m.ops.less       xs ip param-modes in out)
      8  (m.ops.equal      xs ip param-modes in out)
      99 [:halt xs ip in out]
      _  (error (.. "Invalid op: " (tostring opcode) " at: " (tostring ip))))))

(fn m.run-loop [state]
  (let [[ok/halt xs ip in out] state]
    (match ok/halt
      :ok   (m.run-loop (m.step xs ip in out))
      :halt [xs out])))

(fn m.run [xs in]
  (let [[xs out] (m.run-loop [:ok (m.zero-base xs) 0 in []])]
    [(m.one-base xs) out]))

(local input
       [3 225 1 225 6 6 1100 1 238 225 104 0 1101 65 73 225 1101 37 7 225 1101 42 58 225 1102 62 44
        224 101 -2728 224 224 4 224 102 8 223 223 101 6 224 224 1 223 224 223 1 69 126 224 101 -92
        224 224 4 224 1002 223 8 223 101 7 224 224 1 223 224 223 1102 41 84 225 1001 22 92 224 101
        -150 224 224 4 224 102 8 223 223 101 3 224 224 1 224 223 223 1101 80 65 225 1101 32 13 224
        101 -45 224 224 4 224 102 8 223 223 101 1 224 224 1 224 223 223 1101 21 18 225 1102 5 51
        225 2 17 14 224 1001 224 -2701 224 4 224 1002 223 8 223 101 4 224 224 1 223 224 223 101 68
        95 224 101 -148 224 224 4 224 1002 223 8 223 101 1 224 224 1 223 224 223 1102 12 22 225 102
        58 173 224 1001 224 -696 224 4 224 1002 223 8 223 1001 224 6 224 1 223 224 223 1002 121 62
        224 1001 224 -1302 224 4 224 1002 223 8 223 101 4 224 224 1 223 224 223 4 223 99 0 0 0 677
        0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105
        1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105
        1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999
        1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 1008 226 677 224 102 2 223 223 1005 224 329
        1001 223 1 223 7 677 226 224 102 2 223 223 1006 224 344 1001 223 1 223 1007 226 677 224
        1002 223 2 223 1006 224 359 1001 223 1 223 1007 677 677 224 102 2 223 223 1005 224 374 1001
        223 1 223 108 677 677 224 102 2 223 223 1006 224 389 101 1 223 223 8 226 677 224 102 2 223
        223 1005 224 404 101 1 223 223 7 226 677 224 1002 223 2 223 1005 224 419 101 1 223 223 8
        677 226 224 1002 223 2 223 1005 224 434 101 1 223 223 107 677 677 224 1002 223 2 223 1006
        224 449 101 1 223 223 7 677 677 224 1002 223 2 223 1006 224 464 101 1 223 223 1107 226 226
        224 102 2 223 223 1006 224 479 1001 223 1 223 1007 226 226 224 102 2 223 223 1006 224 494
        101 1 223 223 108 226 677 224 1002 223 2 223 1006 224 509 101 1 223 223 1108 226 677 224
        102 2 223 223 1006 224 524 1001 223 1 223 1008 226 226 224 1002 223 2 223 1005 224 539 101
        1 223 223 107 226 226 224 102 2 223 223 1006 224 554 101 1 223 223 8 677 677 224 102 2 223
        223 1005 224 569 101 1 223 223 107 226 677 224 102 2 223 223 1005 224 584 101 1 223 223
        1108 226 226 224 1002 223 2 223 1005 224 599 1001 223 1 223 1008 677 677 224 1002 223 2 223
        1005 224 614 101 1 223 223 1107 226 677 224 102 2 223 223 1005 224 629 101 1 223 223 1108
        677 226 224 1002 223 2 223 1005 224 644 1001 223 1 223 1107 677 226 224 1002 223 2 223 1006
        224 659 1001 223 1 223 108 226 226 224 102 2 223 223 1006 224 674 101 1 223 223 4 223 99 226])

(print (.. "Day 5 / Part 1: "
           (-> (m.run input [1]) (. 2) m.last)))
(print (.. "Day 5 / Part 2: "
           (-> (m.run input [5]) (. 2) m.last)))

(comment

 (global fennel (require :fennel))
 (global fv (fennel.dofile "/home/matt/code/fennel/fennelview.fnl"))
 (global m (require :day5))

 (fn m.run-tests [tests]
   (print)
   (each [_ [xs in e out] (pairs tests)]
     (let [(ok? result) (pcall m.run xs in)
           pass? (and ok?
                      (= (fv result) (fv [e out])))]
       (print (if pass? (.. "✓ - " (fv [xs in]))
                  ok?   (.. "✗ - expected: " (fv [e out]) " actual: " (fv result))
                  :else (.. "✗ - " (fv [xs in]) " - " result))))))

 (m.run-tests
  [[[1 0 0 0 99]          []  [2 0 0 0 99]          []]
   [[2 3 0 3 99]          []  [2 3 0 6 99]          []]
   [[2 3 0 1 99]          []  [2 2 0 1 99]          []]
   [[2 4 4 5 99 0]        []  [2 4 4 5 99 9801]     []]
   [[1 1 1 4 99 5 6 0 99] []  [30 1 1 4 2 5 6 0 99] []]
   [[3 0 4 0 99]          [7] [7 0 4 0 99]          [7]]
   [[1101 3 7 0 99]       []  [10 3 7 0 99]         []]
   [[1002 4 3 4 33]       []  [1002 4 3 4 99]       []]
   [[1101 100 -1 4 0]     []  [1101 100 -1 4 99]    []]])

 (m.run-tests
  [[[3 9 8 9 10 9 4 9 99 -1 8] [8] [3 9 8 9 10 9 4 9 99 1 8] [1]]
   [[3 9 8 9 10 9 4 9 99 -1 8] [4] [3 9 8 9 10 9 4 9 99 0 8] [0]]
   [[3 9 7 9 10 9 4 9 99 -1 8] [4] [3 9 7 9 10 9 4 9 99 1 8] [1]]
   [[3 9 7 9 10 9 4 9 99 -1 8] [8] [3 9 7 9 10 9 4 9 99 0 8] [0]]
   [[3 3 1108 -1 8 3 4 3 99]   [8] [3 3 1108 1 8 3 4 3 99] [1]]
   [[3 3 1108 -1 8 3 4 3 99]   [4] [3 3 1108 0 8 3 4 3 99] [0]]
   [[3 3 1107 -1 8 3 4 3 99]   [4] [3 3 1107 1 8 3 4 3 99] [1]]
   [[3 3 1107 -1 8 3 4 3 99]   [8] [3 3 1107 0 8 3 4 3 99] [0]]
   [[3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [] [] []]
   [[3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [] [] []]])

 (let [program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
   (print)
   (each [_ i (pairs [4 8 16])]
     (print (.. (fv i) " " (fv (. (m.run program [i]) 2))))))

 )

m
