(local m {})

(local c (require :intcode))

(fn m.split-on [t o]
  (let [r []]
    (var row [])
    (each [_ v (pairs t)]
      (if (= v o)
          (do (table.insert r row)
              (set row []))
          (table.insert row v)))
    r))

(fn m.string->bytes [s]
  (let [r []]
    (each [c (string.gmatch s ".")]
      (table.insert r (string.byte c)))
    r))

(fn m.intersections [data]
  (let [w (length (. data 1))
        h (length data)
        t []]
    (for [x 2 (- w 1)]
      (for [y 2 (- h 1)]
        (if (and (= (. data y x) 35)
                 (= (. data (+ y 1) x) 35)
                 (= (. data (- y 1) x) 35)
                 (= (. data y (+ x 1)) 35)
                 (= (. data y (- x 1)) 35))
            (table.insert t [(- x 1) (- y 1)])))) ;; zero base
    t))

(fn m.alignment-parameters [intersections]
  (var sum 0)
  (each [_ [x y] (pairs intersections)]
    (set sum (+ sum (* x y))))
  sum)

(fn m.find-robot [data]
  (let [h (length data)
        w (length (. data 1))]
    (var robot nil)
    (for [x 1 w]
      (for [y 1 h]
        (when (. {60 true 62 true 94 true 118 true}
                 ;; [ [ 60 "<" ] [ 62 ">" ] [ 94 "^" ] [ 118 "v" ] ]
                 (. data y x))
          (set robot {: x : y :direction (string.char (. data y x))}))))
    robot))

(fn m.turn [from to]
  (. {"<" {"^" :R
           "v" :L}
      ">" {"^" :L
           "v" :R}
      "^" {"<" :L
           ">" :R}
      "v" {"<" :R
           ">" :L}}
     from to))

(fn m.infront [{: x : y : direction}]
  (if (= direction "<") {:x (- x 1) :y y}
      (= direction ">") {:x (+ x 1) :y y}
      (= direction "^") {:x x :y (- y 1)}
      (= direction "v") {:x x :y (+ y 1)}))

(fn m.forward? [data robot]
  (let [{: x : y} (m.infront robot)]
    (-?> data (. y) (. x) (= 35)))) ;;  (string.byte "#")

(fn m.scaffold-direction [data {: x : y : direction}]
  (if (and (not (= direction "v")) (-?> data (. (- y 1)) (. x) (= (string.byte "#")))) "^"
      (and (not (= direction "^")) (-?> data (. (+ y 1)) (. x) (= (string.byte "#")))) "v"
      (and (not (= direction ">")) (-?> data (. y) (. (- x 1)) (= (string.byte "#")))) "<"
      (and (not (= direction "<")) (-?> data (. y) (. (+ x 1)) (= (string.byte "#")))) ">"))

(fn m.end? [data {: x : y : direction}]
  (var options 0)
  (each [_ n (pairs [(when (not (= direction "v")) {:x (. x) :y (. (- y 1))})
                     (when (not (= direction "^")) {:x (. x) :y (. (+ y 1))})
                     (when (not (= direction ">")) {:x (. (- x 1)) :y (. y)})
                     (when (not (= direction "<")) {:x (. (+ x 1)) :y (. y)})])]
    (when (-?> data (. n.y) (. n.x) (= (string.byte "#")))
      (set options (+ options 1))))
  (= options 0))

(fn m.move [{: x : y : direction}]
  (if (= direction "v") {:x x :y (. (+ y 1)) : direction}
      (= direction "^") {:x x :y (. (- y 1)) : direction}
      (= direction ">") {:x (. (+ x 1)) :y y : direction}
      (= direction "<") {:x (. (- x 1)) :y y : direction}))

(fn m.path [data robot path]
  (if (m.end? data robot)
      path
      (m.forward? data robot)
      (let [robot (m.move robot)
            last-step (. path (length path))]
        (if (= (type last-step) "number")
            (tset path (length path) (+ last-step 1))
            (table.insert path 1))
        (m.path data robot path))
      :else
      (let [new-direction (m.scaffold-direction data robot)
            turn (m.turn robot.direction new-direction)]
        (table.insert path turn)
        (tset robot :direction new-direction)
        (m.path data robot path))))

(print (.. "Day 17 / Part 1: "
           (let [[xs out] (c.run (c.read-program "input17.txt") [])]
             (-> out
                 (m.split-on 10)
                 m.intersections
                 m.alignment-parameters))))

(print (.. "Day 17 / Part 2: "
           (let [routine (table.concat [:A :B :A :C :B :A :B :C :C :B] ",")
                 fn-a (table.concat [:L "12" :L "12" :R "4"] ",")
                 fn-b (table.concat [:R "10" :R "6" :R "4" :R "4"] ",")
                 fn-c (table.concat [:R "6" :L "12" :L "12"] ",")
                 in (m.string->bytes (table.concat [routine fn-a fn-b fn-c "n" ""] "\n"))
                 xs (c.zero-base (c.read-program "input17.txt"))
                 _ (tset xs 0 2)
                 [status xs ip rb in out] (c.run-loop [:ok xs 0 0 in []])]
             (. out (length out)))))

(comment

 (global m (require :day17))

 (global c (require :intcode))

 (let [[xs out] (c.run (c.read-program "input17.txt") [])
       data (-> out (m.split-on 10))
       robot (m.find-robot data)]
   (tset data robot.y robot.x (string.byte "#"))
   (m.path data robot []))
 ;; [ L 12 L 12 R 4 R 10 R 6 R 4 R 4 L 12 L 12 R 4 R 6 L 12 L 12 R 10 R 6 R 4 R 4 L 12 L 12 R 4 R 10 R 6 R 4 R 4 R 6 L 12 L 12 R 6 L 12 L 12 R 10 R 6 R 4 R 4 ]

 L 12 L 12 R 4    ; A
 R 10 R 6 R 4 R 4 ; B
 R 6 L 12 L 12    ; C

 [
  L 12 L 12 R 4    ; A
  R 10 R 6 R 4 R 4 ; B
  L 12 L 12 R 4    ; A
  R 6 L 12 L 12    ; C
  R 10 R 6 R 4 R 4 ; B
  L 12 L 12 R 4    ; A
  R 10 R 6 R 4 R 4 ; B
  R 6 L 12 L 12    ; C
  R 6 L 12 L 12    ; C
  R 10 R 6 R 4 R 4 ; B
  ]

 )

m
