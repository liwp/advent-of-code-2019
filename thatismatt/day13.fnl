(local c (require :intcode))

(local m {})

(fn m.map [t f]
  (let [r []]
    (each [_ v (pairs t)]
      (table.insert r (f v)))
    r))

(fn m.vals [t]
  (let [r []]
    (each [_ v (pairs t)]
      (table.insert r v))
    r))

(fn m.sort [t]
  (let [r [(table.unpack t)]]
    (table.sort r)
    r))

(fn m.sort-by [t f]
  (let [r [(table.unpack t)]]
    (table.sort r (fn [a b] (< (f a) (f b))))
    r))

(fn m.hash-point [pt]
  (.. (tostring pt.x) "-" (tostring pt.y)))

(fn m.out->tileset [tileset out]
  (for [i 1 (length out) 3]
    (let [x (. out i)
          y (. out (+ i 1))
          tile (. out (+ i 2))
          pt {: x : y : tile}]
      (tset tileset (m.hash-point pt) pt)))
  tileset)

(local id->tile
       {;; 0 is an empty tile. No game object appears in this tile.
        0 " "
        ;; 1 is a wall tile. Walls are indestructible barriers.
        1 "#"
        ;; 2 is a block tile. Blocks can be broken by the ball.
        2 "@"
        ;; 3 is a horizontal paddle tile. The paddle is indestructible.
        3 "-"
        ;; 4 is a ball tile. The ball moves diagonally and bounces off objects.
        4 "0"})

(fn m.display [tileset]
  (print "Score: " (. tileset "-1-0" :tile))
  (for [y 0 22]
    (var row "")
    (for [x 0 41]
      (let [t (. tileset (m.hash-point {: x : y}))]
        (set row (.. row (. id->tile t.tile)))))
    (print row)))

(fn m.interactive []
  (match (io.read)
    "a" -1
    "d" 1
    _ 0))

(fn m.computer [tileset]
  (var ball nil)
  (var paddle nil)
  (each [_ t (pairs tileset)]
    (if (= t.tile 3)
        (set paddle t)
        (= t.tile 4)
        (set ball t)))
  (if (= paddle.x ball.x) 0
      (< paddle.x ball.x) 1
      (< ball.x paddle.x) -1))

(fn m.game-loop [joystick state tileset0]
  (let [[status xs ip rb in out] (c.run-loop state)
        tileset1 (m.out->tileset tileset0 out)]
    ;; (m.display tileset1)
    (if (= status :pause-input)
        (m.game-loop joystick
                     [:ok xs ip rb
                      [(joystick tileset1)]
                      []] tileset1)
        tileset1)))

(print (.. "Day 13 / Part 1: "
           (let [output (-> (c.read-program "input13.txt")
                            (c.run [])
                            (. 2))
                 t {}]
             (for [i 3 (length output) 3]
               (let [v (. output i)]
                 (tset t v (+ 1 (or (. t v) 0)))))
             (. t 2))))

(print (.. "Day 13 / Part 2: "
           (let [program (c.zero-base (c.read-program "input13.txt"))
                 _ (tset program 0 2)
                 tileset (m.game-loop m.computer [:ok program 0 0 [] []] {})]
             (. tileset "-1-0" :tile))))

(comment

 (global c (require :intcode))
 (global m (require :day13))

 (let [xs (-> tiles (m.map #(. $ :x)) (m.sort))
       ys (-> tiles (m.map #(. $ :y)) (m.sort))
       x-max (. xs (length xs))
       y-max (. ys (length ys))]
   [x-max y-max])

 (let [program (c.zero-base (c.read-program "input13.txt"))]
   (tset program 0 2)
   (m.game-loop
    ;; m.interactive
    m.computer
    [:ok program 0 0 [] []]
    {}))

 )

m
