(local m {})

(local c (require :intcode))

(fn m.binary-chop [f lower upper target]
  (let [mid (math.ceil (/ (+ lower upper) 2))
        result (f mid)]
    (if (> result target)
        (m.binary-chop f lower mid target)
        (< result target)
        (m.binary-chop f mid upper target)
        :else
        mid)))

(fn m.bounds [row]
  (let [f (fn [x y] (. (c.run (c.read-program "input19.txt") [x y]) 2 1))
        g (fn [x y] (+ (f x y) (f (- x 1) y)))
        middle (math.ceil (* (/ 84 100) row))
        _ (assert (= 0 (f 0 row)))
        _ (assert (= 1 (f middle row)))
        _ (assert (= 0 (f (* middle 2) row)))
        left  (m.binary-chop #(g $ row)
                             0
                             middle
                             1)
        right (m.binary-chop #(- (g $ row))
                             middle
                             (* middle 2)
                             -1)]
    [left right]))

(fn m.max-width [top]
  (let [bottom (+ top 99)
        [tl tr] (m.bounds top)
        [bl br] (m.bounds bottom)]
    (- tr bl)))

(fn m.nearest-square [lower upper target]
  (let [mid (math.ceil (/ (+ lower upper) 2))
        result (m.max-width mid)]
    (if (and (= result target)
             (< (m.max-width (- mid 1)) target))
        mid
        (< result target)
        (m.nearest-square mid upper target)
        :else
        (m.nearest-square lower mid target))))

(print (.. "Day 19 / Part 1: "
           (do (var total 0)
               (for [x 0 49]
                 (for [y 0 49]
                   (let [[_ [out]] (c.run (c.read-program "input19.txt") [x y])]
                     (when (= out 1)
                       (set total (+ total 1))))))
               total)))

(print "Day 19 / Part 2: ")
(let [initial-guess (m.nearest-square 1000 10000 100)]
  (var first true)
  (for [row (- initial-guess 10) initial-guess]
    (let [left (- (. (m.bounds row) 2) 100)
          width (m.max-width row)]
      (print left row width
             (when (and first (= width 100))
               (set first false)
               "<---")))))

(comment

 (global m (require :day19))

 (global c (require :intcode))

 (m.binary-chop (fn [x] (* x x)) 0 10 16)

 ;;  012345678
 ;; 0.xx......
 ;; 1..xxx....
 ;; 2...xxxx..
 ;; 3....xxxxx

 )

m
