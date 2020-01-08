(local c (require :intcode))

(local m {})

(fn m.count [t]
  (var c 0)
  (each [_ (pairs t)]
    (set c (+ c 1)))
  c)

(fn m.hash-point [pt]
  (.. (tostring pt.x) "-" (tostring pt.y)))

(fn m.unhash-point [s]
  (let [[x y] [(string.match s "(-?%d+)-(-?%d+)")]]
    {:x (tonumber x)
     :y (tonumber y)}))

(fn m.init-robot []
  {:direction :N ;; :N :E :S :W
   :position {:x 0 :y 0}
   :panels []})

(fn m.panel [r pt]
  (or (. r.panels (m.hash-point pt)) 0))

(fn m.paint [r c]
  (tset r.panels (m.hash-point r.position) c)
  r)

(local L 0)
(local R 1)

(fn m.move [r d]
  (if (or (and (= d L) (= r.direction :N))
          (and (= d R) (= r.direction :S)))
      (do (tset r :direction :W)
          (tset r.position :x (- r.position.x 1)))
      (or (and (= d L) (= r.direction :S))
          (and (= d R) (= r.direction :N)))
      (do (tset r :direction :E)
          (tset r.position :x (+ r.position.x 1)))
      (or (and (= d L) (= r.direction :W))
          (and (= d R) (= r.direction :E)))
      (do (tset r :direction :S)
          (tset r.position :y (- r.position.y 1)))
      (or (and (= d L) (= r.direction :E))
          (and (= d R) (= r.direction :W)))
      (do (tset r :direction :N)
          (tset r.position :y (+ r.position.y 1)))
      (error (.. "Unknown direction combination: " (tostring d) " " (tostring r.direction))))
  r)

(fn m.panels-range [r]
  (var x-min 0)
  (var x-max 0)
  (var y-min 0)
  (var y-max 0)
  (each [k _ (pairs r.panels)]
    (let [{: x : y} (m.unhash-point k)]
      (set x-max (math.max x x-max))
      (set x-min (math.min x x-min))
      (set y-max (math.max y y-max))
      (set y-min (math.min y y-min))))
  {: x-min : x-max : y-min : y-max})

(fn m.draw [r]
  (let [{: x-min : x-max : y-min : y-max} (m.panels-range r)]
    (for [y y-max y-min -1]
      (var row "")
      (for [x x-min x-max]
        (set row (.. row (match (m.panel r {: x : y})
                           1 "#"
                           0 " "))))
      (print row)))
  r)

(fn m.run-robot [robot xs ip rb]
  (let [[status xs ip rb in [colour direction]] (c.run-loop [:ok xs ip rb [(m.panel robot robot.position)] []])]
    (m.paint robot colour)
    (m.move robot direction)
    (if (= status :halt)
        robot
        (m.run-robot robot xs ip rb))))

(fn m.make-input []
  (c.zero-base [3 8 1005 8 326 1106 0 11 0 0 0 104 1 104 0 3 8 102 -1 8 10 101 1 10 10 4 10 1008 8 1 10 4 10 1001 8 0 29 2 1003 17 10 1006 0 22 2 106 5 10 1006 0 87 3 8 102 -1 8 10 101 1 10 10 4 10 1008 8 1 10 4 10 1001 8 0 65 2 7 20 10 2 9 17 10 2 6 16 10 3 8 102 -1 8 10 1001 10 1 10 4 10 1008 8 0 10 4 10 101 0 8 99 1006 0 69 1006 0 40 3 8 102 -1 8 10 1001 10 1 10 4 10 1008 8 1 10 4 10 101 0 8 127 1006 0 51 2 102 17 10 3 8 1002 8 -1 10 1001 10 1 10 4 10 108 1 8 10 4 10 1002 8 1 155 1006 0 42 3 8 1002 8 -1 10 101 1 10 10 4 10 108 0 8 10 4 10 101 0 8 180 1 106 4 10 2 1103 0 10 1006 0 14 3 8 102 -1 8 10 1001 10 1 10 4 10 108 0 8 10 4 10 1001 8 0 213 1 1009 0 10 3 8 1002 8 -1 10 1001 10 1 10 4 10 108 0 8 10 4 10 1002 8 1 239 1006 0 5 2 108 5 10 2 1104 7 10 3 8 102 -1 8 10 101 1 10 10 4 10 108 0 8 10 4 10 102 1 8 272 2 1104 12 10 1 1109 10 10 3 8 102 -1 8 10 1001 10 1 10 4 10 108 1 8 10 4 10 102 1 8 302 1006 0 35 101 1 9 9 1007 9 1095 10 1005 10 15 99 109 648 104 0 104 1 21102 937268449940 1 1 21102 1 343 0 1105 1 447 21101 387365315480 0 1 21102 1 354 0 1105 1 447 3 10 104 0 104 1 3 10 104 0 104 0 3 10 104 0 104 1 3 10 104 0 104 1 3 10 104 0 104 0 3 10 104 0 104 1 21101 0 29220891795 1 21102 1 401 0 1106 0 447 21101 0 248075283623 1 21102 412 1 0 1105 1 447 3 10 104 0 104 0 3 10 104 0 104 0 21101 0 984353760012 1 21102 1 435 0 1105 1 447 21102 1 718078227200 1 21102 1 446 0 1105 1 447 99 109 2 21202 -1 1 1 21102 40 1 2 21101 0 478 3 21101 468 0 0 1106 0 511 109 -2 2106 0 0 0 1 0 0 1 109 2 3 10 204 -1 1001 473 474 489 4 0 1001 473 1 473 108 4 473 10 1006 10 505 1102 1 0 473 109 -2 2105 1 0 0 109 4 1202 -1 1 510 1207 -3 0 10 1006 10 528 21102 1 0 -3 22102 1 -3 1 22101 0 -2 2 21101 0 1 3 21102 1 547 0 1105 1 552 109 -4 2105 1 0 109 5 1207 -3 1 10 1006 10 575 2207 -4 -2 10 1006 10 575 21202 -4 1 -4 1105 1 643 21202 -4 1 1 21201 -3 -1 2 21202 -2 2 3 21102 1 594 0 1106 0 552 22102 1 1 -4 21101 1 0 -1 2207 -4 -2 10 1006 10 613 21101 0 0 -1 22202 -2 -1 -2 2107 0 -3 10 1006 10 635 22101 0 -1 1 21101 0 635 0 106 0 510 21202 -2 -1 -2 22201 -4 -2 -4 109 -5 2105 1 0]))

(print (.. "Day 11 / Part 1: "
           (-> (m.init-robot)
               (m.run-robot (m.make-input) 0 0)
               (. :panels)
               (m.count))))

(print "Day 11 / Part 2: ")
(-> (m.init-robot)
    (m.paint 1)
    (m.run-robot (m.make-input) 0 0)
    (m.draw))

(comment

 (global m (require :day11))

 (global c (require :intcode))

 (-> (m.init-robot)
     (m.move L) (m.paint 1) (m.draw)
     (m.move L) (m.paint 1) (m.draw)
     (m.move L) (m.paint 1) (m.draw)
     (m.move L) (m.paint 1) (m.draw)
     (m.move R) (m.paint 1) (m.draw)
     (m.move R) (m.paint 1) (m.draw)
     (m.move R) (m.paint 1) (m.draw)
     (m.move R) (m.paint 1) (m.draw))

 (do
   (var s :x)
   (let [[s] [:y]]
     (print s))
   (print s))

 )

m
