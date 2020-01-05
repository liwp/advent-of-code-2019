(local m {})

(fn m.map [f t]
  (let [r []]
    (each [_ v (pairs t)]
      (table.insert r (f v)))
    r))

(fn m.filter [f t]
  (let [r []]
    (each [_ v (pairs t)]
      (when (f v)
        (table.insert r v)))
    r))

(fn m.group-by [f t]
  (let [r []]
    (each [_ v (pairs t)]
      (let [k (f v)]
        (when (not (. r k))
          (tset r k []))
        (table.insert (. r k) v)))
    r))

(fn m.keys [t]
  (let [r []]
    (each [k v (pairs t)]
      (table.insert r k))
    r))

(fn m.map-keys [f t]
  (let [r []]
    (each [k v (pairs t)]
      (tset r (f k) v))
    r))

(fn m.count [t]
  (var c 0)
  (each [_ (pairs t)]
    (set c (+ c 1)))
  c)

(fn m.max-kv [t]
  (var max-v 0)
  (var max-k 0)
  (each [k v (pairs t)]
    (when (> v max-v)
      (set max-v v)
      (set max-k k)))
  [max-k max-v])

(fn m.sort-by [f t]
  (let [r [(table.unpack t)]]
    (table.sort r (fn [a b] (< (f a) (f b))))
    r))

(fn m.map-kv [f t]
  (let [r []]
    (each [k v (pairs t)]
      (let [[l w] (f k v)]
        (tset r l w)))
    r))

(fn m.zero-base [t]
  (m.map-keys #(- $ 1) t))

;; (fn m.gcd [a b]
;;   (if (not= b 0)
;;       (m.gcd b (% a b))
;;       (math.abs a)))

;; (fn m.hash-direction [o a]
;;   (let [dx (- a.x o.x)
;;         dy (- a.y o.y)
;;         gcd (m.gcd dx dy)
;;         x (/ dx gcd)
;;         y (/ dy gcd)]
;;     (.. (tostring x) " " (tostring y))))

(fn m.cartesian->polar [{: x : y}]
  (let [yi (- y) ;; map y goes down
        a (math.atan (/ yi x))]
    {:r (math.sqrt (+ (* x x) (* y y)))
     :a (if (>= x 0)
            (- (/ math.pi 2) a)
            (- (* 1.5 math.pi) a))}))

(fn m.hash-direction [o a]
  (let [c {:x (- a.x o.x)
           :y (- a.y o.y)}
        p (m.cartesian->polar c)]
    p.a))

(fn m.hash-point [pt]
  (.. (tostring pt.x) "-" (tostring pt.y)))

(fn m.lines [s]
  (let [t []]
    (each [m (string.gmatch s "[^\n]+")]
      (table.insert t m))
    (m.zero-base t)))

(fn m.chars [s]
  (let [t []]
    (each [c (string.gmatch s ".")]
      (table.insert t c))
    (m.zero-base t)))

(fn m.parse-map [s]
  (let [t []]
    (each [y l (pairs (m.lines s))]
      (each [x c (pairs (m.chars l))]
        (when (= c "#")
          (table.insert t {: y : x}))))
    t))

(fn m.best-location [map]
  (let [asteroids (m.parse-map map)
        r {}]
    (each [_ o (pairs asteroids)]
      (let [t {}]
        (each [_ a (pairs asteroids)]
          (when (or (not= o.x a.x)
                    (not= o.y a.y))
            (tset t (m.hash-direction o a) true)))
        (tset r (m.hash-point o) t)))
    (->> r
         (m.map-kv (fn [k v] [k (m.count v)]))
         m.max-kv)))

(fn m.vaporize [map o]
  (let [a->asteroids
        (->> map
             m.parse-map
             (m.filter #(not= (m.hash-point $) (m.hash-point o)))
             (m.map (fn [a]
                      (let [c {:x (- a.x o.x)
                               :y (- a.y o.y)}
                            p (m.cartesian->polar c)]
                        (tset p :id (m.hash-point a))
                        p)))
             (m.group-by #(. $ :a))
             (m.map-kv (fn [k v] [k (m.sort-by #(. $ :r) v)])))
        ks (m.keys a->asteroids)
        r []]
    (table.sort ks)
    (each [i k (pairs ks)]
      (let [vs (. a->asteroids k)
            v (table.remove vs 1)]
        (tset r i v.id)))
    r))

(local input-eg1 "
.#..#
.....
#####
....#
...##")

(local input-eg2 "
#.#
.#.
#.#")

(local input-eg3 "
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(var input "")
(each [l (io.lines "input10.txt")] (set input (.. input "\n" l)))

(print (.. "Day 10 / Part 1: "
           (. (m.best-location input) 2)))

(print (.. "Day 10 / Part 2: "
           (. (m.vaporize input {:x 31 :y 20}) 200)))

(comment

 (global m (require :day10))

 (m.parse-map input-eg2)

 (m.best-location input-eg1)

 (m.best-location input-eg2)

 (m.best-location input-eg3)

 (m.vaporize input-eg3 {:x 11 :y 13})

 )

m
