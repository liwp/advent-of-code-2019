(local m {})

(fn m.update [t k f]
  (tset t k (f (. t k))))

(fn m.inc [v]
  (+ v 1))

(fn m.dec [v]
  (- v 1))

(fn m.gravity [planets]
  (each [i v (pairs planets)]
    (let [{:pos v-pos :vel v-vel} v]
      (each [j w (pairs planets)]
        (let [{:pos w-pos :vel w-vel} w]
          (when (not= i j)
            (m.update v-vel :x (if (< v-pos.x w-pos.x) m.inc
                                   (= v-pos.x w-pos.x) #$
                                   (> v-pos.x w-pos.x) m.dec))
            (m.update v-vel :y (if (< v-pos.y w-pos.y) m.inc
                                   (= v-pos.y w-pos.y) #$
                                   (> v-pos.y w-pos.y) m.dec))
            (m.update v-vel :z (if (< v-pos.z w-pos.z) m.inc
                                   (= v-pos.z w-pos.z) #$
                                   (> v-pos.z w-pos.z) m.dec))))))))

(fn m.velocity [planets]
  (each [i v (pairs planets)]
    (let [{: pos : vel} v]
      (m.update pos :x #(+ $ vel.x))
      (m.update pos :y #(+ $ vel.y))
      (m.update pos :z #(+ $ vel.z)))))

(fn m.positions->planets [poss]
  (let [t []]
    (each [_ pos (pairs poss)]
      (table.insert t {:pos pos :vel {:x 0 :y 0 :z 0}}))
    t))

(fn m.energy [{: pos : vel}]
  (* (+ (math.abs pos.x) (math.abs pos.y) (math.abs pos.z))
     (+ (math.abs vel.x) (math.abs vel.y) (math.abs vel.z))))

(fn m.planets= [dim ps1 ps2]
  (var eq? true)
  (for [i 1 4]
    (let [p1 (. ps1 i)
          p2 (. ps2 i)]
      (set eq? (and eq?
                    (= (. p1.vel dim) (. p2.vel dim))
                    (= (. p1.pos dim) (. p2.pos dim))))))
  eq?)

(fn m.factors
  [n k acc]
  (if (= 1 n)
      acc
      (= 0 (% n k))
      (do (table.insert acc k)
          (m.factors (// n k) k acc))
      (m.factors n (m.inc k) acc)))

(fn m.count [t]
  (let [r {}]
    (each [_ v (pairs t)]
      (tset r v (m.inc (or (. r v) 0))))
    r))

(fn m.period [dim planets0 planets i]
  (m.gravity planets)
  (m.velocity planets)
  (if (m.planets= dim planets0 planets)
      i
      (m.period dim planets0 planets (m.inc i))))

(fn m.periods [make-positions]
  (let [periods []
        common-periods []]
    (each [_ dim (pairs [:x :y :z])]
      (let [period (m.period dim
                             (m.positions->planets (make-positions))
                             (m.positions->planets (make-positions))
                             1)]
        (table.insert periods (m.count (m.factors period 2 [])))))
    (each [_ ps (pairs periods)]
      (each [f n (pairs ps)]
        (tset common-periods f (math.max (or (. common-periods f) 0) n))))
    (var p 1)
    (each [f n (pairs common-periods)]
      (set p (* p (math.pow f n))))
    (math.floor p)))

(print (.. "Day 12 / Part 1: "
           (let [planets (m.positions->planets
                          [{:x -7 :y 17 :z -11}
                           {:x 9 :y 12 :z 5}
                           {:x -9 :y 0 :z -4}
                           {:x 4 :y 6 :z 0}])]
             (for [i 1 1000]
               (m.gravity planets)
               (m.velocity planets))
             (var energy 0)
             (each [_ p (pairs planets)]
               (set energy (+ energy (m.energy p))))
             energy)))

(print (.. "Day 12 / Part 2: "
           (m.periods #[{:x -7 :y 17 :z -11}
                        {:x 9 :y 12 :z 5}
                        {:x -9 :y 0 :z -4}
                        {:x 4 :y 6 :z 0}]))
)

m
