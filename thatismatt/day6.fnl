(local m {})

(fn m.map-keys [f t]
  (let [r []]
    (each [k _ (pairs t)]
      (table.insert r (f k)))
    r))

(fn m.reduce [f i t]
  (var r i)
  (each [_ v (pairs t)]
    (set r (f r v)))
  r)

(fn m.set [t]
  (let [r []]
    (each [_ v (pairs t)]
      (tset r v true))
    r))

(fn m.parse-orbit [s]
  [(string.match s "^(.*)%)(.*)$")])

(fn m.parse-orbits-file [filename]
  (let [orbits {}
        objects {}]
    (each [l (io.lines filename)]
      (let [[a b] (m.parse-orbit l)]
        (tset orbits b a)
        (tset objects a true)
        (tset objects b true)))
    {:orbits orbits
     :objects objects}))

(fn m.count-orbits [object orbits count]
  (let [focus (. orbits object)]
    (if focus
        (m.count-orbits focus orbits (+ 1 count))
        count)))

(fn m.list-orbits [object orbits t]
  (let [focus (. orbits object)]
    (if focus
        (do (table.insert t focus)
            (m.list-orbits focus orbits t))
        t)))

(fn m.total-orbits [filename]
  (let [{: objects : orbits} (m.parse-orbits-file filename)]
    (->> objects
         (m.map-keys #(m.count-orbits $ orbits 0))
         (m.reduce #(+ $1 $2) 0))))

(print (.. "Day 6 / Part 1: "
           (m.total-orbits "input6.txt")))

(print (.. "Day 6 / Part 2: "
           (let [result (m.parse-orbits-file "input6.txt")
                 san-orbits (m.list-orbits "SAN" (. result :orbits) [])
                 you-orbits (m.list-orbits "YOU" (. result :orbits) [])
                 san-set (m.set san-orbits)
                 you-set (m.set you-orbits)]
             (+ (m.reduce (fn [a o] (if (not (. you-set o)) (+ a 1) a))
                          0 san-orbits)
                (m.reduce (fn [a o] (if (not (. san-set o)) (+ a 1) a))
                          0 you-orbits)))))

m
