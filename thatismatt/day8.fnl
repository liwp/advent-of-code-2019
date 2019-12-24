(local m {})

(fn m.map [t f]
  (let [r []]
    (each [_ v (pairs t)]
      (table.insert r (f v)))
    r))

(fn m.each [t f]
  (each [_ v (pairs t)]
    (f v)))

(fn m.sort [t f]
  (let [r [(table.unpack t)]]
    (table.sort r f)
    r))

(fn m.partition [t n]
  (let [r []]
    (while (next t)
      (let [p []]
        (for [_ 1 n]
          (table.insert p (table.remove t 1)))
        (table.insert r p)))
    r))

(fn m.string->digits [s]
  (let [t []]
    (each [c (: s :gmatch ".")]
      (table.insert t (tonumber c)))
    t))

(fn m.group [t]
  (let [r {}]
    (each [_ v (pairs t)]
      (tset r v (+ (or (. r v) 0) 1)))
    r))

(fn m.digits->layers [digits n]
  (let [r []]
    (for [l 1 (length digits) n]
      (let [layer []]
        (for [i 1 n]
          (table.insert layer (. digits (+ (- l 1) i))))
        (table.insert r layer)))
    r))

(fn m.read-layers [filename n]
  (-> (m.string->digits ((io.lines filename)))
      (m.digits->layers n)))

(fn m.merge-layers [layers]
  (let [r (table.remove layers 1)]
    (each [_ l (pairs layers)]
      (each [i d (pairs l)]
        (when (= (. r i) 2)
          (tset r i d))))
    r))

(print (.. "Day 8 / Part 1: "
           (let [layers (m.read-layers "input8.txt" (* 25 6))
                 layer (-> layers
                           (m.map m.group)
                           (m.sort (fn [a b] (< (. a 0) (. b 0))))
                           (. 1))]
             (* (. layer 1) (. layer 2)))))

(print "Day 8 / Part 2:")
(-> (m.read-layers "input8.txt" (* 25 6))
    (m.merge-layers)
    (m.map #(-> $ (= 1) (if "#" " ")))
    (m.partition 25)
    (m.map #(table.concat $ ""))
    (m.each #(print $)))

m
