(local m {})

(fn m.file-lines [filename]
  (let [r []]
    (each [l (io.lines filename)]
      (table.insert r l))
    r))

(fn m.count [t]
  (var c 0)
  (each [_ (pairs t)]
    (set c (+ c 1)))
  c)

(fn m.map [t f]
  (let [r []]
    (each [_ v (pairs t)]
      (table.insert r (f v)))
    r))

(fn m.key-by [t f]
  (let [r {}]
    (each [_ v (pairs t)]
      (tset r (f v) v))
    r))

(fn m.parse-chemicals [s]
  (let [t []]
    (each [q c (string.gmatch s "([0-9]+) ([A-Z]+)")]
      (table.insert t {:chemical c :quantity (tonumber q)}))
    t))

(fn m.parse-reaction [line]
  (let [(in out) (string.match line "([^=]*)=>(.+)")]
    {:in (m.parse-chemicals in)
     :out (. (m.parse-chemicals out) 1)}))

(fn m.parse-reactions [lines]
  (m.key-by (m.map lines m.parse-reaction)
            #$.out.chemical))

;; slow!
(fn m.react [reactants products reactions ore]
  (let [(product product-quantity) (next products)
        reactant-quantity (. reactants product)
        reaction (. reactions product)]
    (if (not product)
        [product reactants products ore]
        reactant-quantity
        (do (tset reactants product nil)
            (tset products product nil)
            (if (> reactant-quantity product-quantity)
                (tset reactants product (- reactant-quantity product-quantity))
                (< reactant-quantity product-quantity)
                (tset products product (- product-quantity reactant-quantity)))
            (m.react reactants products reactions ore))
        reaction
        (do (tset products product nil)
            (each [_ {: chemical : quantity} (pairs reaction.in)]
              (tset products chemical (+ (or (. products chemical) 0) quantity)))
            (if (> reaction.out.quantity product-quantity)
                (tset reactants product (- reaction.out.quantity product-quantity))
                (< reaction.out.quantity product-quantity)
                (tset products product (- product-quantity reaction.out.quantity)))
            (m.react reactants products reactions ore))
        (= product "ORE")
        (do (tset products product nil)
            (m.react reactants products reactions (+ ore product-quantity)))
        :else [product reactants products ore])))

(fn m.react [reactants products reactions ore]
  (let [(product product-quantity) (next products)
        reaction (. reactions product)]
    (if (not product)
        [product reactants products ore]
        reaction
        (let [available-quantity (or (. reactants product) 0)
              required-quantity  (- product-quantity available-quantity)]
          (tset products product nil)
          (if (= required-quantity 0)
              (do (tset reactants product nil)
                  (m.react reactants products reactions ore))
              (< required-quantity 0)
              (do (tset reactants product (- available-quantity product-quantity))
                  (m.react reactants products reactions ore))
              (> required-quantity 0)
              (let [per-reaction-quantity reaction.out.quantity
                    reaction-multiple     (math.ceil (/ required-quantity per-reaction-quantity))
                    reaction-quantity     (* per-reaction-quantity reaction-multiple)]
                (each [_ {: chemical : quantity} (pairs reaction.in)]
                  (tset products chemical (+ (or (. products chemical) 0) (* quantity reaction-multiple))))
                (tset reactants product (- reaction-quantity required-quantity))
                (m.react reactants products reactions ore))))
        (= product "ORE")
        (do (tset products product nil)
            (m.react reactants products reactions (+ ore product-quantity)))
        :else [product reactants products ore])))

(fn m.binary-chop [f lower guess upper target]
  (let [result (f guess)]
    (if (or (= lower guess) (= upper guess))
        guess
        (> result target)
        (m.binary-chop f lower (math.floor (/ (+ lower guess) 2)) guess target)
        (< result target)
        (m.binary-chop f guess (math.floor (/ (+ upper guess) 2)) upper target))))

(print (.. "Day 14 / Part 1: "
           (. (m.react {}
                       {"FUEL" 1}
                       (-> "input14.txt" m.file-lines m.parse-reactions)
                       0) 4)))

(print (.. "Day 14 / Part 2: "
           (m.binary-chop #(. (m.react {}
                                       {"FUEL" $}
                                       (-> "input14.txt" m.file-lines m.parse-reactions)
                                       0) 4)
                          1e6 1e8 1e10 1e12)))

(comment

 (global m (require :day14))

 (m.react {}
          {"FUEL" 1}
          (m.parse-reactions ["157 ORE => 5 NZVS"
                              "165 ORE => 6 DCFZ"
                              "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
                              "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
                              "179 ORE => 7 PSHF"
                              "177 ORE => 5 HKGWZ"
                              "7 DCFZ, 7 PSHF => 2 XJWVT"
                              "165 ORE => 2 GPVTF"
                              "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"])
          0)
 ;; 13312

 (m.react {}
          {"FUEL" 1}
          (m.parse-reactions ["9 ORE => 2 A"
                              "8 ORE => 3 B"
                              "7 ORE => 5 C"
                              "3 A, 4 B => 1 AB"
                              "5 B, 7 C => 1 BC"
                              "4 C, 1 A => 1 CA"
                              "2 AB, 3 BC, 4 CA => 1 FUEL"])
          0)
 ;; 165

 (m.react {}
          {"FUEL" 1}
          (m.parse-reactions ["2 ORE => 1 A"
                              "2 A => 1 FUEL"])
          0)
 ;; 4

 (m.react {}
          {"FUEL" 1}
          (m.parse-reactions ["1 ORE => 2 A"
                              "2 A => 1 FUEL"])
          0)
 ;; 1

 (m.react {}
          {"FUEL" 1}
          (m.parse-reactions ["1 ORE => 2 A"
                              "3 A => 1 FUEL"])
          0)
 ;; 2

 (m.react {}
          {"FUEL" 1}
          (m.parse-reactions ["1 ORE => 2 A"
                              "3 A => 1 B"
                              "1 A => 1 C"
                              "1 B, 1 C => 1 FUEL"])
          0)
 ;; 2

 (m.react {}
          {"FUEL" 1}
          (m.parse-reactions ["10 ORE => 10 A"
                              "1 ORE => 1 B"
                              "7 A, 1 B => 1 C"
                              "7 A, 1 C => 1 D"
                              "7 A, 1 D => 1 E"
                              "7 A, 1 E => 1 FUEL"])
          0)
 ;; 31

 (let [t (-> "input14.txt" m.file-lines m.parse-reactions
             (m.map #$.out.chemical))]
   (table.sort t)
   t)

 (-> "input14.txt" m.file-lines m.parse-reactions m.count)

 (macro time [...]
   `(let [start# (os.clock)
          result# ,...
          end# (os.clock)]
      (print (- end# start#))
      result#))

 (time
  (. (m.react {}
              {"FUEL" 1000}
              (-> "input14.txt" m.file-lines m.parse-reactions)
              0) 4))
 ;; 1.412089s
 ;; 0.001918s

 )

m
