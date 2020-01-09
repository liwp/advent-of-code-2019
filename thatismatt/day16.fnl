(local m {})

(tset m :input "59791911701697178620772166487621926539855976237879300869872931303532122404711706813176657053802481833015214226705058704017099411284046473395211022546662450403964137283487707691563442026697656820695854453826690487611172860358286255850668069507687936410599520475680695180527327076479119764897119494161366645257480353063266653306023935874821274026377407051958316291995144593624792755553923648392169597897222058613725620920233283869036501950753970029182181770358827133737490530431859833065926816798051237510954742209939957376506364926219879150524606056996572743773912030397695613203835011524677640044237824961662635530619875905369208905866913334027160178")

(fn m.map [t f]
  (let [r []]
    (each [_ v (pairs t)]
      (table.insert r (f v)))
    r))

(fn m.chars [s]
  (let [t []]
    (each [c (string.gmatch s ".")]
      (table.insert t c))
    t))

(tset m :base-pattern [0 1 0 -1])

(fn m.generate-multipliers- [position count]
  (let [r []]
    (for [i 1 count]
      (table.insert r (. m.base-pattern (+ (% (// i position) 4) 1))))
    r))

(tset m :multipliers-cache {})

(fn m.generate-multipliers [position count]
  (let [k (.. position "," count)]
    (or (. m.multipliers-cache k)
        (let [ms (m.generate-multipliers- position count)]
          (tset m.multipliers-cache k ms)
          ms))))

(fn m.phase [digits]
  (let [result []
        n (length digits)]
    (for [i 1 n]
      (let [multipliers (m.generate-multipliers i n)]
        (tset result i
              (do (var r 0)
                  (for [j 1 n]
                    (set r (+ r (* (. digits j) (. multipliers j)))))
                  (% (math.abs r) 10)))))
    result))

(fn m.phase-loop [digits phase-fn n]
  (if (= n 0)
      digits
      (m.phase-loop (phase-fn digits) phase-fn (- n 1))))

(fn m.phases [s phase-fn n]
  (-> s m.chars (m.map tonumber) (m.phase-loop phase-fn n)))

(fn m.repeat [s n]
  (var r "")
  (for [i 1 n]
    (set r (.. r s)))
  r)

(fn m.phase-2 [digits]
  (let [n (length digits)
        result {n (. digits n)}]
    (for [i (- n 1) 1 -1]
      (tset result i
            (% (+ (. result (+ i 1)) (. digits i)) 10)))
    result))

(fn m.decode [s n r]
  (let [offset (tonumber (string.sub s 1 7))
        result (m.phases (m.repeat s r) m.phase-2 n)
        message []]
    (for [i 1 8]
      (tset message i (. result (+ offset i))))
    (table.concat message)))

(print (.. "Day 16 / Part 1: "
           (let [result (m.phases m.input m.phase 100)
                 message []]
             (for [i 1 8]
               (tset message i (. result i)))
             (table.concat message))))

(print (.. "Day 16 / Part 2: "
           (m.decode m.input 100 10000)))

(comment

 (global m (require :day16))

 (m.phases "12345678" m.phase 4) ;; [ 0 1 0 2 9 4 9 8 ]
 (m.phases "80871224585914546619083218645595" m.phase 100) ;; becomes 24176176.
 (m.phases "19617804207202209144916044189917" m.phase 100) ;; becomes 73745418.
 (m.phases "69317163492948606335995924319873" m.phase 100) ;; becomes 52432133.

 (macro time [...]
   `(let [start# (os.clock)
          result# ,...
          end# (os.clock)]
      (print (- end# start#))
      result#))

 (m.decode "03036732577212944063491565474664" 100 15)

 (time
  (m.decode "03036732577212944063491565474664" 100 10000))

 (time
  (m.decode m.input 100 10000))
 37615297

 (tonumber (string.sub m.input 1 7))

 (let [digits (time (m.chars (m.repeat m.input 10000)))]
   (time (length digits)))

 (let [digits (time (m.chars (m.repeat m.input 10000)))]
   (time (for [i 1 10000]
           (length digits)
           (for [j 1 100]
             (tset {} 1 (% (+ 0 0) 10))))))

 (fn f [i]
   (if (= 0 i)
       i
       (f (- i 1))))

 (time (f 10000000))

 )

m
