(ns day14)

(defn parse-chemicals [s]
  (->> (re-seq #"([0-9]+) ([A-Z]+)" s)
       (map (fn [[_ q c]] [c (Integer/parseInt q)]))))

(defn parse-reaction [line]
  (let [[_ in-str out-str] (re-matches #"([^=]*)=>(.+)" line)
        in (parse-chemicals in-str)
        out (first (parse-chemicals out-str))]
    [(first out) {:quantity (second out) :in in}]))

(defn parse-reactions [lines]
  (->> lines
       (map parse-reaction)
       (into {})))

(defn react [reactants products reactions ore]
  (let [[product product-quantity] (first products)
        products                   (dissoc products product)
        reaction                   (reactions product)]
    (cond (not product)
          [product reactants products ore]
          (= product "ORE")
          (recur reactants products reactions (+ ore product-quantity))
          reaction
          (let [available-quantity (reactants product 0)
                required-quantity  (- product-quantity available-quantity)]
            (cond (= required-quantity 0)
                  (recur (dissoc reactants product) products reactions ore)
                  (< required-quantity 0)
                  (recur (assoc reactants product (- available-quantity product-quantity)) products reactions ore)
                  (> required-quantity 0)
                  (let [per-reaction-quantity (:quantity reaction)
                        reaction-multiple     (long (Math/ceil (/ required-quantity per-reaction-quantity)))
                        reaction-quantity     (* per-reaction-quantity reaction-multiple)
                        products              (->> (:in reaction)
                                                   (map (fn [[c q]] [c (* q reaction-multiple)]))
                                                   (remove (fn [[c q]] (zero? q)))
                                                   (into {})
                                                   (merge-with + products))
                        reactants             (assoc reactants product (- reaction-quantity required-quantity))]
                    (recur reactants products reactions ore))))
          :else [product reactants products ore])))

(->> (react {}
            {"FUEL" 1}
            (-> "input14.txt" clojure.java.io/file clojure.java.io/reader line-seq parse-reactions)
            0)
     last
     (str "Day 14 / Part 1: ")
     println)

(comment

(react {}
       {"FUEL" 1}
       (parse-reactions ["2 ORE => 1 A"
                         "2 A => 1 FUEL"])
       0)
;; 4

(react {}
       {"FUEL" 1}
       (parse-reactions ["1 ORE => 2 A"
                         "2 A => 1 FUEL"])
       0)
;; 1

(react {}
       {"FUEL" 1}
       (parse-reactions ["1 ORE => 2 A"
                         "3 A => 1 FUEL"])
       0)
;; 2

(react {}
       {"FUEL" 1}
       (parse-reactions ["1 ORE => 2 A"
                         "3 A => 1 B"
                         "1 A => 1 C"
                         "1 B, 1 C => 1 FUEL"])
       0)
;; 2

(react {}
       {"FUEL" 1}
       (parse-reactions ["9 ORE => 2 A"
                         "8 ORE => 3 B"
                         "7 ORE => 5 C"
                         "3 A, 4 B => 1 AB"
                         "5 B, 7 C => 1 BC"
                         "4 C, 1 A => 1 CA"
                         "2 AB, 3 BC, 4 CA => 1 FUEL"])
       0) ;; 165

(react {}
       {"FUEL" 1}
       (-> "input14.txt" clojure.java.io/file clojure.java.io/reader line-seq parse-reactions)
       0)
;; 899155

(react {}
       {"FUEL" 100}
       (-> "input14.txt" clojure.java.io/file clojure.java.io/reader line-seq parse-reactions)
       0)
;; 42229401

(react {}
       {"FUEL" 1000}
       (-> "input14.txt" clojure.java.io/file clojure.java.io/reader line-seq parse-reactions)
       0)
;; 418539417

)
