(defn split-digits [n]
  (->> n str (map #(Integer/parseInt (str %)))))

(defn all-increasing [n]
  (apply <= n))

(defn digit-pair? [[n1 n2 n3 n4 n5 n6]]
  (or (= n1 n2)
      (= n2 n3)
      (= n3 n4)
      (= n4 n5)
      (= n5 n6)))

(defn digit-pair-no-triple? [[n1 n2 n3 n4 n5 n6]]
  (or (and (= n1 n2) (not= n2 n3))
      (and (not= n1 n2) (= n2 n3) (not= n3 n4))
      (and (not= n2 n3) (= n3 n4) (not= n4 n5))
      (and (not= n3 n4) (= n4 n5) (not= n5 n6))
      (and (not= n4 n5) (= n5 n6))))

(def all-increasing-passwords
  (->> (range 136760 595731)
       (map split-digits)
       (filter all-increasing)))

(->> all-increasing-passwords
     (filter digit-pair?)
     count
     (str "Day 4 / Part 1: ")
     println)
;; 1873

(->> all-increasing-passwords
     (filter digit-pair-no-triple?)
     count
     (str "Day 4 / Part 2: ")
     println)
;; 1264
