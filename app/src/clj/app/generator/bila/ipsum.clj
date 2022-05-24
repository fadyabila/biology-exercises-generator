(ns app.generator.bila.ipsum)

(defn ipsum-arit
  []
  (->> (for [a (range 10 13)
             b (range 8 11)
             d (range 5 8)]
         (let [p (/ 1 d)]
           {:a  a
            :p p
            :pb (str "$ " "x > " a " $")
            :p3 (str "$ " "x > " b " $")
            :p1 (str "$ " "1 < x \\leq " a " $")
            :p2 (str "$ " "1000 < x \\geq 1" " $")}))
       shuffle))

;; sample output ipsum-arit
;; {:a  12,
;;  :p  1/5,
;;  :pb "$ x > 12 $",
;;  :p1 "$ 1 < x \\leq 12 $",
;;  :p2 "$ 0 < x \\geq 1 $"}

;; (ipsum-arit)
