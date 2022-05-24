(ns app.generator.bila.biology.evolusi
  (:require [clojure.set :as cset]))
(use '[clojure.set :only (map-invert)])

(defn filteranswers [map_answers]
  (->> (for [map_answer map_answers]
         (let [sisa (count (map-invert map_answer))]
           (cond (< sisa (count map_answer)) nil
                 :else (map-invert (map-invert map_answer)))))
       (filter (complement nil?))))

(defn bio-analogi-dan-homologi-01
  [] (let [bukti ["analogi" "homologi"]
           pernyataan [["diamati pada spesies yang berasal dari leluhur yang  sama"
                        "dapat diamati pada struktur organ sama"
                        "dapat diamati pada organisme dengan fungsi organ berbeda"
                        "terjadi karena adanya ancestor yang sama"
                        "diamati pada organisme memilik yangi kesamaan lebih pada tingkat gen"
                        "terjadi pada organisme yang memiliki hubungan kekerabatan yang dekat"]
                       ["dapat diamati pada spesies berasal dari leluhur yang berbeda"
                        "diamati dengan organisme memiliki yang stuktur organ berbeda"
                        "diamati pada organisme yang memiliki fungsi organ yang sama"
                        "terjadi karena lingkungan yang sama"
                        "terjadi karena kebutuhan organ yang sama"
                        "terjadi karena penggunaan organ yang sama"
                        "terjadi karena persamaan tidak berdasarkan kesamaan pada tingkat gen"
                        "terjadi karena adanya adaptasi lingkungan"]]
           intro ["Analogi dan homologi adalah salah satu bukti evolusi."
                  "Salah satu bukti evolusi adalah analogi dan homologi."
                  "Adanya evolusi dapat diamati dengan adanya analogi dan homologi pada organisme."]]
       (->> (for [soal ["Perbedaan dari analogi dan homologi adalah ...."
                        "Analogi dan homologi memiliki perbedaan diantaranya pada ...."
                        "Analogi dan homologi dapat dibedakan dengan ciri, bahwa pada ...."]
                  re (range 50)
                  p (range 2)]
              (let [a (get bukti p)
                    b (rand-nth (get pernyataan p))]
                {:intro (rand-nth intro)
                 :soal soal
                 :pb (str a " " b)
                 :p1 (#(str (get bukti %) " " (rand-nth (get pernyataan (rand-nth (remove #{%} (range 2))))))
                      (rand-int 2))
                 :p2 (#(str (get bukti %) " " (rand-nth (get pernyataan (rand-nth (remove #{%} (range 2))))))
                      (rand-int 2))
                 :p3 (#(str (get bukti %) " " (rand-nth (get pernyataan (rand-nth (remove #{%} (range 2))))))
                      (rand-int 2))}))
            filteranswers
            shuffle
            (take 50))))

(defn bio-beda-reproduksi-seksual-dan-aseksual1
  [] (let [reproduksi ["seksual" "aseksual"]
           seksual ["sifat yang diturunkan dipilih secara acak"
                    "memungkinkan munculnya sifat baru"
                    "terjadi proses kombinasi DNA"
                    "anakan tidak identik dengan induknya"
                    "hanya dapat diamati pada organisme eukariot"
                    "berasal dari dua sel yang berbeda"
                    "anakan dapat memiliki sifat yang lebih unggul untuk bertahan hidup"
                    "salah satu contohnya adalah proses pembentukan embrio hewan"
                    "salah satu contohnya adalah proses pembentukan embrio tumbuhan"]
           aseksual ["terjadi tanpa adanya proses kombinasi DNA"
                     "anakan identik dengan induknya"
                     "contohnya pembelahan biner"
                     "berasal dari satu sel yang sama"
                     "dapat diamati pada organisme prokariot"
                     "tidak memungkinkan munculnya sifat baru"]
           pernyataan (concat seksual aseksual)]
       (->> (let [p (range 2)
                  re (range 100)]
              (let [a (get reproduksi p)
                    soal [(str "Perbedaan antara reproduksi seksual dan reproduksi aseksual dapat diamati bahwa pada reproduksi " a " ....")
                          (str "Di antara perbedaan antara reproduksi seksual dan reproduksi aseksual adalah, reproduksi " a " ....")]]
                {:soal (rand-nth soal)
                 :pb (cond (= (.contains seksual a) true) "seksual"
                           (= (.contains aseksual a) true) "aseksual")
                 :p1 (rand-nth (get pernyataan (rand-nth (remove #(= % p) (range 2)))))
                 :p2 (rand-nth (get pernyataan (rand-nth (remove #(= % p) (range 2)))))
                 :p3 (rand-nth (get pernyataan (rand-nth (remove #(= % p) (range 2)))))})
              ;filteranswers
              shuffle
              distinct
              (take 30)))))

(defn bio-beda-reproduksi-seksual-dan-aseksual
  []
  (let [reproduksi ["seksual" "aseksual"]
        seksual ["sifat yang diturunkan dipilih secara acak"
                 "memungkinkan munculnya sifat baru"
                 "terjadi proses kombinasi DNA"
                 "anakan tidak identik dengan induknya"
                 "hanya dapat diamati pada organisme eukariot"
                 "berasal dari dua sel yang berbeda"
                 "anakan dapat memiliki sifat yang lebih unggul untuk bertahan hidup"
                 "salah satu contohnya adalah proses pembentukan embrio hewan"
                 "salah satu contohnya adalah proses pembentukan embrio tumbuhan"]
        aseksual ["terjadi tanpa adanya proses kombinasi DNA"
                  "anakan identik dengan induknya"
                  "contohnya pembelahan biner"
                  "berasal dari satu sel yang sama"
                  "dapat diamati pada organisme prokariot"
                  "tidak memungkinkan munculnya sifat baru"]]
    (->>
     (for [R reproduksi S seksual A aseksual
           soal [(str "Perbedaan antara reproduksi seksual dan reproduksi aseksual dapat diamati bahwa pada reproduksi " R " ....")
                 (str "Di antara perbedaan antara reproduksi seksual dan reproduksi aseksual adalah, reproduksi " R " ....")]]
       (cond
         (= (reproduksi 0) R) (merge (zipmap [:p1 :p2 :p3] (shuffle aseksual)) {:soal soal :pb S})
         (= (reproduksi 1) R) (merge (zipmap [:p1 :p2 :p3] (shuffle seksual)) {:soal soal :pb A})))
     distinct shuffle (take 30) shuffle)))