(ns app.generator.bila.biology.biokimia
  (:require [clojure.set :as cset]))
(use '[clojure.set :only (map-invert)])

(defn filteranswers [map_answers]
  (->> (for [map_answer map_answers]
         (let [sisa (count (map-invert map_answer))]
           (cond (< sisa (count map_answer)) nil
                 :else (map-invert (map-invert map_answer)))))
       (filter (complement nil?))))

(defn bio-karbohidrat-3-03
  [] (let [correct ["Ikatan pada alfa-glukosa mudah dipecah." "Ikatan beta-glukosa susah dipecah."
                    "Glikogen adalah karbohidrat yang terdiri atas kumpulan alfa-glukosa." "Selulosa adalah karbohidrat yang terdiri atas kumpulan beta-glukosa."
                    "Glikogen hanya dimiliki oleh hewan." "Selulosa hanya dimiliki oleh tumbuhan."
                    "Perbedaan alfa dan beta-glukosa adalah letak $\\operatorname{H}$ dan $\\operatorname{OH}$ pada atom $\\operatorname{C}$ nomor $1.$"
                    "Alfa dan beta-glukosa adalah glukosa yang memiliki perbedaan kekuatan ikatan."]
           wrong ["Ikatan alfa-glukosa susah dipecah." "Ikatan beta-glukosa mudah dipecah."
                  "Glikogen adalah karbohidrat yang terdiri atas kumpulan beta-glukosa." "Selulosa adalah karbohidrat yang terdiri atas kumpulan alfa-glukosa."
                  "Glikogen dimiliki oleh tumbuhan." "Selulosa dimiliki oleh hewan."
                  "Perbedaan alfa dan beta-glukosa adalah letak $\\operatorname{H}$ dan $\\operatorname{OH}$ pada atom $\\operatorname{C}$ nomor $4.$"
                  "Alfa dan beta-glukosa memiliki struktur ikatan yang sama."]
           intro ["Perhatikan pernyataan di bawah ini!"
                  "Terdapat pernyataan tentang alfa dan beta-glukosa di bawah ini."]
           pilihan (concat correct wrong)
           opsi ["benar" "salah" "tidak tahu"]]
       (->> (for [soal ["Pernyataan tersebut benar atau salah?"
                        "Benar atau salahkah pernyataan di atas?"]
                  q pilihan
                  re (range 100)]
              (let [intro (rand-nth intro)]
                {:intro intro
                 :pernyataan q
                 :soal soal
                 :pb (if (= (.contains correct q) true) "benar" "salah")
                 :p1 (rand-nth opsi)
                 :p2 (rand-nth opsi)}))
            filteranswers
            distinct
            shuffle
            (take 35))))

(defn bio-lipid-2-01
  [] (let [correct ["Fosfolipid terdiri atas dua rantai asam lemak."
                    "Fosfolipid tersusun dari fosfat dan asam lemak."
                    "Fosfolipid adalah bahan penyusun membran sel."
                    "Pada fosfolipid, gliserol berikatan dengan satu gugus fosfat."
                    "Pada membran sel, terdapat fosfolipid bilayer."
                    "Bagian fosfat dan gliserol bersifat hidrofilik (suka air)."
                    "Bagian asam lemak bersifat hidrofobik (tidak suka air)."]
          wrong["Fosfolipid terdiri atas tiga rantai asam lemak."
                "Fosfolipid adalah lipid yang mempunya tiga gugus fosfat."
                "Fosfolipid bukan bahan penyusun membran sel."
                "Membran sel memiliki satu lapisan fosfolipid."
                "Pada fosfolipid, semua atom $\\operatorname{C}$ pada gliserol berikatan dengan asam lemak."
                "Bagian asam lemak bersifat hidrofilik (suka air)."
                "Bagian fosfat dan gliserol bersifat hidrofobik (tidak suka air)."]
           intro ["Perhatikan pernyataan di bawah ini!"
                  "Terdapat pernyataan tentang fosfolipid di bawah ini."]
           pilihan (concat correct wrong)
           opsi ["benar" "salah" "tidak tahu"]]
       (->> (for [soal ["Pernyataan tersebut benar atau salah?"
                        "Benar atau salahkah pernyataan di atas?"]
                  q pilihan
                  re (range 100)]
              (let [intro (rand-nth intro)]
                {:intro intro
                 :pernyataan q
                 :soal soal
                 :pb (if (= (.contains correct q) true) "benar" "salah")
                 :p1 (rand-nth opsi)
                 :p2 (rand-nth opsi)}))
            filteranswers
            distinct
            shuffle
            (take 35))))

(defn bio-protein-2-01
  [] (let [correct ["Asam amino adalah bahan dasar penyusun protein."
                    "Dua asam amino atau lebih dapat berikatan dengan membentuk ikatan peptida."
                    "Jika asam amino berikatan, maka akan terbentuk $\\operatorname{H_2O}.$"
                    "Proses pembentukan ikatan asam amino melalui reaksi kondensasi dapat menghasilkan $\\operatorname{H_2O}.$"
                    "Ikatan peptida dapat diputus oleh bantuan enzim."
                    "Dua ikatan asam amino disebut dipeptida."
                    "Ikatan asam amino yang lebih dari dua disebut polipeptida."
                    "Banyak polipeptida disebut protein."]
           wrong ["Asam lemak adalah bahan dasar penyusun protein."
                  "Dua asam amino atau lebih dapat berikatan dengan membentuk ikatan ester."
                  "Jika asam amino berikatan, maka akan terbentuk $\\operatorname{CO_2}.$."
                  "Proses pembentukan ikatan asam amino melalui reaksi hidrolisis dapat menghasilkan $\\operatorname{H_2O}.$"
                  "Ikatan peptida dapat diputus pada proses glikolisis."
                  "Dua ikatan asam amino disebut polipeptida."
                  "Ikatan asam amino yang lebih dari dua disebut dipeptida."]
           intro ["Perhatikan pernyataan di bawah ini!"
                  "Protein adalah komponen utama dalam pertumbuhan dan pembentukan tubuh. Berikut ini adalah pernyataan tentang protein."]
           pilihan (concat correct wrong)
           opsi ["benar" "salah" "tidak tahu"]]
       (->> (for [soal ["Pernyataan tersebut benar atau salah?"
                        "Benar atau salahkah pernyataan di atas?"]
                  q pilihan
                  re (range 100)]
              (let [intro (rand-nth intro)]
                {:intro intro
                 :pernyataan q
                 :soal soal
                 :pb (if (= (.contains correct q) true) "benar" "salah")
                 :p1 (rand-nth opsi)
                 :p2 (rand-nth opsi)}))
            filteranswers
            distinct
            shuffle
            (take 30))))

(defn bio-lipid-3-02
  [] (let [correct ["Hormon dapat berupa steroid atau protein."
                    "Steroid adalah bahan penyusun hormon."
                    "Contoh steorid adalah kolesterol."
                    "Contoh steroid adalah estradiol."
                    "Contoh steoroid adalah estrogen."
                    "Contoh steroid adalah testosteron."
                    "Asam lemak dibagi menjadi 2, yaitu asam lemak jenuh dan asam lemak tak jenuh."
                    "Asam lemak jenuh memiliki ikatan karbon tunggal."
                    "Asam lemak tak jenuh memiliki ikatan karbon ganda."
                    "Asam lemak tak jenuh lebih sehat karena ikatannya mudah diputus."
                    "Ikatan asam lemak jenuh susah diputus."
                    "Steroid adalah turunan lemak yang tidak mengandung gugus asam lemak dan gugus ester."
                    "Asam lemak jenuh berwujud solid pada suhu ruang."
                    "Asam lemak tak jenuh berwujud cair pada suhu ruang."]
          wrong ["Steroid adalah bahan penyusun membran sel."
                 "Contoh steroid adalah penyusun membran sel."
                 "Contoh steroid adalah DNA."
                 "Asam lemak terdiri dari asam lemak jenuh saja."
                 "Asam lemak terdiri dari asam lemak tak jenuh saja."
                 "Asam lemak jenuh memiliki ikatan karbon ganda."
                 "Asam lemak tak jenuh memiliki ikatan karbon tunggal."
                 "Ikatan asam lemak jenuh lebih mudah diputus dibandingkan ikatan asam lemak tak jenuh."
                 "Ikatan asam lemak tak jenuh susah untuk diputus."
                 "Steroid adalah turunan lemak yang gugus hidroksi pada gliserolnya diganti dengan asam karboksilat dan asam folat."
                 "Asam lemak jenuh berwujud cair pada suhu ruang."
                 "Asam lemak tak jenuh berwujud solid pada suhu ruang."]
          intro ["Perhatikan pernyataan di bawah ini!"
                 "Lemak adalah salah satu senyawa makromolekul yang berfungsi sebagai cadangan makanan. 
                  Berikut ini adalah pernyataan tentang lemak."]
          pilihan (concat correct wrong)
          opsi ["benar" "salah" "tidak tahu"]]
      (->> (for [soal ["Pernyataan tersebut benar atau salah?"
                        "Apakah pernyataan di atas benar?"]
                  q pilihan
                  re (range 60)]
              (let [intro (rand-nth intro)]
                {:intro intro
                 :pernyataan q
                 :soal soal
                 :pb (if (= (.contains correct q) true) 
                       "benar" "salah")
                 :p1 (rand-nth opsi)
                 :p2 (rand-nth opsi)}))
            filteranswers
            distinct
            shuffle
            (take 60))))

(defn bio-protein-3-01
  [] (let [soal1 ["Pernyataan yang benar mengenai struktur protein ditunjukkan oleh nomor ...."
                  "Pernyataan nomor berapakah yang tepat mengenai struktur protein?"]
           soal2 ["Pernyataan yang <u>salah</u> mengenai struktur protein ditunjukkan oleh nomor ...."
                  "Pernyataan nomor berapakah yang <u>salah</u> mengenai struktur protein?"]
           
           correct ["Urutan asam amino menentukan jenis protein."
                    "R-<i>group</i> dapat diubah sesuai jenis asam amino."
                    "Asam amino memiliki gugus asam dan gugus amina."
                    "Asam amino memiliki gugus $\\operatorname{COOH}$ dan gugus $\\operatorname{NH_2}$."
                    "Terdapat $20$ jenis asam amino."
                    "Tingkatan struktur protein terbagi dari struktur primer, struktur sekunder, struktur tersier, dan struktur kuartener."
                    "Struktur primer berbentuk untaian."
                    "Struktur sekunder berbentuk alfa-heliks dan beta-<i>sheet</i> (lembaran)."
                    "Struktur tersier berbentuk protein globular."
                    "Struktur tersier digunakan untuk hormon."
                    "Struktur tersier digunakan untuk enzim."]
           wrong ["Jenis protein tidak ditentukan oleh asam amino."
                  "Jenis asam amino diketahui dari perbedaan gugus asam."
                  "Jenis asam amino diketahui dari perbedaan gugus amina."
                  "Asam amino hanya memiliki gugus R."
                  "Terdapat $25$ jenis asam amino."
                  "Tingkatan struktur protein hanya terbagi dari struktur primer dan sekunder."
                  "Struktur protein hanya berbentuk struktur kuartener."
                  "Struktur sekunder berbentuk untaian."
                  "Struktur primer berbentuk alfa-heliks."
                  "Struktur primer berbentuk beta-<i>sheet</i> (lembaran)."
                  "Struktur sekunder digunakan untuk hormon."
                  "Struktur primer digunakan untuk enzim."
                  "Struktur tersier digunakan untuk protein filamen."]
           option1 ["1 saja"
                    "2 saja"
                    "3 saja"
                    "1 dan 2"
                    "1 dan 3"
                    "2 dan 3"
                    "semua nomor benar"
                    "semua nomor salah"]
           option2 ["1 saja"
                    "2 saja"
                    "3 saja"
                    "1 dan 2"
                    "1 dan 3"
                    "2 dan 3"
                    "semua nomor salah"
                    "semua nomor benar"]
           pilihan (into [] (concat correct wrong))]
       (->> (for [intro ["Perhatikan pernyataan di bawah ini!"
                         "Protein adalah komponen utama dalam pertumbuhan dan pembentukan tubuh. Berikut ini adalah pernyataan tentang struktur protein."]
                  soal ["1" "2"]
                  repeat (range 300)]
              (let [isi1 (rand-nth pilihan)
                    isi2 (rand-nth (remove #{isi1} pilihan))
                    isi3 (rand-nth (remove #{isi1} (remove #{isi2} pilihan)))
                    pilihanB (cond
                               (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) false))
                                    (= (.contains correct isi3) false)) "1 saja"
                               (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) true))
                                    (= (.contains correct isi3) false)) "2 saja"
                               (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) false))
                                    (= (.contains correct isi3) true)) "3 saja"
                               (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) true))
                                    (= (.contains correct isi3) false)) "1 dan 2"
                               (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) false))
                                    (= (.contains correct isi3) true)) "1 dan 3"
                               (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) true))
                                    (= (.contains correct isi3) true)) "2 dan 3"
                               (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) true))
                                    (= (.contains correct isi3) true)) "semua nomor benar"
                               (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) false))
                                    (= (.contains correct isi3) false)) "semua nomor salah")
                    pilihanB1 (cond
                                (and (and (= (.contains wrong isi1) true) (= (.contains wrong isi2) false))
                                     (= (.contains wrong isi3) false)) "1 saja"
                                (and (and (= (.contains wrong isi1) false) (= (.contains wrong isi2) true))
                                     (= (.contains wrong isi3) false)) "2 saja"
                                (and (and (= (.contains wrong isi1) false) (= (.contains wrong isi2) false))
                                     (= (.contains wrong isi3) true)) "3 saja"
                                (and (and (= (.contains wrong isi1) true) (= (.contains wrong isi2) true))
                                     (= (.contains wrong isi3) false)) "1 dan 2"
                                (and (and (= (.contains wrong isi1) true) (= (.contains wrong isi2) false))
                                     (= (.contains wrong isi3) true)) "1 dan 3"
                                (and (and (= (.contains wrong isi1) false) (= (.contains wrong isi2) true))
                                     (= (.contains wrong isi3) true)) "2 dan 3"
                                (and (and (= (.contains wrong isi1) true) (= (.contains wrong isi2) true))
                                     (= (.contains wrong isi3) true)) "semua nomor salah"
                                (and (and (= (.contains wrong isi1) false) (= (.contains wrong isi2) false))
                                     (= (.contains wrong isi3) false)) "semua nomor benar")

                    pb pilihanB
                    p1 (rand-nth (remove #{pb} option1))
                    p2 (rand-nth (remove #{p1} (remove #{pb} option1)))
                    p3 (rand-nth (remove #{p2} (remove #{p1} (remove #{pb} option1))))

                    ps pilihanB1
                    ps1 (rand-nth (remove #{ps} option2))
                    ps2 (rand-nth (remove #{ps1} (remove #{ps} option2)))
                    ps3 (rand-nth (remove #{ps2} (remove #{ps1} (remove #{ps} option2))))]
                (condp = soal
                  "1" {:soal    (rand-nth soal1)
                       :intro   intro
                       :isi1    isi1
                       :isi2    isi2
                       :isi3    isi3
                       :pb      pb
                       :p1      p1
                       :p2      p2
                       :p3      p3}
                  "2" {:soal    (rand-nth soal2)
                       :intro   intro
                       :isi1    isi1
                       :isi2    isi2
                       :isi3    isi3
                       :pb      ps
                       :p1      ps1
                       :p2      ps2
                       :p3      ps3})))
            flatten
            distinct
            shuffle
            (take 30))))

(defn bio-protein-3-02
  [] (let [mulut ["pencernaan protein secara mekanik menggunakan gigi"
                  "tidak ada pencernaan protein secara kimiawi di mulut"
                  "hanya terjadi pencernaan mekanik"
                  "pencernaan protein secara kimiawi tidak terjadi di mulut"]
           lambung ["menghasilkan $\\operatorname{HCl}$ (asam klorida) untuk mengaktifkan enzim pepsin"
                    "penghancuran protein oleh enzim pepsin"
                    "pencernaan oleh enzim pepsin akan membentuk proteosa, pepton, dan polipeptida"]
           usus ["pengubahan proteosa, pepton, dan polipeptida oleh enzim tripsin dan kimotripsin"
                 "menghasilkan ikatan polipeptida yang lebih kecil"
                 "pelepasan ikatan-ikatan pada polipeptida berukuran kecil menjadi asam amino oleh enzim peptidase"
                 "penyerapan dan pengaliran asam amino hasil penguraian protein ke seluruh tubuh"]
           pengecoh ["pencernaan mekanik menggunakan enzim"
                     "metabolisme asam amino oleh enzim amilase"
                     "pemecahan protein oleh enzim lipase"
                     "pencernaan oleh enzim pepsin akan membentuk asam lemak dan glukosa"
                     "pengubahan polipeptida kecil menjadi asam lemak oleh enzim peptidase"]
           organ ["mulut" "lambung" "usus"]
           pernyataan (vector mulut lambung usus)]
       (->> (for [intro ["Proses penguraian protein dapat terjadi di saluran pencernaan."
                         "Proses penguraian protein dilakukan oleh organ-organ pencernaan."]
                  re (range 100)
                  p (range 3)]
              (let [a (get organ p)
                    soal [(str "Proses yang terjadi pada " a " adalah ....")
                          (str "Organ " a " akan melakukan proses ....")]]
                {:intro intro
                 :soal (rand-nth soal)
                 :pb (rand-nth (get pernyataan p))
                 :p1 (rand-nth (concat pengecoh (get pernyataan (rand-nth (remove #(= % p) (range 3))))))
                 :p2 (rand-nth (concat pengecoh (get pernyataan (rand-nth (remove #(= % p) (range 3))))))
                 :p3 (rand-nth (concat pengecoh (get pernyataan (rand-nth (remove #(= % p) (range 3))))))}))
            filteranswers
            shuffle
            (take 50))))