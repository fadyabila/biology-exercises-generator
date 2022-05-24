(ns app.generator.bila.biology.klasifikasi_organisme
    (:require [clojure.set :as cset]))
(use '[clojure.set :only (map-invert)])

(defn filteranswers [map_answers]
  (->> (for [map_answer map_answers]
         (let [sisa (count (map-invert map_answer))]
           (cond (< sisa (count map_answer)) nil
                 :else (map-invert (map-invert map_answer)))))
       (filter (complement nil?))))

(defn bio-evolusi-tumbuhan-akuatik-ke-terestrial-01
  [] (let [correct ["Tumbuhan pertama kali hidup di lingkungan akuatik."
                    "Tumbuhan akuatik pertama adalah alga."
                    "Alga memiliki klorofil."
                    "Lumut hidup pada tempat lembab atau sudah bisa beradaptasi di lingkungan terestrial."
                    "Alga tidak memiliki pembuluh vaskular."
                    "Lumut memiliki pembuluh vaskular."
                    "Tumbuhan berbiji memiliki pembuluh vaskular."
                    "Lumut memiliki klorofil."
                    "Tumbuhan berbiji mulai hidup pada lingkungan terestrial."
                    "Paku hidup pada tempat lembab atau sudah bisa beradaptasi di lingkungan terestrial."
                    "Paku memiliki pembuluh vaskular."
                    "Paku memiliki klorofil."
                    "Tumbuhan pertama kali hidup di lingkungan akuatik karena tersedia sumber nutrisi yang melimpah."]
           wrong ["Tumbuhan pertama kali hidup di lingkungan terestrial."
                  "Tumbuhan akuatik pertama adalah lumut."
                  "Tumbuhan akuatik pertama adalah alga."
                  "Tumbuhan akuatik pertama adalah tumbuhan berbiji."
                  "Alga memiliki pembuluh vaskular."
                  "Lumut tidak memiliki klorofil."
                  "Tumbuhan paku tidak memiliki klorofil."
                  "Tumbuhan berbiji mulai hidup pada lingkungan akuatik."
                  "Tumbuhan berbiji tidak memiliki pembuluh vaskular."
                  "Tumbuhan pertama kali hidup di lingkungan akuatik karena tidak tersedia nutrisi di lingkungan terestrial."]
           intro ["Perhatikan pernyataan di bawah ini!"
                  "Terdapat pernyataan tentang evolusi tumbuhan akuatik ke terestrial di bawah ini."]
           pilihan (concat correct wrong)
           opsi ["benar" "salah" "tidak tahu"]]
       (->> (for [soal ["Pernyataan tersebut benar atau salah, ya?"
                        "Apakah pernyataan tersebut benar?"]
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
            (take 50))))

(defn bio-evolusi-tumbuhan-akuatik-ke-terestrial-02
  [] (let [correct ["Tumbuhan akuatik migrasi ke daratan karena lebih sedikit saingan di darat."
                    "Tumbuhan akuatik migrasi ke daratan karena untuk mengisi relung di darat."
                    "Di daratan, cahaya matahari melimpah."
                    "Di lingkungan akuatik, cahaya matahari hanya bisa sampai di zona tertentu."
                    "Di daratan, cahaya matahari dapat menyinari seluruh permukaan daratan."
                    "Di lingkungan akuatik, cahaya matahari tidak sampai di zona afotik."
                    "Cahaya matahari menyinari zona litoral di lingkungan akuatik."
                    "Cahaya matahari menyinari zona fotik di lingkungan akuatik."
                    "Cahaya matahari menyinari zona limnetik di lingkungan akuatik."
                    "Tumbuhan akuatik migrasi ke daratan karena di lingkungan akuatik saling berebut relung di zona fotik."
                    "Tumbuhan akuatik migrasi ke daratan karena di lingkungan akuatik saling berebut relung di zona limnetik."
                    "Tumbuhan akuatik migrasi ke daratan karena di lingkungan akuatik saling berebut relung di zona litoral."]
           wrong ["Tumbuhan akuatik migrasi ke daratan karena banyak saingan di daratan."
                  "Tumbuhan akuatik migrasi ke daratan karena relung di darat penuh."
                  "Di daratan, cahaya matahari tidak menyinari seluruh permukaannya."
                  "Di lingkungan akuatik, cahaya matahari melimpah."
                  "Di lingkungan akuatik, cahaya matahari dapat menembus semua zona perairan."
                  "Di lingkungan akuatik, cahaya matahari sampai di zona afotik."
                  "Cahaya matahari menyinari zona afotik di lingkungan akuatik."
                  "Cahaya matahari tidak sampai ke zona fotik di lingkungan akuatik."
                  "Tumbuhan akuatik migrasi ke daratan karena tidak ada perebutan relung di lingkungan akuatik."
                  "Cahaya matahari tidak sampai ke zona limnetik di lingkungan akuatik."]
           intro ["Perhatikan pernyataan di bawah ini!"
                  "Terdapat pernyataan tentang alasan tumbuhan akuatik perlu migrasi ke terestrial di bawah ini."]
           pilihan (concat correct wrong)
           opsi ["benar" "salah" "tidak tahu"]]
      (->> (for [soal ["Pernyataan tersebut benar atau salah, ya?"
                        "Apakah pernyataan tersebut benar?"]
                  q pilihan
                  re (range 200)]
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
            (take 50))))

(defn bio-organ-sensoris-Condrichthyes
  [] (let [correct ["Chondrichthyes memiliki rahang."
                    "Chondrichthyes memiliki <i>lateral line system</i> untuk mendeteksi getaran."
                    "Chondrichthyes memiliki <i>lateral line system</i> yang terdiri dari <i>statocyst</i>."
                    "Chondrichthyes memiliki <i>electro-receptor</i> untuk mendeteksi aliran listrik."
                    "Chondrichthyes mempunyai organ sensoris berupa <i>lateral line system</i> dan <i>electro-receptor</i> untuk mendeteksi gerakan di sekitarnya."
                    "Chondrichthyes <i>lateral line system</i> berfungsi sebagai organ pendengaran."
                    "Chondrichthyes <i>electro-receptor</i> berfungsi sebagai organ pengganti telinga."
                    "Chondrichthyes mempunyai mata yang memiliki penglihatan yang tajam."]
           wrong ["Chondrichthyes tidak memiliki rahang."
                  "Chondrichthyes memiliki alat pendengaran berupa telinga."
                  "Chondrichthyes memiliki <i>lateral line system</i> untuk menyokong fungsi sirip."
                  "Chondrichthyes organ <i>electro-receptor</i> berfungsi untuk melihat objek."
                  "Chondrichthyes memiliki daya lihat yang lemah."
                  "Chondrichthyes hanya memiliki <i>electro-receptor</i> saja."
                  "Chondrichthyes tidak memiliki <i>lateral line system</i>."]
           intro ["Perhatikan pernyataan di bawah ini!"
                  "Berikut ini adalah pernyataan tentang Condrichthyes."]
           pilihan (concat correct wrong)
           opsi ["benar" "salah" "tidak tahu"]]
       (->> (for [soal ["Pernyataan tersebut benar atau salah?"
                        "Apakah pernyataan tersebut benar?"]
                  q pilihan
                  re (range 100)]
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
            (take 30))))

(defn bio-reptilia-1-struktur-dasar
  [] (let [soal1 ["Pernyataan yang tepat mengenai karakteristik Reptilia adalah ...."
                  "Manakah pernyataan tentang karakteristik Reptilia di atas yang benar?"]
           soal2 ["Pernyataan mengenai karakteristik Reptilia yang <u>salah</u> adalah ...."
                  "Manakah yang <u>bukan</u> merupakan pernyataan tentang karakteristik Reptilia?"]

           correct ["Memiliki sisik."
                    "Sisik adalah adaptasi pada kehidupan terestrial."
                    "Hidup pada lingkuan terestrial."
                    "Sisik adalah modifikasi dari kulit yang tersusun oleh sel kulit mati yang kaya akan keratin."
                    "Sisik pada Reptilia berfungsi untuk proteksi dan agar tidak dehidrasi."
                    "Reptilia dapat melakukan ganti kulit."
                    "Merupakan hewan vertebrata berdarah dingin (poikiloterm)."
                    "Memiliki empat tungkai, kecuali ular."
                    "Bersifat ovipar."
                    "Bernapas dengan paru-paru."
                    "Fertilisasi internal."]
           wrong ["Kulitnya licin dan berlendir."
                  "Tidak memiliki sisik pada kulitnya."
                  "Memiliki dua cara hidup, yaitu di darat dan di air."
                  "Kulit pada Reptilia licin karena hidup di tempat lembab."
                  "Kulit Reptilia berfungsi untuk respirasi."
                  "Merupakan hewan homoiterm."
                  "Bersifat vivipar."
                  "Bernapas dengan kulit."
                  "Bernapas menggunakan insang."
                  "Hidup di lingkungan akuatik."
                  "Fertilisasi eksternal."]
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
                         "Seorang siswa sedang melakukan praktikum untuk mengidentifikasi Reptilia."]
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

(defn bio-struktur-amniotic-egg-01
  [] (let [soal1 ["Bagian yang tepat mengenai struktur <i>amniotic egg</i> adalah ...."
                  "Manakah bagian tentang <i>amniotic egg</i> di atas yang benar?"]
           soal2 ["Bagian dari struktur <i>amniotic egg</i> yang <u>salah</u> adalah ...."
                  "Manakah yang <u>bukan</u> merupakan struktur <i>amniotic egg</i>?"]
           
           correct ["<i>amnion</i>"
                    "<i>allantois</i>"
                    "<i>chorion</i>"
                    "<i>yolk sac</i>"
                    "kuning telur (<i>yolk</i>)"
                    "<i>albumen</i>"
                    "cangkang"
                    "ruang <i>amniotic</i>"
                    "cairan ketuban (<i>amniotic fluid</i>)"
                    "embrio"]
           wrong ["tidak bercangkang"
                  "<i>allantois fluid</i>"
                  "hanya bermembran saja"
                  "ruang <i>chorion</i>"
                  "<i>albumen sac</i>"
                  "sperma"
                  "<i>jelly coat</i>"
                  "<i>vitelline membrane</i>"
                  "<i>animal pole</i>"
                  "<i>vegetal pole</i>"]
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
       (->> (for [intro ["Perhatikan bagian dari struktur <i>amniotic egg</i> di bawah ini!"
                         "Berikut ini adalah struktur dari <i>amniotic egg</i>."]
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

(defn bio-pembelahan-biner-bakteri
  [] (let [soal1 ["Pernyataan berikut ini yang sesuai dengan pembelahan biner pada bakteri dan archaea adalah ...."
                  "Di bawah ini yang merupakan pernyataan benar tentang pembelahan biner adalah ...."]
           soal2 ["Pernyataan yang <u>salah</u> mengenai pembelahan biner adalah ...."
                  "Pernyataan pembelahan biner pada bakteri dan archaea yang <u>tidak</u> tepat adalah ...."]
           
           correct ["membelah diri dari satu sel menjadi dua, empat, delapan, dst dengan materi genetik yang sama persis"
                    "membelah dalam jangka waktu 1-3 jam"
                    "membelah dalam setiap 20 menit"
                    "sifat anak sama persis dengan induknya"
                    "terjadi di lingkungan yang menguntungkan"
                    "diawali dengan replikasi materi genetik bakteri"
                    "setelah mengalami replikasi materi genetik, sel bakteri memanjang"
                    "terbentuk <i>septum</i>, yaitu lekukan dinding sel sebelum sel bakteri membelah menjadi dua"]
           wrong ["terjadi di lingkungan yang tidak menguntungkan"
                  "sel anak berbeda dengan sel induk"
                  "satu sel dapat membelah menjadi tiga"
                  "ukuran sel anak lebih kecil dari sel induk"
                  "pada koloni bakteri, terjadi pemisahan koloni yang besar menjadi koloni yang lebih kecil"
                  "sel induk membentuk tunas"
                  "sel bakteri langsung membelah menjadi dua tanpa diawali replikasi materi genetik"]]
     (->> (for [benar correct
                salah wrong
                intro ["Reproduksi aseksual bakteri dan archaea dilakukan dengan tiga cara, yaitu pembelahan biner, pembentukan tunas, dan fragmentasi."
                       "Bakteri dan archaea melakukan tiga cara reproduksi aseksual, yaitu pembelahan biner, pembentukan tunas, dan fragmentasi."]
                repeat (range 300)]
            (let [soal (rand-nth (concat soal1 soal2))]
              {:intro intro
               :soal soal
               :pb (if (= (.contains soal1 soal) true) 
                     benar salah)
               :p1 (if (= (.contains soal1 soal) true)
                     (rand-nth wrong) (rand-nth correct))
               :p2 (if (= (.contains soal1 soal) true)
                     (rand-nth wrong) (rand-nth correct))
               :p3 (if (= (.contains soal1 soal) true)
                     (rand-nth wrong) (rand-nth correct))}))
            filteranswers
            distinct
            shuffle
            (take 50))))

(defn bio-pembentukan-tunas-bakteri
  [] (let [correct ["membentuk tunas dalam bentuk ranting terlebih dahulu"
                    "tunas akan membuat badan bakteri dan membentuk bakteri baru"
                    "menghasilkan anak yang melekat pada tubuh induknya"
                    "ukuran sel anak lebih kecil dibandingkan sel induk"
                    "sifat anak sama persis dengan induknya"
                    "sel anak akan lepas dan memisahkan diri dari tubuh induk jika sudah matang"
                    "terjadi di lingkungan yang menguntungkan"]
           wrong ["terjadi di lingkungan yang tidak menguntungkan"
                  "sifat anak berbeda dengan induknya"
                  "ukuran sel anak lebih besar dibandinkan sel induk"
                  "membelah menjadi dua individu baru"
                  "dinding sel akan membentuk lekukan sebelum membelah menjadi dua"
                  "pada koloni bakteri, terjadi pemisahan koloni yang besar menjadi koloni yang lebih kecil"
                  "membelah setiap 20 menit"]]
       (->> (for [benar correct
                  salah wrong
                  intro ["Reproduksi aseksual bakteri dan archaea dilakukan dengan tiga cara, yaitu pembelahan biner, pembentukan tunas, dan fragmentasi."
                         "Bakteri dan archaea melakukan tiga cara reproduksi aseksual, yaitu pembelahan biner, pembentukan tunas, dan fragmentasi."]
                  repeat (range 300)]
              (let [soal1 ["Pernyataan berikut ini yang sesuai dengan pembentukan tunas pada bakteri dan archaea adalah ...."
                           "Di bawah ini yang merupakan pernyataan benar tentang pembentukan tunas adalah ...."]
                    soal2 ["Pernyataan yang <u>salah</u> mengenai pembentukan tunas adalah ...."
                           "Pernyataan pembentukan tunas pada bakteri dan archaea yang <u>tidak</u> tepat adalah ...."]
                    soal (rand-nth (concat soal1 soal2))]
                {:intro intro
                 :soal soal
                 :pb (if (= (.contains soal1 soal) true)
                       benar salah)
                 :p1 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))
                 :p2 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))
                 :p3 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))}))
            filteranswers
            distinct
            shuffle
            (take 50))))

(defn bio-fragmentasi-bakteri
  [] (let [correct ["pemutusan bagian tubuh untuk membentuk individu baru"
                    "bisa terjadi secara sengaja maupun tidak sengaja"
                    "sifat anak sama persis dengan induknya"
                    "pada koloni bakteri, terjadi pemisahan koloni yang besar menjadi koloni yang lebih kecil"
                    "terjadi di lingkungan yang menguntungkan"]
           wrong ["terjadi di lingkungan yang tidak menguntungkan"
                  "sifat anak berbeda dengan induknya"
                  "ukuran sel anak lebih besar dibandinkan sel induk"
                  "membelah menjadi dua individu baru"
                  "dinding sel akan membentuk lekukan sebelum membelah menjadi dua"
                  "membelah setiap 20 menit"
                  "sel induk membentuk tunas pada tubuhnya"]]
       (->> (for [benar correct
                  salah wrong
                  intro ["Reproduksi aseksual bakteri dan archaea dilakukan dengan tiga cara, yaitu pembelahan biner, pembentukan tunas, dan fragmentasi."
                         "Bakteri dan archaea melakukan tiga cara reproduksi aseksual, yaitu pembelahan biner, pembentukan tunas, dan fragmentasi."]
                  repeat (range 300)]
              (let [soal1 ["Pernyataan berikut ini yang sesuai dengan fragmentasi pada bakteri dan archaea adalah ...."
                           "Di bawah ini yang merupakan pernyataan benar tentang fragmentasi adalah ...."]
                    soal2 ["Pernyataan yang <u>salah</u> mengenai fragmentasi adalah ...."
                           "Pernyataan fragmentasi pada bakteri dan archaea yang <u>tidak</u> tepat adalah ...."]
                    soal (rand-nth (concat soal1 soal2))]
                {:intro intro
                 :soal soal
                 :pb (if (= (.contains soal1 soal) true)
                       benar salah)
                 :p1 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))
                 :p2 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))
                 :p3 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))}))
            filteranswers
            distinct
            shuffle
            (take 50))))

(defn bio-transformasi-bakteri
  [] (let [soal1 ["Pernyataan yang tepat mengenai transformasi adalah ...."
                  "Berikut ini yang merupakan proses transformasi yang benar adalah ...."]
           soal2 ["Di bawah ini yang <u>bukan</u> pernyataan tentang transformasi adalah ...."
                  "Pernyataan yang <u>tidak</u> tepat mengenai transformasi yaitu ...."]
           
           correct ["bakteri mendapatkan DNA lain dari lingkungan sekitarnya"
                    "bakteri mengambil DNA dari jenis bakteri lain yang masih berkerabat"
                    "sifat bakteri akan berubah karena terbentuk DNA rekombinan"
                    "merubah struktur DNA induk"
                    "hasil transformasi adalah sel rekombinan, yaitu kromosom yang mengandung DNA dari dua sel yang berbeda"
                    "terjadi di lingkungan yang kurang menguntungkan atau ekstrem"
                    "materi genetik yang diambil dapat melebur menjadi kromosom atau tidak melebur menjadi plasmid"]
           wrong ["sel anak memiliki sifat yang sama dengan sel induk"
                  "terjadi di lingkungan yang menguntungkan"
                  "sel induk membelah menjadi dua"
                  "DNA induk melakukan replikasi"
                  "dinding sel akan membentuk lekukan sebelum sel membelah menjadi dua"
                  "bakteri mendapatkan DNA dari bakteriofag yang sudah menginfeksi bakteri lain"]]
       (->> (for [benar correct
                  salah wrong
                  intro ["Bakteri dan archaea dapat melakukan pertukaran gen. Pertukaran gen dibagi menjadi tiga, yaitu transformasi, transduksi, dan konjugasi."
                         "Transformasi, transduksi, dan konjugasi adalah cara pertukaran gen pada bakteri dan archaea."]
                  repeat (range 300)]
              (let [soal (rand-nth (concat soal1 soal2))]
                {:intro intro
                 :soal soal
                 :pb (if (= (.contains soal1 soal) true)
                       benar salah)
                 :p1 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))
                 :p2 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))
                 :p3 (if (= (.contains soal1 soal) true)
                       (rand-nth wrong) (rand-nth correct))}))
            filteranswers
            distinct
            shuffle
            (take 40))))

(defn bio-transduksi-bakteri
  [] (let [soal1 ["Pernyataan yang benar mengenai proses transduksi adalah ...."
                  "Berikut ini yang merupakan pernyataan yang tepat tentang transduksi adalah ...."]
           soal2 ["Manakah pernyataan yang <u>salah</u> mengenai transduksi?"
                  "Peristiwa transduksi yang <u>tidak</u> tepat ditunjukkan oleh ...."]
           
           correct ["Bakteri mendapatkan DNA dari bakteriofag yang sudah menginfeksi bakteri lain."
                    "DNA virus dari sel bakteri pertama (donor) akan menyatu dengan DNA bakteri inang (resipien)."
                    "Bakteri bisa mati jika berlangsung dengan proses litik karena sel bakteri akan pecah."
                    "Terdapat dua proses, yaitu litik dan lisogenik."
                    "Bakteri masih bisa hidup dengan proses lisogenik dan akan terbentuk DNA rekombinan."
                    "Merubah struktur DNA induk."
                    "Terjadi di lingkungan yang kurang menguntungkan atau ekstrem."]
           wrong ["Sel anak memiliki sifat yang sama dengan sel induk."
                  "Terjadi di lingkungan yang menguntungkan."
                  "Sel induk membelah menjadi dua."
                  "Bakteri mendapatkan DNA lain dari lingkungan sekitarnya."
                  "Bakteri mengambil DNA dari jenis bakteri lain yang masih berkerabat."
                  "Pada koloni bakteri, terjadi pemisahan koloni yang besar menjadi koloni yang lebih kecil."
                  "Ukuran sel anak lebih besar dari sel induk."]
           option1 ["nomor 1 saja"
                    "nomor 1 dan 2"
                    "nomor 1 dan 3"
                    "nomor 2 saja"
                    "nomor 2 dan 3"
                    "nomor 3 saja"
                    "semua nomor benar"
                    "semua nomor salah"]
           option2 ["nomor 1 saja"
                    "nomor 1 dan 2"
                    "nomor 1 dan 3"
                    "nomor 2 saja"
                    "nomor 2 dan 3"
                    "nomor 3 saja"
                    "semua nomor salah"
                    "semua nomor benar"]
           pilihan (into [] (concat correct wrong))]
           (->> (for [intro ["Perhatikan pernyataan pertukaran gen dengan cara transduksi pada bakteri berikut ini!"
                             "Di bawah ini adalah pernyataan tentang transduksi pada bakteri."]
                      soal ["1" "2"]
                      repeat (range 300)]
                  (let [isi1 (rand-nth pilihan)
                        isi2 (rand-nth (remove #{isi1} pilihan))
                        isi3 (rand-nth (remove #{isi1} (remove #{isi2} pilihan)))
                        pilihanB (cond
                                   (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) false))
                                        (= (.contains correct isi3) false)) "nomor 1 saja"
                                   (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) true))
                                        (= (.contains correct isi3) false)) "nomor 2 saja"
                                   (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) false))
                                        (= (.contains correct isi3) true)) "nomor 3 saja"
                                   (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) true))
                                        (= (.contains correct isi3) false)) "nomor 1 dan 2"
                                   (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) false))
                                        (= (.contains correct isi3) true)) "nomor 1 dan 3"
                                   (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) true))
                                        (= (.contains correct isi3) true)) "nomor 2 dan 3"
                                   (and (and (= (.contains correct isi1) true) (= (.contains correct isi2) true))
                                        (= (.contains correct isi3) true)) "semua nomor benar"
                                   (and (and (= (.contains correct isi1) false) (= (.contains correct isi2) false))
                                        (= (.contains correct isi3) false)) "semua nomor salah")
                        pilihanB1 (cond
                                    (and (and (= (.contains wrong isi1) true) (= (.contains wrong isi2) false))
                                         (= (.contains wrong isi3) false)) "nomor 1 saja"
                                    (and (and (= (.contains wrong isi1) false) (= (.contains wrong isi2) true))
                                         (= (.contains wrong isi3) false)) "nomor 2 saja"
                                    (and (and (= (.contains wrong isi1) false) (= (.contains wrong isi2) false))
                                         (= (.contains wrong isi3) true)) "nomor 3 saja"
                                    (and (and (= (.contains wrong isi1) true) (= (.contains wrong isi2) true))
                                         (= (.contains wrong isi3) false)) "nomor 1 dan 2"
                                    (and (and (= (.contains wrong isi1) true) (= (.contains wrong isi2) false))
                                         (= (.contains wrong isi3) true)) "nomor 1 dan 3"
                                    (and (and (= (.contains wrong isi1) false) (= (.contains wrong isi2) true))
                                         (= (.contains wrong isi3) true)) "nomor 2 dan 3"
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
            (take 40))))

(defn bio-konjugasi-bakteri
  [] (let [correct ["Konjugasi adalah perpindahan DNA dari satu bakteri ke bakteri lainnya."
                    "Pada proses konjugasi, bakteri membentuk pilus (sejenis protein) sebagai jembatan materi genetik."
                    "Pilus pada proses konjugasi akan hilang ketika proses transfer DNA selesai."
                    "Konjugasi biasanya terjadi pada spesies yang sama."
                    "Hasil konjugasi akan membentuk DNA rekombinan."
                    "Konjugasi terjadi pada lingkungan yang kurang menguntungkan atau ekstrem."]
           wrong ["Konjugasi adalah kemampuan bakteri mendapatkan DNA lain dari lingkungan sekitarnya."
                  "Konjugasi adalah cara bakteri mendapatkan DNA dari bakteriofag."
                  "Konjugasi menghasilkan sel anakan yang sama persis dengan induknya."
                  "Konjugasi terjadi pada lingkungan yang menguntungkan."
                  "Konjugasi adalah reproduksi bakteri membelah menjadi dua."
                  "Konjugasi menghasilkan ukuran sel anak lebih besar dari sel induk."]
           intro ["Perhatikan pernyataan di bawah ini!"
                  "Berikut ini adalah pernyataan tentang konjugasi pada bakteri."]
           pilihan (concat correct wrong)
           opsi ["benar" "salah" "tidak tahu"]]
       (->> (for [soal ["Pernyataan tersebut benar atau salah?"
                        "Apakah pernyataan tersebut benar?"
                        "Pernyataan di atas adalah ...."]
                  q pilihan
                  re (range 100)]
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
            (take 30))))