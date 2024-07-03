(defpackage :questions-my
  (:use :cl)
  (:export :personal-burnout-questions :work-related-burnout-questions :client-related-burnout-questions))

(in-package :questions-my)

(defun personal-burnout-questions ()
  "Return a list of personal burnout questions."
  (list (list "Berapa kerap anda berasa letih?" "1(a)." nil)
        (list "Berapa kerap anda berasa letih secara fizikal?" "1(b)." nil)
        (list "Berapa kerap anda berasa letih secara emosi?" "1(c)." nil)
        (list "Berapa kerap anda berfikir: “Saya sudah tidak sanggup meneruskannya”?' " "1(d)." nil)
        (list "Berapa kerap anda berasa lesu?" "1(e)." nil)
        (list "Berapa kerap anda berasa lemah dan senang mendapat penyakit?" "1(f)." nil)))

(defun work-related-burnout-questions ()
  "Return a list of work-related burnout questions."
  (list (list "Adakah kerja anda meletihkan emosi anda?" "2(a)." nil)
        (list "Adakah anda berasa lesu upaya (burnout) disebabkan pekerjaan anda?" "2(b)." nil)
        (list "Adakah anda berasa kecewa dengan pekerjaan anda?" "2(c)." nil)
        (list "Adakah anda berasa lesu pada akhir hari bekerja?" "2(d)." nil)
        (list "Adakah anda keletihan pada waktu pagi apabila memikirkan sehari lagi di tempat kerja?" "2(e)." nil)
        (list "Adakah anda berasa setiap waktu bekerja memenatkan anda?" "2(f)." nil)))

(defun client-related-burnout-questions ()
  "Return a list of client-related burnout questions."
  (list (list "Adakah anda mengalami kesukaran untuk berurusan dengan klien?" "3(a)." nil)
        (list "Adakah anda berasa berurusan dengan klien mengecewakan?" "3(b)." nil)
        (list "Adakah bekerja dengan klien menghabiskan tenaga anda?" "3(c)." nil)
        (list "Apabila bekerja dengan klien, adakah anda berasa lebih banyak memberi daripada menerima?" "3(d)." nil)
        (list "Adakah anda bosan bekerja dengan klien?" "3(e)." nil)
        (list "Adakah anda tertanya-tanya sejauh mana anda mampu meneruskan urusan dengan klien?" "3(f)." nil)))
