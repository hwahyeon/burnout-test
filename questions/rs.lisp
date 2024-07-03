(defpackage :questions-rs
  (:use :cl)
  (:export :personal-burnout-questions :work-related-burnout-questions :client-related-burnout-questions))

(in-package :questions-rs)

(defun personal-burnout-questions ()
  "Return a list of personal burnout questions."
  (list (list "Koliko često se osećate umorno?" "1." nil)
        (list "Koliko često se osećate fizički iscpljeno?" "2." nil)
        (list "Koliko često se osećate emocionalno iscpljeno?" "3." nil)
        (list "Koliko često pomislite „Ne mogu više ovako“?" "4." nil)
        (list "Koliko često se osećate istrošeno?" "5." nil)
        (list "Koliko često se osećate slabo i podložno bolestima?" "6." nil)))

(defun work-related-burnout-questions ()
  "Return a list of work-related burnout questions."
  (list (list "Da li je Vaš posao emocionalno iscrpljući?" "7." nil)
        (list "Da li se osećate iscrpljeno zbog svog posla?" "8." nil)
        (list "Da li Vas Vaš posao frustrira?" "9." nil)
        (list "Da li se osećate istrošeno na kraju radnog dana?" "10." nil)
        (list "Da li se osećate iscpljeno ujutru pri pomisli na još jedan dan na poslu?" "11." nil)
        (list "Da li osećate da Vam je svaki radni sat naporan?" "12." nil)
        (list "Da li imate dovoljno vremena za porodicu i prijatelje nakon posla?" "13." t))) ;(inverse scoring)

(defun client-related-burnout-questions ()
  "Return a list of client-related burnout questions."
  (list (list "Da li smatrate da je teško raditi sa decom?" "14." nil)
        (list "Da li smatrate da je rad sa decom frustrirajući?" "15." nil)
        (list "Da li rad sa decom crpi Vašu energiju?" "16." nil)
        (list "Da li se osećate da više dajete nego što dobijate u radu sa decom?" "17." nil)
        (list "Da li ste umorni od rada sa decom?" "18." nil)
        (list "Da li se nekada pitate koliko ćete još biti u stanju da radite sa decom?" "19." nil)))
