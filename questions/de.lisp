(defpackage :questions-de
  (:use :cl)
  (:export :personal-burnout-questions :work-related-burnout-questions :client-related-burnout-questions))

(in-package :questions-de)

(defun personal-burnout-questions ()
  "Return a list of personal burnout questions."
  (list (list "Wie oft fühlen Sie sich müde?" "1." nil)
        (list "Wie oft sind Sie körperlich erschöpft?" "2." nil)
        (list "Wie oft sind Sie emotional erschöpft?" "3." nil)
        (list "Wie oft denken Sie: 'Ich halte es nicht mehr aus'?" "4." nil)
        (list "Wie oft fühlen Sie sich ausgelaugt?" "5." nil)
        (list "Wie oft fühlen Sie sich geschwächt und anfällig krank zu werden?" "6." nil)))

(defun work-related-burnout-questions ()
  "Return a list of work-related burnout questions."
  (list (list "Fühlen Sie sich am Ende eines Arbeitstages ausgelaugt?" "7." nil)
        (list "Sind Sie am Morgen schon beim Gedanken an den bevorstehenden Arbeitstag erschöpft?" "8." nil)
        (list "Empfinden Sie jede Arbeitsstunde als ermüdend?" "9." nil)
        (list "Haben Sie genügend Energie für Familie und Freunde in Ihrer Freizeit?" "10." t)
        (list "Ist Ihre Arbeit emotional erschöpfend?" "11." nil)
        (list "Frustriert Sie Ihre Arbeit?" "12." nil)
        (list "Fühlen Sie sich aufgrund Ihrer Arbeit ausgebrannt?" "13." nil)))

(defun client-related-burnout-questions ()
  "Return a list of client-related burnout questions."
  (list (list "Fällt es Ihnen schwer, mit Klienten zu arbeiten?" "14." nil)
        (list "Finden Sie es frustrierend, mit Klienten zu arbeiten?" "15." nil)
        (list "Raubt es Ihnen Ihre Energie, mit Klienten zu arbeiten?" "16." nil)
        (list "Haben Sie das Gefühl, dass Sie mehr geben als Sie zurückbekommen, wenn Sie mit Klienten arbeiten?" "17." nil)
        (list "Ermüdet es Sie, mit Klienten zu arbeiten?" "18." nil)
        (list "Fragen Sie sich, wie lange Sie es noch schaffen werden, mit Ihren Klienten zu arbeiten?" "19." nil)))
