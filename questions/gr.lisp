(defpackage :questions-gr
  (:use :cl)
  (:export :personal-burnout-questions :work-related-burnout-questions :client-related-burnout-questions))

(in-package :questions-gr)

(defun personal-burnout-questions ()
  "Return a list of personal burnout questions."
  (list (list "How often do you feel tired?" "1." nil)
        (list "How often are you physically exhausted?" "2." nil)
        (list "How often are you emotionally exhausted?" "3." nil)
        (list "How often do you think: 'I can't take it anymore'?" "4." nil)
        (list "How often do you feel worn out?" "5." nil)
        (list "How often do you feel weak and susceptible to illness?" "6." nil)))

(defun work-related-burnout-questions ()
  "Return a list of work-related burnout questions."
  (list (list "Do you feel worn out at the end of the working day?" "7." nil)
        (list "Are you exhausted in the morning at the thought of another day at work?" "8." nil)
        (list "Do you feel that every working hour is tiring for you?" "9." nil)
        (list "Do you have enough energy for family and friends during leisure time? (inverse scoring)" "10." t)
        (list "Is your work emotionally exhausting?" "11." nil)
        ; (list "Does your work frustrate you?" "12." nil)
        ; (list "Do you feel burnt out because of your work?" "13." nil)
        ))

(defun client-related-burnout-questions ()
  "Return a list of client-related burnout questions."
  (list (list "Do you find it hard to work with clients?" "14." nil)
        (list "Does it drain your energy to work with clients?" "15." nil)
        (list "Do you find it frustrating to work with clients?" "16." nil)
        ; (list "Do you feel that you give more than you get back when you work with clients?" "17." nil)
        (list "Are you tired of working with clients?" "18." nil)
        (list "Do you sometimes wonder how long you will be able to continue working with clients?" "19." nil)))
