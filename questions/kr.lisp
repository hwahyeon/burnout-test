(defpackage :questions-kr
  (:use :cl)
  (:export :personal-burnout-questions :work-related-burnout-questions :client-related-burnout-questions))

(in-package :questions-kr)

(defun personal-burnout-questions ()
  "Return a list of personal burnout questions."
  (list (list "얼마나 자주 피곤하다고 느끼십니까?" "1." nil)
        (list "얼마나 자주 육체적으로 지치십니까?" "2." nil)
        (list "얼마나 자주 정서적으로 지치십니까?" "3." nil)
        (list "얼마나 자주 '더 이상 못 참겠다고 생각하십니까?' " "4." nil)
        (list "얼마나 자주 녹초가 되었다고 느끼십니까?" "5." nil)
        (list "얼마나 자주 병에 걸릴 정도로 허약해졌다고 느끼십니까?" "6." nil)))

(defun work-related-burnout-questions ()
  "Return a list of work-related burnout questions."
  (list (list "일이 당신을 정서적으로 지치게 합니까?" "7." nil)
        (list "일이 때문에 녹초가 된 기분입니까?" "8." nil)
        (list "일이 당신을 좌절하게 합니까?" "9." nil)
        (list "근무를 마친 뒤에도 피곤하다고 느끼십니까?" "10." nil)
        (list "아침에 출근할 생각을 하면 지치십니까?" "11." nil)
        (list "근무시간 내내 피곤하다고 느끼십니까?" "12." nil)))

(defun client-related-burnout-questions ()
  "Return a list of client-related burnout questions."
  (list (list "클라이언트와 일하기 힘듭니까?" "13." nil)
        (list "클라이언트와의 일로 좌절감을 느끼십니까?" "14." nil)
        (list "클라이언트와의 일하기 에너지가 고갈됩니까?" "15." nil)
        (list "클라이언트에게 얻는 것보다 주는 것이 많다고 느끼십니까?" "16." nil)
        (list "클라이언트와의 일하는 데 지치십니까?" "17." nil)
        (list "얼마나 더 클라이언트와 일할 수 있을지 회의감이 드십니까?" "18." nil)))
