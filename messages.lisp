(defpackage :messages
  (:use :cl :utils)
  (:export :print-welcome-message :print-cbi-intro :print-test-results))

(in-package :messages)


(defun print-welcome-message ()
  "Prints the welcome message for the burnout test program."
  (format t "==============================================~%")
  (format t "             Burnout Test Program             ~%")
  (format t "==============================================~%")
  (format t "~%")
  (format t "CBI (Copenhagen Burnout Inventory) is a tool used to measure~%")
  (format t "burnout levels. It is divided into three categories:~%")
  (format t "  1. Personal Burnout~%")
  (format t "  2. Work-related Burnout~%")
  (format t "  3. Client-related Burnout~%")
  (format t "~%")
  (format t "Please answer the questions about your burnout level.~%")
  (format t "Respond with a number between 1 and 5 for each question:~%")
  (format t "  1. Never/almost never~%")
  (format t "  2. Seldom~%")
  (format t "  3. Sometimes~%")
  (format t "  4. Often~%")
  (format t "  5. Always~%")
  (format t "~%")
  (format t "Type 'back' to go to the previous question.~%")
  (format t "==============================================~%"))


(defun print-cbi-intro ()
  "Prints the introduction to the different versions of CBI available."
  (format t "==============================================~%")
  (format t "            Available CBI Versions            ~%")
  (format t "==============================================~%")
  (format t "Currently, this program offers the original CBI, a Korean version (K-CBI), a Greek version (CBI-gr), a Serbian version and a Malaysian version(CBI-M).~%")
  (format t "~%To proceed, enter:~%")
  ; (format t "- DE for the German version,~%")
  (format t "- KR for the Korean version,~%")
  (format t "- GR for the Greek version,~%")
  (format t "- RS for the Serbian version,~%")
  (format t "- MY for the Malaysian version,~%")
  (format t "- or press Enter for the original CBI.~%")
  (format t "==============================================~%"))


(defun print-test-results (personal-average work-average client-average name)
  "Prints the burnout test results."
  (format t "==============================================~%")
  (format t "                 Test Results                 ~%")
  (format t "==============================================~%")
  (format t "~&Personal Burnout Average Score: ~,2f (~a)~%"
          personal-average (interpret-cbi-score personal-average))
  (format t "Work-related Burnout Average Score: ~,2f (~a)~%"
          work-average (interpret-cbi-score work-average))
  (format t "Client-related Burnout Average Score: ~,2f (~a)~%"
          client-average (interpret-cbi-score client-average))
  (format t "~%")
  (format t "~&Thank you for completing the Burnout Test, ~a.~%~%" name)
  (format t "This test should not be used as a substitute for professional diagnosis.~%")
  (format t "For a comprehensive assessment, please consult a licensed psychologist, therapist, or other qualified healthcare provider.~%")
  (format t "~%")
  (format t "~%"))