(defpackage :utils
  (:use :cl)
  (:export :clear-screen :read-input :valid-response-p :print-welcome-message :print-cbi-intro :interpret-cbi-score :calculate-average :ask-question :ask-questions))

(in-package :utils)

(defun clear-screen ()
  "Clears the terminal screen."
  (format t "~c[2J~c[H" #\esc #\esc))


(defun read-input (prompt &optional (default ""))
  "Reads input from the user with a given prompt. Returns the input or a default value if the input is empty."
  (format t "~a" prompt)
  (finish-output)
  (let ((input (string-trim " " (read-line))))
    (if (string= input "") default input)))


(defun valid-response-p (response)
  "Check if the response is valid (between 1 and 5 or 'back')."
  (or (string= response "back")
      (and (every #'digit-char-p response)
           (let ((num (parse-integer response)))
             (and (>= num 1) (<= num 5))))))


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


(defun interpret-cbi-score (score)
  "Interpret the CBI score and print the corresponding message."
  (cond
    ((<= score 24) "Low")
    ((<= score 49) "Moderate")
    ((<= score 74) "High")
    ((<= score 100) "Very High")
    (t "Invalid score")))


(defun calculate-average (responses)
  "Calculate the average score from the list of responses."
  (/ (reduce #'+ responses) (length responses)))


(defun ask-question (question number &optional (inverse-scoring nil))
  "Prompt the user with a question and return a valid numeric response."
  (loop
     (clear-screen)
     (format t "~&~a~a~%    1. Never/almost never~%    2. Seldom~%    3. Sometimes~%    4. Often~%    5. Always~%Your answer (or type 'back' to go to the previous question): " number question)
     (finish-output)
     (let ((response (string-trim " " (read-line))))
       (if (valid-response-p response)
           (return (if (string= response "back")
                       :back
                       (let ((score (case (parse-integer response)
                                      (1 0)
                                      (2 25)
                                      (3 50)
                                      (4 75)
                                      (5 100))))
                         (if inverse-scoring
                             (- 100 score)
                             score))))
           (progn
             (format t "~&Invalid input. Please enter a number between 1 and 5, or type 'back' to go to the previous question.~%")
             (finish-output))))))


(defun ask-questions (questions)
  "Ask a series of questions and return the responses, supporting backtracking."
  (let ((responses (make-array (length questions) :initial-element nil))
        (i 0))
    (loop while (< i (length questions))
          for question-data = (nth i questions)
          for question = (first question-data)
          for number = (second question-data)
          for inverse-scoring = (third question-data)
          do (let ((response (ask-question question number inverse-scoring)))
               (if (eq response :back)
                   (when (> i 0)
                     (setf i (1- i)))
                   (progn
                     (setf (aref responses i) response)
                     (setf i (1+ i))))))
    (coerce responses 'list)))