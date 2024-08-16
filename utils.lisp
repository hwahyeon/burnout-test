(defpackage :utils
  (:use :cl)
  (:export :clear-screen :read-input :valid-response-p :calculate-average :ask-question :ask-questions :interpret-cbi-score :press-any-key-to-exit))

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
      (if (string= response "back")
          (return :back)
          (let* ((parsed-response (ignore-errors (parse-integer response :junk-allowed t)))
                 (valid (and (integerp parsed-response)
                              (<= 1 parsed-response 5)))
                 (score (case parsed-response
                          (1 0)
                          (2 25)
                          (3 50)
                          (4 75)
                          (5 100))))
            (if valid
                (return (if inverse-scoring
                            (- 100 score)
                            score))
                ;; If the input is invalid → loop again
                ))))))


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

(defun press-any-key-to-exit ()
  "Prompt the user to press any key to exit."
  (format t "Press any key to exit...")
  (finish-output)
  (read-line))