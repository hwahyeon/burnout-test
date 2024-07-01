(defpackage :burnout-test
  (:use :cl)
  (:export :main))

(in-package :burnout-test)

(defun print-welcome-message ()
  "Prints the welcome message for the burnout test program."
  (format t "====================================~%")
  (format t "        Burnout Test Program         ~%")
  (format t "====================================~%")
  (format t "~%Please answer the following 19 questions about your burnout level.~%")
  (format t "Respond with a number between 1 and 5 for each question:~%")
  (format t "    1. Never/almost never~%")
  (format t "    2. Seldom~%")
  (format t "    3. Sometimes~%")
  (format t "    4. Often~%")
  (format t "    5. Always~%")
  (format t "Type 'back' to go to the previous question.~%")
  (format t "====================================~%"))

(defun read-name ()
  "Prompt the user to enter their name and return it as a string. Default to 'Anonymous' if no input is provided."
  (format t "Please enter your name: ")
  (finish-output)
  (let ((name (string-trim " " (read-line))))
    (if (string= name "")
        "Anonymous"
        name)))

(defun clear-screen ()
  "Clears the terminal screen."
  (format t "~c[2J~c[H" #\esc #\esc))

(defun ask-question (question number &optional (inverse-scoring nil))
  "Prompt the user with a question and return a valid numeric response."
  (loop
     (clear-screen)
     (format t "~&~a~a~%    1. Never/almost never~%    2. Seldom~%    3. Sometimes~%    4. Often~%    5. Always~%Your answer (or type 'back' to go to the previous question): " number question)
     (finish-output)
     (let ((response (string-trim " " (read-line))))
       (cond
         ((string= response "back")
          (return-from ask-question :back))
         ((and (not (string= response ""))
               (every #'digit-char-p response)
               (let ((num (parse-integer response)))
                 (and (>= num 1) (<= num 5))))
          (return-from ask-question (let ((score (case (parse-integer response)
                                                   (1 0)
                                                   (2 25)
                                                   (3 50)
                                                   (4 75)
                                                   (5 100))))
                                      (if inverse-scoring
                                          (- 100 score)
                                          score))))
         (t
          (format t "~&Invalid input. Please enter a number between 1 and 5.~%")
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
        (list "Does your work frustrate you?" "12." nil)
        (list "Do you feel burnt out because of your work?" "13." nil)))

(defun client-related-burnout-questions ()
  "Return a list of client-related burnout questions."
  (list (list "Do you find it hard to work with clients?" "14." nil)
        (list "Does it drain your energy to work with clients?" "15." nil)
        (list "Do you find it frustrating to work with clients?" "16." nil)
        (list "Do you feel that you give more than you get back when you work with clients?" "17." nil)
        (list "Are you tired of working with clients?" "18." nil)
        (list "Do you sometimes wonder how long you will be able to continue working with clients?" "19." nil)))

(defun calculate-average (responses)
  "Calculate the average score from the list of responses."
  (/ (reduce #'+ responses) (length responses)))

(defun interpret-cbi-score (score)
  "Interpret the CBI score and print the corresponding message."
  (cond
    ((<= score 24) "Low")
    ((<= score 49) "Moderate")
    ((<= score 74) "High")
    ((<= score 100) "Very High")
    (t "Invalid score")))

(defun main ()
  "Main function for the burnout test program."
  (clear-screen)
  (print-welcome-message)
  (let ((name (read-name)))
    (format t "~&Hello, ~a! Welcome to the Burnout Test Program.~%~%" name)
    (format t "Personal Burnout Questions:~%")
    (let ((personal-responses (ask-questions (personal-burnout-questions))))
      (format t "~&Work-related Burnout Questions:~%")
      (let ((work-responses (ask-questions (work-related-burnout-questions))))
        (format t "~&Client-related Burnout Questions:~%")
        (let ((client-responses (ask-questions (client-related-burnout-questions))))
          ;; Calculate average scores for each category
          (let ((personal-average (calculate-average personal-responses))
                (work-average (calculate-average work-responses))
                (client-average (calculate-average client-responses)))
            (format t "~&Personal Burnout Average Score: ~,2f (~a)~%"
                    personal-average (interpret-cbi-score personal-average))
            (format t "Work-related Burnout Average Score: ~,2f (~a)~%"
                    work-average (interpret-cbi-score work-average))
            (format t "Client-related Burnout Average Score: ~,2f (~a)~%"
                    client-average (interpret-cbi-score client-average)))
          (format t "~&Thank you for completing the Burnout Test, ~a.~%~%" name)
          (format t "Press any key to exit...")
          (finish-output)
          (read-line))))))

;; Code to create the executable
(sb-ext:save-lisp-and-die "burnout-test.exe"
                          :executable t
                          :toplevel 'burnout-test:main
                          :purify t)
