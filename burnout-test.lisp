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
  (format t "    1. Never~%")
  (format t "    2. Rarely~%")
  (format t "    3. Sometimes~%")
  (format t "    4. Most of the time~%")
  (format t "    5. Always~%")
  (format t "====================================~%"))

(defun read-name ()
  "Prompt the user to enter their name and return it as a string."
  (format t "Please enter your name: ")
  (finish-output)
  (read-line))

(defun ask-question (question number)
  "Prompt the user with a question and return a valid numeric response."
  (loop
     (format t "~&~a~a~%    1. Never~%    2. Rarely~%    3. Sometimes~%    4. Most of the time~%    5. Always~%Your answer: " number question)
     (finish-output)
     (let ((response (read-line)))
       (if (and (every #'digit-char-p response)
                (let ((num (parse-integer response)))
                  (and (>= num 1) (<= num 5))))
           (return (parse-integer response))
           (progn
             (format t "~&Invalid input. Please enter a number between 1 and 5.~%")
             (finish-output))))))

(defun personal-burnout-questions ()
  "Ask personal burnout questions and return the responses."
  (list
   (ask-question "How often do you feel tired?" "1.")
   (ask-question "How often are you physically exhausted?" "2.")
   (ask-question "How often are you emotionally exhausted?" "3.")
   (ask-question "How often do you think: 'I can't take it anymore'?" "4.")
   (ask-question "How often do you feel worn out?" "5.")
   (ask-question "How often do you feel weak and susceptible to illness?" "6.")))

(defun work-related-burnout-questions ()
  "Ask work-related burnout questions and return the responses."
  (list
   (ask-question "Do you feel worn out at the end of the working day?" "7.")
   (ask-question "Are you exhausted in the morning at the thought of another day at work?" "8.")
   (ask-question "Do you feel that every working hour is tiring for you?" "9.")
   (ask-question "Do you have enough energy for family and friends during leisure time? (inverse scoring)" "10.")
   (ask-question "Is your work emotionally exhausting?" "11.")
   (ask-question "Does your work frustrate you?" "12.")
   (ask-question "Do you feel burnt out because of your work?" "13.")))

(defun client-related-burnout-questions ()
  "Ask client-related burnout questions and return the responses."
  (list
   (ask-question "Do you find it hard to work with clients?" "14.")
   (ask-question "Does it drain your energy to work with clients?" "15.")
   (ask-question "Do you find it frustrating to work with clients?" "16.")
   (ask-question "Do you feel that you give more than you get back when you work with clients?" "17.")
   (ask-question "Are you tired of working with clients?" "18.")
   (ask-question "Do you sometimes wonder how long you will be able to continue working with clients?" "19.")))

(defun calculate-average (responses)
  "Calculate the average score from the list of responses."
  (/ (reduce #'+ responses) (length responses)))

(defun interpret-cbi-score (score)
  "Interpret the CBI score and print the corresponding message."
  (cond
    ((<= score 1.49) "Very Low")
    ((<= score 2.49) "Low")
    ((<= score 3.49) "Moderate")
    ((<= score 4.49) "High")
    ((<= score 5) "Very High")
    (t "Invalid score")))

(defun main ()
  "Main function for the burnout test program."
  (print-welcome-message)
  (let ((name (read-name)))
    (format t "~&Hello, ~a! Welcome to the Burnout Test Program.~%~%" name)
    (format t "Personal Burnout Questions:~%")
    (let ((personal-responses (personal-burnout-questions)))
      (format t "~&Work-related Burnout Questions:~%")
      (let ((work-responses (work-related-burnout-questions)))
        (format t "~&Client-related Burnout Questions:~%")
        (let ((client-responses (client-related-burnout-questions)))
          ;; Calculate average scores for each category
          (let ((personal-average (calculate-average personal-responses))
                (work-average (calculate-average work-responses))
                (client-average (calculate-average client-responses)))
            (format t "~&Personal Burnout Average Score: ~a (~a)~%"
                    personal-average (interpret-cbi-score personal-average))
            (format t "Work-related Burnout Average Score: ~a (~a)~%"
                    work-average (interpret-cbi-score work-average))
            (format t "Client-related Burnout Average Score: ~a (~a)~%"
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