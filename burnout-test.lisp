(defpackage :burnout-test
  (:use :cl)
  (:export :main))

(in-package :burnout-test)

(defun print-welcome-message ()
  "Prints the welcome message for the burnout test program."
  (format t "====================================~%")
  (format t "        Burnout Test Program         ~%")
  (format t "====================================~%"))

(defun read-name ()
  "Prompt the user to enter their name and return it as a string."
  (format t "Please enter your name: ")
  (finish-output)
  (read-line))

(defun ask-question (question)
  "Prompt the user with a question and return the response."
  (format t "~a~%" question)
  (finish-output)
  (read-line))

(defun personal-burnout-questions ()
  "Ask personal burnout questions and return the responses."
  (list
   (ask-question "1. How often do you feel tired?")
   (ask-question "2. How often are you physically exhausted?")
   (ask-question "3. How often are you emotionally exhausted?")
   (ask-question "4. How often do you think: 'I can't take it anymore'?")
   (ask-question "5. How often do you feel worn out?")
   (ask-question "6. How often do you feel weak and susceptible to illness?")))

(defun work-related-burnout-questions ()
  "Ask work-related burnout questions and return the responses."
  (list
   (ask-question "1. Is your work emotionally exhausting?")
   (ask-question "2. Do you feel burnt out because of your work?")
   (ask-question "3. Does your work frustrate you?")
   (ask-question "4. Do you feel worn out at the end of the working day?")
   (ask-question "5. Are you exhausted in the morning at the thought of another day at work?")
   (ask-question "6. Do you feel that every working hour is tiring for you?")
   (ask-question "7. Do you have enough energy for family and friends during leisure time?")))

(defun client-related-burnout-questions ()
  "Ask client-related burnout questions and return the responses."
  (list
   (ask-question "1. Do you find it hard to work with clients?")
   (ask-question "2. Does it drain your energy to work with clients?")
   (ask-question "3. Do you find it frustrating to work with clients?")
   (ask-question "4. Do you feel that you give more than you get back when you work with clients?")
   (ask-question "5. Are you tired of working with clients?")
   (ask-question "6. Do you sometimes wonder how long you will be able to continue working with clients?")))

(defun main ()
  "Main function for the burnout test program."
  (print-welcome-message)
  (let ((name (read-name)))
    (format t "Hello, ~a! Welcome to the Burnout Test Program.~%~%" name)
    (format t "Personal Burnout Questions:~%")
    (let ((personal-responses (personal-burnout-questions)))
      (format t "Work-related Burnout Questions:~%")
      (let ((work-responses (work-related-burnout-questions)))
        (format t "Client-related Burnout Questions:~%")
        (let ((client-responses (client-related-burnout-questions)))
          (format t "Thank you for completing the Burnout Test, ~a.~%~%" name)
          (format t "Press any key to exit...")
          (finish-output)
          (read-line))))))

(sb-ext:save-lisp-and-die "burnout-test.exe"
                          :executable t
                          :toplevel 'burnout-test:main
                          :purify t)