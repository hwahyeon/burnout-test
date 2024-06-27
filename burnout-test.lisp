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

(defun main ()
  "Main function for the burnout test program."
  (print-welcome-message)
  (let ((name (read-name)))
    (format t "Hello, ~a! Welcome to the Burnout Test Program.~%~%" name)
    ;; burn out test start
    (format t "Press any key to exit...")
    (finish-output)
    (read-line)))  ;;

;; create exe file
(sb-ext:save-lisp-and-die "burnout-test.exe"
                          :executable t
                          :toplevel 'burnout-test:main
                          :purify t)