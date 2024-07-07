(load "utils.lisp")
(load "messages.lisp")

(load "questions/en.lisp")
(load "questions/kr.lisp")
(load "questions/gr.lisp")
(load "questions/rs.lisp")
(load "questions/my.lisp")

(defpackage :burnout-test
  (:use :cl :utils :messages)
  (:export :main))

(in-package :burnout-test)

(defun read-name ()
  "Prompt the user to enter their name and return it as a string. Default to 'Anonymous' if no input is provided."
  (read-input "Please enter your name: " "Anonymous"))

(defun get-questions-package (country-code)
  "Get the questions package based on the country code."
  (intern (string-upcase (format nil "questions-~a"
    (if (member (string-upcase country-code) '("EN" "KR" "GR" "RS" "MY") :test 'string=)
        country-code
        "EN")))
        :keyword))

(defun ask-all-questions (questions-package)
  "Ask all categories of questions and return the responses."
  (format t "Personal Burnout Questions:~%")
  (let ((personal-responses (ask-questions (funcall (intern "PERSONAL-BURNOUT-QUESTIONS" questions-package)))))
    (format t "~&Work-related Burnout Questions:~%")
    (let ((work-responses (ask-questions (funcall (intern "WORK-RELATED-BURNOUT-QUESTIONS" questions-package)))))
      (format t "~&Client-related Burnout Questions:~%")
      (let ((client-responses (ask-questions (funcall (intern "CLIENT-RELATED-BURNOUT-QUESTIONS" questions-package)))))
        (values personal-responses work-responses client-responses)))))

(defun main ()
  "Main function for the burnout test program."
  (clear-screen)
  (print-welcome-message)
  (let ((name (read-name)))
    (clear-screen)
    (print-cbi-intro)
    (let* ((country-code (string-upcase (read-input "Please enter your country code: ")))
           (questions-package (get-questions-package country-code)))
      (multiple-value-bind (personal-responses work-responses client-responses)
          (ask-all-questions questions-package)
        ;; Calculate average scores for each category
        (clear-screen)
        (let* ((personal-average (calculate-average personal-responses))
               (work-average (calculate-average work-responses))
               (client-average (calculate-average client-responses)))
          (print-test-results personal-average work-average client-average name)
          (press-any-key-to-exit))))))

;; Code to create the executable
(sb-ext:save-lisp-and-die "burnout-test.exe"
                          :executable t
                          :toplevel 'burnout-test:main
                          :purify t)
