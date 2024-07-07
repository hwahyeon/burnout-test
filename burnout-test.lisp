(load "utils.lisp")
(load "messages.lisp")

(defpackage :burnout-test
  (:use :cl :utils :messages)
  (:export :main))

(in-package :burnout-test)

(defun read-name ()
  "Prompt the user to enter their name and return it as a string. Default to 'Anonymous' if no input is provided."
  (read-input "Please enter your name: " "Anonymous"))

(defun load-questions (country-code)
  "Load the appropriate questions based on the country code."
  (let* ((package-suffix (case (intern country-code :keyword)
                           (:KR "kr")
                           (:GR "gr")
                           (:RS "rs")
                           (:MY "my")
                           (t "en")))
         (questions-path (format nil "questions/~a.lisp" package-suffix)))
    (load questions-path)
    (intern (string-upcase (format nil "questions-~a" package-suffix)) :keyword)))

(defun main ()
  "Main function for the burnout test program."
  (clear-screen)
  (print-welcome-message)
  (let ((name (read-name)))
    (clear-screen)
    (print-cbi-intro)
    (let* ((country-code (string-upcase (read-input "Please enter your country code: ")))
           (questions-package (load-questions country-code)))
      (format t "Personal Burnout Questions:~%")
      (let ((personal-responses (ask-questions (funcall (intern "PERSONAL-BURNOUT-QUESTIONS" questions-package)))))
        (format t "~&Work-related Burnout Questions:~%")
        (let ((work-responses (ask-questions (funcall (intern "WORK-RELATED-BURNOUT-QUESTIONS" questions-package)))))
          (format t "~&Client-related Burnout Questions:~%")
          (let ((client-responses (ask-questions (funcall (intern "CLIENT-RELATED-BURNOUT-QUESTIONS" questions-package)))))
            ;; Calculate average scores for each category
            (clear-screen)
            (let* ((personal-average (calculate-average personal-responses))
                   (work-average (calculate-average work-responses))
                   (client-average (calculate-average client-responses)))
              (print-test-results personal-average work-average client-average name)
              (press-any-key-to-exit))))))))

;; Code to create the executable
(sb-ext:save-lisp-and-die "burnout-test.exe"
                          :executable t
                          :toplevel 'burnout-test:main
                          :purify t)
