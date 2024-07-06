(load "utils.lisp")

(defpackage :burnout-test
  (:use :cl :utils)
  (:export :main))

(in-package :burnout-test)

(defun read-name ()
  "Prompt the user to enter their name and return it as a string. Default to 'Anonymous' if no input is provided."
  (read-input "Please enter your name: " "Anonymous"))

(defun main ()
  "Main function for the burnout test program."
  (clear-screen)
  (print-welcome-message)
  (let ((name (read-name)))
    (clear-screen)
    (print-cbi-intro)
    (let* ((country-code (string-upcase (read-input "Please enter your country code: ")))
           (package-suffix (case (intern country-code :keyword)
                            ;  (:DE "de")
                             (:KR "kr")
                             (:GR "gr")
                             (:RS "rs")
                             (:MY "my")
                             (t "en")))
           (questions-package (intern (string-upcase (format nil "questions-~a" package-suffix)) :keyword)))
      ;; Load the questions based on country code
      (load (format nil "questions/~a.lisp" package-suffix))
      (format t "~&Hello, ~a! Welcome to the Burnout Test Program.~%~%" name)
      (format t "Personal Burnout Questions:~%")
      (let ((personal-responses (ask-questions (funcall (intern (string-upcase "personal-burnout-questions") questions-package)))))
        (format t "~&Work-related Burnout Questions:~%")
        (let ((work-responses (ask-questions (funcall (intern (string-upcase "work-related-burnout-questions") questions-package)))))
          (format t "~&Client-related Burnout Questions:~%")
          (let ((client-responses (ask-questions (funcall (intern (string-upcase "client-related-burnout-questions") questions-package)))))
            ;; Calculate average scores for each category
            (clear-screen)
            (format t "==============================================~%")
            (format t "                 Test Results                 ~%")
            (format t "==============================================~%")
            (let ((personal-average (calculate-average personal-responses))
                  (work-average (calculate-average work-responses))
                  (client-average (calculate-average client-responses)))
              (format t "~&Personal Burnout Average Score: ~,2f (~a)~%"
                      personal-average (interpret-cbi-score personal-average))
              (format t "Work-related Burnout Average Score: ~,2f (~a)~%"
                      work-average (interpret-cbi-score work-average))
              (format t "Client-related Burnout Average Score: ~,2f (~a)~%"
                      client-average (interpret-cbi-score client-average)))
            (format t "~%")
            (format t "~&Thank you for completing the Burnout Test, ~a.~%~%" name)
            (format t "This test should not be used as a substitute for professional diagnosis.~%")
            (format t "For a comprehensive assessment, please consult a licensed psychologist, therapist, or other qualified healthcare provider.~%")
            (format t "~%")
            (format t "~%")
            (format t "Press any key to exit...")
            (finish-output)
            (read-line)))))))

;; Code to create the executable
(sb-ext:save-lisp-and-die "burnout-test.exe"
                          :executable t
                          :toplevel 'burnout-test:main
                          :purify t)
