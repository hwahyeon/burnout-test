(defpackage :burnout-test
  (:use :common-lisp :capi))

(in-package :burnout-test)

(defun create-simple-window ()
  (capi:contain (make-instance 'capi:interface
                               :title "Burnout Test"
                               :layout (make-instance 'capi:column-layout
                                                      :description (list (make-instance 'capi:text-input-pane
                                                                                        :text "Welcome to the Burnout Test"))))))

(defun main ()
  (create-simple-window))

(main)
