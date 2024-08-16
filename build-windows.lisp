(load "burnout-test.lisp")

(in-package :burnout-test)

(sb-ext:save-lisp-and-die "burnout-test.exe"
                          :executable t
                          :toplevel 'burnout-test:main
                          :purify t)