(in-package #:cl-user)
(defpackage #:svm-load
  (:use #:cl)
  (:import-from #:svm-load/load/program
                #:make-program)
  (:export #:make-program))
(in-package #:svm-load)
