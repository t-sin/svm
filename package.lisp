(in-package #:cl-user)
(defpackage #:svm
  (:use #:cl)
  (:import-from #:svm-as
                #:read-asm)
  (:export #:read-asm))
(in-package #:svm)
