(in-package #:cl-user)
(defpackage #:svm-as
  (:use #:cl)
  (:import-from #:svm-as/as/read
                #:read-asm)
  (:export #:read-asm))
(in-package #:svm-as)
