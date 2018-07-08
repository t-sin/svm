(in-package #:cl-user)
(defpackage #:svm-as
  (:use #:cl)
  (:import-from #:svm-as/as/read
                #:read-asm)
  (:import-from #:svm-as/as/validate
                #:validate-asm)
  (:export #:read-asm
           #:validate-asm))
(in-package #:svm-as)
