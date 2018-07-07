(in-package #:cl-user)
(defpackage #:svm-as
  (:use #:cl)
  (:import-from #:svm-as/as/read
                #:read-asm)
  (:import-from #:svm-as/as/validate
                #:validate-asm)
  (:import-from #:svm-as/as/program
                #:make-program)
  (:export #:read-asm
           #:validate-asm
           #:make-program))
(in-package #:svm-as)
