(in-package #:cl-user)
(defpackage #:svm
  (:use #:cl)
  (:import-from #:svm-as
                #:read-asm
                #:validate-asm)
  (:import-from #:svm-vm
                #:make-vm*
                #:dump-vm
                #:step-program
                #:run-program)
  (:export #:read-asm
           #:make-vm*
           #:dump-vm
           #:step-program
           #:run-program))
(in-package #:svm)
