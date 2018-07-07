(in-package #:cl-user)
(defpackage #:svm
  (:use #:cl)
  (:import-from #:svm-as
                #:read-asm
                #:validate-asm
                #:make-program)
  (:import-from #:svm-vm
                #:make-vm
                #:dump-vm
                #:load-program
                #:step-program
                #:run-program)
  (:export #:read-asm
           #:make-program
           #:make-vm
           #:dump-vm
           #:load-program
           #:step-program
           #:run-program))
(in-package #:svm)
