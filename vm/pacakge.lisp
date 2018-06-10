(in-package #:cl-user)
(defpackage #:svm-vm
  (:use #:cl)
  (:import-from #:virtual-machine
                #:make-vm
                #:dump-vm
                #:step-program
                #:run-program)
  (:export #:make-vm
           #:dump-vm
           #:step-program
           #:run-program))
(in-package #:svm-vm)
