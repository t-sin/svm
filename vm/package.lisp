(in-package #:cl-user)
(defpackage #:svm-vm
  (:use #:cl)
  (:import-from #:svm-vm/vm/virtual-machine
                #:make-vm*
                #:dump-vm
                #:load-program
                #:step-program
                #:run-program)
  (:export #:make-vm*
           #:dump-vm
           #:load-program
           #:step-program
           #:run-program))
(in-package #:svm-vm)
