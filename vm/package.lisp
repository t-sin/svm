(in-package #:cl-user)
(defpackage #:svm-vm
  (:use #:cl)
  (:import-from #:svm-vm/vm
                #:make-vm)
  (:import-from #:svm-vm/vm/load
                #:load-program)
  (:import-from #:svm-vm/vm/run
                #:dump-vm
                #:step-program
                #:run-program)
  (:export #:make-vm
           #:dump-vm
           #:load-program
           #:step-program
           #:run-program))
(in-package #:svm-vm)
