(in-package #:cl-user)
(defpackage #:svm
  (:use #:cl)
  (:import-from #:svm-as
                #:read-asm
                #:validate-asm)
  (:import-from #:svm-load
                #:make-program)
  (:import-from #:svm-vm
                #:make-vm
                #:dump-vm
                #:load-program
                #:step-program
                #:run-program)
  (:import-from #:svm-vm/vm/simple-memory
                #:make-simple-memory
                #:access
                #:dump-simple-memory)
  (:export #:read-asm
           #:make-program
           #:make-vm
           #:dump-vm
           #:load-program
           #:step-program
           #:run-program

           #:make-memory*
           #:init-vm))
(in-package #:svm)

(defun make-memory* ()
  (list (make-simple-memory 1000)
        'access
        #'dump-simple-memory))

(defun init-vm (pathname)
  (let* ((program (with-open-file (in pathname)
                    (make-program (read-asm in))))
         (vm (apply #'make-vm (make-memory*))))
    (load-program program vm)
    (values program vm)))
