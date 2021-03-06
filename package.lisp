(in-package #:cl-user)
(defpackage #:svm
  (:use #:cl)
  (:import-from #:svm-as
                #:read-asm
                #:validate-asm)
  (:import-from #:svm-ir
                #:make-ir)
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
           #:make-ir
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

(defun init-vm (asm)
  (let* ((ast (etypecase asm
                (pathname (with-open-file (in asm) (read-asm in)))
                (string (with-input-from-string (in asm) (read-asm in)))
                (stream (read-asm asm))))
         (program (make-ir ast))
         (vm (apply #'make-vm (make-memory*))))
    (load-program program vm)
    (values vm program)))
