(in-package #:cl-user)
(defpackage #:svm-vm/vm/virtual-machine
  (:use #:cl)
  (:export #:make-vm
           #:dump-vm
           #:step-program
           #:run-program))
(in-package #:svm-vm/vm/virtual-machine)


(defstruct vm
  memory access-mem dump-mem
  pc r0 r1 r2 r3 r4 r5 r6 r7)

(defun dump-vm (vm))

(defun step-program (vm))

(defun run-program (vm))
