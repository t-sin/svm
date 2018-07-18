(in-package #:cl-user)
(defpackage #:svm-vm/vm
  (:use #:cl)
  (:export #:<vm>
           #:<vm>-memory
           #:<vm>-access-mem
           #:<vm>-dump-mem
           #:<vm>-pc
           #:<vm>-r0
           #:<vm>-r1
           #:<vm>-r2
           #:<vm>-r3
           #:<vm>-r4
           #:<vm>-r5
           #:<vm>-r6

           #:make-vm
           #:vm-read
           #:vm-write))
(in-package #:svm-vm/vm)

(defstruct <vm>
  memory access-mem dump-mem
  pc r0 r1 r2 r3 r4 r5 r6)

(defun make-vm (memory mem-accessor mem-dumper
                &optional (pc 0) (r0 0) (r1 0) (r2 0) (r3 0) (r4 0) (r5 0) (r6 0))
  (make-<vm> :memory memory :access-mem mem-accessor :dump-mem mem-dumper
             :pc pc :r0 r0 :r1 r1 :r2 r2 :r3 r3 :r4 r4 :r5 r5 :r6 r6))

(defun vm-read (vm addr)
  (funcall (fdefinition (<vm>-access-mem vm))
           (<vm>-memory vm) addr))

(defun vm-write (vm addr byte)
  (funcall (fdefinition `(setf ,(<vm>-access-mem vm)))
           byte (<vm>-memory vm) addr))
