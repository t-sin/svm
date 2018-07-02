(in-package #:cl-user)
(defpackage #:svm-vm/vm/virtual-machine
  (:use #:cl)
  (:import-from #:svm-program
                #:<data>-type
                #:<data>-value)
  (:export #:make-vm
           #:dump-vm
           #:load-program
           #:step-program
           #:run-program))
(in-package #:svm-vm/vm/virtual-machine)

(defstruct <vm>
  memory access-mem dump-mem
  pc r0 r1 r2 r3 r4 r5 r6 r7)

(defun make-vm (memory mem-accessor mem-dumper
                &optional (pc 0) (r0 0) (r1 0) (r2 0) (r3 0)
                  (r4 0) (r5 0) (r6 0) (r7 0))
  (make-<vm> :memory memory
             :access-mem mem-accessor
             :dump-mem mem-dumper
             :pc pc
             :r0 r0
             :r1 r1
             :r2 r2
             :r3 r3
             :r4 r4
             :r5 r5
             :r6 r6
             :r7 r7))

(defun encode-data (data)
  (let ((val (<data>-value data)))
    (ecase (<data>-type data)
      (:int (vector #x00 val))
      (:byte (vector #x00 val))
      (:bytes (vector #x01 (length val) val))
      (:char (apply #'vector #x02 (coerce (babel:string-to-octets (string val)) 'list)))
      (:str (apply #'vector #x03 (length val) (coerce (babel:string-to-octets val) 'list))))))

(defun load-program (program vm)
  (flet ((vm-read (addr)
           (funcall (fdefinition (<vm>-access-mem vm))
                    (<vm>-memory vm) addr))
         (vm-write (addr byte)
           (funcall (fdefinition `(setf ,(<vm>-access-mem vm)))
                    byte (<vm>-memory vm) addr)))
    vm))

(defun dump-vm (vm))

(defun step-program (vm))

(defun run-program (vm))
