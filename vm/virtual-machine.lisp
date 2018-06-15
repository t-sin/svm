(in-package #:cl-user)
(defpackage #:svm-vm/vm/virtual-machine
  (:use #:cl)
  (:export #:make-vm*
           #:dump-vm
           #:step-program
           #:run-program))
(in-package #:svm-vm/vm/virtual-machine)

(defstruct vm
  "String Virtual Machine.

# Memory

SVM has a memory segmented as bytes. The memory holds data as a single or multiple bytes.

Any things can be a memory satisfies some requirements. These requriments are belows:

- can be created as single data structure
- can read/write a byte through accessor function
- can dump through stringify the memory (to debugging or to seriarizing)

# Data representation

All data in SVM have each representation, *based on bytes*.

The smallest data is a byte, but some data types are needed multiple bytes because of single
byte is too small to express. Multiple-bytes data are coded with ordering **little endian**
-- least significant byte are placed as left, and most significant byte are placed at left.

## General format

- data table
- code area

## Binary instruction format

- word length

"
  memory access-mem dump-mem
  pc r0 r1 r2 r3 r4 r5 r6 r7)

(defun make-vm* (memory mem-accesser mem-dumper
                &optional (pc 0) (r0 0) (r1 0) (r2 0) (r3 0)
                          (r4 0) (r5 0) (r6 0) (r7 0))
  (make-vm :memory memory
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

(defun dump-vm (vm))

(defun step-program (vm))

(defun run-program (vm))
