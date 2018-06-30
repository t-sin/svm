(in-package #:cl-user)
(defpackage #:svm-program
  (:use #:cl)
  (:export #:<data>
           #:make-<data>
           #:<data>-type
           #:<data>-value

           #:<operation>
           #:make-<operation>
           #:<operation>-op
           #:<operation>-opr1
           #:<operation>-opr2
           #:<operation>-opr3

           #:<program>
           #:make-<program>
           #:<program>-data
           #:<program>-code))
(in-package #:svm-program)

(defstruct <data>
  type value)

(defstruct <operation>
  op opr1 opr2 opr3)

(defstruct <program>
  data code)
