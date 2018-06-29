(in-package #:cl-user)
(defpackage #:svm-as/as/program
  (:use #:cl)
  (:export #:<constant>
           #:make-<constant>
           #:<constant>-type
           #:<constant>-value

           #:<operation>
           #:make-<operation>
           #:<operation>-op
           #:<operation>-opr1
           #:<operation>-opr2
           #:<operation>-opr3

           #:<program>
           #:make-<program>
           #:<program>-constants
           #:<program>-code))
(in-package #:svm-as/as/program)

(defstruct <constant>
  type value)

(defstruct <operation>
  op opr1 opr2 opr3)

(defstruct <program>
  constants code)
