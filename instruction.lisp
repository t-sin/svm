(in-package #:cl-user)
(defpackage #:svm-ins
  (:use #:cl)
  (:export #:+opcode-specs+))
(in-package #:svm-ins)

(defmacro defop (name () &body body))

(defop :add ()
  ((opr1 :type :int)
   (opr2 :type :int)))
