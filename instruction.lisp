(in-package #:cl-user)
(defpackage #:svm-ins
  (:use #:cl)
  (:export #:reg
           #:addr
           #:byte
           #:int
           #:+types+

           #:<operand>
           #:make-<operand>
           #:<operand>-p
           #:<operand>-name
           #:<operand>-types

           #:<instruction>
           #:make-<instruction>
           #:<instruction>-p
           #:<instruction>-name
           #:<instruction>-operands
           #:<instruction>-doc

           #:+opcode-specs+))
(in-package #:svm-ins)

(defvar +types+ '(:reg :addr :byte :int)
  "Types are available on String VM. There are several and few types are only here:

reg: register numbers. this can be possible as in 0 (program counter), 1~7 (general purpus register).
addr: memory address. this can be possible as inmmediate value as integer literal.
byte: unsigned numbers represented as 8-bit.
int: signed numbers represented as 32-bit.")

(defstruct <operand>
  name types)

(defmethod print-object ((o <operand>) s)
  (format s "<~a>" (<operand>-name o)))

(defstruct <instruction>
  name operands doc)

(defmethod print-object ((i <instruction>) s)
  (format s "<~a [~{~s~^ ~}]>" (<instruction>-name i) (<instruction>-operands i)))

(defparameter +opcode-specs+ nil)

(defun parse-operand (operand-def)
  (let* ((name (car operand-def))
         (attr (cdr operand-def)))
    (cond ((not (= (length operand-def) 3)) (error (format nil "malformed definition: ~s." operand-def)))
          ((not (symbolp name)) (error (format nil "operand name must be a symbol: ~s." name)))
          ((null (getf attr :type)) (error (format nil "operand type is not specified: ~s." attr)))
          (t (make-<operand> :name name :types (getf attr :type))))))

(defmacro defop (name () doc body)
  (let ((name (intern (symbol-name name) :keyword)))
    `(setf (getf +opcode-specs+ ,name)
           (make-<instruction> :name ,name
                               :operands (mapcar #'parse-operand ',body)
                               :doc ,doc))))

;;; arithmetic instructions

(defop add ()
  ""
  ((opr1 :type (:int :reg))
   (opr2 :type (:int :reg))))

(defop mul ()
  ""
  ((opr1 :type (:int :reg))
   (opr2 :type (:int :reg))))

(defop div ()
  ""
  ((opr1 :type (:int :reg))
   (opr2 :type (:int :reg))))

;;; memory access instructions

(defop load ()
  ""
  ((opr1 :type (:reg :addr))
   (opr2 :type :reg)))

(defop store ()
  ""
  ((opr2 :type :reg)
   (opr1 :type (:reg :addr))))

;;; flow controlling instructions

(defop ifeq ()
  ""
  ((cond :type :reg)
   (addr :type :addr)))

(defop ifneq ()
  ""
  ((cond :type :reg)
   (addr :type :addr)))

(defop jump ()
  ""
  ((addr :type :addr)))
