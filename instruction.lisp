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

           #:+opcode-specs+
           #:print-instructions))
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
  (format s "~s" (<operand>-types o)))

(defstruct <instruction>
  name opcode operand1 operand2 operand3 doc)

(defmethod print-object ((i <instruction>) s)
  (format s "<~x:~a ~s ~s ~s]>"
          (<instruction>-opcode i) (<instruction>-name i)
          (<instruction>-operand1 i)
          (<instruction>-operand2 i)
          (<instruction>-operand3 i)))

(defparameter +opcode-specs+ nil)

(defun parse-operand (operand-def)
  (let* ((name (car operand-def))
         (attr (cdr operand-def)))
    (cond ((not (= (length operand-def) 3)) (error (format nil "malformed definition: ~s." operand-def)))
          ((not (symbolp name)) (error (format nil "operand name must be a symbol: ~s." name)))
          ((null (getf attr :type)) (error (format nil "operand type is not specified: ~s." attr)))
          (t (make-<operand> :name name :types (getf attr :type))))))

(defmacro defop ((name opcode) doc &optional body)
  (let ((name (intern (symbol-name name) :keyword)))
    `(push (make-<instruction> :name ,name
                               :opcode ,opcode
                               ,@(loop
                                   :for operand-def :in body
                                   :for n :from 1 :upto (length body)
                                   :append (list (intern (format nil "OPERAND~a" n) :keyword)
                                                 `(parse-operand ',operand-def)))
                               :doc ,doc)
           +opcode-specs+)))


(defun print-instructions ()
  (flet ((print-ins (ins)
           (format t "; ~a (0x~2,'0x) ~s~%    ~a~%"
                   (<instruction>-name ins) (<instruction>-opcode ins)
                   (<instruction>-operands ins)
                   (<instruction>-doc ins))))
    (mapcan #'print-ins +opcode-specs+)))


;;; miscellenous operations

(defop (nop #x00)
  "")

(defop (hw #x01)
  "Hellow SVM world!")

;;; memory access instructions

(defop (load #x04)
  ""
  ((opr1 :type (:reg :addr :int))
   (opr2 :type :reg)))

(defop (store #x05)
  ""
  ((opr2 :type :reg)
   (opr1 :type (:reg :addr))))

;;; flow controlling instructions

(defop (ifeq #x08)
  ""
  ((cond :type :reg)
   (addr :type :addr)))

(defop (ifneq #x09)
  ""
  ((cond :type :reg)
   (addr :type :addr)))

(defop (jump #x0a)
  ""
  ((addr :type :addr)))

;;; arithmatic instructions

(defop (shl #x10)
  ""
  ((opr :type (:int :ref))
   (res :type (:reg))))

(defop (shr #x11)
  ""
  ((opr :type (:int :ref))
   (res :type (:reg))))

(defop (add #x12)
  ""
  ((opr1 :type (:int :reg))
   (opr2 :type (:int :reg))
   (res :type :reg)))

(defop (mul #x13)
  ""
  ((opr1 :type (:int :reg))
   (opr2 :type (:int :reg))
   (res :type :reg)))

(defop (div #x14)
  ""
  ((opr1 :type (:int :reg))
   (opr2 :type (:int :reg))
   (res :type :reg)))
