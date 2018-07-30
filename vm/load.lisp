(in-package #:cl-user)
(defpackage #:svm-vm/vm/load
  (:use #:cl)
  (:import-from #:svm-ins
                #:+opcode-specs+
                #:<instruction>-name
                #:<instruction>-opcode
                #:<instruction>-arity)
  (:import-from #:svm-vm/vm
                #:<vm>-pc
                #:vm-read
                #:vm-write)
  (:export #:load-program))
(in-package #:svm-vm/vm/load)

(defun encode-data (data)
  (let ((val (getf data :value)))
    (ecase (getf data :type)
      (:label (vector #x00 val))
      (:addr (vector #x00 val))
      (:int (vector #x00 val))
      (:byte (vector #x00 val))
      (:bytes (vector #x01 (length val) val))
      (:char (apply #'vector #x02 (coerce (babel:string-to-octets (string val)) 'list)))
      (:str (apply #'vector #x03 (length val) (coerce (babel:string-to-octets val) 'list))))))

(defun encode-register (name)
  (let ((s (string-downcase (symbol-name name))))
    (or (and (char= (char s 0) #\r) (parse-integer (subseq s 1)))
        7)))

(defun encode-operand (operand op program)
  (ecase (getf operand :type)
    (:null nil)
    (:const (gethash (getf operand :value) (getf program :datamap2)))
    (:label (if (eq (<instruction>-name (getf op :op)) :load)
                (gethash (getf operand :value) (getf program :datamap2))
                (encode-register :r6)))
    (:reg (encode-register (getf operand :value)))
    (:addr (getf operand :value))))

(defun encode-op (op program)
  (let ((opcode (ash (<instruction>-opcode (getf op :op)) 2))
        (operand1 (or (encode-operand (getf op :opr1) op program) 0))
        (operand2 (or (encode-operand (getf op :opr2) op program) 0))
        (operand3 (or (encode-operand (getf op :opr3) op program) 0)))
    (ecase (<instruction>-arity (getf op :op))
      (0 (vector opcode 0 0))
      (1 (vector opcode operand1 0))
      (2 (vector opcode operand1 operand2))
      (3 (vector opcode
                 (logand operand1 #x0f)
                 (logior (ash operand2 4) operand3))))))

(defun flatten-walk (function &rest vectors)
  (loop
    :for encoded :in vectors
    :do (loop
          :for bytes :across encoded
          :do (loop
                :for byte :across bytes
                :do (assert (<= byte #xff)
                            (byte) "Element on the memory must be a byte but ~s." byte)
                :do (funcall function byte)))))

(defun load-program (program vm)
  (let ((encoded-data (make-array 0 :adjustable t :fill-pointer 0))
        (encoded-code (make-array 0 :adjustable t :fill-pointer 0))
        (datamap2 (make-hash-table))
        (entry-point 0)
        (code-size 0))
    (loop
      :for d :across (getf program :data)
      :do (vector-push-extend (encode-data d) encoded-data))
    (setf entry-point (apply #'+ (map 'list #'length encoded-data)))

    (loop
      :for k :being :each :hash-keys :of (getf program :datamap) :using (hash-value v)
      :do (assert (<= v (length encoded-data)))
      :do (setf (gethash k datamap2)
                (loop :for n :from 0 :below v :sum (length (aref encoded-data n)))))
    (setf (getf program :datamap2) datamap2)

    (loop
      :for op :across (getf program :code)
      :do (vector-push-extend (encode-op op program) encoded-code))
    (setf code-size (apply #'+ (map 'list #'length encoded-code)))

      (loop
        :for k :being :each :hash-keys :of (getf program :datamap) :using (hash-value v)
        :when (let ((name (symbol-name k)))
                (and (char= (char name 0) #\:)
                     (char= (char name (1- (length name))) #\:)))
        :do (let ((addr (aref (aref encoded-data v) 1)))
              (setf (aref (aref encoded-data v) 1) (+ entry-point (* 3 addr)))))

      (setf (aref (aref encoded-data (gethash :EP (getf program :datamap))) 1) entry-point
            (aref (aref encoded-data (gethash :EOC (getf program :datamap))) 1) code-size)
      (setf (<vm>-pc vm) entry-point)
      (let ((addr 0))
        (flatten-walk (lambda (b) (progn (vm-write vm addr b) (incf addr)))
                      encoded-data encoded-code)))
  vm)
