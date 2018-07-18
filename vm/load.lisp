(in-package #:cl-user)
(defpackage #:svm-vm/vm/load
  (:use #:cl)
  (:import-from #:svm-ins
                #:+opcode-specs+
                #:<instruction>-name
                #:<instruction>-opcode
                #:<instruction>-arity)
  (:import-from #:svm-vm/vm
                #:<vm>-dump-mem
                #:<vm>-memory
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

(defun encode-operand (operand)
  (ecase (getf operand :type)
    (:null nil)
    (:reg (encode-register (getf operand :value)))
    (:addr (getf operand :value))))

(defun encode-op (op)
  (let ((opcode (ash (<instruction>-opcode (getf op :op)) 2))
        (operand1 (or (encode-operand (getf op :opr1)) 0))
        (operand2 (or (encode-operand (getf op :opr2)) 0))
        (operand3 (or (encode-operand (getf op :opr3)) 0)))
    (ecase (<instruction>-arity (getf op :op))
      (0 (vector opcode 0 0))
      (1 (vector opcode (ash operand1 4) 0))
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
  (let ((encoded-data (loop
                        :named encode-data
                        :with vec := (make-array 0 :adjustable t :fill-pointer 0)
                        :for d :across (getf program :data)
                        :do (vector-push-extend (encode-data d) vec)
                        :finally (return-from encode-data vec)))
        (encoded-code (loop
                        :named encode-code
                        :with vec := (make-array 0 :adjustable t :fill-pointer 0)
                        :for op :across (getf program :code)
                        :do (vector-push-extend (encode-op op) vec)
                        :finally (return-from encode-code vec))))
    (let ((entry-point (apply #'+ (map 'list #'length encoded-data)))
          (code-size (apply #'+ (map 'list #'length encoded-code))))
      (setf (aref (aref encoded-data (gethash :EP (getf program :datamap))) 1) entry-point
            (aref (aref encoded-data (gethash :EOC (getf program :datamap))) 1) code-size)
      (setf (<vm>-pc vm) entry-point)
      (let ((addr 0))
        (flatten-walk (lambda (b) (progn (vm-write vm addr b) (incf addr)))
                      encoded-data encoded-code))))
  vm)
