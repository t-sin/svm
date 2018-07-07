(in-package #:cl-user)
(defpackage #:svm-vm/vm/virtual-machine
  (:use #:cl)
  (:import-from #:svm-ins
                #:<instruction>-opcode
                #:<instruction>-arity)
  (:import-from #:svm-program
                #:<data>-type
                #:<data>-value
                #:<operation>-op
                #:<operation>-opr1
                #:<operation>-opr2
                #:<operation>-opr3
                #:<program>-data
                #:<program>-datamap
                #:<program>-code)
  (:export #:make-vm
           #:dump-vm
           #:load-program
           #:step-program
           #:run-program))
(in-package #:svm-vm/vm/virtual-machine)

(defstruct <vm>
  memory access-mem dump-mem
  pc r0 r1 r2 r3 r4 r5 r6)

(defun make-vm (memory mem-accessor mem-dumper
                &optional (pc 0) (r0 0) (r1 0) (r2 0) (r3 0)
                  (r4 0) (r5 0) (r6 0))
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
             :r6 r6))

(defun encode-data (data)
  (let ((val (<data>-value data)))
    (ecase (<data>-type data)
      (:label (vector #x00 (* val 3)))
      (:int (vector #x00 val))
      (:byte (vector #x00 val))
      (:bytes (vector #x01 (length val) val))
      (:char (apply #'vector #x02 (coerce (babel:string-to-octets (string val)) 'list)))
      (:str (apply #'vector #x03 (length val) (coerce (babel:string-to-octets val) 'list))))))

(defun encode-register (name)
  (let ((s (string-downcase (symbol-name name))))
    (or (and (char= (char s 0) #\r) (parse-integer (subseq s 1)))
        7)))

(defun encode-operand (operand datamap encoded-data)
  (labels ((calc-offset* (pos)
             (loop
               :for n :from 0 :below pos
               :with offset := 0
               :do (incf offset (length (aref encoded-data n)))
               :finally (return-from calc-offset* offset)))
           (calculate-offset ()
             (let ((pos (gethash (<data>-value operand) datamap)))
               (if pos (logior #b1000 (calc-offset* pos) 0)))))
    (ecase (<data>-type operand)
      (:null nil)
      (:reg (encode-register (<data>-value operand)))
      (:label (gethash (<data>-value operand) datamap))
      (:const (calculate-offset))
      (:int (calculate-offset))
      (:byte (calculate-offset))
      (:char (calculate-offset))
      (:strr (calculate-offset))
      (:addr (logand #b1000 (<data>-value operand))))))

(defun encode-op (op datamap encoded-data)
  (let ((opcode (<instruction>-opcode (<operation>-op op)))
        (operand1 (or (encode-operand (<operation>-opr1 op) datamap encoded-data) 0))
        (operand2 (or (encode-operand (<operation>-opr2 op) datamap encoded-data) 0))
        (operand3 (or (encode-operand (<operation>-opr3 op) datamap encoded-data) 0)))
    (ecase (<instruction>-arity (<operation>-op op))
      (0 (vector opcode 0 0))
      (1 (vector opcode
                 (ash (logand operand1 #x0f00) -8)
                 (logand operand1 #x0f00)))
      (2 (vector opcode operand1 operand2))
      (3 (vector opcode
                 (logior (ash operand1 4) operand2)
                 (ash operand3 4))))))


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

(defun vm-read (vm addr)
  (funcall (fdefinition (<vm>-access-mem vm))
           (<vm>-memory vm) addr))

(defun vm-write (vm addr byte)
  (funcall (fdefinition `(setf ,(<vm>-access-mem vm)))
           byte (<vm>-memory vm) addr))

(defun load-program (program vm)
  (let* ((encoded-data (loop
                         :named encode-data
                         :with vec := (make-array 0 :adjustable t :fill-pointer 0)
                         :for d :across (<program>-data program)
                         :do (vector-push-extend (encode-data d) vec)
                         :finally (return-from encode-data vec)))
         (encoded-code (let ((datamap (<program>-datamap program)))
                         (loop
                           :named encode-code
                           :with vec := (make-array 0 :adjustable t :fill-pointer 0)
                           :for op :across (<program>-code program)
                           :do (vector-push-extend (encode-op op datamap encoded-data) vec)
                           :finally (return-from encode-code vec)))))
    (print encoded-code)
    (let ((entry-point (apply #'+ (map 'list #'length encoded-data))))
      (setf (<vm>-pc vm) entry-point)
      (let ((addr 0))
        (flatten-walk (lambda (b) (progn (vm-write vm addr b) (incf addr)))
                      encoded-data encoded-code))))
  vm)

(defun dump-vm (vm))

(defun decode-op (vm)
  (let* ((opcb (prog1
                   (vm-read vm (<vm>-pc vm))
                 (incf (<vm>-pc vm))))
         (opb1 (prog1
                   (vm-read vm (<vm>-pc vm))
                 (incf (<vm>-pc vm))))
         (opb2 (prog1
                   (vm-read vm (<vm>-pc vm))
                 (incf (<vm>-pc vm)))))
    (let* ((opcode opcb)
           (oprand1 (ash (logand opb1 #b11110000) -4))
           (oprand2 (logand opb1 #b00001111))
           (oprand3 (ash (logand opb2 #b11110000) -4)))
      (values opcode oprand1 oprand2 oprand3))))

(defun step-program (vm)
  (multiple-value-bind (opcode operand1 operand2 operand3)
      (decode-op vm)
    nil))

(defun run-program (vm))
