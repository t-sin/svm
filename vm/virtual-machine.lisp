(in-package #:cl-user)
(defpackage #:svm-vm/vm/virtual-machine
  (:use #:cl)
  (:import-from #:svm-ins
                #:+opcode-specs+
                #:<instruction>-name
                #:<instruction>-opcode
                #:<instruction>-arity)
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
                &optional (pc 0) (r0 0) (r1 0) (r2 0) (r3 0) (r4 0) (r5 0) (r6 0))
  (make-<vm> :memory memory :access-mem mem-accessor :dump-mem mem-dumper
             :pc pc :r0 r0 :r1 r1 :r2 r2 :r3 r3 :r4 r4 :r5 r5 :r6 r6))

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

(defun vm-read (vm addr)
  (funcall (fdefinition (<vm>-access-mem vm))
           (<vm>-memory vm) addr))

(defun vm-write (vm addr byte)
  (funcall (fdefinition `(setf ,(<vm>-access-mem vm)))
           byte (<vm>-memory vm) addr))

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

(defun dump-vm (vm)
  (funcall (<vm>-dump-mem vm) vm))

(defun decode-data (vm addr)
  (let ((type (vm-read vm addr)))
    (ecase type
      (0 (vm-read vm (1+ addr)))  ; byte
      (1 (let ((len (vm-read vm (1+ addr))))  ; bytes
                (loop
                  :for n :from (+ addr 2) :below (+ addr 2 len)
                  :collect (vm-read vm n))))
      (2 (error ":char is not implemented!"))  ; char
      (3 (error ":str is not implemented!")))))  ; str

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
    (let* ((opcode (ash opcb -2))
           (ins (find opcode +opcode-specs+ :key #'<instruction>-opcode)))
      (ecase (<instruction>-arity ins)
        (0 (values ins nil nil nil))
        (1 (values ins opb1 nil nil))
        (2 (values ins opb1 opb2 nil))
        (3 (values ins (logand opb1 #x0f)
                   (ash (logand opb2 #xf0) -4)
                   (logand opb2 #x0f)))))))

(defun decode-register (byte)
  (cond ((or (< byte 0) (<= 8 byte)) (error "invalid register name `~s`" byte))
        ((= byte 7) 'svm-vm/vm/virtual-machine::pc)
        (t (intern (format nil "R~a" byte) :svm-vm/vm/virtual-machine))))

(defparameter *print-op* nil)
(defun print-op (target &rest args)
  (when *print-op*
    (apply #'format t target args)))

(defun step-program (vm)
  (multiple-value-bind (ins operand1 operand2 operand3)
      (decode-op vm)
    (ecase (<instruction>-name ins)
      (:nop (print-op "do notiong~%"))
      (:exit (print-op "exit.~%")
             (values nil :exit))
      (:hw (format t "hello world!~%"))

      (:load (print-op "load ~s into ~s~%" operand1 operand2)
             (setf (slot-value vm (decode-register operand2)) (decode-data vm operand1)))
      (:store (vm-write vm operand2 (slot-value vm (decode-register operand1)))
              (print-op "store ~s into ~s~%" operand1 operand2))
      (:move (print-op "move ~s to ~s~%" operand1 operand2)
             (setf (slot-value vm (decode-register operand2))
                   (slot-value vm (decode-register operand1))))

      (:jump (print-op "jump ~s~%" operand1))
      (:ifzero (when (zerop (slot-value vm (decode-register operand1)))
                 (setf (slot-value vm (decode-register 7))
                       (slot-value vm (decode-register operand2))))
               (print-op "jump ~s when (zerop ~s)~%" operand2 operand1))
      (:ifeq (when (= (slot-value vm (decode-register operand1))
                      (slot-value vm (decode-register operand2)))
               (setf (slot-value vm (decode-register 7))
                     (slot-value vm (decode-register operand3))))
             (print-op "jump ~s when (eql ~s ~s)~%" operand3 operand1 operand2))
      (:ifneq (when (/= (slot-value vm (decode-register operand1))
                        (slot-value vm (decode-register operand2)))
                (setf (slot-value vm (decode-register 7))
                      (slot-value vm (decode-register operand3))))
              (print-op "jump ~s unless (not (eql ~s ~s)~%" operand3 operand1 operand2))

      (:shl (print-op "(ash ~s ~s) and store into ~s~%" operand1 operand2 operand3))
      (:shr (print-op "(ash ~s -~s) and store into ~s~%" operand1 operand2 operand3))
      (:add (print-op "add ~s to ~s and store into ~s~%" operand1 operand2 operand3)
            (setf (slot-value vm (decode-register operand3))
                  (+ (slot-value vm (decode-register operand1))
                     (slot-value vm (decode-register operand2)))))
      (:mul (print-op "multiply ~s with ~s and store into ~s~%" operand1 operand2 operand3)
            (setf (slot-value vm (decode-register operand3))
                  (* (slot-value vm (decode-register operand1))
                     (slot-value vm (decode-register operand2)))))
      (:div (print-op "divide ~s by ~s and store into ~s~%" operand1 operand2 operand3)
            (setf (slot-value vm (decode-register operand3))
                  (/ (slot-value vm (decode-register operand1))
                     (slot-value vm (decode-register operand2))))))))

(defparameter *print-register* nil)
(defparameter *print-memory* nil)

(defun print-vm (vm)
  (when *print-memory*
    (format t "; mem: ~s~%" (<vm>-memory vm)))
  (when *print-register*
    (format t "; reg: ")
    (mapcan (lambda (b)
              (let ((reg (decode-register b)))
                (format t "~a: ~s, " (symbol-name reg) (slot-value vm reg))))
            '(7 0 1 2 3 4 5 6)))
  (when (or *print-memory* *print-register*)
    (terpri)))

(defun run-program (vm)
  (print-vm vm)
  (loop
    (multiple-value-bind (val status)
        (step-program vm)
      (print-vm vm)
      (when (eq status :exit)
        (return-from run-program val)))))
