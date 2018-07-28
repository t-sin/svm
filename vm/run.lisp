(in-package #:cl-user)
(defpackage #:svm-vm/vm/run
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
  (:export #:dump-vm
           #:step-program
           #:run-program))
(in-package #:svm-vm/vm/run)

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
        ((= byte 7) 'svm-vm/vm::pc)
        (t (intern (format nil "R~a" byte) :svm-vm/vm))))

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

      (:jump (setf (slot-value vm (decode-register 7))
                   (slot-value vm (decode-register operand1)))
             (print-op "jump ~s~%" operand1))
      (:ifzero (when (zerop (slot-value vm (decode-register operand2)))
                 (setf (slot-value vm (decode-register 7))
                       (slot-value vm (decode-register operand1))))
               (print-op "jump ~s when (zerop ~s)~%" operand1 operand2))
      (:ifeq (when (= (slot-value vm (decode-register operand2))
                      (slot-value vm (decode-register operand3)))
               (setf (slot-value vm (decode-register 7))
                     (slot-value vm (decode-register operand1))))
             (print-op "jump ~s when (eql ~s ~s)~%" operand1 operand2 operand3))
      (:ifneq (when (/= (slot-value vm (decode-register operand2))
                        (slot-value vm (decode-register operand3)))
                (setf (slot-value vm (decode-register 7))
                      (slot-value vm (decode-register operand1))))
              (print-op "jump ~s unless (not (eql ~s ~s)~%" operand1 operand2 operand3))

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
