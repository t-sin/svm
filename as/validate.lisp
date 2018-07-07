(in-package #:cl-user)
(defpackage #:svm-as/as/validate
  (:use #:cl)
  (:import-from #:svm-ins
                #:make-<operand>
                #:<operand>-p
                #:<operand>-name
                #:<operand>-types

                #:make-<instruction>
                #:<instruction>-p
                #:<instruction>-name
                #:<instruction>-operands
                #:<instruction>-doc

                #:+opcode-specs+)
  (:export #:validate-op
           #:validate-code
           #:validate-data
           #:validate-asm))
(in-package #:svm-as/as/validate)

;;; Note: It's very simple validation to focus implementing VM. I wanna write a VM.

(defun validate-integer (str)
  (and (not (zerop (length str)))
       (loop
         :for idx :from (if (char= (char str 0) #\-) 1 0) :below (length str)
         :always (member (char str idx) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                         :test #'char=))))

(defun validate-address (str)
  (and (not (zerop (length str)))
       (member (char str 0) '(#\% #\&) :test #'char=)
       (not (find-if (lambda (ch) (member ch '(#\space #\tab #\newline) :test #'char=))
                     str))))

(defun validate-register (str)
  (member str '("$pc" "$r0" "$r1" "$r2" "$r3" "$r4" "$r5" "$r6")
          :test #'string=))

(defun validate-operand (operand operand-spec)
  (flet ((validate (spec)
           (case spec
             (:int (validate-integer operand))
             (:reg (validate-register operand))
             (:addr (validate-address operand))
             (t (error (format nil "oops! what is ~s? Probably opcode definition has an error!" spec))))))
    (let ((optype (<operand>-types operand-spec)))
      (if (listp optype)
          (some #'validate optype)
          (validate optype)))))

(defun validate-op (operation)
  (let ((opc (car operation)))
    (let ((opspec (getf +opcode-specs+ opc)))
      (if (null opspec)
          (error (format nil "SVM does not know the opecode ~s." opc))
          (let ((oprspecs (<instruction>-operands opspec)))
            (if (/= (length (cdr operation))
                    (length oprspecs))
                (error (format nil "Too few or too much operands.~% definition: ~s~%actual: ~s~%"
                               oprspecs operation))
                (loop
                  :for opr :in (cdr operation)
                  :for oprspec :in oprspecs
                  :for result := (validate-operand opr oprspec)
                  :do (unless result
                        (error (format nil "~s validation failed at (~s ~s)." opc opr oprspec)))
                  :always result)))))))

(defun validate-code (code)
  (every #'validate-op code))

(defun validate-data (data) t)

(defun validate-asm (asm)
  (and (validate-data (getf asm :data))
       (validate-code (getf asm :code))))

