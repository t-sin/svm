(in-package #:cl-user)
(defpackage #:svm-as/as/program
  (:use #:cl)
  (:import-from #:svm-ins
                #:reg
                #:addr
                #:byte
                #:int
                #:+types+

                #:<operand>
                #:<operand>-name
                #:<operand>-types

                #:<instruction>
                #:<instruction>-name
                #:<instruction>-operands
                #:<instruction>-doc

                #:+opcode-specs+)
  (:import-from #:svm-program
                #:<data>
                #:make-<data>
                #:<data>-type
                #:<data>-value

                #:<operation>
                #:make-<operation>
                #:<operation>-op
                #:<operation>-opr1
                #:<operation>-opr2
                #:<operation>-opr3

                #:<program>
                #:make-<program>
                #:<program>-data
                #:<program>-datamap
                #:<program>-code)
  (:export #:construct-program))
(in-package #:svm-as/as/program)

(defun get-type (str)
  (flet ((first-char= (ch) (char= (char str 0) ch))
         (last-char= (ch) (char= (char str (1- (length str))) ch)))
    (cond ((first-char= #\$) :reg)
          ((first-char= #\%) :addr)
          ((first-char= #\&) :const)
          ((and (first-char= #\") (last-char= #\") :str))
          (t :int))))

(defun internal-repr (str type)
  (ecase type
    (:reg (intern (subseq str 1) :keyword))
    (:addr (parse-integer (subseq str 1)))
    (:const (intern (string-upcase (subseq str 1)) :keyword))
    (:str (subseq str 1 (1- (length str))))
    (:byte (parse-integer str))
    (:int (parse-integer str))))

(defun construct-program (ast)
  (let ((data (make-array 0 :element-type '<data>
                          :adjustable t :fill-pointer 0))
        (name-pos-table (make-hash-table :test 'eq))
        (code (make-array 0 :element-type '<operation>
                          :adjustable t :fill-pointer 0)))
    (flet ((push-data (name value)
             (let* ((type (get-type value))
                    (dat (make-<data> :type type
                                      :value (internal-repr value type)))
                    (pos (vector-push-extend dat data)))
               (setf (gethash name name-pos-table) pos))))
      (loop
        :for (name value) :in (getf ast :data)
        :do (push-data name value))
      (values ast data name-pos-table code))))
