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
    (cond ((null str) :null)
          ((first-char= #\$) :reg)
          ((first-char= #\&) :addr)
          ((first-char= #\%) :const)
          ((and (first-char= #\") (last-char= #\") :str))
          (t :int))))

(defun internal-repr (str type)
  (ecase type
    (:null nil)
    (:reg (intern (string-upcase (subseq str 1)) :keyword))
    (:addr (parse-integer (subseq str 1)))
    (:const (intern (string-upcase (subseq str 1)) :keyword))
    (:str (subseq str 1 (1- (length str))))
    (:byte (parse-integer str))
    (:int (parse-integer str))))

(defparameter *name-count* 0)

(defun construct-program (ast)
  (let ((data (make-array 0 :element-type '<data>
                          :adjustable t :fill-pointer 0))
        (datamap (make-hash-table :test 'eq))
        (code (make-array 0 :element-type '<operation>
                          :adjustable t :fill-pointer 0)))
    (flet ((push-data (name value)
             (let* ((type (get-type value))
                    (dat (make-<data> :type type
                                      :value (internal-repr value type)))
                    (pos (vector-push-extend dat data)))
               (setf (gethash name datamap) pos))))
      (loop
        :for (name value) :in (getf ast :data)
        :do (push-data name value)))
    (flet ((parse-and-push-operand (str)
             (let* ((type (get-type str))
                    (value (internal-repr str type))
                    (dat (make-<data> :type type :value value)))
               (when (member type '(:const :byte :int :str))
                 (let ((pos (vector-push-extend (internal-repr str type) data))
                       (name (intern (format nil "$name~a$" *name-count*) :keyword)))
                   (setf (gethash name datamap) pos)))
               dat)))
      (loop
        :for (opc opr1 opr2 opr3) :in (getf ast :code)
        :do (format t "~s: ~s ~s ~s~%" opc opr1 opr2 opr3)
        :do (format t "  ~a ~a ~a~%" (get-type opr1) (get-type opr2) (get-type opr3))
        :do (let* ((i (find opc +opcode-specs+ :key #'<instruction>-name))
                   (operand1 (parse-and-push-operand opr1))
                   (operand2 (parse-and-push-operand opr2))
                   (operand3 (parse-and-push-operand opr3))
                   (o (make-<operation> :op i
                                        :opr1 operand1
                                        :opr2 operand2
                                        :opr3 operand3)))
              (vector-push-extend o code))))
    (make-<program> :data data :datamap datamap :code code)))
