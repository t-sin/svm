(in-package #:cl-user)
(defpackage #:svm-load/load/program
  (:use #:cl)
  (:import-from #:svm-ins
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
                #:<program>-code
                #:<program>-jumptable)
  (:export #:make-program))
(in-package #:svm-load/load/program)

(defun get-type (str)
  (flet ((first-char= (ch) (char= (char str 0) ch))
         (last-char= (ch) (char= (char str (1- (length str))) ch)))
    (cond ((null str) :null)
          ((first-char= #\$) :reg)
          ((first-char= #\&) :addr)
          ((first-char= #\%) :const)
          ((first-char= #\:) :label)
          ((and (first-char= #\") (last-char= #\") :str))
          (t :int))))

(defun internal-repr (str type)
  (ecase type
    (:null nil)
    (:reg (intern (string-upcase (subseq str 1)) :keyword))
    (:addr (parse-integer (subseq str 1)))
    (:label (intern (format nil "~a:" str) :keyword))
    (:const (intern (string-upcase (subseq str 1)) :keyword))
    (:str (subseq str 1 (1- (length str))))
    (:byte (parse-integer str))
    (:int (parse-integer str))))

(defparameter *name-count* 0)

(defun make-data (ast datavec datamap)
  (flet ((push-data (name value)
           (let* ((type (get-type value))
                  (dat (make-<data> :type type
                                    :value (internal-repr value type)))
                  (pos (vector-push-extend dat datavec)))
             (setf (gethash name datamap) pos))))
    (loop
      :for (name value) :in (getf ast :data)
      :do (push-data name value))))

(defun make-code1 (ast codevec datavec datamap jumptable)
  (flet ((parse-and-push-operand (str)
           (let* ((type (get-type str))
                  (value (internal-repr str type))
                  (dat (make-<data> :type type :value value)))
             (if (member type '(:byte :int :str))
                 (let ((pos (vector-push-extend dat datavec))
                       (name (intern (format nil "$name~a$" *name-count*) :keyword)))
                   (setf (gethash name datamap) pos)
                   (make-<data> :type :const :value name))
                 dat))))
    (loop
      :for n :from 0 :upto (length (getf ast :code))
      :for op :in (getf ast :code)
      :if (stringp op)
      :do (let* ((dat (make-<data> :type :label :value n))
                 (pos (vector-push-extend dat datavec)))
            (setf (gethash (intern (format nil "~a:" op) :keyword) datamap) pos))
      :else
      :do (destructuring-bind (opc &optional opr1 opr2 opr3) op
            (let* ((i (find opc +opcode-specs+ :key #'<instruction>-name))
                   (operand1 (parse-and-push-operand opr1))
                   (operand2 (parse-and-push-operand opr2))
                   (operand3 (parse-and-push-operand opr3))
                   (o (make-<operation> :op i
                                        :opr1 operand1
                                        :opr2 operand2
                                        :opr3 operand3)))
              (vector-push-extend o codevec))))))

(defun make-program (ast)
  (let ((data (make-array 0 :element-type '<data>
                          :adjustable t :fill-pointer 0))
        (datamap (make-hash-table :test 'eq))
        (code (make-array 0 :element-type '<operation>
                          :adjustable t :fill-pointer 0)))
    (make-data ast data datamap)
    (make-code1 ast code data datamap nil)
    (make-<program> :data data :datamap datamap :code code)))
