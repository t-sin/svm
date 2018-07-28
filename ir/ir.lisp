(in-package #:cl-user)
(defpackage #:svm-ir/ir/ir
  (:use #:cl)
  (:import-from #:babel
                #:string-to-octets)
  (:import-from #:svm-ins
                #:<operand>
                #:<operand>-name
                #:<operand>-types

                #:<instruction>
                #:<instruction>-name
                #:<instruction>-doc

                #:+opcode-specs+)
  (:export #:make-ir))
(in-package #:svm-ir/ir/ir)

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
                  (dat (list :kind :data :type type
                             :value (internal-repr value type)))
                  (pos (vector-push-extend dat datavec)))
             (setf (gethash name datamap) pos))))
    (loop
      :for (name value) :in (getf ast :data)
      :do (push-data name value))))

(defun make-code (ast datavec codevec datamap jumptable)
  (let ((n 0))
    (flet ((parse-operand (str oprnum)
             (let* ((type (get-type str))
                    (value (internal-repr str type))
                    (dat (list :type type :value value)))
               (cond ((member type '(:byte :int :str))
                      (let ((pos (vector-push-extend dat datavec))
                            (name (intern (format nil "$name~a$" *name-count*) :keyword)))
                        (incf *name-count*)
                        (setf (gethash name datamap) pos)
                        (list :type :const :value name)))
                     ((eq type :label)
                      (let ((pos (vector-push-extend
                                  (list :op (find :load +opcode-specs+ :key #'<instruction>-name)
                                        :opr1 (list :type :label :value value)
                                        :opr2 (list :type :reg
                                                    :value (intern (format nil "R~a" (+ oprnum 3)) :keyword))
                                        :opr3 (list :type :null))
                                  codevec)))
                        (incf n)
                        (setf (gethash value datamap) pos)
                        (list :type :label :value value)))
                     (t dat)))))
      (loop
        :for op :in (getf ast :code)
        :if (stringp op)
        :do (let* ((dat (list :type :label :value n))
                   (pos (vector-push-extend dat datavec))
                   (name (intern (format nil "~a:" op) :keyword)))
              (setf (gethash name jumptable) n)
              (setf (gethash name datamap) pos))
        :else
        :do (destructuring-bind (opc &optional opr1 opr2 opr3) op
              (let* ((i (find opc +opcode-specs+ :key #'<instruction>-name))
                     (operand1 (parse-operand opr1 1))
                     (operand2 (parse-operand opr2 2))
                     (operand3 (parse-operand opr3 3))
                     (o (list :op i :opr1 operand1 :opr2 operand2 :opr3 operand3)))
                (vector-push-extend o codevec))
              (incf n))))))

(defun make-ir (ast)
  (let ((data (make-array 0 :adjustable t :fill-pointer 0))
        (datamap (make-hash-table :test 'eq))
        (code (make-array 0 :adjustable t :fill-pointer 0))
        (jumptable (make-hash-table :test 'eq)))
    (setf (gethash :EP datamap)
          (vector-push-extend (list :type :byte :value 0) data))
    (setf (gethash :EOC datamap)
          (vector-push-extend (list :type :byte :value 0) data))
    (make-data ast data datamap)
    (make-code ast data code datamap jumptable)
    (list :data data :datamap datamap :datamap2 nil :code code :jumptable jumptable)))
