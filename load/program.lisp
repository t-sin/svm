(in-package #:cl-user)
(defpackage #:svm-load/load/program
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
  (:import-from #:svm-program

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
                  (dat (list :kind :data :type type
                             :value (internal-repr value type)))
                  (pos (vector-push-extend dat datavec)))
             (setf (gethash name datamap) pos))))
    (loop
      :for (name value) :in (getf ast :data)
      :do (push-data name value))))

(defun make-code (ast datavec codevec datamap jumptable)
  (flet ((parse-and-push-operand (str)
           (let* ((type (get-type str))
                  (value (internal-repr str type))
                  (dat (list :type type :value value)))
             (if (member type '(:byte :int :str))
                 (let ((pos (vector-push-extend dat datavec))
                       (name (intern (format nil "$name~a$" *name-count*) :keyword)))
                   (incf *name-count*)
                   (setf (gethash name datamap) pos)
                   (list :type :const :value name))
                 dat))))
    (loop
      :for n :from 0 :upto (length (getf ast :code))
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
                   (operand1 (parse-and-push-operand opr1))
                   (operand2 (parse-and-push-operand opr2))
                   (operand3 (parse-and-push-operand opr3))
                   (o (list :op i :opr1 operand1 :opr2 operand2 :opr3 operand3)))
              (vector-push-extend o codevec))))))

(defun calc-data-offset (idx data)
  (loop
    :for n :from 0 :below idx
    :for d := (aref data n)
    :sum (ecase (getf d :type)
           (:int (+ 1 1))
           (:byte (+ 1 1))
           (:label (+ 1 1))
           (:bytes (+ 2 (length (getf d :value))))
           (:char (+ 1 (length (string-to-octets (getf d :value)))))
           (:str (+ 2 (length (string-to-octets (getf d :value))))))))

(defun calc-code-offset (idx data)
  (+ (calc-data-offset (length data) data)
     (* (1+ idx) 3)))

(defun calc-address (data code datamap jumptable)
  (let ((addr-types '(:const :label)))
    (flet ((has-const-or-label? (op)
             (or (member (getf (getf op :opr1) :type) addr-types)
                 (member (getf (getf op :opr2) :type) addr-types)
                 (member (getf (getf op :opr3) :type) addr-types)))
           (calc-and-replace (operand reg newcode)
             (when (and operand (member (getf operand :type) addr-types))
               (ecase (getf operand :type)
                 ;; たぶんここでは、具体的なメモリアドレスを計算するの美しくないきがする
                 ;; なので、:constと:labelはそのままにして、jumptableやdatamapの値をそのまま入れるのがよさそう
                 ;; 実際のメモリアドレスは、load-program時にする
                 ;; => あきらめた ;p
                 ;;    アセンブラの設計ミスかな
                 (:const (progn
                           (setf (getf operand :value) (calc-data-offset (gethash (getf operand :value) datamap)
                                                                          data))
                           (setf (getf operand :type) :addr)))
                 (:label (let ((addr (gethash (getf operand :value) datamap)))
                           (setf (getf (aref data addr) :value)
                                 (calc-code-offset (gethash (getf operand :value) jumptable) data))
                           (vector-push-extend
                            (list :op (find :load +opcode-specs+ :key #'<instruction>-name)
                                  :opr1 (list :type :addr :value (calc-data-offset addr data))
                                  :opr2 (list :type :reg :value reg)
                                  :opr3 (list :type :null))
                            newcode)
                           (setf (getf operand :value) reg)
                           (setf (getf operand :type) :reg)))))))
      (loop
        :for n :from 0 :below (length code)
        :for op := (aref code n)
        :with newcode := (make-array 0 :adjustable t :fill-pointer 0)
        :do (if (has-const-or-label? op)
                (progn
                  (calc-and-replace (getf op :opr1) :r4 newcode)
                  (calc-and-replace (getf op :opr2) :r5 newcode)
                  (calc-and-replace (getf op :opr3) :r6 newcode)
                  (vector-push-extend op newcode))
                (vector-push-extend op newcode))
        :finally (return-from calc-address newcode)))))

(defun make-program (ast)
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
    (let ((newcode (calc-address data code datamap jumptable)))
      (make-<program> :data data :datamap datamap :code newcode :jumptable jumptable))))
