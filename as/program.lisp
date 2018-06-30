(in-package #:cl-user)
(defpackage #:svm-as/as/program
  (:use #:cl)
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
                #:<program>-code)
  (:export #:construct-program))
(in-package #:svm-as/as/program)

(defun construct-program (ast)
  ast)
