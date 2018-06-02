(in-package #:cl-user)
(defpackage #:svm-as/as/program
  (:use #:cl)
  (:export #:constant
           #:make-constant
           #:constant-type
           #:constant-name
           #:constant-value

           #:instruction
           #:make-instruction
           #:instruction-name
           #:instruction-operand-1
           #:instruction-operand-2
           #:instruction-operand-3

           #:program
           #:make-program
           #:program-constant
           #:program-code))
(in-package #:svm-as/as/program)

(defstruct constant
  type name value)

(defstruct instruction
  name operand-1 operand-2 operand-3)

(defstruct program
  constant code)
