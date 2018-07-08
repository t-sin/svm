(defsystem #:svm-as
  :class :package-inferred-system
  :description "Space VM assembler"
  :version "0.1"
  :author "TANAKA Shinichi"
  :license "LLGPL"
  :depends-on ("split-sequence"
               "svm-ins"
               "svm-as/as/package"))
