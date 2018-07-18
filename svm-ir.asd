(defsystem #:svm-ir
  :class :package-inferred-system
  :description "Internal representation for Space VM program"
  :version "0.1"
  :author "TANAKA Shinichi"
  :license "LLGPL"
  :depends-on ("svm-ins"
               "svm-ir/ir/package"))
