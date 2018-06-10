(defsystem #:svm-as
  :class :package-inferred-system
  :description "Virtual Machine for SVM"
  :version "0.1"
  :author "TANAKA Shinichi"
  :license "LLGPL"
  :depends-on ("svm-ins"
               "svm-vm/vm/package"))
