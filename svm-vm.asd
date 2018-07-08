(defsystem #:svm-vm
  :class :package-inferred-system
  :description "Virtual Machine in Space"
  :version "0.1"
  :author "TANAKA Shinichi"
  :license "LLGPL"
  :depends-on ("babel"
               "svm-ins"
               "svm-vm/vm/package"))
