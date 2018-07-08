(defsystem #:svm-load
  :class :package-inferred-system
  :description "Space VM program loader"
  :version "0.1"
  :author "TANAKA Shinichi"
  :license "LLGPL"
  :depends-on ("svm-ins"
               "svm-program"
               "svm-as/load/package"))
