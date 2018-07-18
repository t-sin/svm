(in-package #:cl-user)
(defpackage #:svm-ir
  (:use #:cl)
  (:import-from #:svm-ir/ir/ir
                #:make-ir)
  (:export #:make-ir))
(in-package #:svm-ir)
