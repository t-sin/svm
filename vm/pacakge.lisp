(in-package #:cl-user)
(defpackage #:svm-vm
  (:use #:cl)
  (:export #:make-vm
           #:run-program))
(in-package #:svm-vm)

(defun make-vm (option) nil)

(defun run-program (vm program))
