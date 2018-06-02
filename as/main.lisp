(in-package #:cl-user)
(defpackage #:svm-as/as/main
  (:use #:cl)
  (:export #:read-asm))
(in-package #:svm-as/as/main)

(defun read-data (stream)
  :data)

(defun read-code (stream)
  :code)

(defun read-asm (stream)
  (loop
    :for line := (read-line stream nil :eof)
    :with program := nil
    :until (eq line :eof)
    :do (let ((trimmed (string-trim '(#\space #\tab) line)))
          (cond ((zerop (length trimmed)) nil)
                ((char= (aref trimmed 0) #\;) nil)
                ((string= trimmed ".data")
                 (if (getf program :data)
                     (error ".data section appears twice!")
                     (setf (getf program :data) (read-data stream))))
                ((string= trimmed ".code")
                 (if (getf program :code)
                     (error ".code section appears twice!")
                     (setf (getf program :code) (read-code stream))))
                (t (error (format nil "this ~s is not section name" trimmed)))))))
