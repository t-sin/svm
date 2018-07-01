(in-package #:cl-user)
(defpackage #:svm-vm/vm/simple-memory
  (:use #:cl)
  (:export #:make-simple-memory
           #:segment
           #:dump-simple-memory))
(in-package #:svm-vm/vm/simple-memory)

(defun make-simple-memory (size)
  (make-array size :element-type '(unsigned-byte 8)
              :initial-element 0))

(defun segment (mem addr)
  (if (and (<= 0 addr)
           (< addr (length mem)))
      (aref mem addr)
      (error (format nil "~s is out of memory!!" addr))))

(defun (setf segment) (byte mem addr)
  (if (and (<= 0 addr)
           (< addr (length mem)))
      (setf (aref mem addr) byte)
      (error (format nil "~s is out of memory!!" addr))))

(defun dump-simple-memory (mem)
  (format nil "~s" mem))
