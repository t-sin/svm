(in-package #:cl-user)
(defpackage #:svm-as
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence-if)
  (:export #:read-asm))
(in-package #:svm-as)

(defun trim (str)
  (string-trim '(#\space #\tab) str))

(defun section-p (line)
  (and (not (zerop (length line)))
       (char= (aref line 0) #\.)))

(defun tokenize (line)
  (let* ((tokens* (split-sequence-if (lambda (c) (member c '(#\space #\tab) :test #'char=))
                                    line))
         (tokens (remove-if (lambda (s) (zerop (length (trim s)))) tokens*)))
    tokens))

(defun read-data (line)
  (tokenize line))

(defun read-code (line)
  (tokenize line))

(defun read-asm (stream)
  (loop
    :for line := (read-line stream nil :eof)
    :with program := nil
    :with section := nil
    :until (eq line :eof)
    :do (let ((line* (trim line)))
          (cond ((zerop (length line*)) nil)
                ((char= (aref line* 0) #\;) nil)
                ((null section)
                 (cond ((string= line* ".data")
                        (setf section :data))
                       ((string= line* ".code")
                        (setf section :code))
                       (t (error (format nil "this ~s is not section name" line*)))))
                ((eq section :data)
                 (cond ((string= line* ".data") nil)
                       ((string= line* ".code") (setf section :code))
                       (t (push (read-data line*) (getf program :data)))))
                ((eq section :code)
                 (cond ((string= line* ".data") (setf section :data))
                       ((string= line* ".code") nil)
                       (t (push (read-code line*) (getf program :code)))))
                (t (error "oops."))))
    :finally (return-from read-asm program)))
