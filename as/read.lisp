(in-package #:cl-user)
(defpackage #:svm-as/as/read
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence-if)
  (:export #:read-asm/as/read))
(in-package #:svm-as/as/read)

(defun trim (str)
  (string-trim '(#\space #\tab) str))

(defun section-p (line)
  (and (not (zerop (length line)))
       (char= (aref line 0) #\.)))

(defun tokenize (line)
  (let (tokens)
    (loop
      :for ch :across line
      :with in-string-p := nil
      :with buffer := nil
      :do (if in-string-p
              (if (char= ch #\")
                  (progn
                    (push (concatenate 'string "\"" (nreverse buffer) "\"") tokens)
                    (setf buffer nil
                          in-string-p nil))
                  (push ch buffer))
              (cond ((char= ch #\") (setf in-string-p t))
                    ((char= ch #\;)
                     (progn
                       (push (concatenate 'string (nreverse buffer)) tokens)
                       (return-from tokenize (remove-if (lambda (s) (zerop (length s))) (nreverse tokens)))))
                    ((member ch '(#\space #\tab) :test #'char=)
                     (progn
                       (push (concatenate 'string (nreverse buffer)) tokens)
                       (setf buffer nil)))
                    (t (push ch buffer))))
      :finally (progn
                 (push (concatenate 'string (nreverse buffer)) tokens)
                 (return-from tokenize (remove-if (lambda (s) (zerop (length s))) (nreverse tokens)))))))

(defun read-data (line)
  (destructuring-bind (name value)
      (tokenize line)
    (list (intern (string-upcase name) :keyword) value)))

(defun read-code (line)
  (destructuring-bind (name . operands)
      (tokenize line)
    (append (list (intern (string-upcase name) :keyword)) operands)))

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
    :finally (return-from read-asm
               (loop
                 :for (k v) :on program :by #'cddr
                 :append (list k (nreverse v))))))
