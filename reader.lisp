;;;; reader.lisp

(in-package #:zacl-reader)

(defparameter *allegro-feature-symbols*
  '("VERSION>=" "VERSION="))

(defun allegro-conditional-p (expression)
  (when (listp expression)
    (member (first expression) *allegro-feature-symbols*
            :test 'string-equal)))

(defun rewrite-feature-expression (expression)
  (cond ((null expression)
         nil)
        ((atom expression)
         expression)
        ((allegro-conditional-p expression)
         'latest-allegro-conditional)
        (t
         (cons (first expression)
               (mapcar #'rewrite-feature-expression (rest expression))))))

(defun read-allegro-reader-conditionals (stream character arg)
  (declare (ignore arg))
  (let* ((original-readtable (copy-readtable nil))
         (original-reader-function (get-dispatch-macro-character #\# character
                                                                 original-readtable)))
    (let* ((expression (read stream))
           (rewritten (rewrite-feature-expression expression))
           (text (prin1-to-string rewritten))
           (expression-stream (make-string-input-stream text))
           (combined-stream (make-concatenated-stream expression-stream
                                                      (make-string-input-stream " ")
                                                      stream))
           (*features* (list* :latest-allegro-conditional *features*)))
      (funcall original-reader-function combined-stream character nil))))

(defparameter *allegro-rewriting-readtable*
  (let ((table (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\+ 'read-allegro-reader-conditionals
                                  table)
    (set-dispatch-macro-character #\# #\- 'read-allegro-reader-conditionals
                                  table)
    table))


