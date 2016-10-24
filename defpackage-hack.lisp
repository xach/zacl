;;;; defpackage-hack.lisp

(in-package #:zacl)

(defmacro user:defpackage (name &rest clauses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defpackage ,name
       (:shadowing-import-from #:zacl-cl
                               #:macroexpand
                               #:read-sequence)
       ,@clauses)))

(defun zacl-cl:read-sequence (sequence stream &key start end partial-fill)
  (declare (ignore partial-fill))
  (read-sequence sequence stream :start start :end end))

(defun zacl-cl:macroexpand (form &optional env stop-on-special-forms-p)
  (declare (ignore stop-on-special-forms-p))
  (macroexpand form env))
