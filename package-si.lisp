;;;; package-si.lisp

(in-package #:zacl)

(defmacro si:without-scheduling (&body body)
  `(excl:without-interrupts ,@body))

(defun si:global-symbol-value (symbol)
  (symbol-value symbol))

(defun (setf si:global-symbol-value) (new-value symbol)
  (setf (symbol-value symbol) new-value))
