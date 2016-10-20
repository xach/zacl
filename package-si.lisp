;;;; package-si.lisp

(in-package #:zacl)

(defmacro si:without-scheduling (&body body)
  `(progn ,@body))
