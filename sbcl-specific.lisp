;;;; sbcl-specific.lisp

(in-package #:zacl)

(defun stream-unix-fd (stream)
  (fd-stream-fd stream))


