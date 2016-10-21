;;;; package-sys.lisp

(in-package #:zacl)

(define-symbol-macro sys:*current-process* (current-thread))

(defun sys::thread-bindstack-index (&rest args)
  (declare (ignore args))
  (error "Not implemented"))
