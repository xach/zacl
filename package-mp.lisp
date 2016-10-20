;;;; package-mp.lisp
;;;
;;; Multiprocessing.
;;;

(in-package #:zacl)

(defmacro mp:with-timeout ((time &body actions) &body body)
  (declare (ignorable time actions))
  `(progn ,@body))

(defmacro mp:with-process-lock (lock-form &body body)
  (declare (ignorable lock-form))
  `(progn ,@body))

(defun mp:make-process-lock ()
  (cons :mp :lock))

