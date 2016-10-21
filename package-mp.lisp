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


;;; Gates

(defgeneric mp:make-gate (initial-state))

(defgeneric mp:open-gate (gate))

(defgeneric mp:close-gate (gate))

(defgeneric mp:gate-open-p (gate))


;;; Processes

(defgeneric mp:make-process (&key name initial-bindings))

(defgeneric mp:process-thread (process))

(defgeneric mp:process-name (process))



