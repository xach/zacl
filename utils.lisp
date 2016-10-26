;;;; utils.lisp

(in-package #:zacl)

(defparameter *build-time-features*
  '(:smp :smp-macros))

(defun call-with-zacl-build-environment (fun)
  (let ((*readtable* zacl-reader:*allegro-rewriting-readtable*)
        (*package* (find-package :user))
        (*features* (append *build-time-features* *features*)))
    (funcall fun)))

(defmacro with-zacl-build-environment (&body body)
  `(call-with-zacl-build-environment (lambda () ,@body)))

(defun aload (file)
  (setf file (merge-pathnames file "file.cl"))
  (with-zacl-build-environment
    (load file)))

(defun acompile (file)
  (setf file (merge-pathnames file "file.cl"))
  (with-zacl-build-environment
    (compile-file file)))

(defun acl (file)
  (aload (acompile file)))

