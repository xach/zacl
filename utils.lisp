;;;; utils.lisp

(in-package #:zacl)

(defun aload (file)
  (let ((*readtable* zacl-reader:*allegro-rewriting-readtable*)
        (*package* (find-package :user)))
    (load file)))

(defun acompile (file)
  (let ((*readtable* zacl-reader:*allegro-rewriting-readtable*)
        (*package* (find-package :user)))
    (compile-file file)))

(defun acl (file)
  (aload (acompile file)))

