;;;; utils.lisp

(in-package #:zacl)

(defun aload (file)
  (let ((*readtable* zacl-reader:*allegro-rewriting-readtable*))
    (load file)))

(defun acompile (file)
  (let ((*readtable* zacl-reader:*allegro-rewriting-readtable*))
    (compile-file file)))

