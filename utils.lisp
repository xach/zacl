;;;; utils.lisp

(in-package #:zacl)

(defparameter *aserve-files*
  '("htmlgen/htmlgen"
      "packages"
      "macs"
      "queue"
      "main"
      "headers"
      "parse"
      "decode"
      "publish"
      "authorize"
      "log"
      "client"
      "proxy"
      "cgi"
      "chunker")
  "Source files of aserve, taken from aserve/load.cl")

(defparameter *build-time-features*
  '(:smp :smp-macros :allegro))

(defun unused-lexical-warning-p (condition)
  (search "Unused lexical" (princ-to-string condition)))

(deftype unused-lexical-warning ()
  `(satisfies unused-lexical-warning-p))

(defun call-with-zacl-build-environment (fun)
  (let ((*readtable* zacl-reader:*allegro-rewriting-readtable*)
        (*package* (find-package :user))
        (*features* (append *build-time-features* *features*)))
    (handler-bind ((unused-lexical-warning #'muffle-warning))
      (funcall fun))))

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


;;; Incremental retrying of stuff. Global state galore.

(defvar *to-build* *aserve-files*)
(defvar *source-directory* (merge-pathnames "src/aserve/"
                                            (user-homedir-pathname)))

(defun reset ()
  (setf *to-build* *aserve-files*))

(defun try ()
  (unless *to-build*
    (cerror "Call reset" "Nothing left to build -- (reset) to start over")
    (reset))
  (let ((*default-pathname-defaults* *source-directory*))
    (loop
      (when (endp *to-build*)
        (return))
      (let ((file (first *to-build*)))
        (let ((fasl (acompile file)))
          (aload fasl)
          (pop *to-build*))))))
