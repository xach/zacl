;;;; ccl.lisp

(in-package #:zacl)

(defun fstat-size (fd)
  (nth-value 2 (ccl::%fstat fd)))

(defun fstat-mtime (fd)
  (nth-value 3 (ccl::%fstat fd)))

(defun stat-mode (pathname)
  (nth-value 1 (ccl::%stat pathname t)))

(defun file-kind (pathname)
  (ccl::%file-kind (stat-mode pathname)))
