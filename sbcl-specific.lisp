;;;; sbcl-specific.lisp

(in-package #:zacl)

(defun stream-unix-fd (stream)
  (fd-stream-fd stream))

(defun fstat-size (fd)
  (let ((stat (sb-posix:fstat fd)))
    (sb-posix:stat-size stat)))

(defun fstat-mtime (fd)
  (let ((stat (sb-posix:fstat fd)))
    (sb-posix:stat-mtime stat)))

(defun stat-mtime (file)
  (let ((stat (sb-posix:stat file)))
    (sb-posix:stat-mtime stat)))

(defun file-kind (pathname)
  (sb-impl::native-file-kind pathname))
