;;;; asdf-tricks.lisp

(in-package #:zacl)

;; This mimics what aserve.asd does.
(defclass asdf::cl-file (asdf:cl-source-file)
  ((type :initform "cl")))

(defmethod asdf:perform :around ((operation asdf:compile-op)
                                 (component asdf::cl-file))
  (with-zacl-build-environment (call-next-method)))
