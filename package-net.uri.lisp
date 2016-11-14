;;;; package-net.uri.lisp

(in-package #:zacl)

(defclass net.uri:uri ()
  ((real-uri
    :reader real-uri
    :initarg :real-uri)
   (plist
    :accessor net.uri:uri-plist
    :initarg :plist))
  (:default-initargs
   :plist nil))

(defmethod real-uri ((uri quri:uri))
  uri)

(defun net.uri:uri-path (uri)
  (uri-path (real-uri uri)))

(defun net.uri:copy-uri (uri &key scheme host)
  (copy-uri (real-uri uri) :scheme scheme :host host))

(defun net.uri:render-uri (uri &optional stream)
  (render-uri (real-uri uri) stream))

(defun net.uri:merge-uris (new-uri uri)
  (merge-uris (real-uri new-uri) (real-uri uri)))

(defun net.uri::uri-string (uri)
  (render-uri (real-uri uri)))

(defun net.uri:parse-uri (uri-string)
  (make-instance 'net.uri:uri :real-uri (uri uri-string)))

(defun net.uri:uri-scheme (uri)
  (values (find-symbol (string-upcase (uri-scheme (real-uri uri)))
                       :keyword)))

(defun (setf net.uri:uri-scheme) (new-value uri)
  (setf (uri-scheme (real-uri uri)) new-value))

(defun net.uri:uri-host (uri)
  (uri-host (real-uri uri)))

(defun (setf net.uri:uri-host) (new-value uri)
  (setf (uri-host (real-uri uri)) new-value))

(defun net.uri:uri-port (uri)
  (uri-port (real-uri uri)))

(defun (setf net.uri:uri-port) (new-value uri)
  (setf (uri-port (real-uri uri)) new-value))

(defun net.uri:uri-path (uri)
  (uri-path (real-uri uri)))

(defun (setf net.uri:uri-path) (new-value uri)
  (setf (uri-path (real-uri uri)) new-value))

(defun (setf net.uri:uri-path) (new-value uri)
  (setf (uri-path (real-uri uri)) new-value))

(defun net.uri:uri-fragment (uri)
  (uri-fragment (real-uri uri)))

(defun (setf net.uri:uri-fragment) (new-value uri)
  (setf (uri-fragment (real-uri uri)) new-value))

(defun net.uri:uri-userinfo (uri)
  (uri-userinfo (real-uri uri)))

(defun net.uri:uri-query (uri)
  (uri-query (real-uri uri)))

(defun net.uri::uri-path-etc (uri)
  (let ((uri (real-uri uri)))
    (with-output-to-string (s)
      (format s "~A" (uri-path uri))
      (when (uri-query uri)
        (format s "?~A" (uri-query uri)))
      (when (uri-fragment uri)
        (format s "#~A" (uri-fragment uri))))))

(defun net.uri::.uri-parsed-path (uri)
  ;; XXX What is this slot for, anyway? Fake it for now.
  (net.uri:uri-path uri))
