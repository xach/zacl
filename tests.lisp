;;;; tests.lisp

(defpackage #:zacl-tests
  (:use #:cl #:fiveam)
  (:import-from #:usocket
                #:socket-listen
                #:get-local-port
                #:socket-close)
  (:import-from #:net.aserve
                #:publish
                #:request-query
                #:with-http-response
                #:with-http-body
                #:*html-stream*)
  (:export #:run-tests))

(in-package #:zacl-tests)

(defun guesstimate-free-port ()
  (let* ((socket (socket-listen "localhost" 0))
         (port (get-local-port socket)))
    (socket-close socket)
    port))

(defvar *test-port* nil)
(defvar *test-host* "127.0.0.1")
(defvar *test-server* nil)

(defun make-test-url (path)
  (format nil "http://~A:~A~A" *test-host* *test-port* path))

(defun call-with-wserver (fun)
  (let* ((*test-port* (guesstimate-free-port))
         (*test-server* (net.aserve:start :port *test-port* :host *test-host*)))
    (unwind-protect
         (funcall fun *test-server*)
      (net.aserve:shutdown :server *test-server*))))

(defmacro with-wserver ((server) &body body)
  `(call-with-wserver (lambda (,server)
                        ,@body)))

(def-suite zacl-tests)

(in-suite zacl-tests)

(test make-a-process
  (let ((process (mp:make-process :name "bob")))
    (is (equalp "bob" (mp:process-name process)))
    (mp:process-kill process)))

(test uri-manipulation
  (let ((uri (net.uri:parse-uri "/"))
        (query "foo=bar")
        (host "www.example.com")
        (fragment "baz")
        (scheme :http)
        (full-uri "http://www.example.com/?foo=bar#baz"))
    (is (null (net.uri:uri-query uri)))
    (is (null (net.uri:uri-host uri)))
    (is (null (net.uri:uri-fragment uri)))
    (is (null (net.uri:uri-scheme uri )))
    (setf (net.uri:uri-query uri) query)
    (setf (net.uri:uri-host uri) host)
    (setf (net.uri:uri-fragment uri) fragment)
    (setf (net.uri:uri-scheme uri) scheme)
    (is (equalp (net.uri:uri-query uri) query))
    (is (equalp (net.uri:uri-host uri) host))
    (is (equalp (net.uri:uri-fragment uri) fragment))
    (is (equalp (net.uri:uri-scheme uri) scheme))
    (is (equalp (net.uri:render-uri uri) full-uri))))

(test utf8-enabled
  (is (= (length "€") 1)))

(test utf8-encoding
  (with-wserver (server)
    (let ((test-string "Hey €¥© Now"))
      (publish :server server
               :path "/utf8"
               :function #'(lambda(req ent)
                             (with-http-response (req ent :content-type "text/html; charset=utf-8")
                               (with-http-body (req ent :external-format :utf-8)
                                 (format *html-stream* test-string)))))
      (is (string= test-string (net.aserve.client:do-http-request
                                   (make-test-url "/utf8")
                                 :external-format :utf-8))))))

(test buffer-output
  (is (equalp #(42)
             (excl:with-output-to-buffer (s)
               (write-byte 42 s)))))


