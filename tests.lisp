;;;; tests.lisp

(defpackage #:zacl-tests
  (:use #:cl #:fiveam)
  (:import-from #:usocket
                #:socket-listen
                #:get-local-port
                #:socket-close)
  (:import-from #:net.aserve
                #:publish
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
(defvar *test-host* "localhost")
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


