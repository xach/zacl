;;;; package-socket.lisp

(in-package #:zacl)

(defun ip-address-integer (ip-address)
  (check-type ip-address (simple-array * (4)) "octet vector")
  (logand #xFFFFFFFF
          (logior (ash (aref ip-address 0) 24)
                  (ash (aref ip-address 1) 16)
                  (ash (aref ip-address 2)  8)
                  (ash (aref ip-address 3)  0))))

(defun integer-ip-address (integer)
  (check-type integer (unsigned-byte 32))
  (let ((ip-address (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref ip-address 0) (ldb (byte 8 24) integer)
          (aref ip-address 1) (ldb (byte 8 16) integer)
          (aref ip-address 2) (ldb (byte 8  8) integer)
          (aref ip-address 3) (ldb (byte 8  0) integer))
    ip-address))


(defclass zacl-socket (fundamental-binary-output-stream
                       fundamental-character-output-stream
                       fundamental-binary-input-stream
                       fundamental-character-input-stream)
  ((socket
    :initarg :socket
    :reader socket)
   (real-stream
    :initarg :real-stream
    :reader real-stream)))

(defmethod stream-write-byte ((stream zacl-socket) byte)
  (write-byte byte (real-stream stream)))

(defmethod stream-write-char ((stream zacl-socket) char)
  (map nil (lambda (octet)
             (write-byte octet (real-stream stream)))
       (string-to-octets (string char))))

(defmethod stream-write-sequence ((stream zacl-socket) sequence start end
                                  &key &allow-other-keys)
  (when (typep sequence 'string)
    (setf sequence (string-to-octets sequence :start start :end end))
    (setf start 0)
    (setf end (length sequence)))
  (write-sequence sequence (real-stream stream) :start start :end end))

(defmethod stream-read-byte ((stream zacl-socket))
  (read-byte (real-stream stream) nil :eof))

(defmethod stream-read-char ((stream zacl-socket))
  (let ((byte (read-byte (real-stream stream) nil :eof)))
    (if (eql byte :eof)
        :eof
        (code-char byte))))

(defmethod stream-read-char-no-hang ((stream zacl-socket))
  (let ((ready (wait-for-input (socket stream) :timeout 0 :ready-only t)))
    (when ready
      (stream-read-char stream))))

(defmethod stream-read-sequence ((stream zacl-socket) sequence start end
                                 &key &allow-other-keys)
  (when (stringp sequence)
    (error "Not implemented"))
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (read-sequence sequence (real-stream stream) :start start :end end))

#+ccl
(defmethod ccl:stream-read-vector ((stream zacl-socket) sequence start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (if (stringp sequence)
      (let ((offset start)
            (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
        (let* ((after-index (read-sequence buffer (real-stream stream)))
               (string (octets-to-string buffer :start 0 :end after-index
                                         :external-format :latin-1)))
          (replace sequence string :start1 start :end1 end
                   :start2 0 :end2 after-index)
          (+ offset after-index)))
      (read-sequence sequence (real-stream stream) :start start :end end)))

(defmethod stream-force-output ((stream zacl-socket))
  (force-output (socket-stream (socket stream))))

(defmethod close ((stream zacl-socket) &key abort)
  (declare (ignore abort))
  (socket-close (socket stream)))

(defun socket:make-socket (&key connect local-port local-host reuse-address
                             remote-port remote-host
                             format (backlog 5) type nodelay)
  (declare (ignore format type))
  (ecase connect
    (:passive
     (let ((socket
            (socket-listen local-host local-port
                           :reuseaddress reuse-address
                           :element-type '(unsigned-byte 8)
                           :backlog backlog)))
       (make-instance 'zacl-socket
                      :socket socket)))
    ((nil)
     (let ((socket
            (socket-connect remote-host remote-port
                            :nodelay nodelay
                            :element-type '(unsigned-byte 8))))
       (make-instance 'zacl-socket
                      :socket socket
                      :real-stream (socket-stream socket))))))

(defun socket:accept-connection (socket)
  (let ((incoming (socket-accept (socket socket))))
    (make-instance 'zacl-socket
                   :socket incoming
                   :real-stream (socket-stream incoming))))

(defun socket:local-host (socket)
  (ip-address-integer (get-local-address (socket socket))))

(defun socket:local-port (socket)
  (get-local-port (socket socket)))

(defun socket:set-socket-options (socket &key nodelay)
  (setf (socket-option (socket socket) :tcp-no-delay) nodelay))

(defun socket:remote-host (socket)
  (ip-address-integer (get-peer-address (socket socket))))

(defun socket:ipaddr-to-dotted (ip-integer)
  (format nil "~A.~A.~A.~A"
          (ldb (byte 8 24) ip-integer)
          (ldb (byte 8 16) ip-integer)
          (ldb (byte 8  8) ip-integer)
          (ldb (byte 8  0) ip-integer)))

(defun socket:dotted-to-ipaddr (dotted &key errorp)
  (if errorp
      (dotted-to-ipaddr dotted)
      (ignore-errors (dotted-to-ipaddr dotted))))

(defmacro socket:with-pending-connect (&body body)
  `(progn ,@body))

(defun socket:ipaddr-to-hostname (ipaddr)
  (ipaddr-to-hostname ipaddr))

(defun socket:lookup-hostname (name)
  (lookup-hostname name))

(defun socket::make-ssl-server-stream (&rest args)
  (declare (ignore args))
  (error "Not implemented -- MAKE-SSL-SERVER-STREAM"))

(defun socket::make-ssl-client-stream (&rest args)
  (declare (ignore args))
  (error "Not implemented -- MAKE-SSL-CLIENT-STREAM"))

(defun socket:socket-control (socket &key read-timeout write-timeout)
  (declare (ignore socket read-timeout write-timeout))
  nil)
