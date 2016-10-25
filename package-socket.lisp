;;;; package-socket.lisp

(in-package #:zacl)

(defun ip-address-integer (ip-address)
  (check-type ip-address (simple-array (unsigned-byte 8) (4)) "octet vector")
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


(defun socket:make-socket (&key connect local-port local-host reuse-address
                             format (backlog 5) type)
  (declare (ignore format type))
  (ecase connect
    (:passive
     (socket-listen local-host local-port
                    :reuseaddress reuse-address
                    :backlog backlog))))

(defun socket:accept-connection (socket)
  (socket-accept socket))

(defun socket:local-host (socket)
  (ip-address-integer (get-local-address socket)))

(defun socket:local-port (socket)
  (get-local-port socket))

(defun socket:set-socket-options (socket &key nodelay)
  (setf (socket-option socket :tcp-no-delay) nodelay))

(defun socket:remote-host (socket)
  (ip-address-integer (get-peer-address socket)))

(defun socket:ipaddr-to-dotted (ip-integer)
  (format nil "~A.~A.~A.~A"
          (ldb (byte 8 24) ip-integer)
          (ldb (byte 8 16) ip-integer)
          (ldb (byte 8  8) ip-integer)
          (ldb (byte 8  0) ip-integer)))

