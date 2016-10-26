;;;; package-mp.lisp
;;;
;;; Multiprocessing.
;;;

(in-package #:zacl)

(defmacro mp:with-timeout ((time &body actions) &body body)
  (declare (ignorable time actions))
  `(progn ,@body))

(defmacro mp:with-process-lock (lock-form &body body)
  (declare (ignorable lock-form))
  `(progn ,@body))

(defun mp:make-process-lock (&key name)
  (cons :mp-lock name))

(defmacro mp:without-scheduling (&body body)
  `(progn ,@body))


;;; Gates

(defgeneric mp:make-gate (initial-state))

(defgeneric mp:open-gate (gate))

(defgeneric mp:close-gate (gate))

(defgeneric mp:gate-open-p (gate))


;;; Processes

(defgeneric mp:make-process (&key name initial-bindings))

(defgeneric mp:process-thread (process))

(defgeneric mp:process-name (process))

(defgeneric mp:process-keeps-lisp-alive-p (process))

(defgeneric (setf mp:process-keeps-lisp-alive-p) (new-value process))

(defgeneric mp:process-property-list (process))

(defgeneric (setf mp:process-property-list) (new-value process))

(defgeneric mp:process-add-run-reason (process stream))

(defgeneric mp:process-allow-schedule ())

(defgeneric mp:process-kill (process))

(defgeneric mp:process-preset (process fun))

(defgeneric mp:process-revoke-run-reason (process stream))

(defgeneric mp:process-run-function (plist function))

(defgeneric mp:process-run-reasons (process))


;;; Queues

;; FIXME: Timeouts.

(defclass mp:queue ()
  ((queue
    :initarg :queue
    :accessor queue
    :initform (make-queue :simple-cqueue))))

(defgeneric mp:enqueue (queue thing)
  (:method (queue thing)
    (qpush (queue queue) thing)))

(defgeneric mp:dequeue (queue &key wait empty-queue-result whostate)
  (:method (queue &key wait empty-queue-result whostate)
    (declare (ignore whostate wait))
    (qpop (queue queue) empty-queue-result)))
