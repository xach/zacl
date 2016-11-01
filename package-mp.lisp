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

(defclass zacl-process (process)
  ((run-reasons
    :initform nil
    :reader mp:process-run-reasons
    :reader process-run-reasons
    ;; private to zacl
    :writer (setf process-run-reasons))
   (run-reasons-lock
    :initform (make-lock)
    :reader run-reasons-lock)
   (property-list
    :initform nil
    :accessor mp:process-property-list)))

(defclass mp:process-lock (excl:lockable-object) ())

(defgeneric mp:make-process (&key name initial-bindings)
  (:method (&key name initial-bindings)
    (make-process name :class 'zacl-process
                  :initial-bindings initial-bindings)))

(defgeneric mp:process-thread (process))

(defgeneric mp:process-name (process)
  (:method (process)
    (process-name process)))

(defgeneric mp:process-keeps-lisp-alive-p (process))

(defgeneric (setf mp:process-keeps-lisp-alive-p) (new-value process))

(def-fake-slot mp:process-keeps-lisp-alive-p process :default-value nil)

(defgeneric mp:process-add-run-reason (process object)
  (:method (process object)
    (with-lock-held ((run-reasons-lock process))
      (push object (process-run-reasons process)))
    (process-enable process)))

(defgeneric mp:process-revoke-run-reason (process object)
  (:method (process object)
    (with-lock-held ((run-reasons-lock process))
      (setf (process-run-reasons process)
            (delete object (process-run-reasons process))))))

(defgeneric mp:process-allow-schedule ()
  (:method ()
    (process-yield *current-process*)))

(defgeneric mp:process-kill (process)
  (:method (process)
    (process-kill process)))

(defgeneric mp:process-preset (process fun &rest args)
  (:method (process fun &rest args)
    (apply #'process-preset process fun args )))

(defgeneric mp:process-run-function (name-or-plist function &rest arguments)
  (:method ((name string) function &rest arguments)
    (apply #'mp:process-run-function (list :name name) function arguments))
  (:method ((plist list) function &rest arguments)
    (apply #'process-run-function (list* :class 'zacl-process plist)
           function arguments)))

(defgeneric mp:process-run-reasons (process)
  (:method (process)
    (process-run-reasons process)))


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
