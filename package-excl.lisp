;;;; package-excl.lisp
;;;
;;; This file is a grab-bag of miscellaneous functions from EXCL.
;;;

(in-package #:zacl)

(defparameter *external-format-translations*
  '((:octets . :latin1)
    (:latin1-base . :latin1)
    (nil . :latin1)))

(defun translate-external-format (external-format)
  (let ((entry (assoc external-format *external-format-translations*)))
    (if entry
        (cdr entry)
        external-format)))


(defvar excl:*initial-terminal-io* *terminal-io*)

(defvar excl:*cl-default-special-bindings* nil)

(defvar excl:*required-top-level-bindings* nil)

(defvar excl:*current-case-mode* :case-insensitive-upper)


(defmacro excl:named-function (name lambda-form)
  "Return the function produced by LAMBDA-FORM wrapped in a named
function object. Useful for debugging, as the function object is no
longer anonymous, but has a meaningful name name."
  (destructuring-bind (lambda-name lambda-list &body body)
      lambda-form
    (unless (eq lambda-name 'cl:lambda)
      (error "Unexpected named-function form"))
    `(flet ((,name ,lambda-list ,@body))
       #',name)))


(defun excl:featurep (feature)
  (find feature *features*))


(defun excl:match-re (pattern string &key (return :string))
  (multiple-value-bind (start end regs-starts regs-ends)
      (scan pattern string)
    (when (and start end)
      (ecase return
        (:index
         (values-list (list* (cons start end) (map 'list 'cons regs-starts regs-ends))))
        (:string
         (values-list (list* (subseq string start end)
                             (map 'list (lambda (start end)
                                          (subseq string start end))
                                  regs-starts
                                  regs-ends))))))))

(defun excl:match-regexp (pattern string &key (return :string))
  (excl:match-re pattern string :return return))


(defmacro excl:with-output-to-buffer ((stream) &body body)
  `(with-output-to-sequence (,stream)
     ,@body))


;;; Streams

(defmacro excl:sm (slot-name object)
  `(slot-value ,object ',slot-name))

(defmacro excl:def-stream-class (name (&rest parents) &body slot-defs)
  `(defclass ,name (,@parents)
     ,@slot-defs))

(defmacro excl:with-stream-class ((class var) &body body)
  (declare (ignore class var))
  `(progn ,@body))

(defgeneric excl:device-read (stream buffer start end blocking))

(defgeneric excl:device-close (stream abort))

(define-condition excl:stream-closed-error (error) ())

(def-fake-slot excl::stream-property-list stream :default-value nil)
(def-fake-slot excl:stream-error-identifier stream :default-value nil)
(def-fake-slot excl:stream-error-code stream :default-value 0)

;;; Misc

(defmacro excl:errorset (form &optional announce catch-breaks)
  "Return NIL if FORM signals an error, T and values as multiple
values otherwise."
  (declare (ignore announce catch-breaks))
  (let ((result (gensym "RESULT")))
    `(let ((,result (multiple-value-list (ignore-errors ,form))))
       (if (not (first ,result))
           nil
           (values-list (list* t ,result))))))

(defun excl:find-external-format (name &key errorp)
  (declare (ignore errorp))
  (declare (ignore name))
  :latin-1)

(defun excl:crlf-base-ef (external-format)
  external-format)

(defun excl:ef-name (external-format)
  external-format)

(defun excl:fixnump (integer)
  (typep integer 'fixnum))

(defun excl:split-into-words (string)
  (split "\\s+" string))

(defun excl:split-on-character (string character)
  (split-sequence character string))

(defmacro excl::with-dynamic-extent-usb8-array ((var len) &body body)
  `(let ((,var (make-array ,len :element-type '(unsigned-byte 8))))
     ,@body))

(defun excl:native-string-sizeof (string &key (external-format :latin1))
  (length (string-to-octets string :external-format external-format)))

(defun excl:string-to-mb (string &key external-format mb-vector null-terminate)
  (declare (ignore mb-vector))
  (when null-terminate
    (error "Cannot null-terminate"))
  (string-to-octets string
                    :external-format (translate-external-format external-format)))

(defun excl:string-to-octets (string &key external-format null-terminate)
  (when null-terminate
    (error "No null terminating!"))
  (string-to-octets string
                    :external-format (translate-external-format external-format)))

(defun excl:mb-to-string (string &key external-format (start 0)
                                   (end (length string)))
  (octets-to-string string
                    :external-format (translate-external-format external-format)
                    :start start
                    :end end))

(defun excl:octets-to-string (octets &key external-format
                                       string string-start string-end truncate
                                       (start 0) (end (length octets)))
  (when (or string string-start string-end truncate)
    (error "Unsupported options given to excl:octets-to-string"))
  (octets-to-string octets
                    :start start
                    :end end
                    :external-format (translate-external-format external-format)))

(defun excl:schedule-finalization (object fun)
  ;; Doesn't work; semantics differ.
  ;; (finalize object fun))
  (declare (ignore object fun)))

(defun excl::unix-signal (signal-number action)
  (declare (ignore signal-number action)))

(defmacro excl:without-package-locks (&body body)
  #-sbcl
  `(progn ,@body)
  #+sbcl
  `(without-package-locks ,@body))

(defmacro excl:without-interrupts (&body body)
  #+(or ccl sbcl)
  `(without-interrupts ,@body)
  #-(or ccl sbcl)
  `(progn ,@body))

(defmacro excl:defvar-nonbindable (name value &optional doc)
  #+sbcl
  `(defglobal ,name ,value ,@(if doc (list doc)))
  #+ccl
  `(defstaticvar ,name ,value ,@(if doc (list doc)) )
  #-(or ccl sbcl)
  `(defvar ,name ,value ,@(if doc  (list doc))))

(defclass excl:lockable-object ()
  ((lock
    :initarg :lock
    :reader lock
    :initform (make-lock))))

(defun call-with-locked-object (object fun)
  (let ((lock (lock object)))
    (with-lock-held (lock)
      (funcall fun))))

(defmacro excl:with-locked-object ((object &key type block non-smp) &body body)
  (declare (ignore type block non-smp))
  `(call-with-locked-object ,object (lambda () ,@body)))

(defstruct excl:synchronizing-structure
  (lock (make-lock)))

(defun call-with-locked-structure (struct fun)
  (with-lock-held ((synchronizing-structure-lock struct))
    (funcall fun)))

(defmacro excl:with-locked-structure ((struct &key block non-smp) &body body)
  (declare (ignore block non-smp))
  `(call-with-locked-structure ,struct (lambda () ,@body)))

(defmacro excl:incf-atomic (place &optional (delta 1))
  #+ccl
  ;; Doesn't work on structure slots on CCL!
  ;;`(atomic-incf-decf ,place ,delta)
  `(incf ,place ,delta)
  ;; XXX
  #-ccl
  `(incf ,place ,delta))

(defmacro excl:decf-atomic (place &optional (delta 1))
  #+ccl
  ;; Doesn't work on structure slots on CCL!
  ;;`(atomic-incf-decf ,place (- ,delta))
  `(decf ,place (- ,delta))
  ;; XXX
  #-ccl
  `(decf ,place (- ,delta)))

(defstruct (basic-lock (:include excl:synchronizing-structure))
  name)

(defun excl::make-basic-lock (&key name)
  (make-basic-lock :name name))

(define-condition excl:socket-error (error)
  ((identifier
    :initarg :identifier
    :reader excl:stream-error-identifier)))


;;; Streams

(defgeneric excl:device-open (stream slot-names initargs))
(defgeneric excl:device-close (stream abort))
(defgeneric excl:device-read (stream buffer start end blocking))
(defgeneric excl:device-write (stream buffer start end blocking))

(defvar excl::*std-control-out-table* nil)

(defclass excl:single-channel-simple-stream (fundamental-stream)
  ((excl::buffer
    :initform (make-array 1024 :element-type '(unsigned-byte 8)))
   (excl::output-handle)
   (excl::buffer-ptr)
   (excl::control-out)
   (external-format
    :initarg :external-format
    :initform :default
    :accessor zacl-cl:stream-external-format)))

(defmethod shared-initialize :after ((stream
                                      excl:single-channel-simple-stream)
                                     slot-names
                                     &rest initargs &key &allow-other-keys)
  (excl:device-open stream slot-names initargs))

(defgeneric underlying-output-stream (stream)
  (:method ((stream excl:single-channel-simple-stream))
    (underlying-output-stream (slot-value stream 'excl::output-handle)))
  (:method ((stream stream-usocket))
    (socket-stream stream))
  (:method ((stream usocket))
    (socket-stream stream))
  (:method ((stream stream))
    stream))

(defgeneric underlying-input-stream (stream)
  (:method ((stream excl:single-channel-simple-stream))
    (underlying-input-stream (slot-value stream 'excl::input-handle)))
  (:method ((stream usocket))
    (socket-stream stream))
  (:method ((stream stream))
    stream))

(defmethod stream-write-string ((stream excl:single-channel-simple-stream) string &optional start end )
  (unless start (setf start 0))
  (unless end (setf end (length string)))
  (write-string string (underlying-output-stream stream)
                :start start
                :end end))

(defmethod ccl:stream-write-vector ((stream excl:single-channel-simple-stream) sequence start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (write-sequence sequence (underlying-output-stream stream)))

(defmethod stream-force-output ((stream excl:single-channel-simple-stream))
  (force-output (underlying-output-stream stream)))

(defmethod stream-force-output ((stream usocket))
  (force-output (underlying-output-stream stream)))

(defmethod stream-read-char ((stream usocket))
  (stream-read-char (socket-stream stream)))

(defmethod stream-read-byte ((stream usocket))
  (stream-read-byte (socket-stream stream)))

(defmethod cl:close ((stream usocket) &key abort)
  (close (socket-stream stream) :abort abort))

(defmacro excl:add-stream-instance-flags (stream &rest flags)
  (declare (ignore stream flags))
  nil)

(defun excl:write-vector (vector stream &key (start 0) (end (length vector)))
  ;; The real write-vector has more complicated blocking
  ;; behavior. Save that for later.
  (write-sequence vector stream :start start :end end)
  end)

(defmethod excl::socket-bytes-written (socket &optional set)
  (declare (ignore socket))
  (or set
      42))

(defmacro excl:pop-atomic (place)
  `(pop ,place))

(defmacro excl:push-atomic (value place)
  `(push ,value ,place ))
