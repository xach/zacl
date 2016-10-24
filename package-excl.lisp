;;;; package-excl.lisp
;;;
;;; This file is a grab-bag of miscellaneous functions from EXCL.
;;;

(in-package #:zacl)

(defvar excl:*initial-terminal-io* *terminal-io*)

(defvar excl:*cl-default-special-bindings* nil)

(defvar excl:*required-top-level-bindings* nil)


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

(def-fake-slot excl::stream-property-list stream :default-value nil)

