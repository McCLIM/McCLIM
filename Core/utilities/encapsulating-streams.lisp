;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

(defvar *original-stream* nil)

(defclass standard-encapsulating-stream (encapsulating-stream
                                         fundamental-character-input-stream
                                         fundamental-character-output-stream)
  ((stream :reader encapsulating-stream-stream :initarg :stream)))

;;; Macro used by methods for other stream classes that need to
;;; respect the possibility of an encapsulating stream when calling
;;; other methods on their stream argument.

(defmacro with-encapsulating-stream ((maybe-encapsulating-stream stream)
                                     &body body)
  "Within BODY binds MAYBE-ENCAPSULATING-STREAM to an encapsulating stream,
if there is one, or STREAM"
  `(let ((,maybe-encapsulating-stream (or *original-stream* ,stream)))
     ,@body))

;;; Macro for defining methods that delegate to the encapsulated
;;; stream.  Call the delegated method directly if possible, otherwise
;;; collect up optional and rest args and apply.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-gf-lambda-list (ll)
  "Returns the required args, optional args, and presence of rest or key args"
  (let ((state 'required)
        (required-args nil)
        (optional-args nil)
        (rest-or-key nil))
    (loop for arg in ll
          do (cond ((member arg lambda-list-keywords)
                    (when (or (eq arg '&rest)
                              (eq arg '&key)
                              (eq arg '&allow-other-keys))
                      (setq rest-or-key t)
                      (loop-finish))
                    (setq state arg))
                   ((eq state 'required)
                    (push arg required-args))
                   ((eq state '&optional)
                    (push arg optional-args))
                   (t (error "How did I get in this lambda list state?~%~
                              state ~S lambda list ~S"
                             state ll))))
    (values (nreverse required-args) (nreverse optional-args) rest-or-key))))

(defmacro def-stream-method (name lambda-list)
  "stream is a required argument"
  (multiple-value-bind (required optional rest)
      (parse-gf-lambda-list lambda-list)
    (let* ((rest-arg (gensym "REST-ARG"))
           (supplied-vars (mapcar #'(lambda (var)
                                      (gensym (format nil "~A-SUPPLIED-P"
                                                      (symbol-name var))))
                                  optional))
           (ll `(,@required
                 ,@(and optional
                        `(&optional
                          ,@(mapcar #'(lambda (var supplied)
                                        `(,var nil ,supplied))
                                    optional
                                    supplied-vars)))

                 ,@(and rest
                        `(&rest ,rest-arg))))
           (required-params (mapcar #'(lambda (var)
                                        (if (consp var)
                                            (car var)
                                            var))
                                    required))
           (apply-list (gensym "APPLY-LIST"))
           (body (if (and (not optional) (not rest))
                     (if (symbolp name)
                         `(,name ,@required-params)
                         `(funcall #',name ,@required-params))
                     `(let ((,apply-list ,(and rest rest-arg)))
                        ,@(mapcar #'(lambda (var supplied)
                                      `(when ,supplied
                                         (push ,var ,apply-list)))
                                  (reverse optional)
                                  (reverse supplied-vars))
                        (apply #',name ,@required-params ,apply-list)))))
      `(defmethod ,name ,ll
         (let ((*original-stream* stream)
               (stream (slot-value stream 'stream)))
           ,body)))))

;;; The basic input and output stream protocols, as specified by the Gray
;;; stream proposal in Chapter Common Lisp Streams .

;;; Not yet, apparently
#+nil
(def-stream-method streamp ((stream standard-encapsulating-stream)))

(def-stream-method input-stream-p
    ((stream standard-encapsulating-stream)))

(def-stream-method output-stream-p
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-element-type
    ((stream standard-encapsulating-stream)))

(def-stream-method open-stream-p ((stream standard-encapsulating-stream)))

(def-stream-method close ((stream standard-encapsulating-stream)
                          &key abort))

(def-stream-method stream-pathname ((stream standard-encapsulating-stream)))

(def-stream-method stream-truename ((stream standard-encapsulating-stream)))

(def-stream-method stream-read-char ((stream standard-encapsulating-stream)))

(def-stream-method stream-unread-char ((stream standard-encapsulating-stream)
                                       character))

(def-stream-method stream-read-char-no-hang
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-peek-char ((stream standard-encapsulating-stream)))

(def-stream-method stream-listen ((stream standard-encapsulating-stream)))

(def-stream-method stream-read-line ((stream standard-encapsulating-stream)))

(def-stream-method stream-clear-input ((stream standard-encapsulating-stream)))

(def-stream-method stream-write-char
    ((stream standard-encapsulating-stream)
     character))

(def-stream-method stream-line-column ((stream standard-encapsulating-stream)))

(def-stream-method stream-start-line-p
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-write-string
    ((stream standard-encapsulating-stream) string &optional start end))

(def-stream-method stream-terpri ((stream standard-encapsulating-stream)))

(def-stream-method stream-fresh-line ((stream standard-encapsulating-stream)))

(def-stream-method stream-finish-output
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-force-output
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-clear-output
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-advance-to-column
    ((stream standard-encapsulating-stream) column))

(def-stream-method stream-read-byte ((stream standard-encapsulating-stream)))

(def-stream-method stream-write-byte ((stream standard-encapsulating-stream)
                                      integer))

;;; STREAM-LINE-LENGTH is a CMUCL extension to Gray Streams which the
;;; pretty printer seems to use. There's a default method which works
;;; for most CLIM streams. For several dumb reasons it doesn't work on
;;; encapsulating streams.
#+CMU
(defmethod ext:stream-line-length ((stream standard-encapsulating-stream))
  nil)
#+SBCL
(defmethod sb-gray:stream-line-length ((stream standard-encapsulating-stream))
  nil)
