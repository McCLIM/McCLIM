;;;; Support for the Scieneer Common Lisp.


;;;; Gray streams can be defined as subclass of the native stream classes.

(in-package :ext)

(export '(fundamental-stream
	  fundamental-input-stream
	  fundamental-output-stream
	  fundamental-character-stream
	  fundamental-binary-stream
	  fundamental-character-input-stream
	  fundamental-character-output-stream
	  fundamental-binary-input-stream
	  fundamental-binary-output-stream
	  stream-read-line
	  stream-start-line-p
	  stream-write-string
	  stream-terpri
	  stream-fresh-line
	  stream-advance-to-column
	  )
	:ext)

(defclass fundamental-stream (stream)
  ()
  (:documentation "Base class for all CLOS streams"))

;;; Define the stream classes.
(defclass fundamental-input-stream (fundamental-stream ext:input-stream) ())

(defclass fundamental-output-stream (fundamental-stream ext:output-stream) ())

(defclass fundamental-character-stream (fundamental-stream ext:character-stream) ())

(defclass fundamental-binary-stream (fundamental-stream ext:binary-stream) ())

(defclass fundamental-character-input-stream (fundamental-input-stream
					      fundamental-character-stream
					      ext:character-input-stream)
  ())

(defclass fundamental-character-output-stream (fundamental-output-stream
					       fundamental-character-stream
					       ext:character-output-stream)
  ())

(defclass fundamental-binary-input-stream (fundamental-input-stream
					   fundamental-binary-stream
					   ext:binary-input-stream)
  ())

(defclass fundamental-binary-output-stream (fundamental-output-stream
					    fundamental-binary-stream
					    ext:binary-output-stream)
  ())

(defgeneric stream-read-line (stream)
  (:documentation
   "Used by 'read-line.  A string is returned as the first value.  The
  second value is true if the string was terminated by end-of-file
  instead of the end of a line.  The default method uses repeated
  calls to 'stream-read-char."))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (let ((res (make-string 80))
	(len 80)
	(index 0))
    (loop
     (let ((ch (stream-read-char stream)))
       (cond ((eq ch :eof)
	      (return (values (cl::shrink-vector res index) t)))
	     (t
	      (when (char= ch #\newline)
		(return (values (cl::shrink-vector res index) nil)))
	      (when (= index len)
		(setq len (* len 2))
		(let ((new (make-string len)))
		  (replace new res)
		  (setq res new)))
	      (setf (schar res index) ch)
	      (incf index)))))))

(defgeneric stream-start-line-p (stream))

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (eql (stream-line-column stream) 0))

(defgeneric stream-terpri (stream)
  (:documentation
   "Writes an end of line, as for TERPRI.  Returns NIL.  The default
  method does (STREAM-WRITE-CHAR stream #\NEWLINE)."))

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (stream-write-char stream #\Newline))

(defgeneric stream-fresh-line (stream)
  (:documentation
   "Outputs a new line to the Stream if it is not positioned at the
  begining of a line.  Returns 't if it output a new line, nil
  otherwise. Used by 'fresh-line. The default method uses
  'stream-start-line-p and 'stream-terpri."))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defgeneric stream-advance-to-column (stream column)
  (:documentation
   "Writes enough blank space so that the next character will be
  written at the specified column.  Returns true if the operation is
  successful, or NIL if it is not supported for this stream.  This is
  intended for use by by PPRINT and FORMAT ~T.  The default method uses
  STREAM-LINE-COLUMN and repeated calls to STREAM-WRITE-CHAR with a
  #\SPACE character; it returns NIL if STREAM-LINE-COLUMN returns NIL."))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream)
				     column)
  (let ((current-column (stream-line-column stream)))
    (when current-column
      (let ((fill (- column current-column)))
	(dotimes (i fill)
	  (stream-write-char stream #\Space)))
      t)))



(defpackage :clim-mop
  (:use :clos))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
	do (export sym :clim-mop)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(clim-lisp-patch::defconstant
            clim-lisp-patch::defclass)
          :clim-lisp-patch))

(defmacro clim-lisp-patch:defconstant (symbol value &optional docu)
  `(defvar ,symbol ,value ,@(and docu (list docu))))

(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch:defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defclass ,name ,@args))))


