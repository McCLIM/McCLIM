;;; Stub for ACCEPTING-VALUES.

#| Random notes:
The state of the accepting-values form is kept in an updating-output
record.  The calls to accept happen within updating-output forms.

An accepting-values stream diverts the calls to accept into calling
accept-present-default, as described in the spec.  This also controls
whether accept creates a new record or just returns the default value.
|#

(in-package :clim-internals)

(defclass accepting-values-record (standard-updating-output-record)
  ())

(defclass accepting-values-stream (standard-encapsulating-stream)
  ())

(defmacro accepting-values
    ((&optional (stream t)
      &rest args
      &key own-window exit-boxes initially-select-query-identifier
           modify-initial-query resynchronize-every-pass resize-frame
           align-prompts label scroll-bars
           x-position y-position width height command-table frame-class)
     &body body)
  (declare (ignorable own-window exit-boxes initially-select-query-identifier
            modify-initial-query resynchronize-every-pass resize-frame
            align-prompts label scroll-bars
            x-position y-position width height command-table frame-class))
  (when (eq stream 't)
    (setq stream '*standard-input*))
  (check-type stream symbol)
  (with-gensyms (accepting-values-continuation)
    `(flet ((,accepting-values-continuation (,stream)
              ,@body))
       (invoke-accepting-values ,stream
                                #',accepting-values-continuation
                                ,@args))))

(defun invoke-accepting-values
    (stream body
     &key own-window exit-boxes initially-select-query-identifier
     modify-initial-query resynchronize-every-pass resize-frame
     align-prompts label scroll-bars
     x-position y-position width height command-table
     (frame-class 'accept-values))
  (let* ((astream (make-instance 'accepting-values-stream
				 :stream stream))
	 (arecord (updating-output (stream
				    :record-type 'accepting-values-stream)
		    (funcall body astream))))
    ))

(defun invoke-accepting-values
    (stream body
     &key own-window exit-boxes initially-select-query-identifier
     modify-initial-query resynchronize-every-pass resize-frame
     align-prompts label scroll-bars
     x-position y-position width height command-table
     (frame-class 'accept-values))
    (funcall body stream))

