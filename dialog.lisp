;;; Stub for ACCEPTING-VALUES.

(in-package :CLIM-INTERNALS)

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
       (declare (dynamic-extent ,accepting-values-continuation))
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
    (funcall body stream))

