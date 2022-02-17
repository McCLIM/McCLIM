(in-package #:climi)

(defmacro with-output-to-drawing-stream ((stream backend destination &rest args) &body body)
  (with-gensyms (cont)
    `(flet ((,cont (,stream) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-output-to-drawing-stream (function ,cont) ,backend ,destination ,@args))))

(defmethod invoke-with-output-to-drawing-stream (continuation backend destination &rest args)
  (let ((port (find-port :server-path backend)))
    (apply #'invoke-with-output-to-drawing-stream continuation port destination args)))

(defmethod invoke-with-output-to-drawing-stream (cont (backend basic-port) (window null) &rest args)
  (let ((window (apply #'open-window-stream :port backend args)))
    (multiple-value-prog1 (funcall cont window)
      (finish-output window))))
