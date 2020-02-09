(cl:in-package #:clouseau)

(defmethod object-state-class :around ((object t) (place t))
  (if (tg:weak-pointer-p object)
      'inspected-weak-pointer
      (call-next-method)))

(defclass inspected-weak-pointer (inspected-object)
  ())

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-weak-pointer)
                                       (style  (eql :badges))
                                       (stream clim:extended-output-stream))
  (when (not (tg:weak-pointer-value object))
    (badge stream "broken")))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-weak-pointer)
                                       (style  (eql :expanded-body))
                                       (stream clim:extended-output-stream))
  (when-let ((value (tg:weak-pointer-value object)))
   (formatting-table (stream)
     (format-place-row stream object 'pseudo-place value :label "Value"))))
