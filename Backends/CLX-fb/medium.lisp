(in-package :clim-clx-fb)

;;; Clx-Fb-MEDIUM class

(defclass clx-fb-medium (render-medium-mixin basic-medium)
  ())

(defmethod medium-finish-output :before ((medium clx-fb-medium))
  (when-let ((mirror (medium-drawable medium)))
    (when (typep mirror 'clx-fb-mirror)
      (%mirror-force-output mirror))))

(defmethod medium-force-output :before ((medium clx-fb-medium))
  (when-let ((mirror (medium-drawable medium)))
    (when (typep mirror 'clx-fb-mirror)
      (%mirror-force-output mirror))))
