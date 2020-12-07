(in-package :clim-clx-fb)

;;; Clx-Fb-MEDIUM class

(defclass clx-fb-medium (render-medium-mixin basic-medium)
  ())

(defmethod medium-finish-output :before ((medium clx-fb-medium))
  (when-let* ((mirror (medium-drawable medium))
              (image (mirror->%image (port medium) mirror)))
    (when (typep image 'clx-fb-mirror)
      (%mirror-force-output image))))

(defmethod medium-force-output :before ((medium clx-fb-medium))
  (when-let* ((mirror (medium-drawable medium))
              (image (mirror->%image (port medium) mirror)))
    (when (typep image 'clx-fb-mirror)
      (%mirror-force-output image))))
