(in-package #:mcclim-render)

;;; Port

(defclass render-port-mixin (ttf-port-mixin basic-port)
  ((all-font-families :initform nil :accessor all-font-families)))

;;; change geometry

(defmethod port-set-mirror-geometry :after
    ((port render-port-mixin) (sheet mirrored-sheet-mixin) region)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (with-bounding-rectangle* (:width w :height h) region
      (%set-image-region mirror (make-rectangle* 0 0 w h)))))

(defmethod realize-mirror ((port render-port-mixin) (sheet mirrored-sheet-mixin))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (multiple-value-bind (width height)
        (bounding-rectangle-size sheet)
      (%create-mirror-image mirror width height))
    mirror))

(defmethod destroy-mirror ((port render-port-mixin) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (setf (image-mirror-image mirror) nil)))
