(in-package #:mcclim-render-internals)

(defclass image-pixmap-mixin (image-mirror-mixin)
  ((width :initarg :width :accessor pixmap-width)
   (height :initarg :height :accessor pixmap-height)))

(defmethod pixmap-depth ((pixmap image-pixmap-mixin))
  (declare (ignore pixmap))
  32)

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y
                             width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-finish-output from-drawable)
  (multiple-value-bind (w h)
      (multiple-value-call #'transform-distance
        (sheet-transformation (medium-sheet to-drawable))
        (untransform-distance (medium-transformation from-drawable)
                              width height))
    (with-bounding-rectangle* (min-x min-y max-x max-y)
        (region-intersection
         (medium-device-region to-drawable)
         (transform-region (medium-native-transformation to-drawable)
                           (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
      (multiple-value-bind (x1 y1)
          (transform-position (medium-native-transformation to-drawable)
                              to-x to-y)
        (multiple-value-bind (x2 y2)
            (transform-position
             (medium-native-transformation from-drawable)
             from-x from-y)
          (%medium-draw-image to-drawable
                              (medium-sheet from-drawable)
                              (+ x2 (- min-x x1))
                              (+ y2 (- min-y y1))
                              (- max-x min-x) (- max-y min-y)
                              min-x min-y))))))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y
                             width height
                             (to-drawable image-pixmap-mixin) to-x to-y)
  (multiple-value-bind (w h)
      (untransform-distance (medium-transformation from-drawable)
                            width height)
    (with-bounding-rectangle* (min-x min-y max-x max-y)
        (region-intersection
         (make-rectangle* 0 0
                          (pixmap-width to-drawable)
                          (pixmap-height to-drawable))
         (make-rectangle* to-x to-y (+ to-x w) (+ to-y h)))
      (multiple-value-bind (x2 y2)
          (transform-position
           (medium-native-transformation from-drawable)
           from-x from-y)
        (%medium-draw-image to-drawable
                            (medium-sheet from-drawable)
                            (+ x2 (- min-x to-x))
                            (+ y2 (- min-y to-y))
                            (- max-x min-x) (- max-y min-y)
                            min-x min-y)))))

(defmethod medium-copy-area ((from-drawable image-pixmap-mixin) from-x from-y
                             width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (when-let* ((to-sheet (medium-sheet to-drawable))
              (to-native (medium-native-transformation to-drawable))
              (to-transformation (sheet-transformation to-sheet)))
    (when (medium-drawable to-drawable)
      (multiple-value-bind (w h)
          (transform-distance to-transformation width height)
        (with-bounding-rectangle* (min-x min-y max-x max-y)
            (region-intersection
             (climi::medium-device-region to-drawable)
             (transform-region to-native
                               (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
          (multiple-value-bind (x1 y1)
              (transform-position to-native to-x to-y)
            (%medium-draw-image to-drawable
                                from-drawable
                                (+ from-x (- min-x x1))
                                (+ from-y (- min-y y1))
                                (- max-x min-x) (- max-y min-y)
                                min-x min-y)))))))

(defmethod medium-copy-area ((from-drawable image-pixmap-mixin) from-x from-y
                             width height
                             (to-drawable image-pixmap-mixin) to-x to-y)
  (%medium-draw-image to-drawable from-drawable
                      from-x from-y width height to-x to-y))
