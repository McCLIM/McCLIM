(in-package #:clim-clx)

;;; Pixmap

(defmethod allocate-pixmap ((medium clx-medium) width height)
  (when-let ((drawable (medium-drawable medium)))
    (xlib:create-pixmap :width (ceiling width)
                        :height (ceiling height)
                        :depth (xlib:drawable-depth drawable)
                        :drawable drawable)))

(defmethod deallocate-pixmap ((pixmap xlib:pixmap))
  (when-let ((picture (find-if (alexandria:of-type 'xlib::picture)
                               (xlib:pixmap-plist pixmap))))
    (xlib:render-free-picture picture))
  (xlib:free-pixmap pixmap))

(defmethod pixmap-width ((pixmap xlib:pixmap))
  (xlib:drawable-width pixmap))

(defmethod pixmap-height ((pixmap xlib:pixmap))
  (xlib:drawable-height pixmap))

(defmethod pixmap-depth ((pixmap xlib:pixmap))
  (xlib:drawable-depth pixmap))

;;; WIDTH and HEIGHT arguments should be integers, but we'll leave the calls
;;; to round "in" for now.

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position
      ((medium-native-transformation from-drawable) from-x from-y)
    (with-transformed-position
        ((medium-native-transformation to-drawable) to-x to-y)
      (multiple-value-bind (width height)
          (transform-distance (medium-transformation from-drawable) width height)
        (xlib:copy-area (medium-drawable from-drawable)
                        (medium-gcontext to-drawable +background-ink+)
                        (round-coordinate from-x) (round-coordinate from-y)
                        (round width) (round height)
                        (medium-drawable to-drawable)
                        (round-coordinate to-x) (round-coordinate to-y))))))

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable xlib:drawable) to-x to-y)
  (with-transformed-position
      ((medium-native-transformation from-drawable) from-x from-y)
    (let ((gcontext (xlib:create-gcontext :drawable to-drawable)))
      (xlib:copy-area (medium-drawable from-drawable)
                      gcontext
                      (round-coordinate from-x)
                      (round-coordinate from-y)
                      (round width)
                      (round height)
                      to-drawable
                      (round-coordinate to-x)
                      (round-coordinate to-y))
      (xlib:free-gcontext gcontext))))

(defmethod medium-copy-area ((from-drawable xlib:drawable) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((medium-native-transformation to-drawable) to-x to-y)
    (xlib:copy-area from-drawable
                    (medium-gcontext to-drawable +background-ink+)
                    (round-coordinate from-x) (round-coordinate from-y)
                    (round width) (round height)
                    (medium-drawable to-drawable)
                    (round-coordinate to-x) (round-coordinate to-y))))

(defmethod medium-copy-area ((from-drawable xlib:drawable) from-x from-y width height
                             (to-drawable xlib:drawable) to-x to-y)
  (let ((gcontext (xlib:create-gcontext :drawable to-drawable)))
    (xlib:copy-area from-drawable
                    gcontext
                    (round-coordinate from-x) (round-coordinate from-y)
                    (round width) (round height)
                    to-drawable
                    (round-coordinate to-x) (round-coordinate to-y))
    (xlib:free-gcontext gcontext)))
