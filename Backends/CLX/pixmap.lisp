(in-package #:clim-clx)

;;; Pixmap

(defun %allocate-pixmap (drawable width height)
  (let ((pixmap (xlib:create-pixmap :width width
                                    :height height
                                    :depth (xlib:drawable-depth drawable)
                                    :drawable drawable)))
    (let ((gcontext (xlib:create-gcontext :drawable pixmap)))
      (unwind-protect
           (progn
             (setf (xlib:gcontext-function gcontext) boole-1)
             (setf (xlib:gcontext-foreground gcontext) #x00000000)
             (xlib:draw-rectangle pixmap gcontext 0 0 width height t))
        (xlib:free-gcontext gcontext)))
    pixmap))

(defun %deallocate-pixmap (drawable)
  (when-let ((picture (find-if (alexandria:of-type 'xlib::picture)
                               (xlib:pixmap-plist drawable))))
    (xlib:render-free-picture picture))
  (xlib:free-pixmap drawable))

(defmethod allocate-pixmap ((medium clx-medium) width height)
  (when-let ((mirror (medium-drawable medium)))
    (let* ((window (window mirror))
           (width (ceiling width))
           (height (ceiling height))
           (pixmap (%allocate-pixmap window width height)))
      (make-instance 'clx-mirror :window pixmap))))

(defmethod deallocate-pixmap ((pixmap clx-mirror))
  (let ((pixmap (clx-drawable pixmap)))
    (%deallocate-pixmap pixmap)))

(defmethod pixmap-width ((pixmap clx-mirror))
  (xlib:drawable-width (window pixmap)))

(defmethod pixmap-height ((pixmap clx-mirror))
  (xlib:drawable-height (window pixmap)))

(defmethod pixmap-depth ((pixmap clx-mirror))
  (xlib:drawable-depth (window pixmap)))

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
        (xlib:copy-area (clx-drawable from-drawable)
                        (medium-gcontext to-drawable +background-ink+)
                        (round-coordinate from-x) (round-coordinate from-y)
                        (round width) (round height)
                        (clx-drawable to-drawable)
                        (round-coordinate to-x) (round-coordinate to-y))))))

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable clx-mirror) to-x to-y)
  (with-transformed-position
      ((medium-native-transformation from-drawable) from-x from-y)
    (let* ((to-drawable (clx-drawable to-drawable))
           (gcontext (xlib:create-gcontext :drawable to-drawable)))
      (xlib:copy-area (clx-drawable from-drawable)
                      gcontext
                      (round-coordinate from-x)
                      (round-coordinate from-y)
                      (round width)
                      (round height)
                      to-drawable
                      (round-coordinate to-x)
                      (round-coordinate to-y))
      (xlib:free-gcontext gcontext))))

(defmethod medium-copy-area ((from-drawable clx-mirror) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((medium-native-transformation to-drawable) to-x to-y)
    (xlib:copy-area (clx-drawable from-drawable)
                    (medium-gcontext to-drawable +background-ink+)
                    (round-coordinate from-x) (round-coordinate from-y)
                    (round width) (round height)
                    (clx-drawable to-drawable)
                    (round-coordinate to-x) (round-coordinate to-y))))

(defun %drawable-copy-area (from fx fy w h to tx ty)
  (let ((gcontext (xlib:create-gcontext :drawable to)))
    (unwind-protect (xlib:copy-area from gcontext fx fy w h to tx ty)
      (xlib:free-gcontext gcontext))))

(defmethod medium-copy-area ((from-drawable clx-mirror) from-x from-y width height
                             (to-drawable clx-mirror) to-x to-y)
  (%drawable-copy-area (clx-drawable from-drawable)
                       (round-coordinate from-x) (round-coordinate from-y)
                       (round width) (round height)
                       (clx-drawable to-drawable)
                       (round-coordinate to-x) (round-coordinate to-y)))
