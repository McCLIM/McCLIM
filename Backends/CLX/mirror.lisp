(in-package :clim-clx)

(defmethod port-set-mirror-region ((port clx-basic-port) mirror mirror-region)
  (with-bounding-rectangle* (x1 y1 x2 y2) mirror-region
    (declare (ignore x1 y1))
    (setf (xlib:drawable-width mirror) (floor x2)
          (xlib:drawable-height mirror) (floor y2))))
                                   
(defmethod port-set-mirror-transformation
    ((port clx-basic-port) mirror mirror-transformation)
  (multiple-value-bind (x y) (transform-position mirror-transformation 0 0)
    (setf (xlib:drawable-x mirror) (floor x)
          (xlib:drawable-y mirror) (floor y))))

(defmethod destroy-mirror ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when (sheet-xmirror sheet)
    (xlib:destroy-window (sheet-xmirror sheet)))
  (when (port-lookup-mirror port sheet)
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod raise-mirror ((port clx-basic-port) (sheet basic-sheet))
  (let ((mirror (sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      (xlib:circulate-window-up mirror))))

(defmethod bury-mirror ((port clx-basic-port) (sheet basic-sheet))
  (let ((mirror (sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      (xlib:circulate-window-down mirror))))

(defmethod mirror-transformation ((port clx-basic-port) mirror)
  (make-translation-transformation (xlib:drawable-x mirror)
                                   (xlib:drawable-y mirror)))

(defmethod port-enable-sheet ((port clx-basic-port) (mirror mirrored-sheet-mixin))
  (xlib:map-window (sheet-direct-xmirror mirror)) )

(defmethod port-disable-sheet ((port clx-basic-port) (mirror mirrored-sheet-mixin))
  (xlib:unmap-window (sheet-direct-xmirror mirror)) )
