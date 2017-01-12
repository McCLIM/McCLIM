(in-package :clim-clx-fb)

(defclass clx-fb-mirrored-sheet-mixin (mcclim-render::image-sheet-mixin
				       standard-single-mirrored-sheet-mixin)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-mirrored-sheet-mixin))
  (with-slots (xmirror) (sheet-direct-mirror sheet)
    xmirror))

;;;
;;; Updating
;;;

(defmethod repaint-sheet :around ((sheet clx-fb-mirrored-sheet-mixin) region)
  (when (sheet-mirror sheet)
    (with-slots (updating-p) (sheet-mirror sheet)
      (let ((old-updating-p updating-p))
	(setf updating-p t)
	(call-next-method)
	(setf updating-p old-updating-p)))))

(defmethod allocate-space :around ((sheet clx-fb-mirrored-sheet-mixin) width height)
  (when (sheet-mirror sheet)
    (with-slots (updating-p) (sheet-mirror sheet)
      (let ((old-updating-p updating-p))
	(setf updating-p t)
	(call-next-method)
	(setf updating-p old-updating-p)))))

;;;
;;;
;;;

(defmethod realize-mirror :after ((port render-port-mixin) (sheet clx-fb-mirrored-sheet-mixin))
  (when (and (sheet-mirror sheet) (sheet-xmirror sheet))
    (with-slots (gcontext) (sheet-mirror sheet)
      (setf gcontext (xlib:create-gcontext :drawable (sheet-xmirror sheet)
					   :background (values 0 0 0)
					   :foreground (values 255 255 255))))))

(defmethod destroy-mirror :before ((port render-port-mixin) (sheet clx-fb-mirrored-sheet-mixin))
  (declare (ignore port))
  (with-slots (gcontext) (sheet-mirror sheet)
    (xlib:free-gcontext gcontext)))

(defclass clx-fb-pixmap (image-pixmap-mixin permanent-medium-sheet-output-mixin basic-pane)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-pixmap))
  nil)
