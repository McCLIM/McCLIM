(in-package :clim-clx-fb)

(defclass clx-fb-mirrored-sheet-mixin (image-sheet-mixin mirrored-sheet-mixin)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (sheet-direct-xmirror (sheet-direct-mirror sheet))))

(defmethod sheet-direct-xmirror ((mirror clx-fb-mirror))
  (with-slots (xmirror) mirror
    xmirror))

(defmethod sheet-direct-xmirror ((mirror image-mirror-mixin))
    nil)

(defmethod realize-mirror :after ((port clx-fb-port) (sheet clx-fb-mirrored-sheet-mixin))
  (when (and (sheet-mirror sheet) (sheet-xmirror sheet))
    (with-slots (gcontext) (sheet-mirror sheet)
      (setf gcontext (xlib:create-gcontext :drawable (sheet-xmirror sheet)
					   :background (values 0 0 0)
					   :foreground (values 255 255 255))))))

(defmethod destroy-mirror :before ((port clx-fb-port) (sheet clx-fb-mirrored-sheet-mixin))
  (declare (ignore port))
  (with-slots (gcontext clx-image) (sheet-mirror sheet)
    (xlib:free-gcontext gcontext)
    ;;(xlib:destroy-image clx-image)
    (setf gcontext nil
	  clx-image nil)))

(defclass clx-fb-pixmap (image-pixmap-mixin basic-pane)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-pixmap))
  nil)
