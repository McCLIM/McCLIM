(in-package :clim-clx-fb)

(defclass clx-fb-mirrored-sheet-mixin (image-sheet-mixin mirrored-sheet-mixin)
  ())

(defmethod realize-mirror :after ((port clx-fb-port) (sheet clx-fb-mirrored-sheet-mixin))
  (when-let* ((mirror (sheet-mirror sheet))
              (%image (mirror->%image port mirror)))
    (with-slots (gcontext) %image
      (setf gcontext (xlib:create-gcontext :drawable mirror
					   :background (values 0 0 0)
					   :foreground (values 255 255 255))))))

(defmethod destroy-mirror :before ((port clx-fb-port) (sheet clx-fb-mirrored-sheet-mixin))
  (when-let* ((mirror (sheet-mirror sheet))
              (%image (mirror->%image port mirror)))
    (with-slots (gcontext clx-image) %image
      (xlib:free-gcontext gcontext)
      ;;(xlib:destroy-image clx-image)
      (setf gcontext nil
            clx-image nil))))

