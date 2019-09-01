(cl:in-package #:clim-clx-fb)

(defclass clx-fb-mirrored-sheet-mixin (image-sheet-mixin mirrored-sheet-mixin)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-mirrored-sheet-mixin))
  (alexandria:when-let ((direct-mirror (sheet-direct-mirror sheet)))
    (sheet-direct-xmirror direct-mirror)))

(defmethod sheet-direct-xmirror ((mirror image-mirror-mixin))
    nil)

;;;;; this is evil.
(defmethod allocate-space :after ((sheet clx-fb-mirrored-sheet-mixin) width height)
  (alexandria:when-let ((direct-xmirror (sheet-direct-xmirror sheet)))
    (with-slots (space-requirement) sheet
      ;; TODO this is disabled
      '(setf (xlib:wm-normal-hints direct-xmirror)
             (xlib:make-wm-size-hints
              :width (round width)
              :height (round height)
              :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
              :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
              :min-width (round (space-requirement-min-width space-requirement))
              :min-height (round (space-requirement-min-height space-requirement)))))))

(defmethod realize-mirror :after ((port render-port-mixin) (sheet clx-fb-mirrored-sheet-mixin))
  (alexandria:when-let ((mirror (sheet-mirror sheet))
                        (xmirror (sheet-xmirror sheet)))
    (with-slots (gcontext) mirror
      (setf gcontext (xlib:create-gcontext :drawable xmirror
                                           :background (mcclim-render-internals::%vals->rgba 0   0   0)
                                           :foreground (mcclim-render-internals::%vals->rgba 255 255 255))))))

(defmethod destroy-mirror :before ((port render-port-mixin) (sheet clx-fb-mirrored-sheet-mixin))
  (declare (ignore port))
  (with-slots (gcontext x-image) (sheet-mirror sheet)
    (xlib:free-gcontext gcontext)
    ;;(xlib:destroy-image clx-image)
    (setf gcontext nil
	  x-image nil)))

(defclass clx-fb-pixmap (image-pixmap-mixin basic-pane)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-pixmap))
  nil)
