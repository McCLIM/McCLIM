(in-package :clim-mezzano)

(defclass mezzano-mirrored-sheet-mixin (image-sheet-mixin
                                        standard-single-mirrored-sheet-mixin)
  ())

(defmethod sheet-direct-xmirror ((sheet mezzano-mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (sheet-direct-xmirror (sheet-direct-mirror sheet))))

(defmethod sheet-direct-xmirror ((mirror mezzano-mirror))
  (debug-format "sheet-direct-xmirror ((mirror mezzano-mirror))")
  (debug-format "    ~S" mirror)
  (break)
  ;; (with-slots (xmirror) mirror
  ;;   xmirror)
  )

(defmethod sheet-direct-xmirror ((mirror image-mirror-mixin))
    nil)

;;;
;;; Updating
;;;

;;;;; this is evil.
(defmethod allocate-space :after ((sheet mezzano-mirrored-sheet-mixin) width height)
;; TODO - does mezzano need these hints?
  ;; (when (sheet-direct-xmirror sheet)
  ;;   (with-slots (space-requirement) sheet
  ;;     '(setf (xlib:wm-normal-hints (sheet-direct-xmirror sheet))
  ;;           (xlib:make-wm-size-hints
  ;;            :width (round width)
  ;;            :height (round height)
  ;;            :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
  ;;            :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
  ;;            :min-width (round (space-requirement-min-width space-requirement))
  ;;            :min-height (round (space-requirement-min-height space-requirement))))))
)

;;;
;;;

(defmethod destroy-mirror :before ((port render-port-mixin) (sheet mezzano-mirrored-sheet-mixin))
  (declare (ignore port))
  ;; TODO destroy surface, fifo, window, frame
  ;; set mez-pixels mez-window mez-frame to NIL - do I need handle to surface?
  )

(defclass mezzano-pixmap (image-pixmap-mixin permanent-medium-sheet-output-mixin basic-pane)
  ())

(defmethod sheet-direct-xmirror ((sheet mezzano-pixmap))
  nil)
