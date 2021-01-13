(in-package :clim-clx)

(defclass clx-mirror ()
  ((window
    :initarg :window
    :reader window)
   (picture
    :initform nil
    :accessor %clx-mirror-picture))
  (:default-initargs :window (alexandria:required-argument :window)))

(defun clx-drawable-format (drawable)
  (let ((root (xlib:drawable-root drawable)))
    (xlib:find-window-picture-format root)))

(defun clx-drawable-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :picture)
      (setf (getf (xlib:drawable-plist drawable) :picture)
            (xlib:render-create-picture
             drawable :format (clx-drawable-format drawable)))))

(defmethod destroy-mirror ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (when-let ((picture (find-if (alexandria:of-type 'xlib::picture)
                                   (xlib:window-plist window))))
        (xlib:render-free-picture picture))
      (remf (xlib:window-plist window) 'sheet)
      (xlib:destroy-window window)
      (xlib:display-force-output (clx-port-display port)))))

;;; Return a string in which every non-STANDARD-CHAR in STRING has
;;; been replaced with #\_. The result is guaranteed to be an ASCII
;;; string.
(defun %ensure-standard-characters (string)
  (substitute-if-not #\_ (alexandria:of-type 'standard-char) string))

(defun %set-window-name (window name)
  (setf (xlib:wm-name window) (%ensure-standard-characters name))
  (xlib:change-property window
                        :_NET_WM_NAME
                        (babel:string-to-octets name :encoding :utf-8)
                        :UTF8_STRING 8))

(defun %set-window-icon-name (window icon-name)
  (setf (xlib:wm-icon-name window) (%ensure-standard-characters icon-name))
  (xlib:change-property window
                        :_NET_WM_ICON_NAME
                        (babel:string-to-octets icon-name :encoding :utf-8)
                        :UTF8_STRING 8))

(defmethod port-set-mirror-name
    ((port clx-basic-port) (sheet mirrored-sheet-mixin) name)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (%set-window-name window name)
      (%set-window-icon-name window name)
      (xlib:display-force-output (xlib:drawable-display window)))))

;;; The format of the _NET_WM_ICON property is described in
;;; "Application Window Properties" section of the "Extended Window
;;; Manager Hints" specification:
;;; https://specifications.freedesktop.org/wm-spec/1.5/ar01s05.html#idm45766085139216
(defun %mirror-install-icons (window icons)
  ;; The initial size of 2 bytes for the width and height plus 16 x 16
  ;; bytes for the pixels of a single icon is just a guess. The vector
  ;; is extended dynamically for larger and/or multiple icons.
  (if (typep icons 'sequence)
      (when (alexandria:emptyp icons)
        (return-from %mirror-install-icons
          (xlib:delete-property window :_NET_WM_ICON)))
      (setf icons (list icons)))
  (let ((bytes (make-array (+ 2 (* 16 16)) :element-type '(unsigned-byte 32)
                                           :adjustable t :fill-pointer 0)))
    (flet ((pack-icon (icon)
             (let* ((width (pattern-width icon))
                    (height (pattern-height icon))
                    (pixel-count (* width height))
                    (pixels (clime:pattern-array icon)))
               ;; For each icon, the first two elements contain the
               ;; width and the height respectively. The remaining
               ;; elements contain the icon pixels represented in an
               ;; ARGB pixel format.
               (vector-push-extend width bytes)
               (vector-push-extend height bytes)
               (loop for i below pixel-count
                     do (vector-push-extend
                         (row-major-aref pixels i) bytes)))))
      ;; Pack the one or more icons in ICONS into the BYTES vector.
      (mapc #'pack-icon icons))
    (xlib:change-property window :_NET_WM_ICON bytes :cardinal 32)))

(defmethod port-set-mirror-icon
    ((port clx-basic-port) (sheet mirrored-sheet-mixin) icon)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (%mirror-install-icons (window mirror) icon)))

(defmethod port-set-mirror-region
    ((port clx-basic-port) (sheet mirrored-sheet-mixin) region)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (with-bounding-rectangle* (x1 y1 x2 y2) region
        (declare (ignore x1 y1))
        (setf (xlib:drawable-width window) (round-coordinate x2)
              (xlib:drawable-height window) (round-coordinate y2))))))

(defmethod port-set-mirror-transformation
    ((port clx-basic-port) (sheet mirrored-sheet-mixin) transformation)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (multiple-value-bind (x y) (transform-position transformation 0 0)
        (setf (xlib:drawable-x window) (round-coordinate x)
              (xlib:drawable-y window) (round-coordinate y))))))

(defmethod raise-mirror ((port clx-basic-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (setf (xlib:window-priority (window mirror)) :above)
    (xlib:display-force-output (clx-port-display port))))

(defmethod bury-mirror ((port clx-basic-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (setf (xlib:window-priority (window mirror)) :below)
    (xlib:display-force-output (clx-port-display port))))

(defmethod port-enable-sheet ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (xlib:map-window (window mirror))
    (xlib:display-force-output (clx-port-display port))))

(defmethod port-disable-sheet ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (xlib:unmap-window (window mirror))
    (xlib:display-force-output (clx-port-display port))))

(defmethod port-shrink-sheet ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (xlib:iconify-window (window mirror) (clx-port-screen port))
    (xlib:display-force-output (clx-port-display port))))
