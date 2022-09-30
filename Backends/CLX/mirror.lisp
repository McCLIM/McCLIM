(in-package #:clim-clx)

(defconstant +icccm-withdrawn-state+ 0)
(defconstant +icccm-normal-state+ 1)
(defconstant +icccm-iconic-state+ 3)

(defclass clx-mirror ()
  ((window
    :initarg :window
    :reader window)
   (buffer
    :initarg :buffer
    :accessor buffer)
   (buffering-p
    :initarg :buffering-p
    :accessor buffering-p))
  (:default-initargs :window (alexandria:required-argument :window)
                     :buffer nil
                     :buffering-p nil))

(defmethod (setf buffering-p) :before (new-value (mirror clx-mirror))
  (when (and new-value (null (buffering-p mirror)))
    (let* ((window (window mirror))
           (width (xlib:drawable-width window))
           (height (xlib:drawable-height window))
           (buffer (buffer mirror)))
      (when (or (null buffer)
                (< (xlib:drawable-width buffer) width)
                (< (xlib:drawable-height buffer) height))
        (and buffer (%deallocate-pixmap buffer))
        (setf buffer (%allocate-pixmap window width height)
              (buffer mirror) buffer))
      (%drawable-copy-area window 0 0 width height buffer 0 0))))

(defun swap-buffers (mirror)
  (let* ((buffer (buffer mirror))
         (window (window mirror)))
    (%drawable-copy-area buffer 0 0
                         (xlib:drawable-width buffer)
                         (xlib:drawable-height buffer)
                         window 0 0)))

(defun clx-drawable (object)
  (etypecase object
    (sheet  (clx-drawable (sheet-mirror object)))
    (medium (clx-drawable (medium-drawable object)))
    (clx-mirror (if (buffering-p object)
                    (buffer object)
                    (window object)))
    (xlib:drawable object)
    (null nil)))

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

(defmethod port-set-mirror-geometry
    ((port clx-basic-port) (sheet mirrored-sheet-mixin) region)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
      (with-bounding-rectangle* (ox1 oy1 :width ow :height oh) (sheet-mirror-geometry sheet)
        (let ((window (window mirror)))
          (when (or (/= x1 ox1) (/= y1 oy1))
            (setf (xlib:drawable-x window) (round-coordinate x1)
                  (xlib:drawable-y window) (round-coordinate y1)))
          (when (or (/= w ow) (/= h oh))
            (setf (xlib:drawable-width window) (round-coordinate w)
                  (xlib:drawable-height window) (round-coordinate h)))))
      (values x1 y1 x2 y2))))

(defmethod destroy-mirror ((port clx-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (remf (xlib:window-plist window) 'sheet)
      (xlib:destroy-window window)
      (xlib:display-force-output (clx-port-display port)))
    (when-let ((buffer (buffer mirror)))
      (%deallocate-pixmap buffer))))

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
  ;; Not all x11 window-managers iconify windows (in particular many
  ;; tiling window-managers). In those window managers, after the call
  ;; of shrink-frame McClim think that the frame is iconified but it
  ;; is not. -- admich 2021-08-21
  (when-let ((window (window (sheet-direct-mirror sheet))))
    (let ((wm-state (car (xlib:get-property window :WM_STATE))))
      (cond
        ((or (eq +icccm-withdrawn-state+ wm-state) (not wm-state))
         (let ((hints (xlib:wm-hints window)))
           (setf (xlib:wm-hints-initial-state hints) :iconic)
           (setf (xlib:wm-hints window) hints)
           (xlib:map-window window)
           (xlib:display-force-output (clx-port-display port))))
        ((eq +icccm-normal-state+ wm-state)
         (xlib:iconify-window window (clx-port-screen port))
         (xlib:display-force-output (clx-port-display port)))
        ((eq +icccm-iconic-state+ wm-state) nil)))))
