(in-package #:mcclim-render)

;;; Port

(defclass render-port-mixin (ttf-port-mixin basic-port)
  ((all-font-families :initform nil :accessor all-font-families)
   (mirror->%image :initform (make-hash-table))))

(defun mirror->%image (port mirror)
  (gethash mirror (slot-value port 'mirror->%image)))

(defun (setf mirror->%image) (%image port mirror)
  (check-type %image (or null image-mirror-mixin))
  (if %image
      (setf (gethash mirror (slot-value port 'mirror->%image)) %image)
      (remhash mirror (slot-value port 'mirror->%image))))

;;; change geometry

(defmethod distribute-event :before
    ((port render-port-mixin) (event window-configuration-event))
  (let ((sheet (event-sheet event))
        (width (climi::window-configuration-event-width event))
        (height (climi::window-configuration-event-height event)))
    (when-let ((mirror (sheet-direct-mirror sheet)))
      (%set-image-region (mirror->%image port mirror)
                         (make-bounding-rectangle 0 0 width height)))))

(defmethod port-set-mirror-geometry :after
    ((port render-port-mixin) (sheet mirrored-sheet-mixin) region)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (with-bounding-rectangle* (:width w :height h) region
      (%set-image-region (mirror->%image port mirror)
                         (make-rectangle* 0 0 w h)))))

;;; realize/destroy mirrors

;;; We return a gensym because the RENDER-PORT-MIXIN maintains a hash table
;;; which maps mirrors to images - a key must be unique. -- jd 2020-11-09
(defmethod realize-mirror ((port render-port-mixin) (sheet mirrored-sheet-mixin))
  (gensym "RENDER-PORT-MIRROR"))
