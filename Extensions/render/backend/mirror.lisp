(in-package #:mcclim-render)

(defclass image-mirror-mixin ()
  ((image
    :initform nil
    :accessor image-mirror-image)
   (dirty-region
    :type region
    :initform +nowhere+
    :accessor image-dirty-region)
   (state
    :initform (aa:make-state)
    :reader image-mirror-state)
   (image-lock
    :initform (clim-sys:make-lock "image"))))

(defmethod image-mirror-image ((sheet sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (image-mirror-image mirror)))

(defmethod (setf image-mirror-image) (image (sheet sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (setf (image-mirror-image mirror) image)))

(defmacro with-image-locked ((mirror) &body body)
  `(clim-sys:with-lock-held ((slot-value ,mirror 'image-lock))
     ,@body))

;;; implementation

(defun %set-image-region (mirror region)
  (let ((image (image-mirror-image mirror)))
    (with-bounding-rectangle* (:width w :height h) region
      (setf w (ceiling w))
      (setf h (ceiling h))
      (if (or (null image)
              (/= w (pattern-width image))
              (/= h (pattern-height image)))
          (%create-mirror-image mirror w h)
          image))))

(defmethod %create-mirror-image (mirror width height)
  (setf width (ceiling width))
  (setf height (ceiling height))
  (let ((new-image  (make-image width height)))
    (setf (image-mirror-image mirror) new-image
          (image-dirty-region mirror) +nowhere+)
    new-image))

(defun %notify-image-updated (mirror region)
  (setf (image-dirty-region mirror)
        (region-union (image-dirty-region mirror) region)))

;;; XXX: this is used for scroll
(defun %draw-image (target source x y width height to-x to-y)
  (with-image-locked (target)
    (let* ((src-image (image-mirror-image source))
           (dst-image (image-mirror-image target))
           (region (copy-image src-image x y width height dst-image to-x to-y)))
      (%notify-image-updated target region))))

(defun %fill-image (mirror x1 y1 x2 y2 ink clip-region
                    &optional stencil (x-dest 0) (y-dest 0))
  (with-image-locked (mirror)
    (when-let ((image (image-mirror-image mirror)))
      (let ((region (fill-image image ink x1 y1 x2 y2 clip-region
                                stencil x-dest y-dest)))
        (%notify-image-updated mirror region)))))

(defun %fill-paths (mirror paths transformation region ink)
  (with-image-locked (mirror)
    (when-let ((image (image-mirror-image mirror)))
      (let* ((state (image-mirror-state mirror))
             (region (aa-fill-paths image ink paths state transformation region)))
        (%notify-image-updated mirror region)))))

(defun %stroke-paths (medium mirror paths line-style transformation region ink)
  (with-image-locked (mirror)
    (when-let ((image (image-mirror-image mirror)))
      (let* ((state (image-mirror-state mirror))
             (region (aa-stroke-paths medium image ink paths line-style
                                      state transformation region)))
        (%notify-image-updated mirror region)))))
