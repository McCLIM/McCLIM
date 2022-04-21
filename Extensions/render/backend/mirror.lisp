(in-package #:mcclim-render)

(defclass image-mirror-mixin ()
  ((image :initform nil :accessor image-mirror-image)
   (image-lock :initform (clim-sys:make-lock "image"))
   (dirty-region :type region :initform +nowhere+)
   (state :initform (aa:make-state))))

(defmethod image-mirror-image ((sheet sheet))
  (when-let* ((mirror (sheet-mirror sheet))
              (image (mirror->%image (port sheet) mirror)))
    (image-mirror-image image)))

(defmethod (setf image-mirror-image) (image (sheet sheet))
  (assert (not (null image)))
  (when-let* ((mirror (sheet-mirror sheet))
              (%image (mirror->%image (port sheet) mirror)))
    (setf (image-mirror-image %image) image)))

;;; implementation

(defun %make-image (mirror width height)
  (check-type mirror image-mirror-mixin)
  (setf width (ceiling width))
  (setf height (ceiling height))
  (with-slots (image) mirror
    (%create-mirror-image mirror (1+ width) (1+ height))))

(defun %set-image-region (mirror region)
  (check-type mirror image-mirror-mixin)
  (with-slots (image) mirror
    (with-bounding-rectangle* (:width width :height height) region
      (let ((width (1+ (ceiling width)))
            (height (1+ (ceiling height))))
        (if (or (null image)
                (/= width (pattern-width image))
                (/= height (pattern-height image)))
            (%create-mirror-image mirror width height)
            nil)))))

(defmethod %create-mirror-image (mirror width height)
  (check-type mirror image-mirror-mixin)
  (with-slots (image dirty-region) mirror
    (setf image (make-image width height))
    (setf dirty-region +nowhere+)))

(defun %notify-image-updated (mirror region)
  (check-type mirror image-mirror-mixin)
  (when region
    (with-slots (dirty-region) mirror
      (setf dirty-region (region-union dirty-region region)))))

;;; XXX: this is used for scroll
(defun %draw-image (mirror src-image x y width height to-x to-y clip-region)
  (check-type mirror image-mirror-mixin)
  (when-let ((image (image-mirror-image mirror)))
    (unless (and (rectanglep clip-region)
                 (region-contains-region-p clip-region
                                           (make-rectangle* to-x
                                                            to-y
                                                            (+ to-x width)
                                                            (+ to-y height))))
      (warn "copy image not correct"))
    (with-slots (image-lock) mirror
      (clim-sys:with-lock-held (image-lock)
        (let* ((image (image-mirror-image mirror))
               (region (copy-image src-image x y width height image to-x to-y)))
          (%notify-image-updated mirror region))))))

(defun %fill-image (mirror x y width height ink clip-region
                    &optional stencil (x-dest 0) (y-dest 0))
  (check-type mirror image-mirror-mixin)
  (when-let ((image (image-mirror-image mirror)))
    #+(or) ;; XXX: clip-region is not correct hance warnings
    (when (or (not (rectanglep clip-region))
              (not (region-contains-region-p clip-region (make-rectangle* x y (+ x width) (+ y height)))))
      (warn "fill image mask not correct [~A -> ~A]" clip-region (make-rectangle* x y (+ x width) (+ y height))))
    (with-slots (image-lock) mirror
      (clim-sys:with-lock-held (image-lock)
        (let ((region (fill-image image ink
                                  :x x :y y :width width :height height
                                  :stencil stencil
                                  :stencil-dx x-dest :stencil-dy y-dest
                                  :clip-region clip-region)))
          (%notify-image-updated mirror region))))))

(defun %fill-paths (mirror paths transformation region ink)
  (check-type mirror image-mirror-mixin)
  (when-let ((image (image-mirror-image mirror)))
    (with-slots (image-lock state) mirror
      (clim-sys:with-lock-held (image-lock)
        (let ((reg (aa-fill-paths image ink paths state transformation region)))
          (with-bounding-rectangle* (min-x min-y max-x max-y) reg
            (%notify-image-updated mirror (make-rectangle* (floor min-x) (floor min-y)
                                                           (ceiling max-x) (ceiling max-y)))))))))

(defun %stroke-paths (medium mirror paths line-style transformation region ink)
  (check-type mirror image-mirror-mixin)
  (when-let ((image (image-mirror-image mirror)))
    (with-slots (image-lock state) mirror
      (clim-sys:with-lock-held (image-lock)
        (let ((reg (aa-stroke-paths medium image ink paths line-style state transformation region)))
          (with-bounding-rectangle* (min-x min-y max-x max-y) reg
            (%notify-image-updated mirror (make-rectangle* (floor min-x) (floor min-y)
                                                           (ceiling max-x) (ceiling max-y)))))))))
