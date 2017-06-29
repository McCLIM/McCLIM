(in-package :mcclim-render)

(defclass image-sheet-mixin (mirrored-sheet-mixin design)
  ((updating-p :initform nil)))

(defmethod allocate-space :before ((sheet image-sheet-mixin) width height)
  (when (sheet-mirror sheet)
    (let ((region (make-rectangle* 0 0 width height)))
      (%set-image-region (sheet-mirror sheet) region))))

(defmethod repaint-sheet :around ((sheet image-sheet-mixin) region)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (with-slots (updating-p) mirror
        (let ((old-updating-p updating-p))
          (setf updating-p t)
          (call-next-method)
          (setf updating-p old-updating-p)))
      (with-slots (image-lock) mirror
        (climi::with-lock-held (image-lock)
          (%notify-image-updated mirror nil))))))

(defmethod allocate-space :around ((sheet image-sheet-mixin) width height)
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (with-slots (updating-p) mirror
        (let ((old-updating-p updating-p))
          (setf updating-p t)
          (call-next-method)
          (setf updating-p old-updating-p)))
      (with-slots (image-lock) mirror
        (climi::with-lock-held (image-lock)
          (%notify-image-updated mirror nil))))))
