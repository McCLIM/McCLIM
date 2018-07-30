(in-package :mcclim-image)

(defmethod medium-draw-pattern* (medium (pattern rgb-pattern) x y transformation)
  ;; If we don't disable the stream recording here, we're going to
  ;; create two separate output records for the same image drawing.
  ;; This will be both slow, and may cause some display problems.
  (if (typep medium 'standard-output-recording-stream)
      (let ((old-recording-p (stream-recording-p medium)))
        (setf (stream-recording-p medium) nil)
        (unwind-protect
             (medium-draw-image-design* medium pattern x y transformation)
          (setf (stream-recording-p medium) old-recording-p)))
      ;; ELSE: Not a recording stream, no need to preserve the recording state
      (medium-draw-image-design* medium pattern x y transformation)))

(defmethod medium-free-image-design ((sheet sheet-with-medium-mixin) design)
  (medium-free-image-design (sheet-medium sheet) design))

(defmethod medium-draw-image-design* :before (current-medium design x y transformation)
  (with-slots (medium medium-data) design
    (unless (eq medium current-medium)
      (when medium
	(medium-free-image-design medium design))
      (setf medium current-medium)
      (setf medium-data nil))))

(defmethod medium-draw-image-design*
    ((medium sheet-with-medium-mixin) design x y transformation)
  (medium-draw-image-design* (sheet-medium medium) design x y transformation))
