(in-package :clim-standard)

;;; We assume the following limitations of the host window systems:
;;;
;;;  mirror transformations:
;;;   . can only be translations
;;;   . are limited to 16-bit signed integer deltas
;;;
;;;  mirror regions:
;;;   . can only be axis-aligend rectangles
;;;   . min-x = min-y = 0
;;;   . max-x, max-y < 2^16
;;;
;;; These are the limitations of the X Window System.
;;;

(defclass standard-mirrored-sheet-mixin (mirrored-sheet-mixin)
  ((mirror-transformation
    :documentation "Our idea of the current mirror transformation. Might not
                    be correct if a foreign application changes our mirror's geometry."
    :initform +identity-transformation+
    :accessor %sheet-mirror-transformation)
   (mirror-region
    :documentation "Our idea of the current mirror region. Might not be
correct if a foreign application changes our mirror's geometry. Also note
that this might be different from the sheet's native region."
    :initform nil
    :accessor %sheet-mirror-region)))

(defmethod invalidate-cached-transformations ((sheet standard-mirrored-sheet-mixin))
  (with-slots (native-transformation device-transformation) sheet
    (setf native-transformation nil
     device-transformation nil))
  (loop for child in (sheet-children sheet)
        do (invalidate-cached-transformations child)))

(defmethod sheet-native-region ((sheet standard-mirrored-sheet-mixin))
  (with-slots (native-region) sheet     
    (unless native-region      
      (let ((this-region (transform-region (sheet-native-transformation sheet)
					   (sheet-region sheet)))
	    (parent (sheet-parent sheet)))
	(setf native-region
	      (if parent
		  (region-intersection this-region
				       (transform-region
					(invert-transformation
					 (%sheet-mirror-transformation sheet))
					(sheet-native-region parent)))
		  this-region))))
    native-region))

(defmethod sheet-native-transformation ((sheet standard-mirrored-sheet-mixin))
  ;; XXX hm...
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
	    (let ((parent (sheet-parent sheet)))
	      (if parent
		  (compose-transformations
		   (invert-transformation
		    (%sheet-mirror-transformation sheet))
		   (compose-transformations
		    (sheet-native-transformation parent)
		    (sheet-transformation sheet)))
		  +identity-transformation+))))
      native-transformation))


(defparameter *configuration-event-p* nil)

(defmethod handle-event ((sheet standard-mirrored-sheet-mixin)
			 (event window-configuration-event))
  (let ((x (window-configuration-event-x event))
	(y (window-configuration-event-y event))
	(width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    (let ((*configuration-event-p* sheet))
      (setf (sheet-region sheet) (make-bounding-rectangle 0 0 width height))
      (setf (sheet-transformation sheet) (make-translation-transformation x y)))))
