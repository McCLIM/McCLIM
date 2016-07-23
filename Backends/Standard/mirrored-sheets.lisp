(in-package :clim-standard)

(defclass standard-mirrored-sheet-mixin (mirrored-sheet-mixin permanent-medium-sheet-output-mixin)
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
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
	    (let ((parent (sheet-parent sheet)))
	      (cond
		((top-level-sheet-pane-p sheet)
		 +identity-transformation+)
		(parent
		 (compose-transformations
		  (invert-transformation
		   (%sheet-mirror-transformation sheet))
		  (compose-transformations
		   (sheet-native-transformation parent)
		   (sheet-transformation sheet))))
		(t
		 (compose-transformations
		  (invert-transformation
		   (%sheet-mirror-transformation sheet))
		  (sheet-transformation sheet)))))))
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


(defmethod note-sheet-transformation-changed :before ((sheet standard-mirrored-sheet-mixin))
  (%update-mirror-geometry sheet))

(defmethod note-sheet-regions-changed :before ((sheet standard-mirrored-sheet-mixin))
  (%update-mirror-geometry sheet))

(defmethod note-sheet-transformation-changed :after ((sheet standard-mirrored-sheet-mixin))
    (loop for child in (sheet-children sheet)
       do (note-parent-mirror-geometry-changed child)))

(defmethod note-sheet-regions-changed :after ((sheet standard-mirrored-sheet-mixin))
    (loop for child in (sheet-children sheet)
       do (note-parent-mirror-geometry-changed child)))


(defun %set-mirror-geometry (sheet x1 y1 x2 y2)
  (let* ((MT (make-translation-transformation x1 y1))
	 (MR (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1)))))
    (setf (%sheet-mirror-region sheet) MR)
    (setf (%sheet-mirror-transformation sheet) MT)
    (when (and (sheet-direct-mirror sheet)
	       (not (eql *configuration-event-p* sheet)))
      (let ((port (port sheet))
	    (mirror (sheet-direct-mirror sheet)))
	(port-set-mirror-region port mirror MR)
	(port-set-mirror-transformation port mirror MT))
      (with-slots (native-transformation device-transformation) sheet
	(setf native-transformation nil
	      device-transformation nil)))))
  
(defmethod %update-mirror-geometry ((sheet standard-mirrored-sheet-mixin))
  (let* ((parent (sheet-parent sheet))
	 (mirrored-ancestor (sheet-mirrored-ancestor parent))
	 (sheet-region-in-native-parent
	  (region-intersection
	   (sheet-native-region parent)
	   (transform-region
	    (sheet-native-transformation parent)
	    (region-intersection
	     (sheet-region parent)
	     (transform-region (sheet-transformation sheet)
			       (sheet-region sheet)))))))
    (if (region-equal sheet-region-in-native-parent +nowhere+)
	(%set-mirror-geometry sheet -5 -5 1 1)
	(with-bounding-rectangle* (mx1 my1 mx2 my2)
	    sheet-region-in-native-parent
	  (%set-mirror-geometry sheet mx1 my1 mx2 my2)))))


(defgeneric note-parent-mirror-geometry-changed (sheet))

(defmethod note-parent-mirror-geometry-changed ((sheet standard-mirrored-sheet-mixin))
  (note-sheet-transformation-changed sheet)
  (note-sheet-region-changed sheet))

(defmethod note-parent-mirror-geometry-changed ((sheet basic-sheet))
  (loop for child in (sheet-children sheet)
     do (note-parent-mirror-geometry-changed child)))


 
