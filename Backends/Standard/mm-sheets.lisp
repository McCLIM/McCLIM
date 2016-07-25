(in-package :clim-standard)

(defclass standard-multi-mirrored-sheet-mixin (standard-mirrored-sheet-mixin)
  ())

(defmethod sheet-native-region ((sheet standard-multi-mirrored-sheet-mixin))
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

(defmethod sheet-native-transformation ((sheet standard-multi-mirrored-sheet-mixin))
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


(defmethod note-sheet-transformation-changed :after ((sheet standard-multi-mirrored-sheet-mixin))
  (repaint-mirrored-sheet-child (sheet-mirrored-ancestor sheet) sheet)
  (loop for child in (sheet-children sheet)
     do (note-parent-mirror-geometry-changed child)))

(defmethod note-sheet-regions-changed :after ((sheet standard-multi-mirrored-sheet-mixin))
  (repaint-mirrored-sheet-child (sheet-mirrored-ancestor sheet) sheet)
  (loop for child in (sheet-children sheet)
     do (note-parent-mirror-geometry-changed child)))
  

(defgeneric note-parent-mirror-geometry-changed (sheet))

(defmethod note-parent-mirror-geometry-changed ((sheet standard-multi-mirrored-sheet-mixin))
  (note-sheet-transformation-changed sheet)
  (note-sheet-region-changed sheet))

(defmethod note-parent-mirror-geometry-changed ((sheet basic-sheet))
  
  (loop for child in (sheet-children sheet)
     do (note-parent-mirror-geometry-changed child)))

(defmethod %update-mirror-geometry ((sheet standard-multi-mirrored-sheet-mixin))
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

