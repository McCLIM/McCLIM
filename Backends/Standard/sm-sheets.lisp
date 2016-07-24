(in-package :clim-standard)


(defclass standard-single-mirrored-sheet-mixin (standard-mirrored-sheet-mixin)
    ())

(defmethod sheet-native-transformation ((sheet standard-single-mirrored-sheet-mixin))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation +identity-transformation+))
    native-transformation))

(defmethod %update-mirror-geometry ((sheet standard-single-mirrored-sheet-mixin))
  (let* ((parent (sheet-parent sheet))
	 (sheet-region-in-native-parent
	  (region-intersection
	   (sheet-region parent)
	   (transform-region (sheet-transformation sheet)
			     (sheet-region sheet)))))
    (if (region-equal sheet-region-in-native-parent +nowhere+)
	(%set-mirror-geometry sheet -5 -5 1 1)
	(with-bounding-rectangle* (mx1 my1 mx2 my2)
	    sheet-region-in-native-parent
	  (%set-mirror-geometry sheet mx1 my1 mx2 my2)))))
