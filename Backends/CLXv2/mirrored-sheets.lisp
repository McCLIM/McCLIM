(in-package :clim-clxv2)

(defclass clxv2-mirrored-sheet-mixin (standard-mirrored-sheet-mixin)
  ())


(defmethod (setf clim:sheet-region) (region (sheet clxv2-mirrored-sheet-mixin))
  (declare (ignore region))
  (call-next-method)
  (update-mirror-geometry sheet))

(defmethod (setf clim:sheet-transformation) (region (sheet clxv2-mirrored-sheet-mixin))
  (declare (ignore region))
  (call-next-method)
  (update-mirror-geometry sheet))

(defmethod clim:invalidate-cached-regions :after ((sheet clxv2-mirrored-sheet-mixin))

  )

(defmethod clim:invalidate-cached-transformations :after ((sheet clxv2-mirrored-sheet-mixin))

)


(defun update-mirror-geometry (sheet &key)
  (flet ((set-nowhere (sheet)
	   (setf (%sheet-mirror-transformation sheet)
		 (make-translation-transformation -5 -5))
	   (setf (%sheet-mirror-region sheet) (make-rectangle* 0 0 1 1))
	   (when (and (sheet-direct-mirror sheet)
		      (not (eql *configuration-event-p* sheet)))
	     (port-set-mirror-region
	      (port sheet)
	      (sheet-direct-mirror sheet)
	      (%sheet-mirror-region sheet))
	     (port-set-mirror-transformation
	      (port sheet)
	      (sheet-direct-mirror sheet)
	      (%sheet-mirror-transformation sheet)))))
    (cond ((null (sheet-parent sheet))
	   ;; Ugh, we have no parent, this must be the graft, we
	   ;; cannot resize it can we?
	   nil)
	  ;; Otherwise, the native transformation has to changed or
	  ;; needs to be computed initially
	  (t
	   (let* ((parent (sheet-parent sheet))
		  (mirrored-ancestor (sheet-mirrored-ancestor parent))
		  (sheet-region-in-native-parent
		   ;; this now is the wanted sheet mirror region
		   (region-intersection
		    (sheet-native-region parent)
		    (transform-region
		     (sheet-native-transformation parent)
		     (region-intersection
		      (sheet-region parent)
		      (transform-region (sheet-transformation sheet)
					(sheet-region sheet)))))))
	     (when (region-equal sheet-region-in-native-parent +nowhere+)
	       (set-nowhere sheet)
	       (return-from update-mirror-geometry))
	     ;; mx1 .. my2 are is now the wanted mirror region in the
	     ;; parent coordinate system.
	     (with-bounding-rectangle* (mx1 my1 mx2 my2)
		 sheet-region-in-native-parent
	       (let* ((MT (make-translation-transformation mx1 my1))
		      (MR (make-rectangle* 0 0 (round (- mx2 mx1)) (round (- my2 my1))))
		      (native-transformation
		       ;; NT = T o PNT o -MT
		       (compose-transformations
			(invert-transformation MT)
			(compose-transformations
			 (sheet-native-transformation (sheet-parent sheet))
			 (sheet-transformation sheet)))))
		 (cond ((and (> (round (- mx2 mx1)) 0)
			     (> (round (- my2 my1)) 0))
			;; finally reflect the change to the host window system
			(setf (%sheet-mirror-region sheet) MR)
			(setf (%sheet-mirror-transformation sheet) MT)
			(when (and (sheet-direct-mirror sheet)
				   (not (eql *configuration-event-p* sheet)))
			  (let ((port (port sheet))
				(mirror (sheet-direct-mirror sheet)))
			 
			    (port-set-mirror-region port mirror MR)
			    (port-set-mirror-transformation port mirror MT)))
			;; update the native transformation if neccessary.
			  ;;(invalidate-cached-transformations sheet)
			;;(%%set-sheet-native-transformation native-transformation sheet))
			)
		       (t
			(set-nowhere sheet))))))))))
