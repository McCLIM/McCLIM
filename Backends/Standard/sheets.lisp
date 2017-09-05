(in-package :clim-standard)

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


(defmethod sheet-direct-mirror ((sheet standard-mirrored-sheet-mixin))
  (port-lookup-mirror (port sheet) sheet))

(defmethod (setf sheet-direct-mirror) (mirror (sheet standard-mirrored-sheet-mixin))
  (port-register-mirror (port sheet) sheet mirror))


(defparameter *configuration-event-p* nil)

(defmethod handle-event ((sheet standard-mirrored-sheet-mixin)
			 (event window-configuration-event))
  (let ((x (window-configuration-event-x event))
	(y (window-configuration-event-y event))
	(width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    (let ((*configuration-event-p* sheet))
      (setf (sheet-transformation sheet) (make-translation-transformation x y))
      (setf (sheet-region sheet) (make-bounding-rectangle 0 0 width height)))))

;;;
;;; mirror geometry
;;;

(defparameter *mirrored-sheet-geometry-changed-p* nil)

(defmethod (setf sheet-transformation) :around (tr (sheet standard-mirrored-sheet-mixin))
  (when (sheet-mirror sheet)
    (let ((*mirrored-sheet-geometry-changed-p* sheet))
      (call-next-method))))

(defmethod (setf sheet-region) :around (re (sheet standard-mirrored-sheet-mixin))
  (when (or (/= (bounding-rectangle-width re) (bounding-rectangle-width (sheet-region sheet)))
	    (/= (bounding-rectangle-height re) (bounding-rectangle-height (sheet-region sheet))))
    (let ((*mirrored-sheet-geometry-changed-p* sheet))
      (call-next-method)
      (dispatch-repaint sheet (sheet-region sheet)))))

(defmethod note-sheet-transformation-changed :before ((sheet standard-mirrored-sheet-mixin))
  (when (sheet-mirror sheet)
    (%update-mirror-geometry sheet)))

(defmethod note-sheet-region-changed :before ((sheet standard-mirrored-sheet-mixin))
  (when (sheet-mirror sheet)
    (%update-mirror-geometry sheet)))

(defgeneric %update-mirror-geometry (sheet))
  
(defmethod %update-mirror-geometry ((sheet standard-mirrored-sheet-mixin))
  ())

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

;;;
;;;

(defun repaint-background (sheet child region)
  (labels ((effective-repaint-region (mirrored-sheet child region)
	     (if (eq mirrored-sheet child)
		 (region-intersection
		  (sheet-region mirrored-sheet)
		  region)
		 (effective-repaint-region mirrored-sheet
					   (sheet-parent child)
					   (transform-region
					    (sheet-transformation child)
					    (region-intersection
					     region
					     (sheet-region child)))))))
    (let ((native-child-region (effective-repaint-region sheet child region)))
      (with-sheet-medium (medium sheet)
	(with-drawing-options (medium :clipping-region native-child-region
				      :ink (pane-background child)
				      :transformation +identity-transformation+)
	  (with-bounding-rectangle* (left top right bottom)
	    native-child-region
	    (medium-draw-rectangle* sheet left top right bottom t)))))))

;;;
;;;
;;;

(defmethod note-sheet-enabled :after ((sheet standard-mirrored-sheet-mixin))
 (when (sheet-direct-mirror sheet)
   (port-enable-sheet (port sheet) sheet)))

(defmethod note-sheet-disabled :after ((sheet standard-mirrored-sheet-mixin))
 (when (sheet-direct-mirror sheet)
   (port-disable-sheet (port sheet) sheet)))

(defmethod %note-mirrored-sheet-child-enabled :after ((sheet standard-mirrored-sheet-mixin) child)
  (dispatch-repaint sheet (sheet-native-region child)))

(defmethod %note-mirrored-sheet-child-disabled :after ((sheet standard-mirrored-sheet-mixin) child)
  (dispatch-repaint sheet (sheet-native-region child)))

(defmethod %note-mirrored-sheet-child-region-changed :after
    ((sheet standard-mirrored-sheet-mixin) child)
  (unless (eql sheet *mirrored-sheet-geometry-changed-p*)
    (dispatch-repaint sheet (sheet-native-region child))))

(defmethod %note-mirrored-sheet-child-transformation-changed :after
    ((sheet standard-mirrored-sheet-mixin) child)
  (unless (eql sheet *mirrored-sheet-geometry-changed-p*)
    (dispatch-repaint sheet (sheet-native-region child))))

(defmethod %note-sheet-pointer-cursor-changed :after ((sheet standard-mirrored-sheet-mixin))
  (set-sheet-pointer-cursor (port sheet) sheet (sheet-pointer-cursor sheet)))

(defmethod %note-mirrored-sheet-child-repaint-request 
    ((sheet standard-mirrored-sheet-mixin) child region)
  (repaint-background sheet child region))

(defmethod %note-mirrored-sheet-child-repaint-request 
    ((sheet standard-mirrored-sheet-mixin) (child always-repaint-background-mixin) region)
  nil)

(defmethod %note-mirrored-sheet-child-repaint-request 
    ((sheet standard-mirrored-sheet-mixin) (child never-repaint-background-mixin) region)
  nil)

(defmethod %note-sheet-repaint-request ((sheet always-repaint-background-mixin)
					region)
  (repaint-background sheet sheet region))
    
