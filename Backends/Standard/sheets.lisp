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
      (setf (sheet-region sheet) (make-bounding-rectangle 0 0 width height))
      (setf (sheet-transformation sheet) (make-translation-transformation x y)))))


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
;;;

(defmethod note-sheet-enabled :after ((sheet standard-mirrored-sheet-mixin))
 (when (sheet-direct-mirror sheet)
   (port-enable-sheet (port sheet) sheet)))

(defmethod note-sheet-disabled :after ((sheet standard-mirrored-sheet-mixin))
 (when (sheet-direct-mirror sheet)
   (port-disable-sheet (port sheet) sheet)))

(defmethod %note-mirrored-sheet-child-enabled :after ((sheet standard-mirrored-sheet-mixin) child)
  (dispatch-event sheet
		  (make-instance 'window-repaint-event
				 :sheet sheet
				 :region (sheet-native-region child))))

(defmethod %note-mirrored-sheet-child-disabled :after ((sheet standard-mirrored-sheet-mixin) child)
  (dispatch-event sheet
		  (make-instance 'window-repaint-event
				 :sheet sheet
				 :region (sheet-native-region child))))

(defmethod %note-mirrored-sheet-child-region-changed :after
    ((sheet standard-mirrored-sheet-mixin) child)
   (when (and (sheet-viewable-p sheet)
	     (not (graftp sheet)))
    (dispatch-event sheet
		    (make-instance 'window-repaint-event
				   :sheet sheet
				   :region (sheet-native-region child)))))

(defmethod %note-mirrored-sheet-child-transformation-changed :after
    ((sheet standard-mirrored-sheet-mixin) child)
   (when (and (sheet-viewable-p sheet)
	     (not (graftp sheet)))
    (dispatch-event sheet
		    (make-instance 'window-repaint-event
				   :sheet sheet
				   :region (sheet-native-region child)))))

(defmethod %note-sheet-pointer-cursor-changed :after ((sheet standard-mirrored-sheet-mixin))
  (set-sheet-pointer-cursor (port sheet) sheet (sheet-pointer-cursor sheet)))

