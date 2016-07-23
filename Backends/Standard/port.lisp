(in-package :clim-standard)

(defclass standard-port (basic-port)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

(defgeneric note-port-sheet-region-changed (sheet port))
(defgeneric note-port-sheet-transformation-changed (sheet port))



(defmethod note-port-sheet-region-changed (sheet port)
  (declare (ignore sheet port))
  nil)

(defmethod note-port-sheet-region-changed (sheet (port standard-port))
  (when (and (sheet-viewable-p sheet)
	     (not (graftp sheet)))
    (dispatch-event (sheet-mirrored-ancestor sheet)
		    (make-instance 'window-repaint-event
				   :sheet (sheet-mirrored-ancestor sheet)
				   :region (sheet-native-region sheet)))))

(defmethod note-port-sheet-transformation-changed (sheet port)
  (declare (ignore sheet port))
  nil)

(defmethod note-port-sheet-transformation-changed (sheet (port standard-port))
  (when (and (sheet-viewable-p sheet)
	     (not (graftp sheet)))
    (dispatch-event (sheet-mirrored-ancestor sheet)
		    (make-instance 'window-repaint-event
				   :sheet (sheet-mirrored-ancestor sheet)
				   :region (sheet-native-region sheet)))))

;;;
;;; 
;;;

(defmethod note-sheet-region-changed :after ((sheet basic-sheet))
  (note-port-sheet-region-changed sheet (port sheet)))

(defmethod note-sheet-transformation-changed :after ((sheet basic-sheet))
  (note-port-sheet-transformation-changed sheet (port sheet)))
