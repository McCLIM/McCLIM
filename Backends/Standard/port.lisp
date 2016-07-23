(in-package :clim-standard)

(defclass standard-port (basic-port)
  ((sheet->pointer-cursor :initform (make-hash-table :test #'eq))
   (mirrored-sheet->current-pointer-cursor :initform (make-hash-table :test #'eq))))

(defmethod port-lookup-pointer-cursor ((port standard-port) sheet)
  (gethash sheet (slot-value port 'sheet->pointer-cursor)))

(defmethod port-lookup-current-pointer-cursor ((port standard-port) sheet)
  (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)))

(defmethod set-sheet-pointer-cursor ((port standard-port) sheet cursor)
  (setf (gethash sheet (slot-value port 'sheet->pointer-cursor)) cursor))

(defmethod set-sheet-current-pointer-cursor :before ((port standard-port) sheet cursor)
  (setf (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)) cursor))



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

(defmethod (setf sheet-enabled-p) :after (new-value (sheet basic-sheet))
  (when (sheet-mirrored-ancestor sheet)
    (dispatch-event (sheet-mirrored-ancestor sheet)
		    (make-instance 'window-repaint-event
				   :sheet (sheet-mirrored-ancestor sheet)
				   :region (sheet-native-region sheet)))))
