(in-package :clim-standard)


(defclass standard-event-port-mixin ()
  ((pointer-grab-sheet :accessor pointer-grab-sheet :initform nil)
   (port-pointer-pressed-sheet :initform nil :accessor port-pointer-pressed-sheet))
  (:documentation "Standard event distribution."))

(defmethod distribute-event ((port standard-event-port-mixin) event)
  (let ((grab-sheet (pointer-grab-sheet port)))
    (cond
      ((typep event 'keyboard-event)
       (dispatch-event (event-sheet event) event))
      ((typep event 'window-event)
       (dispatch-event (event-sheet event) event))
      ((typep event 'pointer-event)
       (if grab-sheet
	   (queue-event grab-sheet event)
	   (dispatch-event (event-sheet event) event)))
      ((typep event 'window-manager-delete-event)
       ;; not sure where this type of event should get sent - mikemac
       ;; This seems fine; will be handled by the top-level-sheet-pane - moore
       (dispatch-event (event-sheet event) event))
      ((typep event 'timer-event)
       (error "Where do we send timer-events?"))
      (t
       (error "Unknown event ~S received in DISTRIBUTE-EVENT" event)))))

;;;
;;; pointer events
;;;
(defmethod distribute-event :before ((port standard-event-port-mixin)
                                     (event pointer-button-press-event))
  (setf (port-pointer-pressed-sheet port) (port-pointer-sheet port)))

(defmethod distribute-event :after ((port standard-event-port-mixin)
                                    (event pointer-button-release-event))
  (setf (port-pointer-pressed-sheet port) nil))

(defmethod distribute-event :around ((port standard-event-port-mixin) (event pointer-event))
  (let ((grab-sheet (pointer-grab-sheet port))
        (pointer-pressed-sheet (port-pointer-pressed-sheet port))
        (pointer-sheet (get-pointer-event-sheet (event-sheet event) event))
        (old-pointer-sheet (or (port-pointer-sheet port) (event-sheet event)))
        (top-level-sheet (get-top-level-sheet (event-sheet event))))
    (cond
      (grab-sheet
       (setf (port-pointer-sheet port) pointer-sheet)
       (call-next-method)
       nil)
      (pointer-pressed-sheet
       (if (sheet-ancestor-p pointer-sheet pointer-pressed-sheet)
	   (setf pointer-sheet pointer-pressed-sheet)
	   (let ((parent (sheet-parent pointer-pressed-sheet)))
             (if (or (null parent)
                     (sheet-ancestor-p pointer-sheet parent))
                 nil
                 (unless (or
                          (typep (event-sheet event) 'unmanaged-top-level-sheet-pane)
                          (typep top-level-sheet 'unmanaged-top-level-sheet-pane))
                   (setf pointer-sheet nil))))))
      (t
       nil))
    (unless grab-sheet
      ;; distribute exit and enter events
      (let ((common-sheet (sheet-common-ancestor old-pointer-sheet
                                                 (or pointer-sheet
                                                     top-level-sheet))))
	(distribute-exit-events old-pointer-sheet common-sheet event)
	(distribute-enter-events pointer-sheet common-sheet event)
	(setf (port-pointer-sheet port) pointer-sheet))
      ;; set the pointer cursor
      (when pointer-sheet
	(let ((pointer-cursor
	       (sheet-pointer-cursor pointer-sheet)))
	  (unless (eql (port-lookup-current-pointer-cursor port (event-sheet event))
		       pointer-cursor)
	    (set-sheet-pointer-cursor port (event-sheet event) pointer-cursor))))
      (unless (or (typep event 'pointer-enter-event)
		  (typep event 'pointer-exit-event))
	(cond
	  ((typep top-level-sheet 'unmanaged-top-level-sheet-pane)
	   (call-next-method))
	  ((or grab-sheet pointer-pressed-sheet)
	   (cond
	     ((eq pointer-sheet (or grab-sheet pointer-pressed-sheet))
	      (call-next-method))
	     ((or (typep event 'pointer-button-release-event)
		  (typep event 'pointer-motion-event))
	      ;; send event to ...
	      (setf (port-pointer-sheet port) (or grab-sheet pointer-pressed-sheet))
	      (call-next-method)
	      (setf (port-pointer-sheet port) pointer-sheet))
	     (t
	      nil)))
	  (t	 
	   (call-next-method)))))))

(defmethod distribute-event ((port standard-event-port-mixin) (event pointer-event))
  (alexandria:when-let* ((sheet (port-pointer-sheet port))
                         (destination (or (pointer-grab-sheet port) sheet)))
    (if (eq sheet (event-sheet event))
        (dispatch-event sheet event)
        ;; events are immutable (explicitly stated in the spec) - that's why we
        ;; need to make an event copy for single-mirrored sheets - event-sheet
        ;; is not the same as sheet we want to distribute event to.
        (let ((new-event (climi::shallow-copy-object event)))
          (setf (slot-value new-event 'climi::sheet) sheet)
          (unless (eq (sheet-mirrored-ancestor sheet)
                      (sheet-mirrored-ancestor (event-sheet event)))
            (multiple-value-bind (cx cy)
                (untransform-position (sheet-delta-transformation (sheet-mirrored-ancestor sheet) nil)
                                      (slot-value new-event 'climi::graft-x)
                                      (slot-value new-event 'climi::graft-y))
              (setf (slot-value new-event 'climi::x) cx
                    (slot-value new-event 'climi::y) cy)))
          (dispatch-event destination new-event)))))

;;;
;;; selection
;;;


(defmethod distribute-event ((port standard-event-port-mixin)
				    (event selection-event))
  (let ((owner (port-selection-owner port)))
    (if owner
	(progn
	  (setf (slot-value event 'clim::sheet) owner)
	  (dispatch-event owner event))
	(dispatch-event (event-sheet event)
			event))))


(defmethod distribute-event ((port standard-event-port-mixin)
				    (event selection-notify-event))
  (let ((owner (port-selection-requester port)))
    (if owner
	(progn
	  (setf (slot-value event 'clim::sheet) owner)
	  (dispatch-event owner event))
	(dispatch-event (event-sheet event)
			event))))


;;;
;;; all events
;;;

(defun distribute-enter-events (sheet-b sheet-t event)
  (dolist (s
            (do ((s sheet-b (sheet-parent s))
                 (lis nil))
                ((or (null s) (climi::graftp s) (eq s sheet-t)) lis)
              (push s lis)))
    (let ((new-event (climi::shallow-copy-object event)))
      ;; should we change also `climi::x' and `climi::y'?
      (setf (slot-value new-event 'climi::sheet) s)
      (change-class new-event 'pointer-enter-event)
      (dispatch-event s new-event))))

(defun distribute-exit-events (sheet-b sheet-t event)
  (when (and sheet-t sheet-b)
    (do ((s sheet-b (sheet-parent s)))
        ((or (null s) (climi::graftp s) (eq s sheet-t)))
      (let ((new-event (climi::shallow-copy-object event)))
        ;; should we change also `climi::x' and `climi::y'?
        (setf (slot-value new-event 'climi::sheet) s)
        (change-class new-event 'pointer-exit-event)
        (dispatch-event s new-event)))))


(defun sheet-common-ancestor (sheet-a sheet-b)
  (cond
    ((or (null sheet-a) (null sheet-b))
     nil)
    ((climi::graftp sheet-a)
     sheet-a)
    ((sheet-ancestor-p sheet-b sheet-a)
     sheet-a)
    (t
     (sheet-common-ancestor (sheet-parent sheet-a) sheet-b))))

(defun get-top-level-sheet (sheet)
  (cond
    ((null sheet)
     nil)
    ((typep sheet 'top-level-sheet-pane)
     sheet)
    (t
     (get-top-level-sheet (sheet-parent sheet)))))

(defun get-pointer-event-sheet (sheet event)
  (labels ((get-pointer-event-sheet-2 (sheet x y)
             (let ((child (child-containing-position sheet x y)))
               ;; only not mirrored child?
               (if child
                   (multiple-value-bind (cx cy)
                       (untransform-position (sheet-transformation child) x y)
                     (get-pointer-event-sheet-2 child  cx cy))
                   sheet))))
    (climi::get-pointer-position (sheet event)
      (get-pointer-event-sheet-2 sheet climi::x climi::y))))
