(in-package :clim-standard)


(defclass standard-event-port-mixin ()
  ((pointer-grab-sheet :accessor pointer-grab-sheet :initform nil)))

(defmethod distribute-event ((port standard-event-port-mixin) event)
  (let ((grab-sheet (pointer-grab-sheet port)))
    (if grab-sheet
	(queue-event grab-sheet event)
	(cond
	  ((typep event 'keyboard-event)
	   (dispatch-event (event-sheet event) event))
	  ((typep event 'window-event)
	   (dispatch-event (event-sheet event) event))
	  ((typep event 'pointer-event)
	   (dispatch-event (event-sheet event) event))
	  ((typep event 'window-manager-delete-event)
	   ;; not sure where this type of event should get sent - mikemac
	   ;; This seems fine; will be handled by the top-level-sheet-pane - moore
	   (dispatch-event (event-sheet event) event))
	  ((typep event 'timer-event)
	   (error "Where do we send timer-events?"))
	  (t
	   (error "Unknown event ~S received in DISTRIBUTE-EVENT" event))))))

(defclass standard-handled-event-port-mixin (standard-event-port-mixin)
  ((port-pointer-pressed-sheet :initform nil :accessor port-pointer-pressed-sheet)))


;;;
;;; pointer events
;;;
(defmethod climi::distribute-event :before ((port standard-handled-event-port-mixin)
					    (event pointer-button-press-event))
  (setf (port-pointer-pressed-sheet port) (port-pointer-sheet port)))

(defmethod climi::distribute-event :after ((port standard-handled-event-port-mixin)
					   (event pointer-button-release-event))
  (setf (port-pointer-pressed-sheet port) nil))

(defmethod climi::distribute-event :around ((port standard-handled-event-port-mixin) (event pointer-event))
  (let ((grab-sheet (pointer-grab-sheet port))
        (pointer-pressed-sheet (port-pointer-pressed-sheet port))
        (pointer-sheet (get-pointer-event-sheet (event-sheet event) event))
        (old-pointer-sheet (or (port-pointer-sheet port) (event-sheet event)))
        (top-level-sheet (get-top-level-sheet (event-sheet event))))
    (cond
      (grab-sheet
       (setf (port-pointer-sheet port) pointer-sheet)
       (call-next-method)
       ;;(if (sheet-ancestor-p pointer-sheet grab-sheet)
       ;;    (setf pointer-sheet grab-sheet)
       ;;    (setf pointer-sheet (sheet-parent grab-sheet))))
       nil)
      ((typep (event-sheet event) 'unmanaged-top-level-sheet-pane)
       nil)
      ((typep top-level-sheet 'unmanaged-top-level-sheet-pane)
       nil)
      (pointer-pressed-sheet
       (if (sheet-ancestor-p pointer-sheet pointer-pressed-sheet)
           (setf pointer-sheet pointer-pressed-sheet)
           (setf pointer-sheet (sheet-parent pointer-pressed-sheet))))
      (t
       nil))
    (unless grab-sheet
      ;; distribute exit and enter events
      (let ((common-sheet (sheet-common-ancestor old-pointer-sheet pointer-sheet)))
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


(defmethod climi::distribute-event ((port standard-handled-event-port-mixin) (event pointer-event))
  (let* ((sheet (port-pointer-sheet port))
	 (grab-sheet (pointer-grab-sheet port))
	 (destination (or (pointer-grab-sheet port) sheet)))
    (when sheet
      (cond ((eq sheet (event-sheet event))
	     (dispatch-event sheet event))
            ((eq (sheet-mirrored-ancestor sheet) (sheet-mirrored-ancestor (event-sheet event)))
	     (dispatch-event destination
			     (make-instance (type-of event)
					    :pointer (slot-value event 'climi::pointer)
					    :button (slot-value event 'climi::button)
					    :x (slot-value event 'climi::x)
					    :y (slot-value event 'climi::y)
					    :graft-x (slot-value event 'climi::graft-x)
					    :graft-y (slot-value event 'climi::graft-y)
					    :sheet sheet
					    :modifier-state (slot-value event 'climi::modifier-state)
					    :timestamp (slot-value event 'climi::timestamp))))
	    (t
	     (multiple-value-bind (cx cy)
		 (untransform-position (sheet-delta-transformation (sheet-mirrored-ancestor sheet) nil)
				     (slot-value event 'climi::graft-x)
				     (slot-value event 'climi::graft-y))
	     (dispatch-event destination
			     (make-instance (type-of event)
					    :pointer (slot-value event 'climi::pointer)
					    :button (slot-value event 'climi::button)
					    :x cx
					    :y cy
					    :graft-x (slot-value event 'climi::graft-x)
					    :graft-y (slot-value event 'climi::graft-y)
					    :sheet sheet
					    :modifier-state (slot-value event 'climi::modifier-state)
					    :timestamp (slot-value event 'climi::timestamp)))))))))



;;;
;;; repaint
;;;


;;;
;;; all events
;;;




(defun distribute-enter-events (sheet-b sheet-t event)
  (dolist (s
            (do ((s sheet-b (sheet-parent s))
                 (lis nil))
                ((or (null s) (climi::graftp s) (eq s sheet-t)) lis)
              (push s lis)))
    (format *debug-io* "enter ~A ~%" s)
    (dispatch-event s
                    (make-instance 'pointer-enter-event
                                   :pointer (slot-value event 'climi::pointer)
                                   :button nil
                                   :x (slot-value event 'climi::x) ;; wrong?
                                   :y (slot-value event 'climi::y) ;; wrong?
                                   :graft-x (slot-value event 'climi::graft-x)
                                   :graft-y (slot-value event 'climi::graft-y)
                                   :sheet s
                                   :modifier-state (slot-value event 'climi::modifier-state)
                                   :timestamp (slot-value event 'climi::timestamp)))))

(defun distribute-exit-events (sheet-b sheet-t event)
  (when (and sheet-t sheet-b)
    (do ((s sheet-b (sheet-parent s)))
        ((or (null s) (climi::graftp s) (eq s sheet-t)))
      (format *debug-io* "exit ~A ~A ~A~%" s (slot-value event 'climi::x)(slot-value event 'climi::y) )
      (dispatch-event s
                      (make-instance 'pointer-exit-event
                                     :pointer (slot-value event 'climi::pointer)
                                     :button nil
                                     :x (slot-value event 'climi::x) ;; wrong?
                                     :y (slot-value event 'climi::y) ;; wrong?
                                     :graft-x (slot-value event 'climi::graft-x)
                                     :graft-y (slot-value event 'climi::graft-y)
                                     :sheet s
                                     :modifier-state (slot-value event 'climi::modifier-state)
                                     :timestamp (slot-value event 'climi::timestamp))))))


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
