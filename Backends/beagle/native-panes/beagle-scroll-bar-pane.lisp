
(in-package :beagle)

;;; Limitations:
;;;
;;;  - ignores different NSControl sizes

(defclass beagle-scroll-bar-pane (scroll-bar)
  ((tk-obj :initform (%null-ptr) :accessor toolkit-object)))


(defmethod initialize-instance :after ((sb beagle-scroll-bar-pane) &rest args)
  (declare (ignore args))
  sb)


(defmethod realize-mirror ((port beagle-port)
			   (sheet beagle-scroll-bar-pane))
  ;; Orientation is defined by the longer relative dimension in
  ;; Cocoa; if maxx-minx > maxy - miny, we will get a :horizontal
  ;; bar; otherwise we get a :vertical bar.

  (let* ((q (compose-space sheet))
	 (rect (make-ns-rect 0.0
				0.0
				(space-requirement-width q)
				(space-requirement-height q)))
	 (mirror (make-instance 'lisp-scroller :with-frame rect)))
    (send mirror 'retain)

    ;; Scrollers are disabled by default; enable it (otherwise the
    ;; lozenge and buttons are not displayed).
    (send mirror :set-enabled #$YES)

    ;; Make knob fill pane initially.
    (send mirror :set-float-value 0.0 :knob-proportion #.(cg-floatify 1.0))
    (setf (toolkit-object sheet) mirror)
    (setf (view-lisp-scroller mirror) sheet)

    ;; In this implementation, native scroll bars don't respond to events;
    ;; Cocoa handles all that for us. We set them up to handle _actions_
    ;; instead (analagous to callbacks). Set the target (recipient of
    ;; actions) to be the scroll bar itself (i.e. the same object that
    ;; generates the actions). Not sure if this is a good architectural
    ;; decision or not...
    (send mirror :set-target mirror)
    (send mirror :set-action (ccl::@selector "takeScrollerAction:"))

    (port-register-mirror (port sheet) sheet mirror)
    (%beagle-mirror->sheet-assoc port mirror sheet)
    (send (sheet-mirror (sheet-parent sheet)) :add-subview mirror)
    (#_free rect)
    mirror))


(defmethod handle-repaint :around ((pane beagle-scroll-bar-pane) region)
  (declare (ignore region))
  ;; send a 'mark view dirty' message so it will be redrawn at the right
  ;; time.
  (send (toolkit-object pane) 'set-needs-display))


(defmethod compose-space ((sb beagle-scroll-bar-pane) &key width height)
  (declare (ignore width height))
  (let ((width (send (@class ns-scroller)
		     :scroller-width-for-control-size
		     #$NSRegularControlSize)))
    ;; For vertical scroll bars, ensure y > x. For horizontal, ensure
    ;; x > y.
    (if (eq (gadget-orientation sb) :vertical)
	(make-space-requirement :min-width width
				:width     width
				:min-height (* 3 width)
				:height (* 4 width))
      (make-space-requirement :min-width (* 3 width)
			      :width (* 4 width)
			      :min-height width
			      :height width))))

;;; Change the value of the scroll bar in the application process i.e.,
;;; consistently with respect to events that have been received.

(defmethod drag-callback :before
    ((gadget beagle-scroll-bar-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (setf (slot-value gadget 'climi::value) value))

(defun update-cocoa-scroll-bar (scroll-bar)
  (let* ((range (- (gadget-max-value scroll-bar)
		   (gadget-min-value scroll-bar)))
	 (value (if (zerop range)
		    0.0
		     (/ (- (gadget-value scroll-bar)
			   (gadget-min-value scroll-bar))
			range)))
	 (ts (climi::scroll-bar-thumb-size scroll-bar))
	 (loz-size (if (<= range 0)
		       1.0
		       (/ ts (+ range ts)))))
    (send (toolkit-object scroll-bar)
	  :set-float-value (coerce (clamp value 0.0 1.0) 'short-float)
	  :knob-proportion (cg-floatify (clamp loz-size 0.0 1.0)))))

(defmethod (setf gadget-min-value) :after
    (new-value (pane beagle-scroll-bar-pane))
  (declare (ignore new-value))
  (update-cocoa-scroll-bar pane))

(defmethod (setf gadget-max-value) :after (new-value (pane beagle-scroll-bar-pane))
  (declare (ignore new-value))
  (update-cocoa-scroll-bar pane))

(defmethod (setf climi::scroll-bar-thumb-size) :after (new-value (pane beagle-scroll-bar-pane))
  (declare (ignore new-value))
  (update-cocoa-scroll-bar pane))

(defmethod (setf gadget-value) :after (new-value (pane beagle-scroll-bar-pane) &key invoke-callback)
  (declare (ignore new-value invoke-callback))
  (update-cocoa-scroll-bar pane))

(climi::defmethod* (setf climi::scroll-bar-values)
    (min-value max-value thumb-size value (scroll-bar beagle-scroll-bar-pane))
  (setf (slot-value scroll-bar 'climi::min-value) min-value
	(slot-value scroll-bar 'climi::max-value) max-value
	(slot-value scroll-bar 'climi::thumb-size) thumb-size
	(slot-value scroll-bar 'climi::value) value)
  (update-cocoa-scroll-bar scroll-bar))

;;; No need to update the scrollbar (most of the time) since Cocoa will move
;;; the 'thumb' appropriately. Stick some debug in to see when it's invoked.
;;; => As expected, this is invoked all the time.

;;; I believe it's safe to leave this alone though since the sb will only be
;;; redrawn once through the event loop it shouldn't be too inefficient to
;;; be changing its value regularly.
#-(and)
(defmethod (setf gadget-value) :before (value (gadget beagle-scroll-bar-pane)
					      &key invoke-callback)
  (declare (ignore invoke-callback))

  ;; (- gadget-max-value gadget-min-value) = range.
  ;; height (or width) of scrollbar        = proportional SIZE of lozenge.
  ;; value                                 = proportional POSITION of lozenge

  (let* ((range (- (gadget-max-value gadget)
		   (gadget-min-value gadget)))
	 (size  (climi::scroll-bar-thumb-size gadget))
	 (position (if (<= range 0)
		       0.0
		     (/ (- value (gadget-min-value gadget) range)))
	 (loz-size (if (<= range 0)
		       1.0
		     (/ size range))))
    (send (toolkit-object gadget)
	  :set-float-value (coerce position 'short-float)
	  :knob-proportion (cg-floatify loz-size)))))


;;; Called in the Cocoa App thread.
(defun scroll-bar-action-handler (pane sender)

  ;; Now we need to decide exactly what we do with these events... not sure
  ;; if this is the right way to invoke the callbacks... shouldn't
  ;; '(scroll-bar-scroll-up-line-callback sb)' come into it somewhere? Hmmm.

  ;; Note: because the coords are all over the place, to make these work nicely
  ;; with non-aqua panes, we reverse the 'up' and 'down' actions.

  ;; I actually think now that McCLIM + Cocoa have a different idea of what
  ;; increment / up and decrement / down mean; or I have misunderstood things
  ;; which wouldn't suprise me... perhaps it's reasonable that 'up line' and
  ;; 'decrement line' are the same thing.

  (let ((hit-part (send sender 'hit-part)))
    (cond ((or (eq hit-part #$NSScrollerKnob)      ; drag knob
	       (eq hit-part #$NSScrollerKnobSlot)) ; click on knob (or alt-click on slot)
	   (let ((value (+ (* (send sender 'float-value) ; 0.0 - 1.0
			      (- (gadget-max-value pane) ; range; 0.0 -> max extent ...
				 (gadget-min-value pane)))
			   (gadget-min-value pane)))) ; ... (probably)
	     (queue-callback #'clim:drag-callback
			     pane
			     (gadget-client pane)
			     (gadget-id pane)
			     value)))
	  ((eq hit-part #$NSScrollerDecrementLine)
	   (queue-callback #'clim:scroll-up-line-callback
			   pane
			   (gadget-client pane)
			   (gadget-id pane)))
	  ((eq hit-part #$NSScrollerDecrementPage)
	   (queue-callback #'clim:scroll-up-page-callback
			   pane
			   (gadget-client pane)
			   (gadget-id pane)))
	  ((eq hit-part #$NSScrollerIncrementLine)
	   (queue-callback #'clim:scroll-down-line-callback
			   pane
			   (gadget-client pane)
			   (gadget-id pane)))
	  ((eq hit-part #$NSScrollerIncrementPage)
	   (queue-callback #'clim:scroll-down-page-callback
			   pane
			   (gadget-client pane)
			   (gadget-id pane))))))

