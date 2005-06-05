
(in-package :beagle)


(defclass beagle-scroll-bar-pane (scroll-bar-pane)
  ((tk-obj :initform (%null-ptr) :accessor toolkit-object)))


(defmethod initialize-instance :after ((sb beagle-scroll-bar-pane) &rest args)
  (declare (ignore args))
  sb)


(defmethod realize-mirror ((port beagle-port)
			   (sheet beagle-scroll-bar-pane))
  ;; How do we construct one of these puppies so it looks right, is the
  ;; correct orientation, etc.? Cocoa docs are a little lacking in this
  ;; regard.
  ;; Orientation is defined by the longer relative dimension; if
  ;; maxx-minx > maxy - miny, we will get a :horizontal bar; otherwise
  ;; we get a vertical bar.
  ;; Use 'init with frame'?
  (let* ((q (compose-space sheet))
	 (rect (ccl::make-ns-rect 0.0
				  0.0
				  (space-requirement-width q)
				  (space-requirement-height q)))
	 (mirror (make-instance 'lisp-scroller :with-frame rect)))
    (send mirror 'retain)
    ;; Scrollers are disabled by default; enable it (otherwise the
    ;; lozenge and buttons are not displayed).
    (send mirror :set-enabled #$YES)
    ;; Make knob fill pane initially.
    (send mirror :set-float-value 0.0 :knob-proportion 1.0)
    (setf (toolkit-object sheet) mirror)
    (setf (view-event-mask mirror) +mirrored-sheet-mixin-event-mask+)
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
    (if (eq (gadget-orientation sb) :vertical)
	(make-space-requirement :min-width width
				:width     width
				:min-height (* 3 width)
				:height (* 4 width))
      (make-space-requirement :min-width (* 3 width)
			      :width (* 4 width)
			      :min-height width
			      :height width))))


(defmethod (setf gadget-value) :before (value (gadget beagle-scroll-bar-pane)
					      &key invoke-callback)
  (declare (ignore invoke-callback))
  ;; (- gadget-max-value gadget-min-value) = range.
  ;; height (or width) of scrollbar = proportional SIZE of lozenge.
  ;; value = proportional POSITION of lozenge
  (let* ((range (- (gadget-max-value gadget) (gadget-min-value gadget)))
	 (size  (if (eq (gadget-orientation gadget) :vertical)
		    (bounding-rectangle-height gadget)
		  (bounding-rectangle-width gadget)))
	 (position (if (<= range 0)
		       0.0
		     (/ value range)))
	 (loz-size (if (<= range 0)
		       1.0
		     (/ size range))))
    (send (toolkit-object gadget)
	  :set-float-value (coerce position 'short-float)
	  :knob-proportion (coerce loz-size 'short-float))
    (send (toolkit-object gadget) 'set-needs-display)))

