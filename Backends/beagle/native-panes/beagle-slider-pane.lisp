
(in-package :beagle)


;;; Limitations:
;;;
;;;  - ignores different NSControl sizes
;;;  - never restricts user to tickmark values only (does McCLIM?)
;;;  - no way to show current value
;;;  - no 'title' (not sure CLIM supports this anyway)
;;;  - inherits from the 'standard' slider-pane, rather than from the abstract slider

(defclass beagle-slider-pane (slider-pane)
  ((tk-obj :initform (%null-ptr) :accessor toolkit-object)))


(defmethod initialize-instance :after ((sp beagle-slider-pane) &rest args)
  (declare (ignore args))
  sp)


(defmethod realize-mirror ((port beagle-port)
			   (sheet beagle-slider-pane))
  ;; Orientation is defined by the longer relative dimension in
  ;; Cocoa; if maxx-minx > maxy - miny, we will get a :horizontal
  ;; bar; otherwise we get a :vertical bar.

  (let* ((q (compose-space sheet))
	 (rect (make-ns-rect 0.0
			     0.0
			     (space-requirement-width q)
			     (space-requirement-height q)))
	 (mirror (make-instance 'lisp-slider :with-frame rect)))
    (send mirror 'retain)

    ;; Sliders are disabled by default; enable it (otherwise nothing
    ;; is displayed). Looks like this is standard for any NSControl
    ;; subclass.
    (send mirror :set-enabled #$YES)

    (setf (toolkit-object sheet) mirror)
    (setf (view-lisp-slider mirror) sheet)

    ;; Set slider up to handle _actions_ from the user.
    (send mirror :set-target mirror)
    (send mirror :set-action (ccl::@selector "takeSliderAction:"))

    ;; check if the slider works in discrete steps. There appears to be
    ;; no way in CLIM to restrict slider values only to these ticks, so
    ;; we make no use of 'setAllowsTickMarkValuesOnly:(BOOL)flag'.
    ;; This should automatically draw the tick marks, and change the style
    ;; of the scroller to have a little pointy bit.
    (when (climi::slider-number-of-quanta sheet)
      (send mirror :set-number-of-tick-marks (climi::slider-number-of-quanta sheet)))

    (port-register-mirror (port sheet) sheet mirror)
    (%beagle-mirror->sheet-assoc port mirror sheet)
    (send (sheet-mirror (sheet-parent sheet)) :add-subview mirror)
    (#_free rect)
    mirror))


(defmethod handle-repaint :around ((pane beagle-slider-pane) region)
  (declare (ignore region))
  ;; send a 'mark view dirty' message so it will be redrawn at the right
  ;; time.
  (send (toolkit-object pane) 'set-needs-display))


#+nil
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


;;; Not sure we want to implement all of these as separate methods...
;;; would be better to use a generic function, specialized on pane
;;; type?
(defun slider-action-handler (pane sender)
  (let ((value    (* (send sender 'float-value) ; 0.0 - 1.0
		     (- (gadget-max-value pane) ; range; 0.0 -> max extent ...
			(gadget-min-value pane)))))  ; ... (probably)
    ;; ::FIXME:: I don't like invoking the drag-callback directly...
    (drag-callback pane
		   (gadget-client pane)
		   (gadget-id pane)
		   value)))