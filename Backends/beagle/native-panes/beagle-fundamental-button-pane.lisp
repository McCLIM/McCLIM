
(in-package :beagle)


;;; Limitations:
;;;
;;;  - ignores different NSControl sizes
;;;  - inherits from the 'standard' pane, rather than from the abstract pane
;;;  - file is not named well considering its expanded responsibilities
;;;  - does not adhere to Cocoa HIG button spacing (override parent default initargs).
;;;    Not sure the button should be responsible for providing sufficient space
;;;    around itself for this, or if the layout panes should be taught the HIG
;;;    inter-control spacing. It does look like this is the way it works for
;;;    'standard' buttons (see compose-space for push-button-pane in gadgets.lisp)
;;;  - locks things up tight when used in DEMODEMO for example (buttons cause new
;;;    frame to be created; suspect Cocoa is tracking the mouse pointer, and waiting
;;;    for the 'button press handler' to return; which it doesn't. It still waits
;;;    though, blocking all events from McCLIM.

;;; This ^^^ blocking / event type behaviour is reminiscent of the problems we have
;;; with drop down menus; suspect a fix for one may fix the other. Everything is
;;; waiting for the event semaphore...

;;; Note: things work fine if the button is used to update state or send commands
;;; to the frame in which it is displayed; these problems only appear to happen
;;; when a new frame is created as the result of a button-press. Probably related
;;; to the thread-UNsafe windowing code.

;;; Note in Cocoa we have 'momentary push buttons', 'toggle buttons', 'switch
;;; buttons' (-> checkbox) and 'radio buttons'. CLIM seems to define the first
;;; and second, the third and fourth are 'ascertained' from the pane type (which
;;; collects multiple buttons into a single pane). I'm sure this can be worked in,
;;; but it kind-of means that toggle buttons will have to change their appearance
;;; depending on what they are contained in.


;;; Provide 'abstract' native button; different realization methods (specialized
;;; on subclass types) provide the different looks (note that the behaviour is
;;; the same for different button types).

;;; CLIM pane types covered here are:-
;;;
;;; push-button-pane
;;; toggle-button-pane

(defclass beagle-abstract-button (push-button-pane)
  ((tk-obj :initform (%null-ptr) :accessor toolkit-object)))


(defmethod handle-repaint :around ((pane beagle-abstract-button) region)
  (declare (ignore region))
  ;; send a 'mark view dirty' message so it will be redrawn at the right
  ;; time.
  (send (toolkit-object pane) 'set-needs-display))


;;; Not sure we want to implement all of these as separate methods...
;;; would be better to use a generic function, specialized on pane
;;; type?
(defun button-action-handler (pane sender)
  (declare (ignore sender))

  ;; The debug in this method shows that when a button causes a new
  ;; frame to be opened (as is the case with DEMODEMO), the callback
  ;; never returns and we sit here, waiting... waiting... eternally.

  ;; ::FIXME:: I don't like invoking the callback directly...
  #+nil
  (format *trace-output* "Invoking button callback...~%")
  (activate-callback pane
		     (gadget-client pane)
		     (gadget-id pane))
  #+nil
  (format *trace-output* "button callback returned, continuing~%")
  #+nil
  (force-output *trace-output*))


(defun %beagle-get-label-size (label sheet)
  (let ((retsize (cg-floatify 0.0))
	(dictionary (reuse-attribute-dictionary (sheet-medium sheet)
						(send (@class ns-font)
						      :system-font-of-size 
						      (cg-floatify 0.0)))))
    (slet ((label-size (send (ccl::%make-nsstring label)
			     :size-with-attributes dictionary)))
      (setf retsize label-size)
      retsize)))



(defclass beagle-push-button-pane (beagle-abstract-button)
  ())


(defmethod initialize-instance :after ((pb beagle-push-button-pane) &rest args)
  (declare (ignore args))
  pb)


(defmethod compose-space ((pb beagle-push-button-pane) &key width height)
  (declare (ignore width height))
  ;; - magic numbers are from the HIG
  (let ((column-spacing           #.(cg-floatify 12.0))
	(row-spacing              #.(cg-floatify 12.0))
	(standard-width-sans-ends #.(cg-floatify 41.0))
	(standard-end-size        #.(cg-floatify 28.0))
                                        ; width of OK, Cancel buttons
	(standard-width           #.(cg-floatify 69.0))
	(standard-height          #.(cg-floatify 20.0))
	(label-size               (%beagle-get-label-size (gadget-label pb) pb)))
      (let ((width (if (< (pref label-size :<NSS>ize.width) standard-width-sans-ends)
		       standard-width-sans-ends
		     (pref label-size :<NSS>ize.width))))
	(make-space-requirement :min-width (+ standard-width column-spacing)
				:width (+ width standard-end-size column-spacing)
				:min-height (+ standard-height row-spacing)
				:height (+ standard-height row-spacing)))))


;;; Support changing the title of the button
(defmethod (setf gadget-label) :after ((pb beagle-push-button-pane) label)
  (send (toolkit-object pb) :set-title (ccl::%make-nsstring label)))


(defmethod realize-mirror ((port beagle-port)
			   (sheet beagle-push-button-pane))

  (let* ((q (compose-space sheet))
	 (rect (make-ns-rect 0.0
			     0.0
			     (space-requirement-width q)
			     (space-requirement-height q)))
	 (mirror (make-instance 'lisp-button :with-frame rect)))
    (send mirror 'retain)

    (setf (toolkit-object sheet) mirror)
    (setf (view-lisp-button mirror) sheet)

    ;; Set button up to handle actions from the user.
    (send mirror :set-target mirror)
    (send mirror :set-action (ccl::@selector "takeButtonAction:"))

    (port-register-mirror (port sheet) sheet mirror)
    (%beagle-mirror->sheet-assoc port mirror sheet)
    (send (sheet-mirror (sheet-parent sheet)) :add-subview mirror)

    ;; Set type appropriately
    (send mirror :set-button-type #$NSMomentaryPushInButton)
    (send mirror :set-image-position #$NSNoImage)
    (send mirror :set-bordered #$YES)
    (send mirror :set-bezel-style #$NSRoundedBezelStyle)

    (send mirror :set-title (ccl::%make-nsstring (gadget-label sheet)))

    (#_free rect)
    mirror))



;;; ::TODO:: - toggle-button-pane (radio / sticky / check)