
;;; -*- Mode: List; Package: BEAGLE -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2003, 2004 by
;;;           Duncan Rose (duncan@robotcat.demon.co.uk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :beagle)

#||

$Id: events.lisp,v 1.3 2005/05/17 20:12:37 drose Exp $

All these are copied pretty much from CLX/port.lisp

Changes made for Cocoa, obviously...

Events in Cocoa
---------------
In X, events are requested... in cocoa, the RUN LOOP passes them out to:

The NSApplication (send-event) + NSResponder methods
The NSWindow (send-event) + NSResponder methods
The NSView (NSResponder methods)

Each thread has a run-loop created for it, but only the run-loop in the application
main thread is run automatically.

CLIM appears to get all the events we want delivered "properly" to the NSView and NSWindow
subclasses. Some CLIM events come from NSWindow notifications rather than events, but we
map them all to CLIM events anyhow.

Suspect some of the coordinates we pass back to CLIM from here may be *wrong*.

||#

;;; The following parameters are *all* added for 'synthesize-pointer-motion-event' only.

(defparameter *-current-event-modifier-state-* 0
  "Contains the most recent modifier state for any ``real'' event. Reset whenever any
event (but not notification) is handled.")
(defparameter *-current-pointer-button-state-* 0
  "Contains the most recent pointer button state for any ``real'' event. Reset whenever
any pointer or button-press event is handled.")
(defparameter *-current-pointer-graft-xy-* nil
  "Contains the (Cocoa) NSPoint foreign object (structure) representing the position of
the mouse pointer in screen coordinates. Reset whenever a ``real'' pointer event
(mouse-move, mouse-drag, enter / exit or button press / release) is handled.")
(defparameter *-current-pointer-view-xy-* nil
  "Contains the (Cocoa) NSPoint foreign object (structure) representing the position of
the mouse pointer in the coordinate system of the NSView it is currently over. Reset
whenever a ``real'' pointer event (mouse-move, mouse-drag, enter / exit or button
press / release) is handled.")

(defvar *keysym-hash-table*
  (make-hash-table :test #'eql))

(defvar *reverse-keysym-hash-table*
  (make-hash-table :test #'eq))

(defun define-keysym (name value)
  (pushnew name (gethash value *keysym-hash-table* nil))
  (setf (gethash name *reverse-keysym-hash-table*) value))

(defun lookup-keysym (value)
  (car (last (gethash value *keysym-hash-table*))))

(defun reverse-lookup-keysym (value)
  (gethash value *reverse-keysym-hash-table*))

(defmethod port-motion-hints ((port beagle-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port sheet))
  (warn "events:port-motion-hints:Motion hints not supported in Beagle backend")
  nil)

(defmethod (setf port-motion-hints) (val (port beagle-port) (sheet mirrored-sheet-mixin))
  (declare (ignore val port sheet))
  (warn "events:setf port-motion-hints:Motion hints (2) not supported in Cocoa backend")
  nil)

;; peek-event from CLX/port.lisp?

;;; Given a NATIVE event, generate an appropriate corresponding CLIM event.
;;; There are a couple of X-specific events handled here which we don't have
;;; in cocoa (grab events and window hints), hopefully that won't matter to
;;; us (apart from the menus use grabbing I think)

;;; All these parameters must be what CLX provides for the :handler argument
;;; to the xlib:process-event method.

;;; We don't actually need all this gubbins for Cocoa events. We just need a
;;; method to convert from a Cocoa event to a CLIM event. As specified, this
;;; would be quite a good fit. Unfortunately, McCLIM seems to have a whole
;;; bunch of non-standard slots in the event objects (root-x, root-y etc.)
;;; and the override-redirect-p, send-event-p, hint-p stuff in this method.

;;; So we actually want to do this slightly differently.

;; From CLX/port.lisp
  
;; NOTE: Although it might be tempting to compress (consolidate)
;; events here, this is the wrong place. In our current architecture
;; the process calling this function (the port's event handler
;; process) just reads the events from the X server, and does it
;; with almost no lack behind the reality. While the application
;; frame's event top level loop does the actual processing of events
;; and thus may produce lack. So the events have to be compressed in
;; the frame's event queue.
;;
;; So event compression is implemented in EVENT-QUEUE-APPEND.
;;
;; This changes for possible _real_ immediate repainting sheets,
;; here a possible solution for the port's event handler loop can be
;; to read all available events off into a temponary queue (and
;; event compression for immediate events is done there) and then
;; dispatch all events from there as usual.
;;
;;--GB
  
;; XXX :button code -> :button (decode-x-button-code code)
;;  (declare (ignorable event-slots))
;;  (declare (special *cocoa-port*))
;;  (let ((sheet (and window
;;                    (port-lookup-sheet port window))))
;;    (when sheet
;;        (:enter-notify
;;          (make-instance 'pointer-enter-event
;;            :pointer 0
;;            :button code :x x :y y
;;            :graft-x root-x
;;            :graft-y root-y
;;            :sheet sheet
;;            :modifier-state (cocoa-event-state-modifiers *cocoa-port* state)
;;            :timestamp time))
;;        (:leave-notify
;;          (make-instance 'pointer-exit-event  ; No grab events in cocoa - may cause problems?
;;            :pointer 0
;;            :button code
;;            :x x :y y
;;            :graft-x root-x
;;            :graft-y root-y
;;            :sheet sheet
;;            :modifier-state (cocoa-event-state-modifiers *cocoa-port* state)
;;            :timestamp time))

(defparameter *mcclim-event-queue* nil)

(defmethod add-event-to-queue (mirror event)
  "Adds an event to the dynamically scoped *mcclim-event-queue* queue, after conversion from
a Cocoa event MACPTR to a CLIM event. This method signals the port event semaphore when an
event is added to the queue. Cocoa events that map onto a NIL CLIM event (i.e. those that are
not handled) are not added to the queue."
  (declare (special *beagle-port
		    *mcclim-event-queue*))
  (let ((clim-event (beagle-event-to-clim-event mirror event)))
    (unless (not clim-event)
      (setf *mcclim-event-queue* (nconc *mcclim-event-queue* (list clim-event)))
      (ccl:signal-semaphore (beagle-port-event-semaphore *beagle-port*)))))

(defmethod add-notification-to-queue (window notification &optional origin-x origin-y width height)
  "Adds an event to the dynamically scoped *mcclim-event-queue* queue, after conversion from
a Cocoa notification MACPTR to a CLIM event. This method signals the port event semaphore
when a notification is added to the queue."
  (declare (special *beagle-port*))
  (let ((clim-event (beagle-notification-to-clim-event window notification origin-x origin-y width height)))
    (unless (not clim-event)
      (setf *mcclim-event-queue* (nconc *mcclim-event-queue* (list clim-event)))
      (ccl:signal-semaphore (beagle-port-event-semaphore *beagle-port*)))))

;;; timeout = timeout delay in seconds
;;; If no timeout is specified (nil timeout), this method hangs around until an event arrives.
(defmethod get-next-event ((port beagle-port) &key wait-function (timeout nil))
  (declare (special *mcclim-event-queue* *beagle-port*)
	   (ignore wait-function))
  (setf *beagle-port* port)
  ;; When event queue is empty, wait for an event to be posted.
  (if (eq timeout nil)
      (ccl:wait-on-semaphore (beagle-port-event-semaphore port))
    (ccl:timed-wait-on-semaphore (beagle-port-event-semaphore port) timeout))
  ;; Event queue semaphore has been raised (in which case the queue
  ;; should be non-null), or the semaphore timed out.
  (let ((event (and *mcclim-event-queue* (pop *mcclim-event-queue*))))
    (if (not event)
	:timeout
      event)))

;;; Convert cocoa events to CLIM events. From the spec, CLIM events have the following
;;; attributes:
;;;
;;; :timestamp
;;; All subclasses of event must take a :timestamp initarg, which is used to specify
;;; the timestamp for the event.... that is a monotonically increasing timestamp for
;;; the event. The timestamp must have at least as many bits of precision as a fixnum
;;;
;;; :sheet
;;; The sheet associated with the event
;;;
;;; :modifier-state
;;; An integer value that encodes the state of all the modifier keys on the keyboard.
;;; This will be a mask consisting of the logior of +shift-key+, +control-key+,
;;; +meta-key+, +super-key+ and +hyper-key+.
;;;

;; This looks rather inefficient; ::FIXME:: make these user-configurable.
;; Can we make use of the other modifier states set by cocoa? Some of
;; them might be useful...

;;; Every key on the keyboard has a physical "key-code". a and A share the same key code, since the
;;; same key is pressed (0 in this case). We can't make use of the key-code with any confidence since
;;; they're at a very low-level. We have to use the 'characters method (or 'characters-ignoring-modifiers)
;;; to pull the actual keys out of the event. Then we need to map these to McCLIM key names. *sigh*

;;; We could use 'characters if we were going through the full Cocoa key-handling path; and we might
;;; be able to make use of this anyway, but for now just use 'characters-ignoring-modifiers and compare
;;; what we get with those values known from Cocoa for function keys etc.
(defun beagle-key-event-to-key-name (event)
  ;; This falls over when the function keys, the arrow keys, the num-lock key (and others)
  ;; are pressed; I guess we don't want to be doing this!
;;;	(key-name (ccl::lisp-string-from-nsstring (send event 'characters-ignoring-modifiers))))
  (let ((key-name (characters-to-key-name (send event 'characters-ignoring-modifiers)))) ; was just 'characters
;;;    (format *terminal-io* "returning key-name: ~A~%" key-name)
    key-name))

(defun beagle-modifier-to-modifier-state (flags)
  (declare (special *-current-event-modifier-state-*))
  (let ((mods 0))
    (if (> (logand flags #$NSShiftKeyMask) 0)
		(progn
		  (setf mods (logior mods +shift-key+))))
    (if (> (logand flags #$NSControlKeyMask) 0)
		(progn
		  (setf mods (logior mods +control-key+))))
    (if (> (logand flags #$NSCommandKeyMask) 0)
		(progn
		  (setf mods (logior mods +meta-key+))))
    (if (> (logand flags #$NSAlternateKeyMask) 0)
		(progn
		  (setf mods (logior mods +super-key+))))
    (if (> (logand flags #$NSAlphaShiftKeyMask) 0) ; caps lock
		(progn
		  (setf mods (logior mods +hyper-key+))))
;;; Unused:
;;;  NSHelpKeyMask
;;;  NSNumericKeyPadKeyMask (key on numeric pad was pressed)
;;;  NSFunctionKeyMask      (function key was pressed)
    (setf *-current-event-modifier-state-* mods)
    mods))

;;; :key-name
;;; Used to specify the key name component for the event. This will be a symbol whose
;;; value is port specific. Key names corresponding to the set of "standard" characters
;;; (such as the alphanumerics) will be a symbol in the keyword package.
;;;
;;; Keyboard character seems different; not quite sure how it's different though!
;;;
;;; :pointer :button :x :y
;;; All subclasses of pointer-event must take the :pointer, :button, :x and :y initargs
;;; which are used to specify the pointer object (?), pointer button, and native x and
;;; y position of the pointer at the time of the event. The sheet's x and y positions
;;; are derived from the supplied native x and y positions and the sheet itself.
;;;
;;; pointer-event-x, pointer-event-y are supposed to return the x and y position of the
;;; pointer at the time the event occurred *in the coordinate system of the sheet that
;;; received the event*. Suspect this is not implemented correctly in McCLIM at the
;;; moment. Actually, after looking at McCLIM/events.lisp, it is done properly.
;;; pointer-event-native-x, pointer-event-native-y are for returning the information in
;;; the native coordinate system.
;;; :button will be one of +pointer-left-button+, +pointer-middle-button+ or
;;; +pointer-right-button+
;;;
;;; pointer-boundary-event-kind
;;; Returns the "kind" of boundary event, which will be one of :ancestor, :virtual,
;;; :inferior, :nonlinear, :nonlinear-virtual or nil. These kinds correspond to the
;;; detail members for X11 enter and exit events.
;;;

;; I'm not sure this is the best way with dealing with the timestamp...

(let ((timestamp 0))
  (defun beagle-notification-to-clim-event (window notification &optional origin-x origin-y width height)
    (declare (special *beagle-port*))
    (let ((return-event nil)
	  (sheet (%beagle-port-lookup-sheet-for-view *beagle-port* (send window 'content-view))))
      ;; We don't get exposure notifications when the window has a (Cocoa) backing store.
      (cond
       ((send (send notification 'name) :is-equal-to-string #@"NSWindowDidBecomeKeyNotification")
        (setf return-event nil)
	(when (send window 'is-visible)  ; only do if window is on-screen...
	  (let* ((content-view (send window 'content-view))
		 (target-sheet (%beagle-port-lookup-sheet-for-view *beagle-port* content-view))
		 (frame (pane-frame target-sheet))
		 ;; Works out which sheet *should* be the focus, not which is currently...
		 ;; or at least, so I think.
		 (focus (climi::keyboard-input-focus frame)))
	    (unless (null target-sheet)
	      (setf (port-keyboard-input-focus *beagle-port*) focus)))))
       ((send (send notification 'name) :is-equal-to-string #@"NSWindowDidExposeNotification")
	(setf return-event
	      (make-instance 'window-repaint-event :timestamp (incf timestamp)
			     :sheet     sheet
			     ;; Region is the whole window on expose... doesn't
			     ;; seem to be a way to specify a region... coord
			     ;; system?
			     :region    (make-rectangle* origin-x origin-y width height))))
       ((send (send notification 'name) :is-equal-to-string #@"NSWindowDidResizeNotification")
	(setf return-event (make-instance 'window-configuration-event :sheet  sheet
					  :x      origin-x	; coord system?
					  :y      origin-y
					  :width  width
					  :height height)))
       ((send (send notification 'name) :is-equal-to-string #@"NSWindowWillCloseNotification")
	(setf return-event (make-instance 'window-destroy-event :sheet sheet)))
       ;; TODO: this logic is the same as the previous version, but
       ;;       is it correct?  it means that if we get a
       ;;       notification that we don't recognize, we ignore it
       (t nil))
      return-event))

  (defun beagle-event-to-clim-event (mirror event)
    (declare (special *-current-pointer-button-state-*
		      *-current-pointer-view-xy-*
		      *-current-pointer-graft-xy-*))
    (let ((window (send event 'window))
	  (return-event event)
	  ;; Can't do this here any more - it breaks NSFlagsChanged event handling :-(
;;;	  (modifier-state (beagle-modifier-to-modifier-state (send event 'modifier-flags)))
	  (event-type (send event 'type)))
      (when (or (equal #$NSLeftMouseUp event-type)
		(equal #$NSLeftMouseDown event-type)
		(equal #$NSRightMouseUp event-type)
		(equal #$NSRightMouseDown event-type)
		(equal #$NSOtherMouseUp event-type)
		(equal #$NSOtherMouseDown event-type))
	(slet ((location-in-window-point (send event 'location-in-window))
	       (window-bounds (send (send window 'content-view) 'bounds)))
	      (setf (pref location-in-window-point :<NSP>oint.y) (- (pref window-bounds :<NSR>ect.size.height)
								    (pref location-in-window-point :<NSP>oint.y)))

	      ;;; *SUSPECT* this will leak; gc won't collect heap-allocated store from make-record will it?
	      (slet ((location-in-view-point (send mirror :convert-point location-in-window-point
						   :from-view (send window 'content-view)))
		     (location-in-screen-point (send window :convert-base-to-screen location-in-window-point)))

		(setf *-current-pointer-graft-xy-* (ccl::make-record :<NSP>oint
								     :x (pref location-in-screen-point :<NSP>oint.x)
								     :y (pref location-in-screen-point :<NSP>oint.y)))
		(setf *-current-pointer-view-xy-* (ccl::make-record  :<NSP>oint
								     :x (pref location-in-view-point :<NSP>oint.x)
								     :y (pref location-in-view-point :<NSP>oint.y)))
	        (setf return-event
		      (make-instance (if (or (equal #$NSLeftMouseUp event-type)
					     (equal #$NSRightMouseUp event-type)
					     (equal #$NSOtherMouseUp event-type))
					 'pointer-button-release-event
				       'pointer-button-press-event)
				     :pointer 0
				     :button (cond ((or (equal event-type #$NSLeftMouseUp)
							(equal event-type #$NSLeftMouseDown))
						    (setf *-current-pointer-button-state-* +pointer-left-button+)
						    +pointer-left-button+)
						   ((or (equal event-type #$NSRightMouseUp)
							(equal event-type #$NSRightMouseDown))
						    (setf *-current-pointer-button-state-* +pointer-right-button+)
						    +pointer-right-button+)
						   (t
						    (setf *-current-pointer-button-state-* +pointer-middle-button+)
						    +pointer-middle-button+))
				     ;; x and y are in window coordinates. They need converting to screen
				     ;; coordinates. Can do this with
				     ;; [window convertBaseToScreen:location-in-window].x or .y.
				     ;; They probably need coercing too :-(
				     :x              (pref location-in-view-point :<NSP>oint.x)
				     :y              (pref location-in-view-point :<NSP>oint.y)
				     :graft-x        (pref location-in-screen-point :<NSP>oint.x)
				     :graft-y        (pref location-in-screen-point :<NSP>oint.y)
				     :sheet          (%beagle-port-lookup-sheet-for-view *beagle-port* mirror)
				     :modifier-state (beagle-modifier-to-modifier-state (send event 'modifier-flags))
				     ;; Timestamp from Cocoa looks like 12345.7 - CLIM wants integer no
				     ;; bigger than a fixnum, so it gets a fixnum. Hope Cocoa doesn't
				     ;; send non-unique timestamps.
				     ;; NSTimeInterval is a double typedef
				     :timestamp      (incf timestamp))))))
      ;; (coerce (* 10 (pref timestamp :<NST>ime<I>nterval)) 'fixnum))))))

      (when (equal #$NSScrollWheel event-type)
	(setf return-event (make-instance 'pointer-button-press-event
					  :pointer 0
					  ;; The 'amount' of scroll can be specified in Cocoa by a
					  ;; larger or smaller delta in either X, Y or Z directions.
					  ;; We ignore this, and always pass up or down and let
					  ;; CLIM set the amount. Could do better with scroll wheel
					  ;; events, CLIM also ignores X and Z deltas...
					  :button (if (plusp (send event 'delta-y))
						      (progn
							(setf *-current-pointer-button-state-* +pointer-wheel-up+)
							+pointer-wheel-up+)
						    (progn
						      (setf *-current-pointer-button-state-* +pointer-wheel-down+)
						      +pointer-wheel-down+))
					  ;; Surely scroll-wheel events do not need x, y coords? input.lisp
					  ;; does a 'call-next-method' after handling the scroll but won't
					  ;; that then get passed as a 'proper' button press? Best pass these
					  ;; as values we're unlikely to ever get clicked.
					  :x 0
					  :y 0
					  :graft-x 0
					  :graft-y 0
					  :sheet          (%beagle-port-lookup-sheet-for-view *beagle-port* mirror)
					  :modifier-state (beagle-modifier-to-modifier-state (send event 'modifier-flags))
					  ;; Timestamp from Cocoa looks like 12345.7 - CLIM wants integer no
					  ;; bigger than a fixnum, so it gets a fixnum. Hope Cocoa doesn't
					  ;; send non-unique timestamps.
					  ;; NSTimeInterval is a double typedef
					  :timestamp      (incf timestamp))))
      
      ;; Keyname should probably be the #$NSF1FunctionKey, #$NSUpArrowFunctionKey etc as defined in the docs
      ;; for NSEvent (these are permitted to be implementation defined - not sure if that's the back end
      ;; implementation or the McCLIM implementation!), apart from the "standard" keys which should be symbols
      ;; in the keyword package (presumably :a :b :c etc.?)

      ;; ::FIXME:: WILL ONLY WORK FOR "STANDARD" KEYS!!!
      
      (when (or (equal #$NSKeyDown event-type)
		(equal #$NSKeyUp   event-type))
	(let ((keyname (beagle-key-event-to-key-name event)))
;;;	  (format *terminal-io* "In event-build with keyname: ~A (characterp = ~A)~%" keyname (characterp keyname))
	  (setf return-event (make-instance (if (equal #$NSKeyDown event-type)
						'key-press-event
					      'key-release-event)
					    :key-name       keyname
					    ;; not needed by spec - should change implementation?
					    :key-character  (and (characterp keyname) keyname)
					    :x              0	; Not needed for key events?
					    :y              0	; Not needed for key events?
					    :graft-x        0	; Not needed for key events?
					    :graft-y        0	; Not needed for key events?
					    ;; Irrespective of where the key event happened, send it
					    ;; to the sheet that has key-focus for the port.
					    :sheet          (beagle-port-key-focus *beagle-port*)
					    :modifier-state (beagle-modifier-to-modifier-state (send event 'modifier-flags))
					    :timestamp (incf timestamp)))))
      (when (or (equal #$NSMouseMoved event-type)
		(equal #$NSLeftMouseDragged event-type)
		(equal #$NSRightMouseDragged event-type)
		(equal #$NSOtherMouseDragged event-type))
	(slet ((location-in-window-point (send event 'location-in-window))
	       (window-bounds (send (send window 'content-view) 'bounds)))
	  ;; Because the location in window is *not* flipped, we need to flip it... (note: we flip by the size
	  ;; of the window's content view, otherwise we end up out by the size of the window title bar).
	  ;; *SUSPECT* this will leak; gc won't collect heap-allocated store from make-record will it?
	  (setf (pref location-in-window-point :<NSP>oint.y) (- (pref window-bounds :<NSR>ect.size.height)
								(pref location-in-window-point :<NSP>oint.y)))
	  (slet ((location-in-view-point (send mirror :convert-point location-in-window-point
					       :from-view (send window 'content-view)))
		 (location-in-screen-point (send window :convert-base-to-screen location-in-window-point)))

	    (setf *-current-pointer-graft-xy-* (ccl::make-record :<NSP>oint
								 :x (pref location-in-screen-point :<NSP>oint.x)
								 :y (pref location-in-screen-point :<NSP>oint.y)))
	    (setf *-current-pointer-view-xy-* (ccl::make-record :<NSP>oint
								:x (pref location-in-view-point :<NSP>oint.x)
								:y (pref location-in-view-point :<NSP>oint.y)))
	    (setf return-event
		  (make-instance 'pointer-motion-event
				 :pointer        0
				 :button         (cond ((equal event-type #$NSMouseMoved)
							(setf *-current-pointer-button-state-* 0)
							0)
						       ((equal event-type #$NSLeftMouseDragged)
							(setf *-current-pointer-button-state-* +pointer-left-button+)
							+pointer-left-button+)
						       ((equal event-type #$NSRightMouseDragged)
							(setf *-current-pointer-button-state-* +pointer-right-button+)
							+pointer-right-button+)
						       (t
							(setf *-current-pointer-button-state-* +pointer-middle-button+)
							+pointer-middle-button+))
				 ;; It looks like McCLIM diverges from the spec again in relation
				 ;; to events (I wonder who is responsible? 8-) and expects :x and
				 ;; :y to be relative to the MIRROR in which the events occur.
				 ;; :x              (pref location-in-screen-point :<NSP>oint.x)
				 ;; :y              (pref location-in-screen-point :<NSP>oint.y)
				 :x              (pref location-in-view-point :<NSP>oint.x)
				 :y              (pref location-in-view-point :<NSP>oint.y)
				 ;; Even though graft-x, graft-y is *not in the spec* we need to populate
				 ;; them because there's code in McCLIM/gadgets.lisp that makes direct
				 ;; use of the graft-x/y slot values. Naughty. So how does this differ
				 ;; from :x and :y which are supposedly in native coordinates? Methinks
				 ;; that the event hierarchy and associated code in McCLIM should perhaps
				 ;; be revisited... currently it appears that these are *only* used to support
				 ;; pointer-motion-events. Strange. It doesn't seem to make any difference what
				 ;; gets set here! Suspect we're not invoking the callback because we're not
				 ;; passing the correct sheet...?
;;;				 :graft-x        (pref location-in-view-point :<NSP>oint.x) ;0
;;;				 :graft-y        (pref location-in-view-point :<NSP>oint.y) ;0
				 :graft-x        (pref location-in-screen-point :<NSP>oint.x) ;0
				 :graft-y        (pref location-in-screen-point :<NSP>oint.y) ;0
				 ;; This is probably wrong too; the NSWindow receives and propogates mouse
				 ;; moved events, but we need to translate them into an appropriate view.
				 ;; Not sure quite how we do that, but I think we need to... we're ok for
				 ;; key down / up, we keep track of the "key view". Do we also need to keep
				 ;; track of what interactors we have? I suspect not. We just need to traverse
				 ;; the NSView hierarchy (or sheet hierarchy, whichever is easiest) until we
				 ;; find the "youngest" view (or sheet) over which the event occurred; this
				 ;; is the sheet that should handle the event.
				 :sheet (%beagle-port-lookup-sheet-for-view *beagle-port* mirror)
				 :modifier-state (beagle-modifier-to-modifier-state (send event 'modifier-flags))
				 :timestamp (incf timestamp))))))
      (when (or (equal #$NSMouseEntered event-type)
		(equal #$NSMouseExited  event-type))
;;;	(format *debug-io* "Got mouse entered / exited event for mirror ~S~%" mirror)
	(slet ((location-in-window-point (send event 'location-in-window))
	       (window-bounds (send (send window 'content-view) 'bounds)))
	  ;; Because the location in window is *not* flipped, we need to flip it... (note: we flip by the size
	  ;; of the window's content view, otherwise we end up out by the size of the window title bar).
	  (setf (pref location-in-window-point :<NSP>oint.y) (- (pref window-bounds :<NSR>ect.size.height)
								(pref location-in-window-point :<NSP>oint.y)))
	  (slet ((location-in-view-point (send mirror :convert-point location-in-window-point
					       :from-view (send window 'content-view)))
		 (location-in-screen-point (send window :convert-base-to-screen location-in-window-point)))

	    (setf *-current-pointer-graft-xy-* (ccl::make-record :<NSP>oint
								     :x (pref location-in-screen-point :<NSP>oint.x)
								     :y (pref location-in-screen-point :<NSP>oint.y)))
	    (setf *-current-pointer-view-xy-* (ccl::make-record  :<NSP>oint
								     :x (pref location-in-view-point :<NSP>oint.x)
								     :y (pref location-in-view-point :<NSP>oint.y)))
	    ;; This event does not provide button state, but we can use *-current-pointer-button-state-*
	    ;; to populate button state in the CLIM event. Obviously, we do not need to update this value
	    ;; (*-current-pointer-button-state-*) for enter / exit events...
	    (setf return-event
		  (make-instance (if (equal #$NSMouseEntered event-type)
				     'pointer-enter-event
				   'pointer-exit-event)
				 :pointer        0
				 :button         *-current-pointer-button-state-*
				 :x              (pref location-in-view-point :<NSP>oint.x)
				 :y              (pref location-in-view-point :<NSP>oint.y)
				 :graft-x        (pref location-in-screen-point :<NSP>oint.x) ;0
				 :graft-y        (pref location-in-screen-point :<NSP>oint.y) ;0
				 :sheet (%beagle-port-lookup-sheet-for-view *beagle-port* mirror)
				 :modifier-state (beagle-modifier-to-modifier-state (send event 'modifier-flags))
				 :timestamp (incf timestamp))))))

      ;; We need to maintain the modifier flags state constantly to be able to
      ;; implement this; suggest a slot in beagle-port?
      (when (equal #$NSFlagsChanged event-type)
;;;	(format *debug-io* "In event-build (flags changed)~%")
	;; Use the 'old' 'modifiers' in conjunction with the new 'modifier-state'
	;; to work out if this is a key up or a key down...
	(setf return-event
	      (destructuring-bind (event-class key)
		  (current-mods-map-to-key (send event 'modifier-flags))
		(make-instance event-class
			       :key-name       key
			       :key-character  nil
			       :x              0
			       :y              0
			       :graft-x        0
			       :graft-y        0
			       ;; Irrespective of where the key event happened, send it
			       ;; to the sheet that has key-focus for the port.
			       :sheet          (beagle-port-key-focus *beagle-port*)
			       :modifier-state (beagle-modifier-to-modifier-state (send event 'modifier-flags))
			       :timestamp (incf timestamp)))))
      
      ;; #$NSHelpRequested- wonder if we can convert this into "user pressed help key" key event?
      ;;                    Then could pull up docs (or could do if there were any!)
      ;; #$NSCursorUpdate

      return-event))

  ;;; This has been added to McCLIM and the CLX back end; I'm not sure what it's supposed
  ;;; to be for. Never mind, add it anyway. defgeneric is in stream-input.lisp
  ;;; SHOULD BE -> (defmethod synthesize-pointer-motion-event ((pointer beagle-pointer))

  (defmethod synthesize-pointer-motion-event (pointer)
    ;; *-current-event-modifier-state-* is set whenever an event or notification is received
    ;; containing this information.
    ;; *-current-pointer-button-state-* is set whenever there is a mouse down or drag, and
    ;; unset on mouse up.
    ;; *-current-pointer-graft-xy-* is set whenever there is a mouse event handled.
    (declare (special *-current-event-modifier-state-*
		      *-current-pointer-button-state-*
		      *-current-pointer-view-xy-*
		      *-current-pointer-graft-xy-*))
    (let* ((port (port pointer))
	   (sheet (port-pointer-sheet port)))
      (when sheet
	(let ((mirror (sheet-direct-mirror sheet)))
	  (when mirror
	    (make-instance 'pointer-motion-event
			   :pointer 0
			   :button *-current-pointer-button-state-*
			   :x (pref *-current-pointer-view-xy-* :<NSP>oint.x)
			   :y (pref *-current-pointer-view-xy-* :<NSP>oint.y)
			   :graft-x (pref *-current-pointer-graft-xy-* :<NSP>oint.x)
			   :graft-y (pref *-current-pointer-graft-xy-* :<NSP>oint.y)
			   :sheet sheet
			   :modifier-state *-current-event-modifier-state-*
			   :timestamp (incf timestamp)))))))

  )  ; end of 'timestamp' closure

;;; This is really, really horribly written. Hopefully it will just be
;;; temporary until everything is 'band-aided' (!?) at which point we'll
;;; look to migrate to Carbon and reimplement a lot of this stuff.
(defun current-mods-map-to-key (current-modifier-state)
  (declare (special *-current-event-modifier-state-*))
  ;; Are there modifiers in 'current-modifier-state' that don't exist in
  ;; *-current-event-modifier-state-* (key down) or vice versa (key up)?
  ;; if shift is in special but not in current, it was a key-up
  ;; if shift in current but not special, key-down
  ;; ditto control, command, alternate, alpha
  ;;#$NSShiftKeyMask +shift-key+
  ;;#$NSControlKeyMask +control-key+
  ;;#$NSCommandKeyMask +meta-key+
  ;;#$NSAlternateKeyMask +super-key+
  ;;#$NSAlphaShiftKeyMask +hyper-key+
  (cond ((null *-current-event-modifier-state-*)
	 '(key-release-event nil))
	((and (> (logand *-current-event-modifier-state-* +shift-key+) 0)
	      (= (logand current-modifier-state #$NSShiftKeyMask) 0))
	 '(key-release-event :shift))
	((and (= (logand *-current-event-modifier-state-* +shift-key+) 0)
	      (> (logand current-modifier-state #$NSShiftKeyMask) 0))
	 '(key-press-event :shift))
	((and (> (logand *-current-event-modifier-state-* +control-key+) 0)
	      (= (logand current-modifier-state #$NSControlKeyMask) 0))
	 '(key-release-event :control))
	((and (= (logand *-current-event-modifier-state-* +control-key+) 0)
	      (> (logand current-modifier-state #$NSControlKeyMask) 0))
	 '(key-press-event :control))
	((and (> (logand *-current-event-modifier-state-* +meta-key+) 0)
	      (= (logand current-modifier-state #$NSCommandKeyMask) 0))
	 '(key-release-event :meta))
	((and (= (logand *-current-event-modifier-state-* +meta-key+) 0)
	      (> (logand current-modifier-state #$NSCommandKeyMask) 0))
	 '(key-press-event :meta))
	((and (> (logand *-current-event-modifier-state-* +super-key+) 0)
	      (= (logand current-modifier-state #$NSAlternateKeyMask) 0))
	 '(key-release-event :super))
	((and (= (logand *-current-event-modifier-state-* +super-key+) 0)
	      (> (logand current-modifier-state #$NSAlternateKeyMask) 0))
	 '(key-press-event :super))
	((and (> (logand *-current-event-modifier-state-* +hyper-key+) 0)
	      (= (logand current-modifier-state #$NSAlphaShiftKeyMask) 0))
	 '(key-release-event :hyper))
	((and (= (logand *-current-event-modifier-state-* +hyper-key+) 0)
	      (> (logand current-modifier-state #$NSAlphaShiftKeyMask) 0))
	 '(key-press-event :hyper))
	(t '(key-release-event))))


;; Need to make use of the Cocoa method for getting modifier state - this is independent of events
;; pretty much (i.e. pointer documentation pane changes depending what modifier keys are pressed
;; prior to a mouse click etc.) ::FIXME::
(defmethod pointer-modifier-state ((pointer beagle-pointer))
  ;;  (multiple-value-bind (x y same-screen-p child mask)
  ;;      (xlib:query-pointer (clx-port-window (port pointer)))
  ;;    (declare (ignore x y same-screen-p child))
  ;;    (x-event-state-modifiers (port pointer) mask)))
  (warn "pointer-modifier-state: implement me")
  nil)

;; Again, make use of Cocoa methods for querying the pointer position. See above ::FIXME::
(defmethod pointer-position ((pointer beagle-pointer))
  (warn "pointer-position: implement me")
  nil)

;; Ditto previous two methods...
(defmethod pointer-button-state ((pointer beagle-pointer))
  (warn "pointer-button-state: implement me")
  nil)

;;; Set the keyboard input focus for the port.
;;; (oops, we lose the timestamp here.)

;;; Cocoa note: the Frame (NSWindow) must be made key for us to receive events; but they
;;; must then be sent to the Sheet that has focus. Whilst there are Cocoa mechanisms to
;;; do this, it's probably best to let CLIM decide on the appropriate sheet and we just
;;; send all key events to it.

(defmethod %set-port-keyboard-focus ((port beagle-port) focus &key timestamp)
  (declare (ignore timestamp))
  (if (eq (beagle-port-key-focus port) focus)
      (format *trace-output* "Attempt to set keyboard focus on sheet ~a which already has focus.~%"
	      focus)
    (let ((mirror (sheet-mirror focus)))
      (if (null mirror)
	  (format *trace-output* "Attempt to set keyboard focus on sheet ~a which has no mirror!~%"
		  focus)
	(let ((window (send mirror 'window)))
	  (if (eql window (%null-ptr))
	      (format *trace-output* "Attempt to set keyboard focus on sheet ~a with no NSWindow!~%"
		      focus)
	    (progn
	      (setf (beagle-port-key-focus port) focus)
	      (unless (send window 'is-key-window)
		(send window 'make-key-window)))))))))


;;; Not sure we need to do this... apparently we do. I have stopped flushing
;;; the window after every drawing op, and now things don't get output
;;; properly; some drawing ops appear not to do a medium-force-output or
;;; a medium-finish-output so this hack does it instead. Note we can't do
;;; quite the same thing as the CLX backend since each window needs flushing
;;; rather than the drawing ops in the 'port output queue' (for want of a
;;; better term).

;;; I don't think this method should actually be in McCLIM personally - surely
;;; the medium-*-output methods are sufficient (if invoked at the appropriate
;;; points)?
(defmethod port-force-output ((port beagle-port))
  ;; For each graft, get the list of children. Loop over this list and
  ;; do a flush on any children that are enabled.
  (map-over-grafts #'(lambda (graft)
		       (loop for sheet in (sheet-children graft)
;;;			     do (format *debug-io* "Got sheet of: ~A~%" sheet)
			     do (when (sheet-enabled-p sheet)
;;;				  (format *debug-io* "Sheet is enabled - flushing window~%")
				  ;; This next line is a hack; I'd much rather do the medium-force-output
				  ;; solution. Unfortunately, all graft children are going to be either
				  ;; top-level-sheet-pane or unmanaged-top-level-sheet-pane types - neither
				  ;; of which have mediums (even though they are mirrored!). Kludge.
				  (send (send (port-lookup-mirror port sheet) 'window)
					'flush-window)))) ;;-if-needed))))
;;;				  (medium-force-output (sheet-medium sheet)))))
		       port))

(defmethod port-grab-pointer ((port beagle-port) pointer sheet)
  (declare (ignore port pointer sheet))
  (warn "events:port-grab-pointer:Pointer grabbing not implemented in Cocoa backend")
  nil)

(defmethod port-ungrab-pointer ((port beagle-port) pointer sheet)
  (declare (ignore port pointer sheet))
  (warn "events:port-ungrab-pointer:Pointer grabbing not implemented in Cocoa backend")
  nil)

(defun characters-to-key-name (ns-string-characters-in)
;;;  (format *terminal-io* "Processing ~S~%" ns-string-characters-in)
;;;  (format *terminal-io* "Got string with length ~A~%" (send ns-string-characters-in 'length))
;;;  (format *terminal-io* "character(0) = ~A~%" (send ns-string-characters-in :character-at-index 0))
  (if (<= (send ns-string-characters-in :character-at-index 0) 255)
      (numeric-keysym-to-character (send ns-string-characters-in :character-at-index 0))
    (progn
      (let ((key-name (lookup-keysym (send ns-string-characters-in :character-at-index 0))))
	;; If key-name is nil after all that, see if we can look up a mapping from those supported in
	;; Cocoa...
	(cond
	 ((null key-name)
	  (let ((clim-key
		 (get-key-name-from-cocoa-constants
		  (send ns-string-characters-in :character-at-index 0))))
	    clim-key))
	 (t key-name))))))

;;; From CLX/keysyms.lisp

(defun numeric-keysym-to-character (keysym)
  (cond
   ((= #x1b keysym)
    (get-key-name-from-cocoa-constants keysym))
   ((and (<= 0 keysym 255))
    (code-char keysym))
   (t nil)))

(defun keysym-to-character (keysym)
  (numeric-keysym-to-character (reverse-lookup-keysym keysym)))

(defconstant *beagle-key-constants* (list
				     #$NSUpArrowFunctionKey      :UP
				     #$NSDownArrowFunctionKey    :DOWN
				     #$NSLeftArrowFunctionKey    :LEFT
				     #$NSRightArrowFunctionKey   :RIGHT
				     #$NSF1FunctionKey           :F1
				     #$NSF2FunctionKey           :F2
				     #$NSF3FunctionKey           :F3
				     #$NSF4FunctionKey           :F4
				     #$NSF5FunctionKey           :F5
				     #$NSF6FunctionKey           :F6
				     #$NSF7FunctionKey           :F7
				     #$NSF8FunctionKey           :F8
				     #$NSF9FunctionKey           :F9
				     #$NSF10FunctionKey          :F10
				     #$NSF11FunctionKey          :F11
				     #$NSF12FunctionKey          :F12
				     #$NSF13FunctionKey          :F13
				     #$NSF14FunctionKey          :F14
				     #$NSF15FunctionKey          :F15
				     #$NSF16FunctionKey          :F16
				     #$NSF17FunctionKey          :F17
				     #$NSF18FunctionKey          :F18
				     #$NSF19FunctionKey          :F19
				     #$NSF20FunctionKey          :F20
				     #$NSF21FunctionKey          :F21
				     #$NSF22FunctionKey          :F22
				     #$NSF23FunctionKey          :F23
				     #$NSF24FunctionKey          :F24
				     #$NSF25FunctionKey          :F25
				     #$NSF26FunctionKey          :F26
				     #$NSF27FunctionKey          :F27
				     #$NSF28FunctionKey          :F28
				     #$NSF29FunctionKey          :F29
				     #$NSF30FunctionKey          :F30
				     #$NSF31FunctionKey          :F31
				     #$NSF32FunctionKey          :F32
				     #$NSF33FunctionKey          :F33
				     #$NSF34FunctionKey          :F34
				     #$NSF35FunctionKey          :F35
				     #$NSInsertFunctionKey       :INSERT
				     #$NSDeleteFunctionKey       :DELETE-CHAR
				     #$NSHomeFunctionKey         :HOME
				     #$NSBeginFunctionKey        :BEGIN
				     #$NSEndFunctionKey          :END
				     #$NSPageUpFunctionKey       :PAGE-UP
				     #$NSPageDownFunctionKey     :PAGE-DOWN
				     #$NSPrintScreenFunctionKey  :SUN-PRINT-SCREEN
				     #$NSScrollLockFunctionKey   :SCROLL-LOCK
				     #$NSPauseFunctionKey        :PAUSE
				     #$NSSysReqFunctionKey       :SYS-REQ
				     #$NSBreakFunctionKey        :BREAK
				     #$NSResetFunctionKey        :RESET
				     #$NSStopFunctionKey         :SUN-STOP
				     #$NSMenuFunctionKey         :MENU
				     #$NSUserFunctionKey         :USER
				     #$NSSystemFunctionKey       :SYSTEM
				     #$NSPrintFunctionKey        :PRINT
				     #$NSClearLineFunctionKey    :CLEAR-LINE
				     #$NSClearDisplayFunctionKey :CLEAR-DISPLAY
				     #$NSInsertLineFunctionKey   :INSERT-LINE
				     #$NSDeleteLineFunctionKey   :DELETE-LINE
				     #$NSInsertCharFunctionKey   :INSERT-CHAR
				     #$NSDeleteCharFunctionKey   :DELETE-CHAR
				     #$NSPrevFunctionKey         :OSF-PREV-FIELD
				     #$NSNextFunctionKey         :NEXT
				     #$NSSelectFunctionKey       :SELECT
				     #$NSExecuteFunctionKey      :EXECUTE
				     #$NSUndoFunctionKey         :UNDO
				     #$NSRedoFunctionKey         :REDO
				     #$NSFindFunctionKey         :FIND
				     #$NSHelpFunctionKey         :HELP
				     #$NSModeSwitchFunctionKey   :MODE-SWITCH
				     #x1b                        :ESCAPE))

;;;(defun get-key-name-from-cocoa-constants (ns-in)
;;;  (loop for target, key in *cocoa-key-constants*
;;;        (do
;;;            (when (send target :is-equal-to-string ns-in)
;;;              key))))

(defvar *beagle-key-hash-table*
  (make-hash-table :test #'eql))

(defvar *reverse-beagle-key-hash-table*
  (make-hash-table :test #'eq))

(defun define-beagle-key (ns-key clim-key)
  (pushnew clim-key (gethash ns-key *beagle-key-hash-table*))
  (setf (gethash clim-key *reverse-beagle-key-hash-table*) ns-key))

(defun lookup-beagle-key (ns-key)
  (car (last (gethash ns-key *beagle-key-hash-table*))))

(defun reverse-lookup-beagle-key (clim-key)
  (gethash clim-key *reverse-beagle-key-hash-table*))

(loop for key-binding on *beagle-key-constants* by #'cddr
   do (define-beagle-key (car key-binding) (cadr key-binding)))

(defun get-key-name-from-cocoa-constants (ns-in)
  (lookup-beagle-key ns-in))
