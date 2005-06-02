;;; -*- Mode: Lisp; Package: BEAGLE -*-

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

Each frame manager type is associated with a port; and may manage multiple frames.
In the cocoa world, a frame *is* an "NSWindow" (or an object mapping an NSWindow
at least).

   +---------------+
   | FRAME-MANAGER |
   +---------------+
   |port           |
   |frames         |
   +---------------+
           ^
           |
 +---------------------+
 | BEAGLE-FRAME-MANAGER |
 +---------------------+

The different kinds of frames we need to manage at the moment are:

  1. (STANDARD-)APPLICATION-FRAME
  2. MENU-FRAME

This makes sense, even for cocoa.

How do we then find a _different_ frame manager to adopt our sheets (say we want to implement a totally
different look and feel, or want to embed the sheet hierarchy in an existing window, etc.)?

||#

(defclass beagle-standard-frame-manager (frame-manager) ()
  (:documentation "Frame manager for Beagle back end that provides the ``cross platform'' McCLIM
look and feel"))

(defclass beagle-aqua-frame-manager (frame-manager) ()
  (:documentation "Frame manager for Beagle back end that provides Apple's Aqua look
and feel for McCLIM. If any pane types are not implemented for Beagle / Aqua, the
``cross platform'' look and feel will be used."))

;;; This is an example of how make-pane-1 might create specialized instances of the generic pane types
;;; based upon the type of the frame-manager. Unlike in the CLX case, we *do* expect there to be Beagle
;;; specific panes (eventually!).
(defmethod make-pane-1 ((fm beagle-aqua-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (or (find-symbol (concatenate 'string
				       (symbol-name '#:beagle-) (symbol-name type))
			  :beagle)
	     (find-symbol (concatenate 'string
				       (symbol-name '#:beagle-) (symbol-name type) (symbol-name '#:-pane))
			  :beagle)
	     (find-symbol (concatenate 'string (symbol-name type) (symbol-name '#:-pane))
			  :climi)
	     type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

;;; We must implement this method to ensure the menu-frame has its top + left slots set.
(defmethod adopt-frame :before ((fm beagle-aqua-frame-manager) (frame menu-frame))
;;;  (format *debug-io* "frame-manager.lisp: ::FIXME:: -> ADOPT-FRAME :before (fm:~S frame:~S)~%" fm frame)
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (slet ((mouse-location (send (@class ns-event) 'mouse-location)))
      ;; Use CLX hackish 10-pixel offset... for now.
      (setf (slot-value frame 'climi::left) (decf (pref mouse-location :<NSP>oint.x) 10)
            (slot-value frame 'climi::top)  (incf (pref mouse-location :<NSP>oint.y) 10)))))


;;; ----------------------------------------------------------------------------

;;; "standard" look and feel (i.e. exactly the same, give or take, as the CLX
;;; (and other?) back ends.

;;; Don't even check for beagle-* panes we don't want to find them.
(defmethod make-pane-1 ((fm beagle-standard-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (or (find-symbol (concatenate 'string (symbol-name type) (symbol-name '#:-pane))
			  :climi)
	     type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

;;; We must implement this method to ensure the menu-frame has its top + left slots set.
(defmethod adopt-frame :before ((fm beagle-standard-frame-manager) (frame menu-frame))
;;;  (format *debug-io* "frame-manager.lisp: ::FIXME:: -> ADOPT-FRAME :before (fm:~S frame:~S)~%" fm frame)
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (slet ((mouse-location (send (@class ns-event) 'mouse-location)))
      ;; Use CLX hackish 10-pixel offset... for now.
      (setf (slot-value frame 'climi::left) (decf (pref mouse-location :<NSP>oint.x) 10)
            (slot-value frame 'climi::top)  (incf (pref mouse-location :<NSP>oint.y) 10)))))


(defmethod adopt-frame :after ((fm beagle-standard-frame-manager) (frame menu-frame))
  (declare (ignore fm))
;;  (format *debug-io* "Entered adopt-frame :after for frame ~a~%" frame)
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (send (send (sheet-direct-mirror (slot-value frame 'top-level-sheet)) 'window)
	  :make-key-and-order-front nil)))  ; <- just :order-front?

#+nil
(defmethod adopt-frame :after ((fm beagle-standard-frame-manager) (frame application-frame))
  (declare (ignore fm))
  (let* ((top-level-sheet (frame-top-level-sheet frame))
	 (mirror (sheet-direct-mirror top-level-sheet)))
    (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
      (declare (ignore w h))
      (when (and x y)
	(let ((point (ccl::make-ns-point (coerce x 'short-float) (coerce y 'short-float))))
	  (send (send mirror 'window) :set-frame-top-left-point point)
	  (#_free point))))))


(defmethod adopt-frame :after ((fm beagle-aqua-frame-manager) (frame menu-frame))
  (declare (ignore fm))
;;  (format *debug-io* "Entered adopt-frame :after for frame ~a~%" frame)
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (send (send (sheet-direct-mirror (slot-value frame 'top-level-sheet)) 'window)
	  :make-key-and-order-front nil)))  ; <- just :order-front?

#+nil
(defmethod adopt-frame :after ((fm beagle-aqua-frame-manager) (frame application-frame))
  (declare (ignore fm))
  (let* ((top-level-sheet (frame-top-level-sheet frame))
	 (mirror (sheet-direct-mirror top-level-sheet)))
    (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
      (declare (ignore w h))
      (when (and x y)
	(let ((point (ccl::make-ns-point (coerce x 'short-float) (coerce y 'short-float))))
;;;	  (format *debug-io* "Setting frame top left point to (~a, ~a)~%" x y)
	  (send (send mirror 'window) :set-frame-top-left-point point))))))


;;; Will this method be invoked for all frame types? E.g. what if we have CLX + Beagle
;;; frame managers? Will this method be invoked for both? Need to run a test...

;;; Aargh, menu frames go through a different path... ENABLE-FRAME is not invoked
;;; for them :-(
(defmethod enable-frame :after (frame)
  (declare (special *beagle-port*))
  ;; How to get the frame manager for the frame? (frame-manager frame) [might be
  ;; needed if we do indeed need to differentiate between CLX fm and Beagle fm].
  ;; A better solution might be to introduce a BEAGLE-FRAME and a CLX-FRAME type.
;;;  (format *trace-output* "Entered ENABLE-FRAME with frame = ~a~%" frame)
  (let* ((sheet  (frame-top-level-sheet frame))
	 (window (send (port-lookup-mirror *beagle-port* sheet) 'window)))
    (unless (send window 'is-key-window)
      (send window :make-key-and-order-front nil))))


;;; Override 'pointer-tracking.lisp' method of the same name since we *don't* do pointer tracking;
;;; should fix this properly in the future at which time we should be able to remove this.

;;; Remove it now, this isn't the way pointer-tracking is implemented any more - this breaks menus
;;; in Beagle, unfortunately, now.

;;;(in-package :clim-internals)
;;;
;;;(defun invoke-tracking-pointer
;;;    (sheet
;;;     pointer-motion-handler presentation-handler
;;;     pointer-button-press-handler presentation-button-press-handler
;;;     pointer-button-release-handler presentation-button-release-handler
;;;     keyboard-handler
;;;     &key pointer multiple-window transformp (context-type t)
;;;     (highlight nil highlight-p))
;;;  ;; (setq pointer (port-pointer (port sheet))) ; FIXME
;;;  (let ((port (port sheet))        
;;;        (presentations-p (or presentation-handler
;;;                             presentation-button-press-handler
;;;                             presentation-button-release-handler)))
;;;    (unless highlight-p (setq highlight presentations-p))
;;;    (with-sheet-medium (medium sheet)
;;;      (flet ((do-tracking ()
;;;               (with-input-context (context-type :override t)
;;;                 ()
;;;                 (loop
;;;                  (let ((event (event-read sheet)))
;;;                    (when (and (eq sheet (event-sheet event))
;;;                               (typep event 'pointer-motion-event))
;;;                      (queue-event sheet event)
;;;                      (highlight-applicable-presentation
;;;                       (pane-frame sheet) sheet *input-context*))
;;;                    (cond ((and (typep event 'pointer-event)
;;;                                #+nil
;;;                                (eq (pointer-event-pointer event)
;;;                                    pointer))                     
;;;                           (let* ((x (pointer-event-x event))
;;;                                  (y (pointer-event-y event))
;;;                                  (window (event-sheet event))
;;;                                  (presentation
;;;                                   (and presentations-p
;;;                                        (find-innermost-applicable-presentation
;;;                                         *input-context*
;;;                                         sheet ; XXX
;;;                                         x y
;;;                                         :modifier-state (event-modifier-state event)))))
;;;                             (when (and highlight presentation)
;;;                               (frame-highlight-at-position
;;;                                (pane-frame sheet) window x y))
;;;                             ;; FIXME Convert X,Y to SHEET coordinates; user
;;;                             ;; coordinates
;;;                             (typecase event
;;;                               (pointer-motion-event
;;;                                (if (and presentation presentation-handler)
;;;                                    (funcall presentation-handler
;;;                                             :presentation presentation
;;;                                             :window window :x x :y y)
;;;                                    (maybe-funcall
;;;                                     pointer-motion-handler
;;;                                     :window window :x x :y y)))
;;;                               (pointer-button-press-event
;;;                                (if (and presentation
;;;                                         presentation-button-press-handler)
;;;                                    (funcall
;;;                                     presentation-button-press-handler
;;;                                     :presentation presentation
;;;                                     :event event :x x :y y)
;;;                                    (maybe-funcall
;;;                                     pointer-button-press-handler
;;;                                     :event event :x x :y y)))
;;;                               (pointer-button-release-event
;;;                                (if (and presentation
;;;                                         presentation-button-release-handler)
;;;                                    (funcall
;;;                                     presentation-button-release-handler
;;;                                     :presentation presentation
;;;                                     :event event :x x :y y)
;;;                                    (maybe-funcall
;;;                                     pointer-button-release-handler
;;;                                     :event event :x x :y y))))))
;;;                          ((typep event
;;;                                  '(or keyboard-event character symbol))
;;;                           (maybe-funcall keyboard-handler
;;;                                          :gesture event #|XXX|#))
;;;                          (t (handle-event #|XXX|# (event-sheet event)
;;;                                                   event))))))))
;;;        (do-tracking)))))

;;; Now we change tracking-pointer-loop instead. I think we *REALLY* should get
;;; rid of pointer grabbing!

(in-package :clim-internals)

(defmethod tracking-pointer-loop
    ((state tracking-pointer-state) frame sheet &rest args
     &key pointer multiple-window transformp context-type highlight)
  (declare (ignore args pointer context-type highlight frame multiple-window))
  (with-sheet-medium (medium sheet)
    (flet ((do-tracking ()
	     (loop
		for event = (event-read sheet)
		do (if (typep event 'pointer-event)
		       (multiple-value-bind (sheet-x sheet-y)
			   (pointer-event-position* event)
			 (multiple-value-bind (x y)
			     (if transformp
				 (transform-position
				  (medium-transformation medium)
				  sheet-x
				  sheet-y)
				 (values sheet-x sheet-y))
			   (tracking-pointer-loop-step state event x y)))
		       (tracking-pointer-loop-step state event 0 0)))))
      (do-tracking))))
;;;      (if multiple-window
;;;	  (with-pointer-grabbed ((port medium) sheet)
;;;	    (do-tracking))
;;;	  (do-tracking)))))
