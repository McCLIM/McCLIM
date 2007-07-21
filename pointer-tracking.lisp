;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)
;;;  (c) copyright 2004 by Tim Moore (moore@bricoworks.com)

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

;;; TODO:
;;;
;;; - Keyboard gestures.
;;;
;;; - Optimization
;;;
;;; - - too many repeated checks within a loop;
;;;
;;; - - remove invoke-tracking-pointer; remove unnecessary checks.

(in-package :clim-internals)

;;; The Spec specifies the tracking-pointer clause arguments as, e.g.,
;;; (&key presentation event x y), implying that the user must write
;;; the &key keyword, but real code doesn't do that. Check if &key is in
;;; the arg list and add it if it is not.
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun fix-tracking-pointer-args (args)
   (unless (member '&allow-other-keys args)
     (setq args (append args '(&allow-other-keys))))
   (if (eq (car args) '&key)
       args
       (cons '&key args))))

(defmacro with-pointer-grabbed ((port sheet &key pointer) &body body)
  (with-gensyms (the-port the-sheet the-pointer grabbed)
    `(let* ((,the-port ,port)
	    (,the-sheet ,sheet)
	    (,the-pointer (or ,pointer (port-pointer ,the-port)))
	    (,grabbed nil))
       ;; Don't end up in the debugger with the pointer grabbed! 
       (handler-bind ((error #'(lambda (c)
				 (declare (ignore c))
				 (when ,grabbed
				   (port-ungrab-pointer ,the-port
							,the-pointer
							,the-sheet)
				   (setq ,grabbed nil)))))
	 (unwind-protect
	      (when (port-grab-pointer ,the-port ,the-pointer ,the-sheet)
		(setq ,grabbed t)
		,@body)
	   (when ,grabbed
	     (port-ungrab-pointer ,the-port ,the-pointer ,the-sheet)))))))

;;; tracking-pointer. The functionality that deals with presentations has been
;;; split off into frames.lisp.


(defgeneric tracking-pointer-loop (state frame sheet
				   &key pointer multiple-window
				   transformp context-type highlight
				   &allow-other-keys))

(defgeneric tracking-pointer-loop-step (tracking-state event x y))

(defgeneric make-tracking-pointer-state (frame sheet args))

(defmacro tracking-pointer
    ((sheet &rest args
            &key pointer multiple-window transformp context-type
            (highlight nil highlight-p))
     &body body)
  (declare (ignorable multiple-window pointer transformp context-type highlight))
  (setq sheet (stream-designator-symbol sheet '*standard-output*))
  (loop
     for (event-name handler-args . handler-body) in body
     for handler-name = (gensym (symbol-name event-name))
     collect `(,handler-name ,(fix-tracking-pointer-args handler-args)
	       ,@handler-body) into bindings
     collect `#',handler-name into fn-names
     append `(,event-name #',handler-name) into tracking-pointer-args
     finally (return `(flet ,bindings
                        (declare (dynamic-extent ,@fn-names))
                        (invoke-tracking-pointer-loop *application-frame*
						      ,sheet
						      ,@tracking-pointer-args
                                                      ,@args
                                                      #-cmu18e :allow-other-keys #-cmu18e t)))))

(defun invoke-tracking-pointer-loop (frame sheet &rest args)
  (apply #'tracking-pointer-loop
	 (make-tracking-pointer-state frame sheet args)
	 frame sheet args))

(defun default-tracking-handler (&rest args)
  (declare (ignore args))
   nil)

(defclass tracking-pointer-state ()
  ((motion-handler :reader motion-handler :initarg :pointer-motion)
   (button-press-handler :reader button-press-handler
 			 :initarg :pointer-button-press)
   (buttton-release-handler :reader button-release-handler
			    :initarg :pointer-button-release)
   (keyboard-handler :reader keyboard-handler :initarg :keyboard))
  (:default-initargs :pointer-motion #'default-tracking-handler
                     :pointer-button-press #'default-tracking-handler
                     :pointer-button-release #'default-tracking-handler
                     :keyboard #'default-tracking-handler))


(defmethod tracking-pointer-loop
    ((state tracking-pointer-state) frame sheet &rest args
     &key pointer multiple-window transformp context-type highlight)
  (declare (ignore args pointer context-type highlight frame))
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
      (if multiple-window
	  (with-pointer-grabbed ((port medium) sheet)
	    (do-tracking))
	  (do-tracking)))))

(defmethod tracking-pointer-loop-step
    ((state tracking-pointer-state) (event pointer-motion-event) x y)
  (funcall (motion-handler state) :event event :window (event-sheet event) :x x :y y))

(defmethod tracking-pointer-loop-step
    ((state tracking-pointer-state) (event pointer-button-press-event) x y)
  (funcall (button-press-handler state)
	   :event event :window (event-sheet event) :x x :y y))

(defmethod tracking-pointer-loop-step
    ((state tracking-pointer-state) (event pointer-button-release-event) x y)
  (funcall (button-release-handler state)
	   :event event :window (event-sheet event) :x x :y y))

(defmethod tracking-pointer-loop-step
    ((state tracking-pointer-state) (event t) x y)
  (declare (ignore x y))
  (if (typep event '(or keyboard-event character symbol))
      (funcall (keyboard-handler state) :gesture event)
      (handle-event (event-sheet event) event)))


;;; DRAG-OUTPUT-RECORD and DRAGGING-OUTPUT.
;;;
;;; XXX Unresolved issues:
;;; multiple-window is completely unsupported.
;;; window-repaint events while dragging.

(defun bound-rectangles (r1-x1 r1-y1 r1-x2 r1-y2 r2-x1 r2-y1 r2-x2 r2-y2)
  (values (min r1-x1 r2-x1) (min r1-y1 r2-y1)
	  (max r1-x2 r2-x2) (max r1-y2 r2-y2)))


(defgeneric drag-output-record
    (stream output
     &key repaint erase feedback finish-on-release multiple-window))

;;; Fancy double-buffered feedback function
(defun make-buffered-feedback-function (record finish-on-release erase-final)
  (multiple-value-bind (record-x record-y)
      (output-record-position record)
    (lambda (record stream initial-x initial-y x y event)
      (flet ((simple-erase ()
	       (when erase-final
		 (when (output-record-parent record)
		   (delete-output-record record (output-record-parent record)))
		 (with-double-buffering
		     ((stream record) (buffer-rectangle))
		   (stream-replay stream buffer-rectangle)))))
	(let ((dx (- record-x initial-x))
	      (dy (- record-y initial-y)))
	  (typecase event
	    (null
	     (setf (output-record-position record) (values (+ dx x) (+ dy y)))
	     (stream-add-output-record stream record)
	     (stream-replay stream record))
	    (pointer-motion-event
	     ;; Don't do an explicit erase. Instead, update the position of the
	     ;; output record and redraw the union of the old and new
	     ;; positions.
	     (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2)
	       record
	       (when (output-record-parent record)
		 (delete-output-record record (output-record-parent record)))
	       (setf (output-record-position record)
		     (values (+ dx x) (+  dy y)))
	       (stream-add-output-record stream record)
	       (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
		 record
		 (multiple-value-bind (area-x1 area-y1 area-x2 area-y2)
		     (bound-rectangles old-x1 old-y1 old-x2 old-y2
				       new-x1 new-y1 new-x2 new-y2)
		   (with-double-buffering
		       ((stream area-x1 area-y1 area-x2 area-y2)
			(buffer-rectangle))
		     (stream-replay stream buffer-rectangle))))))
	    (pointer-button-press-event
	     (unless finish-on-release
	       (simple-erase)))
	    (pointer-button-release-event
	     (when finish-on-release
	       (simple-erase)))
	    (t nil)))))))

;;; If the user supplies a feedback function, create a function to
;;; call it with the simple :draw / :erase arguments.

(defun make-simple-feedback-function
    (record feedback finish-on-release erase-final)
  (declare (ignore record))
  (lambda (record stream initial-x initial-y x y event)
    (typecase event
      (null
       (funcall feedback record stream initial-x initial-y x y :draw))
      (pointer-motion-event
       (funcall feedback record stream initial-x initial-y x y :erase)
       (funcall feedback record stream initial-x initial-y x y :draw))
      (pointer-button-press-event
       (unless finish-on-release
	 (when erase-final
	   (funcall feedback record stream initial-x initial-y x y :erase))))
      (pointer-button-release-event
       (when (and finish-on-release erase-final)
	 (funcall feedback record stream initial-x initial-y x y :erase)))
      (t nil))))


(defmethod drag-output-record
    ((stream output-recording-stream) (record output-record)
     &key (repaint t) (erase #'erase-output-record)
     feedback finish-on-release multiple-window
     feedback-event erase-final)
  (declare (ignore erase repaint multiple-window))
  (let ((feedback-event-fn
	 (cond (feedback-event
		feedback-event)
	       (feedback
		(make-simple-feedback-function record
					       feedback
					       finish-on-release
					       erase-final))
	       (t (make-buffered-feedback-function record
						   finish-on-release
						   erase-final)))))
    (setf (stream-current-output-record stream)
	  (stream-output-history stream))
    (let* ((pointer (port-pointer (port stream)))
	   (pointer-state (pointer-button-state pointer)))
      (multiple-value-bind (x0 y0)
	  (stream-pointer-position stream)
	(funcall feedback-event-fn record stream x0 y0 x0 y0 nil)
	(tracking-pointer (stream)
	 (:pointer-motion (&key event x y)
	   ;; XXX What about the sheet?
	   (funcall feedback-event-fn record stream x0 y0 x y event)
	   (funcall feedback-event-fn record stream x0 y0 x y event))
	 (:pointer-button-press (&key event x y)
	   (unless finish-on-release
	     (funcall feedback-event-fn record stream x0 y0 x y event)
	     (return-from drag-output-record (values x y))))
	 (:pointer-button-release (&key event x y)
	   ;; If the button released was one of those held down on entry to
	   ;; drag-output-record, we're done.
	   (when (and finish-on-release
		      (not (zerop (logand pointer-state
					  (pointer-event-button event)))))
	     (funcall feedback-event-fn record stream x0 y0 x y event)
	     (return-from drag-output-record (values x y)))))))))
  
(defmacro dragging-output ((&optional (stream '*standard-output*) &rest args
			    &key (repaint t) finish-on-release multiple-window)
			   &body body)
  (declare (ignore repaint finish-on-release multiple-window))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-gensyms (record)
    `(let ((,record (with-output-to-output-record (,stream)
		      ,@body)))
       (drag-output-record ,stream ,record :erase-final t ,@args))))

(defun dragging-drawing (stream drawer &key (finish-on-release t)
                         (pointer (port-pointer (port stream)))
                         multiple-window)
  "Draws something simple in response to pointer events for
`pointer' and returns the coordinates of the pointer when the
function finishes. The function finishes when mouse button one is
no longer held down if `finish-on-release' is true; if it is
false, it finishes when the mouse is clicked. `Drawer' should
draw something on `stream', and is called with tree arguments:
two integers, the X and the Y coordinates for the pointer motion
triggering the draw, and either the symbol `:draw' or `:erase'
signalling what the function should do. `Drawer' will be called
with the previously used coordinates whenever pointer motion
occurs, so it can erase the previous output (elegantly done by
using `+flipping-ink+' for drawing and ignoring the state
symbol)."
  (with-output-recording-options (stream :draw t :record nil)
    (let ((ox nil) (oy nil))           ; So we can erase the old line.
      (labels ((draw (x y)
                 (funcall drawer x y :draw))
               (erase (x y)
                 (funcall drawer x y :erase))
               (motion (x y)
                 (when ox (erase ox oy))
                 (draw x y)
                 (setf ox x oy y))
               (end (event x y)
                 (when (eql (event-sheet event) stream)
                   (when ox (draw ox oy))
                   (return-from dragging-drawing
                     (values x y)))))
        ;; Make an initial draw. We need to convert the screen
        ;; coordinates from the pointer into sheet-local coordinates.
        (multiple-value-call #'transform-position
          (sheet-native-transformation stream) (pointer-position pointer))
        (tracking-pointer (stream :pointer pointer
                                  :multiple-window multiple-window)
          (:pointer-motion (window x y)
                           (when (eql window stream)
                             (motion x y)))
          (:pointer-button-press (event x y)
                                 (end event x y))
          (:pointer-button-release (event x y)
                                   (when finish-on-release
                                     (end event x y))))))))

(defun pointer-place-rubber-band-line* (&key (stream *standard-output*)
                                        (pointer (port-pointer (port stream)))
                                        multiple-window start-x start-y
                                        (finish-on-release t))
  "Let the user drag a line on `stream', returning the
coordinates of the line ends as four values. `Pointer' is the
pointer that will be tracked (the default should be used unless
the port has multiple pointing devices), `multiple-window' is
currently unimplemented and `start-x'/`start-y', if provided (and
both or none must be provided) are the coordinates for one end of
the line. If these arguments are not provided, the user will have
to press a mouse button to specify the beginning of the line. If
`finish-on-release' is true, the function will end when the user
releases the mouse button. If false, the user will have to click
to finish inputting the line."
  (assert (not (eq (not (not start-x)) (not start-y))) nil
          "You must provide either both `:start-x' and `:start-y'
or none at all")
  (or start-x
      (block nil
        (tracking-pointer (stream :pointer pointer
                                  :multiple-window multiple-window)
          (:pointer-button-press (event x y)
                                 (declare (ignore event))
                                 (setf start-x x)
                                 (setf start-y y)
                                 (return)))))
  (assert (and (>= start-x 0) (>= start-y 0)))
  (labels ((draw (x y state)
             (declare (ignore state))
             (with-drawing-options (stream :ink +flipping-ink+)
               (draw-line* stream start-x start-y x y))))
    (multiple-value-call #'values
      (values start-x start-y)
      (dragging-drawing stream #'draw :finish-on-release finish-on-release
                        :pointer pointer :multiple-window multiple-window))))

;; The CLIM 2.2 spec is slightly unclear about how the next two
;; functions are supposed to behave, especially wrt. the user
;; experience. I think these functions are supposed to present a
;; rectangle on screen and let the user drag around the edges - this
;; would make supporting both left/top and right/bottom make sense,
;; and provide a way for the :rectangle argument to
;; `pointer-input-rectangle' to make sense. However, this would be a
;; very weird user experience, so I (Troels) have instead chosen to
;; consider left/top and right/bottom to be the same thing, preferring
;; left/top if both are specified. The :rectangle argument to
;; `pointer-input-rectangle' is ignored. The user is meant to drag out
;; a rectangle with the mouse, possibly by first providing a starting
;; point. This is intuitive behavior and I see no point in supporting
;; something more complicated. These changes should be invisible to
;; the calling program.

(defun pointer-input-rectangle* (&key (stream *standard-output*)
                                 (pointer (port-pointer (port stream)))
                                 multiple-window left top right bottom
                                 (finish-on-release t))
  "Let the user drag a rectangle on `stream' and return four
values, the coordinates of the rectangle. `Pointer' is the
pointer that will be tracked (the default should be used unless
the port has multiple pointing devices), `multiple-window' is
currently unimplemented and both `left'/`top' and
`right'/`bottom' specify an initial position for a rectangle
corner. You must provide either both parts of any of these two
coordinate pairs or none at all. If you provide both `left'/`top'
and `right'/`bottom', the `left'/`top' values will be used,
otherwise, the non-nil set will be used. If neither is specified,
the user will be able to specify the origin corner of the
rectangle by clicking the mouse. If `finish-on-release' is true,
the function will end when the user releases the mouse button. If
false, the user will have to click to finish inputting the
rectangle."
  (assert (not (eq (not (not top)) (not left))) nil
          "You must provide either none or both of `:top' and `:left'")
  (assert (not (eq (not (not right)) (not bottom))) nil
          "You must provide either none or both of `:right' and `:bottom'")
  (setf top (or top bottom)
        left (or left right))
  (unless top
    (block nil
      (tracking-pointer (stream :pointer pointer
                                :multiple-window multiple-window)
        (:pointer-button-press (event x y)
                               (declare (ignore event))
                               (setf left x)
                               (setf top y)
                               (return)))))
  (multiple-value-bind (x y)
      (labels ((draw (x y state)
                 (declare (ignore state))
                 (with-drawing-options (stream :ink +flipping-ink+)
                   (draw-rectangle* stream left top x y :filled nil))))
        (dragging-drawing stream #'draw :finish-on-release finish-on-release
                          :pointer pointer :multiple-window multiple-window))
    ;; Normalise so that x1 < x2 ^ y1 < y2.
    (values (min left x) (min top y)
            (max left x) (max top y))))

(defun pointer-input-rectangle (&rest args &key (stream *standard-output*)
                                (pointer (port-pointer (port stream)))
                                multiple-window rectangle
                                (finish-on-release t))
  "Like `pointer-input-rectangle*', but returns a bounding
rectangle instead of coordinates."
  (declare (ignore pointer multiple-window rectangle finish-on-release))
  (with-keywords-removed (args (:rectangle))
    (apply #'make-bounding-rectangle (apply #'pointer-input-rectangle args))))
