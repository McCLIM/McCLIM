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
  (declare (ignorable pointer transformp context-type highlight))
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
						      :allow-other-keys t)))))

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
  (declare (ignore pointer context-type highlight frame))
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
  (funcall (motion-handler state) :window (event-sheet event) :x x :y y))

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

(defgeneric drag-output-record
    (stream output
     &key repaint erase feedback finish-on-release multiple-window))

(defmethod drag-output-record
    ((stream output-recording-stream) (record output-record)
     &key (repaint t) (erase #'erase-output-record)
     feedback finish-on-release multiple-window
     feedback-event)
  (declare (ignore repaint multiple-window))
  (multiple-value-bind (dx dy)
      (output-record-position record)
    (flet ((feedback-fn (record stream initial-x initial-y x y action)
	     (declare (ignore initial-x initial-y))
	     (if (eq action :erase)
		 (funcall erase record stream)
		 (progn
		   (setf (output-record-position record)
			 (values (+ dx x) (+ dy y)))
		   (stream-add-output-record stream record)
		   (stream-replay stream record))))
	   (feedback-event-fn (record stream initial-x initial-y x y
				      action event)
	     (declare (ignore event))
	     (when (or (eq action :draw) (eq action :erase))
	       (funcall feedback record stream initial-x initial-y x y
			action))))
      (declare (dynamic-extent #'feedback-fn #'feedback-event-fn))
      (unless feedback
	(setq feedback #'feedback-fn))
      (unless feedback-event
	(setq feedback-event #'feedback-event-fn))
      (setf (stream-current-output-record stream)
	    (stream-output-history stream))
      (let* ((pointer (port-pointer (port stream)))
	     (pointer-state (pointer-button-state pointer)))
	(multiple-value-bind (x0 y0)
	    (stream-pointer-position stream)
	  (funcall feedback-event record stream x0 y0 x0 y0 :draw nil)
	  (tracking-pointer (stream)
	   (:pointer-motion (&key event x y)
	     ;; XXX What about the sheet?
	     (funcall feedback-event record stream x0 y0 x y :erase event)
	     (funcall feedback-event record stream x0 y0 x y :draw event))
	   (:pointer-button-press (&key event x y)
	     (funcall feedback-event record stream x0 y0 x y
		      :button-press event)
	     (unless finish-on-release
	       (return-from drag-output-record (values x y))))
	   (:pointer-button-release (&key event x y)
	     ;; If the button released was one of those held down on entry to
	     ;; drag-output-record, we're done.
	     (when (and finish-on-release
			(not (zerop (logand pointer-state
					    (pointer-event-button event)))))
	       (funcall feedback-event record stream x0 y0 x y
			:button-release event)
	       (return-from drag-output-record (values x y))))))))))
  
(defmacro dragging-output ((&optional (stream '*standard-output*) &rest args
			    &key repaint finish-on-release multiple-window)
			   &body body)
  (declare (ignore repaint finish-on-release multiple-window))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-gensyms (record)
    `(let ((,record (with-output-to-output-record (,stream)
		      ,@body)))
       (multiple-value-prog1
	   (drag-output-record ,stream ,record ,@args)
	 (erase-output-record ,record ,stream)))))

