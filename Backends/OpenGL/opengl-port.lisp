;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by  Julien Boninfante (boninfan@emi.u-bordeaux.fr)

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

(in-package :CLIM-INTERNALS)

(defparameter *drawing-flag* (the boolean nil))

(defmacro to-gl (val)
  "Coerce VAL to the type expected by the OpenGL backend (currently 
  double-float)"
  `(coerce ,val 'double-float))

;; OpenGL port class

(defclass opengl-port (basic-port opengl-graphical-system-port-mixin)
  ((signature->sheet
     :type hash-table
     :initform (make-hash-table))
   (sheet->signature-dl
     :type hash-table
     :initform (make-hash-table :test #'eq))
   (xevent
     :initform (xlib-gl:make-xevent)
     :reader opengl-port-xevent)
   (xpeek
     :initform (xlib-gl:make-xevent)
     :reader opengl-port-xpeek)
   (synthesized-events :initform nil :accessor synthesized-events
		       :documentation "List of events resulting from
   the generation of multiple CLIM events from a single X event.")
   (current-sheet :initform nil :accessor current-sheet
		  :documentation "Current sheet under the pointer.")))

(defun recognize-sheet (port signature)
  (declare (type (unsigned-byte 24) signature)
	   (type opengl-port port))
  (gethash signature (slot-value port 'signature->sheet)))

(defun opengl-sheet-infos (port sheet)
  (declare (type opengl-port port)
	   (type sheet sheet))
  (gethash sheet (slot-value port 'sheet->signature-dl)))

(defmacro signature (port sheet)
  `(car (opengl-sheet-infos ,port ,sheet)))

(defmacro dl (port sheet)
  `(cdr (opengl-sheet-infos ,port ,sheet)))

(defmethod port-register-signature ((port opengl-port) sheet)
 ;(declare (type immediate-repainting-mixin sheet))
  (declare (type clim-repainting-mixin sheet))
  (let ((signature (get-signature)))
    (declare (type (unsigned-byte 24) signature))
    (with-slots (sheet->signature-dl signature->sheet) port
      (setf (gethash signature signature->sheet) sheet
	    (gethash sheet sheet->signature-dl) (cons signature (gl:glGenLists 1))))))

(defmethod port-unregister-signature ((port opengl-port) sheet)
 ;(declare (type immediate-repainting-mixin sheet))
  (declare (type clim-repainting-mixin sheet))
  (with-slots (sheet->signature-dl signature->sheet) port
    (let ((signature-and-dl (gethash sheet sheet->signature-dl)))
      (declare (type cons signature-and-dl))
      (gl:glDeleteLists (cdr signature-and-dl) 1)
      (remhash sheet sheet->signature-dl)
      (remhash (car signature-and-dl) signature->sheet))))

(defmethod port-set-sheet-region ((port opengl-port) (sheet sheet) region)
  (declare (ignorable port sheet)
	   (ignore region))
  nil)

(defmethod port-set-sheet-transformation ((port opengl-port) (sheet sheet) transformation)
  (declare (ignorable port sheet)
	   (ignore transformation))
  nil)

(defun parse-opengl-server-path (path)
  (pop path)
  (let* ((s (get-environment-variable "DISPLAY"))
	 (colon (position #\: s))
	 (dot (position #\. s :start colon))
	 (host-name (subseq s 0 colon))
	 (display-number (parse-integer s :start (1+ colon) :end dot))
	 (screen-number (if dot (parse-integer s :start (1+ dot)) 0)))
    (list :opengl
	  :host (getf path :host host-name)
	  :display-id (getf path :display-id display-number)
	  :screen-id (getf path :screen-id screen-number))))

(setf (get :opengl :port-type) 'opengl-port)
(setf (get :opengl :server-path-parser) 'parse-opengl-server-path)

;; opengl medium

(defmethod make-medium ((port opengl-port) sheet)
  (make-instance 'opengl-medium 
  ; :port port 
  ; :graft (find-graft :port port) 
    :sheet sheet))

;;; opengl specific functions

(defmethod opengl-init ((port opengl-port))
  (gl:glClearColor 0.0 0.0 0.0 0.0)
  (gl:glShadeModel gl:GL_FLAT))

(defun draw-sheet (port sheet mode)
  (declare (type opengl-port port)
	   (type sheet sheet)
	   (type boolean mode))
  (when (typep sheet 'clim-repainting-mixin)
    (if mode
        #+nil
	(repaint-sheet sheet (sheet-region sheet))
        #-nil
	(handle-repaint sheet (sheet-region sheet))
	(gl:glCallList (dl port sheet))))
  (loop for child of-type sheet in (sheet-children sheet)
	do (draw-sheet port child mode)))

(defun opengl-draw (port sheet mode)
  (declare (type boolean mode))
  (setf *drawing-flag* t)
  (gl:glClear gl:GL_COLOR_BUFFER_BIT)
  (draw-sheet port sheet mode)
  (setf *drawing-flag* nil))

(defmethod draw-the-entire-scene ((port opengl-port))
  (gl:glDrawBuffer gl:GL_BACK)
  (let ((sheet (opengl-port-top-level port)))
    (when sheet
;     (with-sheet-medium (medium sheet)
;       (with-slots (red green blue) (medium-background medium)
;	  (declare (type single-float red green blue))
;	  (gl:glClearColor red green blue 0.0)))
      (opengl-draw port sheet t)
      (flush port sheet))))

#+nil
(defmethod repaint-sheet :around ((sheet immediate-repainting-mixin) region)
  (declare (ignore region))
  (if *drawing-flag*
      (call-next-method)
      (draw-the-entire-scene (port sheet))))

(defmethod handle-repaint :around ((sheet clim-repainting-mixin) region)
  (declare (ignore region))
  (if *drawing-flag*
      (call-next-method)
      (draw-the-entire-scene (port sheet))))

;;; OpenGl back-end event managing

;; OpenGL back-end is implemented following the
;; light-widget pattern. So, there is only the 
;; top-level-sheet-pane which will have an
;; grpahical window. One consequence is that
;; every graphical system's event will have 
;; the mirror of this pane as window argument.

; timeout is expressed in seconds
(defmethod get-next-event ((port opengl-port) &key wait-function timeout)
  (declare (ignore wait-function))
  (if timeout
      (loop with start-time of-type integer = (round (get-internal-run-time) internal-time-units-per-second)
	    with end-time of-type integer = (+ start-time timeout)
	    for time = (round (get-internal-run-time) internal-time-units-per-second)
	    with event = nil
	    while (and (null event) (<= time end-time))
	    do (unless (event-not-present-p port)
		 (setf event (get-next-event-aux port)))
	    finally (return (or event :timeout)))
      (get-next-event-aux port)))


;; -------------------------------------------------------------
;; Explainings for sheet finding
;;
;; The purpose is to find the right sheet associated to its 
;; drawing location from coordinates (x,y) on the screen. 
;; The main idea is :
;;   1- associate with all grafted sheets a color (some kind of
;;      blue level for example)
;;   2- when trying to recognize the sheet :
;;     2.1- redraw the entire scene, with only these blue level 
;;          colors and not the normal colors, with four clipping
;;          planes defining an rectangle with dimensions 1x1 at
;;          the (x,y) coordinates (the location's ones).
;;     2.2- Get the pixel color, and then retrieve the sheet 
;;          thanks to the association defined in part 1-.
;;  The *current-sheet-signature* variable represents the last sheet found
;;  by this operation. If pointer (or everything which defines 
;;  the (x,y) coordinates) is outside the main window, 
;;  *current-sheet-signature* is set to 0.0 .
;; -------------------------------------------------------------

(defparameter *current-sheet-signature* (the (unsigned-byte 24) 0))

(defmacro increment-signature (signature)
  `(setf ,signature (the (unsigned-byte 24) (+ (the (unsigned-byte 24) 1) ,signature)))
 ;`(setf ,signature (the (unsigned-byte 24) (random #xFFF)))
   )

(defun initialize-signature-count ()
  (let ((count 0))
    (declare (type (unsigned-byte 24) count))
    (defun get-signature ()
      (increment-signature count)
      count)))

(defun find-related-sheet (port)
  (recognize-sheet port *current-sheet-signature*))

(defmacro pixel-to-color (pixel)
  `(logior (aref ,pixel 0) (ash (aref ,pixel 1) 8) (ash (aref ,pixel 2) 16)))

#|

ok, there is a very big vulnerability in this design, and that is that if your
pixel layout changes, your signatures need to reflect this, or your events
will disappear into the blue yonder...

The following is for a 565 16 bit display, which is what I happen to use.

I'll have a look at fixing this in a more portable fashion in a bit,
until then, feel free to adjust the numbers, etc - BTS

|#

(defmacro color-to-signature (color) ; 565
  `(logior (ash (logand ,color #b000000000000000011111000) -3)
           (ash (logand ,color #b000000001111110000000000) -5)
           (ash (logand ,color #b111110000000000000000000) -8)))

(defmacro signature-to-color (signature)
  `(logior (ash (logand ,signature #b0000000000011111) 3)
           (ash (logand ,signature #b0000011111100000) 5)
           (ash (logand ,signature #b1111100000000000) 8)))

;(defmacro color-to-signature (color) color)
;(defmacro signature-to-color (signature) signature)

; this does the drawing
(defun find-sheet-signature (port x y)
  (clim-ffi:with-c-data ((pixel (array unsigned-char 3)))
    (let ((sx (to-gl x))
	  (sy (to-gl y)))
      (declare (type double-float sx sy))
      (gl:glReadBuffer gl:GL_BACK)
      (gl:glMatrixMode gl:GL_PROJECTION)
      (gl:glPushMatrix)
      (gl:glLoadIdentity)
      (gl:glOrtho sx (1+ sx) (1+ sy) sy -1d0 1d0)
      (gl:glMatrixMode gl:GL_MODELVIEW)
      (gl:glViewport 0 0 1 1)
      (opengl-draw port (opengl-port-top-level port) nil)
      (gl:glReadPixels 0 0 1 1 gl:GL_RGB gl:GL_UNSIGNED_BYTE pixel)
      (gl:glViewport (aref viewport-infos 0) (aref viewport-infos 1)
		     (aref viewport-infos 2) (aref viewport-infos 3))
      (gl:glMatrixMode gl:GL_PROJECTION)
      (gl:glPopMatrix)
      (gl:glMatrixMode gl:GL_MODELVIEW)

      #+nil
      (progn			       ; debugging - to see the fields
	(opengl-draw port (opengl-port-top-level port) nil)
	(format *debug-io* "pixel = ~A~%" (color-to-signature (pixel-to-color pixel)))
	(flush port (opengl-port-top-level port))) ; to see what's happening
      (color-to-signature (pixel-to-color pixel)))))

  
;; Event

;; window-map-event added, required by the back-end
;; This event only serve for one thing. That's why
;; its definition and use is very specialized. An 
;; user MUST not use this event.

(defclass window-map-event (window-event)
  ()
  (:documentation "The goal of this event is to invoke the opengl-init function"))

;; Top-level-sheet-pane

(defmethod handle-event ((pane top-level-sheet-pane) (event window-repaint-event))
  (draw-the-entire-scene (port pane)))

(defmethod handle-event ((pane top-level-sheet-pane) (event window-map-event))
  (opengl-init (port pane)))

(defmethod handle-event :before ((pane top-level-sheet-pane) (event window-configuration-event))
  (opengl-reshape (window-configuration-event-width event) (window-configuration-event-height event)))

;; port specific function

(defmethod port-compute-native-region ((port opengl-port) (sheet sheet))
  (transform-region (sheet-native-transformation sheet)
		    (sheet-region sheet)))
    
(defmethod port-compute-native-transformation ((port opengl-port) (sheet sheet))
  (sheet-delta-transformation sheet (opengl-port-top-level port)))

; 255 represents the Maximum number with (unsigned-byte 8) representation = (1111 1111)base2
(defmacro set-color (signature)
  `(let ((color (signature-to-color ,signature)))
     (gl:glColor3ub (logand      color      #xFF)
		    (logand (ash color  -8) #xFF)
		    (logand (ash color -16) #xFF))))

; this is where we draw the sheets in their signature colours, in order to determine
; where we clicked, I'm not convinced that this is a particularly good idea, myself
(defun recompute-recognizing-drawing (sheet)
  (let* ((port (port sheet))
	 (infos (opengl-sheet-infos port sheet))
	 (native-region (sheet-native-region sheet)))
    (declare (type opengl-port port)
	     (type cons infos)
	     (type region native-region))
    (etypecase native-region
      ; point
      (standard-point
       (gl:glNewList (cdr infos) gl:GL_COMPILE)
       (set-color (car infos))
       (gl:glBegin gl:GL_POINTS)
       (gl:glVertex2d (to-gl (point-x native-region))
		      (to-gl (point-y native-region)))
       (gl:glEnd)
       (gl:glEndList))

      ; line
      (line
       (multiple-value-bind (x1 y1) (line-start-point* native-region)
	 (declare (type coordinate x1 y1))
	 (multiple-value-bind (x2 y2) (line-end-point* native-region)
	   (declare (type coordinate x2 y2))
	   (gl:glNewList (cdr infos) gl:GL_COMPILE)
	   (set-color (car infos))
	   (gl:glBegin gl:GL_LINES)
	   (gl:glVertex2d (to-gl x1) (to-gl y1))
	   (gl:glVertex2d (to-gl x2) (to-gl y2))
	   (gl:glEnd)
	   (gl:glEndList))))

      ; polyline
      (standard-polyline
       (gl:glNewList (cdr infos) gl:GL_COMPILE)
       (set-color (car infos))
       (if (polyline-closed native-region)
	   (gl:glBegin gl:GL_LINE_LOOP)
	   (gl:glBegin gl:GL_LINE_STRIP))
       (map-over-polygon-coordinates (lambda (p)
				       (gl:glVertex2d (to-gl (point-x p))
						      (to-gl (point-y p))))
				     native-region)
       (gl:glEnd)
       (gl:glEndList))

      ; polygon
      #+nil
      (standard-polyline ; see above duplication - one is wrong - FIXME - BTS
       (gl:glNewList (cdr infos) gl:GL_COMPILE)
       (set-color (car infos))
       (gl:glBegin gl:GL_POLYGON)
       (map-over-polygon-coordinates #'gl:glVertex2d native-region)
       (gl:glEnd)
       (gl:glEndList))

      ; rectangle
      (standard-rectangle
       (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* native-region)
	 (declare (type coordinate x1 y1 x2 y2))
	 (gl:glNewList (cdr infos) gl:GL_COMPILE)
	 (set-color (car infos))
	 (gl:glRectd (to-gl x1) (to-gl y1) (to-gl x2) (to-gl y2))
	 (gl:glEndList)))
      
      ; ellipse and elliptical-arc
      ((or standard-ellipse standard-elliptical-arc)
       (let ((start-angle (ellipse-start-angle native-region))
	     (end-angle (ellipse-end-angle native-region))
	     (ellipse-transformation (slot-value native-region 'tr)))
	 (declare (type double-float start-angle end-angle))
	 (multiple-value-bind (center-x center-y) (ellipse-center-point* native-region)
	   (declare (type coordinate center-x center-y))
	   (gl:glNewList (cdr infos) gl:GL_COMPILE)
	   (set-color (car infos))
	   (if (typep native-region 'standard-ellipse)
	       (gl:glBegin gl:GL_POLYGON)
	       (gl:glBegin gl:GL_LINE_STRIP))
	   (gl:glPushMatrix)
	   (gl:glLoadIdentity)
	   (gl:glTranslated center-x center-y 0d0)
	   (loop with dtheta of-type double-float = (/ pi 100) ; half-ellipse is cut in 100 slices
		 for theta of-type double-float from start-angle to end-angle by dtheta
		 do (multiple-value-bind (x y) (transform-position ellipse-transformation (cos theta) (sin theta))
		      (declare (type double-float x y))
		      (gl:glVertex2d x y)))  
	   (gl:glPopMatrix)
	   (gl:glEnd)
	   (gl:glEndList))))
      (nowhere-region
       ; what should I do here? - BTS
       nil)
      ; default : region intersection/union/difference are not handled. [yet?]
      ; error
      (format *debug-io* "Tried to draw an unknown type of shape ~A~%" (type-of native-region)))))

(defmethod note-sheet-region-changed :after ((sheet clim-repainting-mixin))
  (recompute-recognizing-drawing sheet))
  
(defmethod note-sheet-transformation-changed :after ((sheet clim-repainting-mixin))
  (recompute-recognizing-drawing sheet))

(defmethod port-copy-area ((port opengl-port) sheet from-x from-y width height to-x to-y)
  (declare (type sheet sheet)
	   (type coordinate from-x from-y to-x to-y)
	   (type real width height))
  (let ((native-transformation (sheet-native-transformation sheet)))
    (multiple-value-bind (x y) (bounding-rectangle* (sheet-region sheet))
      (declare (type coordinate x y))
      (multiple-value-bind (from-tx from-ty) (transform-position native-transformation (- from-x x) (- from-y y))
	(declare (type coordinate from-tx from-ty))
	(multiple-value-bind (to-tx to-ty) (transform-position native-transformation (- to-x x) (- to-y y))
	  (declare (type coordinate to-tx to-ty))
	  (multiple-value-bind (twidth theight) (transform-position native-transformation width height)
	    (declare (type coordinate twidth theight))
	    (gl:glRasterPos2D (to-gl to-tx) (to-gl to-ty))
	    (gl:glCopyPixels (round from-tx) (round from-ty) (round twidth) (round theight) gl:GL_COLOR)))))))

;; Repaint protocol
(defmacro with-special-choices ((sheet) &body body)
  `(progn
     ,@body))


(defmethod realize-mirror ((port opengl-port) (sheet sheet))
  nil)

(defmethod unrealize-mirror ((port opengl-port) (sheet sheet))
  nil)


(defmethod note-sheet-grafted :after ((sheet clim-repainting-mixin))
  (port-register-signature (port sheet) sheet))

(defmethod note-sheet-degrafted :after ((sheet clim-repainting-mixin))
  (port-unregister-signature (port sheet) sheet))

(defmethod window-clear ((sheet mirrored-sheet-mixin))
  nil)

(defmethod sheet-native-transformation :around ((sheet sheet))
  (sheet-delta-transformation sheet (opengl-port-top-level (port sheet))))

(defmethod sheet-native-transformation :around ((sheet top-level-sheet-pane))
  +identity-transformation+)

(defmethod sheet-native-region :around ((sheet sheet))
  (transform-region (sheet-native-transformation sheet) (sheet-region sheet)))

(defmethod sheet-native-region :around ((sheet top-level-sheet-pane))
  (sheet-region sheet))
