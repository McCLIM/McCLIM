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

(defclass opengl-medium (basic-medium) ())

;;; The medium transformation is now handled by the
;;; transform-coordinates-mixin in the McCLIM front end. For OpenGL it
;;; might be better to make it part of the model matrix. --moore
#|
(defun medium-transform-position (medium x y)
  (declare (type real x y)
	   (type opengl-medium medium))
  (multiple-value-bind (xr yr) (bounding-rectangle* 
				(sheet-region (medium-sheet medium)))
    (declare (type coordinate xr yr))
    (transform-position (medium-device-transformation medium)
			(- x xr) (- y yr))))

(defun medium-transform-distance (medium dx dy)
  (declare (type real dx dy))
  (transform-distance (medium-device-transformation medium)
                      dx dy))
|#

(defmacro with-OpenGL-graphics ((medium) &body body)
  `(let ((ink (medium-ink ,medium))
	 (line-style (medium-line-style ,medium))
	 ;(clipping-region (medium-clipping-region ,medium))
	 (sheet (medium-sheet ,medium)))
     (cond ((eq ink +foreground-ink+) (setf ink (medium-foreground ,medium)))
	   ((eq ink +background-ink+) (setf ink (medium-background ,medium)))
	   (t nil))
     (with-context (sheet)
       (multiple-value-bind (red green blue) (color-rgb ink)
	 (declare (type single-float red green blue))
	 (gl:glColor3f red green blue))
       (gl:glLineWidth (coerce (line-style-thickness line-style) 'single-float))
       ;(unless (eq clipping-region +everywhere+) 
       ; nil 
       (gl:glLoadIdentity)
       (progn ,@body))))

(defmacro activate-dash ()
  (let ((dashes (gensym)))
    `(let ((,dashes (line-style-dashes line-style)))
       (when ,dashes
	 (if (eq ,dashes t)
	     (gl:glLineStipple 1 3)
	     (gl:glLineStipple 1 3))) ; case of sequence must be implemented
       (gl:glEnable gl:GL_LINE_STIPPLE))))

(defmacro desactivate-dash ()
  `(gl:glDisable gl:GL_LINE_STIPPLE))

;; Points

(defmacro with-points ((medium) &body body)
  `(with-OpenGL-graphics (,medium)
     (gl:glPointSize (coerce (line-style-thickness line-style) 'single-float))
     (gl:glBegin gl:GL_POINTS)
     (progn ,@body)
     (gl:glEnd)))

(defmethod medium-draw-point* ((medium opengl-medium) x y)
  (declare (type coordinate x y))
  (multiple-value-bind (tx ty) (medium-transform-position medium x y)
    (declare (type coordinate tx ty))
    (with-points (medium)
      (gl:glVertex2d tx ty))))

(defmethod medium-draw-points* ((medium opengl-medium) coord-seq)
  (declare (type cons coord-seq))
  (assert (evenp (length coord-seq)))
  (with-points (medium)
    (loop for (x y) on coord-seq by #'cddr
	  do (multiple-value-bind (tx ty) (medium-transform-position medium x y)
	       (declare (type coordinate x y))
	       (gl:glVertex2d tx ty)))))


;; Lines

(defmacro with-lines ((medium) &body body)
  `(with-OpenGL-graphics (,medium)
     (activate-dash)
     (gl:glBegin gl:GL_LINES)
     (unwind-protect
	 (progn ,@body))
     (gl:glEnd)
     (desactivate-dash)))

(defmethod medium-draw-line* ((medium opengl-medium) x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (multiple-value-bind (tx1 ty1) (medium-transform-position medium x1 y1)
    (declare (type coordinate tx1 ty1))
    (multiple-value-bind (tx2 ty2) (medium-transform-position medium x2 y2)
      (declare (type coordinate tx2 ty2))
      (with-lines (medium)
	(gl:glVertex2d tx1 ty1)
	(gl:glVertex2d tx2 ty2)))))

(defmethod medium-draw-lines* ((medium opengl-medium) coord-seq)
  (declare (type cons coord-seq))
  (assert (evenp (length coord-seq)))
  (with-lines (medium)
    (loop for (x y) on coord-seq by #'cddr
	  do (multiple-value-bind (tx ty) (medium-transform-position medium x y)
	       (declare (type coordinate tx ty))
	       (gl:glVertex2d tx ty)))))

;; Polygon and Polyline

(defmethod medium-draw-polygon* ((medium opengl-medium) coord-seq closed filled)
  (declare (type boolean filled closed)
	   (type cons coord-seq))
  (assert (evenp (length coord-seq)))
  (with-OpenGL-graphics (medium)
    (if filled
	(progn
	  (gl:glPolygonMode gl:GL_FRONT gl:GL_FILL)
	  (gl:glBegin gl:GL_POLYGON))
	(progn
	  (activate-dash)
	  (if closed
	      (progn
		(gl:glPolygonMode gl:GL_FRONT gl:GL_LINE)
		(gl:glBegin gl:GL_POLYGON))
	      (gl:glBegin gl:GL_LINE_STRIP))))
    (loop for (x y) on coord-seq by #'cddr
	  do (multiple-value-bind (tx ty) (medium-transform-position medium x y)
	       (declare (type coordinate tx ty))
	       (gl:glVertex2d tx ty)))
    (gl:glEnd)
    (desactivate-dash)))
	  

;; Rectangle

(defmethod medium-draw-rectangle* ((medium opengl-medium) x1 y1 x2 y2 filled)
  (declare (type real x1 y1 x2 y2)
	   (type boolean filled))
  (with-OpenGL-graphics (medium)
    (multiple-value-bind (tx1 ty1) (medium-transform-position medium x1 y1)
      (declare (type coordinate tx1 ty1))
      (multiple-value-bind (tx2 ty2) (medium-transform-position medium x2 y2)
	(declare (type coordinate tx2 ty2))
	(if filled
	    (if (rectilinear-transformation-p (medium-transformation medium))
		(progn
		  (when (< tx2 tx1)
		    (rotatef tx1 tx2))
		  (when (< ty2 ty1)
		    (rotatef ty1 ty2))
		  (gl:glRectd tx1 ty1 tx2 ty2))
		(progn
		  (gl:glBegin gl:GL_QUADS)
		  (gl:glVertex2d tx1 ty1)
		  (gl:glVertex2d tx2 ty1)
		  (gl:glVertex2d tx2 ty2)
		  (gl:glVertex2d tx1 ty2)
		  (gl:glEnd)))
	    (progn
	      (activate-dash)
	      (gl:glBegin gl:GL_LINE_LOOP)
	      (gl:glVertex2d tx1 ty1)
	      (gl:glVertex2d tx2 ty1)
	      (gl:glVertex2d tx2 ty2)
	      (gl:glVertex2d tx1 ty2)
	      (gl:glEnd)
	      (desactivate-dash)))))))


;; Ellipse and Elliptical Arc

; In the ellipse, we find the transformation, tr, which transforms the
; unit circle into the ellipse.
; To draw an ellipse (or part of an ellipse), arc between start-angle
; and end-angle is cut in 100 slices. Then a polygon is drawn with all
; points representing the slices.
(defmethod medium-draw-ellipse* ((medium opengl-medium) center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (declare (type real center-x center-y
		 radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
	   (type real start-angle end-angle)
	   (type boolean filled))
  (with-OpenGL-graphics (medium)
    (multiple-value-bind (tx ty)
	(medium-transform-position medium center-x center-y)
      (declare (type coordinate tx ty))
      (multiple-value-bind (dx1 dy1)
          (medium-transform-distance medium radius-1-dx radius-1-dy)
	(declare (type coordinate dx1 dy1))
        (multiple-value-bind (dx2 dy2)
            (medium-transform-distance medium radius-2-dx radius-2-dy)
	  (declare (type coordinate dx2 dy2))
	  (when (= (* dx1 dy2) (* dy1 dx2)) ; vectors radius-1 and radius-2 are colinear
	    (error 'ellipse-not-well-defined))
	  (gl:glTranslated tx ty 0d0)
	  (let* ((ell (make-ellipse* 0d0 0d0 dx1 dy1 dx2 dy2
				     :start-angle start-angle
				     :end-angle end-angle))
		 (tr (slot-value ell 'tr))
		 (2pi (* 2 pi))
		 (d-theta (- end-angle start-angle)))
	    (if filled
		(progn 
		  (gl:glPolygonMode gl:GL_FRONT gl:GL_FILL)
		  (gl:glBegin gl:GL_POLYGON)
		  (when (/= (mod d-theta 2pi) 0) ; case of the pie slice
		    (gl:glVertex2d 0d0 0d0)))
		(progn
		  (activate-dash)
		  (gl:glBegin gl:GL_LINE_STRIP)))
	    (loop with step-angle of-type real = (/ (- end-angle start-angle) 100)
		  for theta from start-angle to end-angle by step-angle
		  do (multiple-value-bind (x y) (transform-position tr (cos theta) (sin theta))
		       (declare (type coordinate x y))
		       (gl:glVertex2d x y)))
	    (gl:glEnd)
	    (desactivate-dash)))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for text styles

(defmethod text-size ((medium opengl-medium) string &key text-style (start 0) end)
  (declare (type fixnum start))
  (let ((port (port medium)))
    (when (characterp string)
      (setf string (make-string 1 :initial-element string)))
    (unless end (setf end (length string)))
    (unless text-style (setf text-style (medium-text-style medium)))
    (if (= start end)
	(values 0 0 0 0 0)
	(let ((position-newline (position #\newline string :start start)))
	  (if position-newline
	    ; newline inside the string
	      (multiple-value-bind (width ascent descent left right
				    font-ascent direction first-not-done)
		  (port-text-extents port text-style string :start start :end position-newline)
		(declare (type real width ascent descent)
			 (ignorable left right font-ascent direction first-not-done))
		(multiple-value-bind (w h x y baseline)
		    (text-size medium string :text-style text-style
			       :start (1+ position-newline) :end end)
		  (values (max w width) (+ ascent descent h)
			  x (+ ascent descent y) (+ ascent descent baseline))))
	    ; no newline inside the string
	      (multiple-value-bind (width ascent descent left right
					  font-ascent direction first-not-done)
		  (port-text-extents port text-style string
				     :start start :end end)
		(declare (type real width ascent descent)
			 (ignorable left right font-ascent direction first-not-done))
		(values width (+ ascent descent) width 0 ascent)))))))

(defmethod medium-draw-text* ((medium opengl-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs)
	   (type real x y)
	   (type fixnum start)
	   (type symbol align-x align-y))
  (multiple-value-bind (text-style-list text-style-start)
      (text-style-list-and-start (port medium) (medium-text-style medium))
    (declare (type fixnum text-style-list))
    (with-OpenGL-graphics (medium)
      (when (characterp string)
	(setq string (make-string 1 :initial-element string)))
      (unless end (setq end (length string)))
      (multiple-value-bind (tx ty)
	  (medium-transform-position medium x y)
	(declare (type coordinate tx ty))
	(multiple-value-bind (text-width text-height x y baseline) 
	    (text-size medium string :start start :end end)
	  (declare (type real text-width text-height baseline)
		   (ignore x y))
	  (unless (and (eq align-x :left) (eq align-y :baseline))	    
	    (setq tx (- tx (ecase align-x
			     (:left 0)
			     (:center (round text-width 2))
			     (:right text-width))))
	    (setq ty (ecase align-y
		       (:top (+ ty baseline))
		       (:center (+ ty baseline (- (floor text-height 2))))
		       (:baseline ty)
		       (:bottom (+ ty baseline (- text-height)))))))
	(let* ((s (subseq string start end))
	       (tab (map '(simple-array (unsigned-byte 8) (*))
			 #'(lambda (char)
			     (declare (type standard-char char))
			     (- (char-code char) text-style-start))
			 s)))
	  (declare (type string s))
	  (gl:glRasterPos2d tx ty)
	  (gl:glListBase text-style-list)
	  (gl:glCallLists (length s) gl:GL_UNSIGNED_BYTE (find-array-address tab)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for text styles

(defmethod text-style-ascent (text-style (medium opengl-medium))
; (let ((font (text-style-to-X-font (port medium) text-style)))
;   (xlib-gl:font-ascent font))
; ^--- FIXME - BTS
  10)

(defmethod text-style-descent (text-style (medium opengl-medium))
; (let ((font (text-style-to-X-font (port medium) text-style)))
;   (xlib-gl:font-descent font))
; ^-- FIXME - BTS
  4)

(defmethod text-style-height (text-style (medium opengl-medium))
; (let ((font (text-style-to-X-font (port medium) text-style)))
;   (+ (xlib-gl:font-ascent font) (xlib-gl:font-descent font)))
; ^-- FIXME - BTS
  14)

(defmethod text-style-character-width (text-style (medium opengl-medium) char)
; (xlib-gl:char-width (text-style-to-X-font (port medium) text-style) (char-code char))
; ^-- FIXME - BTS
  8)

(defmethod text-style-width (text-style (medium opengl-medium))
  (text-style-character-width text-style medium #\m))

