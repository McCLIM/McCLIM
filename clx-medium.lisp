;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

;;; CLX-MEDIUM class

(defclass clx-medium (medium)
  ((gc :initform nil)
   )
  )

(defgeneric medium-gcontext (medium ink))

(defmethod medium-gcontext ((medium clx-medium) (ink color))
  (let* ((port (port medium))
	 (mirror (port-lookup-mirror port (medium-sheet medium)))
	 (line-style (medium-line-style medium)))
    (with-slots (gc) medium
      (if (null gc)
	  (setq gc (xlib:create-gcontext :drawable mirror)))
      (setf (xlib:gcontext-font gc) (text-style-to-X-font port (medium-text-style medium))
	    (xlib:gcontext-foreground gc) (X-pixel port ink)
	    (xlib:gcontext-background gc) (X-pixel port (medium-background medium))
	    (xlib:gcontext-line-width gc) (line-style-thickness line-style))
      gc)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +foreground-ink+)))
  (medium-gcontext medium (medium-foreground medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +background-ink+)))
  (medium-gcontext medium (medium-background medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +flipping-ink+)))
  (let ((gc (medium-gcontext medium (medium-background medium))))
    (setf (xlib:gcontext-background gc) (X-pixel (port medium) (medium-foreground medium)))
    gc))

(defmacro with-CLX-graphics ((medium) &body body)
  `(let* ((port (port ,medium))
	  (mirror (port-lookup-mirror port (medium-sheet ,medium)))
	  (line-style (medium-line-style ,medium))
	  (ink (medium-ink ,medium))
	  (gc (medium-gcontext ,medium (medium-ink ,medium))))
     line-style ink
     (unwind-protect
	 (progn ,@body)
       #+ignore(xlib:free-gcontext gc))))

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-CLX-graphics (medium)
    (multiple-value-bind (tx ty) (transform-position (medium-transformation medium) x y)
      (if (< (line-style-thickness line-style) 2)
	  (xlib:draw-point mirror gc (round tx) (round ty))
	(let ((diameter (round (line-style-thickness line-style))))
	  (xlib:draw-arc mirror gc
			 (round tx) (round ty)
			 diameter diameter
			 0 (* 2 pi)
			 t))))))

(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (loop for (x y) on coord-seq by #'cddr
	do (medium-draw-point* medium x y)))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (with-CLX-graphics (medium)
    (multiple-value-bind (tx1 ty1) (transform-position (medium-transformation medium) x1 y1)
      (multiple-value-bind (tx2 ty2) (transform-position (medium-transformation medium) x2 y2)
	(xlib:draw-line mirror gc (round tx1) (round ty1) (round tx2) (round ty2))))))

(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (with-CLX-graphics (medium)
    (loop with points = (apply #'vector coord-seq)
	  for i below (length coord-seq) by 2
	  do (multiple-value-bind (tx ty)
		 (transform-position (medium-transformation medium)
				     (aref points i) (aref points (1+ i)))
	       (setf (aref points i) (round tx)
		     (aref points (1+ i)) (round ty)))
	  finally (xlib:draw-lines mirror gc points))))

(defmethod medium-draw-rectangle* ((medium clx-medium) left top right bottom filled)
  (with-CLX-graphics (medium)
    (multiple-value-bind (x1 y1)
	(transform-position (medium-transformation medium) left top)
      (multiple-value-bind (x2 y2)
	  (transform-position (medium-transformation medium) right bottom)
	(if (< x2 x1)
	    (rotatef x1 x2))
	(if (< y2 y1)
	    (rotatef y1 y2))
	(xlib:draw-rectangle mirror gc
			     (round x1) (round y1)
			     (round (- x2 x1)) (round (- y2 y1))
			     filled)))))

(defmethod medium-draw-text* ((medium clx-medium) string x y
			      start end
			      align-x align-y
			      toward-x toward-y transform-glyphs)
  (declare (ignore align-x align-y toward-x toward-y transform-glyphs))
  (with-CLX-graphics (medium)
    (if (characterp string)
	(setq string (make-string 1 :initial-element string)))
    (if (null end)
	(setq end (length string)))
    (multiple-value-bind (tx ty) (transform-position (medium-transformation medium) x y)
      (xlib:draw-glyphs mirror gc (round tx) (round ty) string :start start :end end))))

(defmethod medium-buffering-output-p ((medium clx-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium clx-medium))
  buffer-p)
