;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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
	  (gc (medium-gcontext ,medium ink)))
     line-style ink
     (unwind-protect
	 (progn ,@body)
       #+ignore(xlib:free-gcontext gc))))

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-CLX-graphics (medium)
    (multiple-value-bind (tx ty)
	(transform-position (compose-transformations (sheet-transformation
						      (medium-sheet medium))
						     (medium-transformation medium))
			    x y)
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
    (multiple-value-bind (tx1 ty1)
	(transform-position (compose-transformations (sheet-transformation
						      (medium-sheet medium))
						     (medium-transformation medium))
			    x1 y1)
      (multiple-value-bind (tx2 ty2)
	  (transform-position (compose-transformations (sheet-transformation
							(medium-sheet medium))
						       (medium-transformation medium))
			      x2 y2)
	(xlib:draw-line mirror gc (round tx1) (round ty1) (round tx2) (round ty2))))))

(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (with-CLX-graphics (medium)
    (loop with points = (apply #'vector coord-seq)
	  for i below (length coord-seq) by 2
	  do (multiple-value-bind (tx ty)
		 (transform-position (compose-transformations (sheet-transformation
							       (medium-sheet medium))
							      (medium-transformation medium))
				     (aref points i) (aref points (1+ i)))
	       (setf (aref points i) (round tx)
		     (aref points (1+ i)) (round ty)))
	  finally (xlib:draw-lines mirror gc points))))

(defmethod medium-draw-rectangle* ((medium clx-medium) left top right bottom filled)
  (with-CLX-graphics (medium)
    (multiple-value-bind (x1 y1)
	(transform-position (compose-transformations (sheet-transformation
						      (medium-sheet medium))
						     (medium-transformation medium))
			    left top)
      (multiple-value-bind (x2 y2)
	  (transform-position (compose-transformations (sheet-transformation
							(medium-sheet medium))
						       (medium-transformation medium))
			      right bottom)
	(if (< x2 x1)
	    (rotatef x1 x2))
	(if (< y2 y1)
	    (rotatef y1 y2))
	(xlib:draw-rectangle mirror gc
			     (round x1) (round y1)
			     (round (- x2 x1)) (round (- y2 y1))
			     filled)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for text styles

(defmethod text-size ((medium clx-medium) string &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (if (= start end)
      (values 0 0 0 0 0)
      (let ((gctxt (medium-gcontext medium (medium-ink medium)))
            (position-newline (position #\newline string :start start)))
        (if position-newline
            (multiple-value-bind (width ascent descent left right
                                        font-ascent direction first-not-done)
                (xlib:text-extents gctxt string :start start :end position-newline)
              (declare
               (ignorable left right font-ascent direction first-not-done))
              (multiple-value-bind (w h x y baseline)
                  (text-size medium string :text-style text-style
                             :start (1+ position-newline) :end end)
                (values (max w width) (+ ascent descent h)
                        x (+ ascent descent y) (+ ascent descent baseline))))
            (multiple-value-bind (width ascent descent left right
                                        font-ascent direction first-not-done)
                (xlib:text-extents gctxt string :start start :end position-newline)
              (declare
               (ignorable left right font-ascent direction first-not-done))
              (values width (+ ascent descent) width 0 ascent))))))

(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-CLX-graphics (medium)
    (when (characterp string)
      (setq string (make-string 1 :initial-element string)))
    (when (null end) (setq end (length string)))
    (multiple-value-bind (tx ty)
	(transform-position (compose-transformations (sheet-transformation
						     (medium-sheet medium))
						    (medium-transformation medium))
			    x y)
      (multiple-value-bind (text-width text-height x y baseline) 
	   (text-size medium string :start start :end end)
	(declare (ignore x y))
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
      (xlib:draw-glyphs mirror gc (round tx) (round ty) string :start start :end end))))

(defmethod medium-buffering-output-p ((medium clx-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium clx-medium))
  buffer-p)
