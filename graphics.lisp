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

(defmethod do-graphics-with-options ((sheet sheet) func &rest options)
  (with-sheet-medium (medium sheet)
    (apply #'do-graphics-with-options-internal medium sheet func options)))

(defmethod do-graphics-with-options ((medium medium) func &rest options)
  (apply #'do-graphics-with-options-internal medium medium func options))

(defmethod do-graphics-with-options ((sheet t) func &rest options)
  (declare (ignore options))
  (funcall func sheet))

(defmethod do-graphics-with-options-internal ((medium medium) orig-medium func
				     &rest args
				     &key ink clipping-region transformation
					  line-style line-unit line-thickness
					  (line-dashes nil dashes-p)
					  line-joint-shape line-cap-shape
					  (text-style nil text-style-p)
					  (text-family nil text-family-p)
					  (text-face nil text-face-p)
					  (text-size nil text-size-p)
				     &allow-other-keys)
  (declare (ignore args))
  (let ((old-ink (medium-ink medium))
	(old-clip (medium-clipping-region medium))
	(old-transform (medium-transformation medium))
	(old-line-style (medium-line-style medium))
	(old-text-style (medium-text-style medium)))
    (unwind-protect
	(progn
	  (if ink
	      (setf (medium-ink medium) ink))
	  (if transformation
	      (setf (medium-transformation medium)
		(compose-transformations old-transform transformation)))
	  (if clipping-region
	      (setf (medium-clipping-region medium)
		(region-intersection old-clip
				     (transform-region clipping-region
						       (medium-transformation medium)))))
	  (if (null line-style)
	      (setf line-style old-line-style))
	  (if (or line-unit line-thickness dashes-p line-joint-shape line-cap-shape)
	      (setf line-style (make-line-style
				:unit (or line-unit
					  (line-style-unit line-style))
				:thickness (or line-thickness
					       (line-style-thickness line-style))
				:dashes (or line-dashes
					    (line-style-dashes line-style))
				:joint-shape (or line-joint-shape
						 (line-style-joint-shape line-style))
				:cap-shape (or line-cap-shape
					       (line-style-cap-shape line-style)))))
	  (setf (medium-line-style medium) line-style)
	  (if text-style-p
	      (setf text-style (merge-text-styles text-style
						  (medium-merged-text-style medium)))
	    (setf text-style (medium-merged-text-style medium)))
	  (if (or text-family-p text-face-p text-size-p)
	      (setf text-style (merge-text-styles (make-text-style text-family
								   text-face
								   text-size)
						  text-style)))
	  (setf (medium-text-style medium) text-style)
	  (funcall func orig-medium))
      (setf (medium-ink medium) old-ink)
      (setf (medium-clipping-region medium) old-clip)
      (setf (medium-transformation medium) old-transform)
      (setf (medium-line-style medium) old-line-style)
      (setf (medium-text-style medium) old-text-style))))

(defmacro with-medium-options ((sheet args)
			       &body body)
  `(flet ((graphics-op (medium)
	    ,@body))
     (declare (dynamic-extent #'graphics-op))
     (apply #'do-graphics-with-options ,sheet #'graphics-op ,args)))

(defun draw-point (sheet point
		   &rest args
		   &key ink clipping-region transformation
			line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (multiple-value-bind (x y) (point-position point)
      (medium-draw-point* medium x y))))

(defun draw-point* (sheet x y
		    &rest args
		    &key ink clipping-region transformation
			 line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-point* medium x y)))

(defun draw-points (sheet point-seq
		    &rest args
		    &key ink clipping-region transformation
			 line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (loop for point in point-seq
	  nconcing (multiple-value-bind (x y) (point-position point)
		     (list x y)) into coord-seq
	  finally (medium-draw-points* medium coord-seq))))

(defun draw-points* (sheet coord-seq
		     &rest args
		     &key ink clipping-region transformation
			  line-style line-thickness line-unit)
  (declare (ignore ink clipping-region transformation
		   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-points* medium coord-seq)))

(defun draw-line (sheet point1 point2
		  &rest args
		  &key ink clipping-region transformation line-style line-thickness
		       line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (x1 y1) (point-position point1)
      (multiple-value-bind (x2 y2) (point-position point2)
	(medium-draw-line* medium x1 y1 x2 y2)))))

(defun draw-line* (sheet x1 y1 x2 y2
		   &rest args
		   &key ink clipping-region transformation line-style line-thickness
			line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-line* medium x1 y1 x2 y2)))

(defun draw-lines (sheet point-seq
		   &rest args
		   &key ink clipping-region transformation line-style line-thickness
			line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (loop for point in point-seq
	  nconcing (multiple-value-bind (x y) (point-position point)
		     (list x y)) into coord-seq
	  finally (medium-draw-lines* medium coord-seq))))

(defun draw-lines* (sheet coord-seq
		    &rest args
		    &key ink clipping-region transformation line-style line-thickness
			 line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-lines* medium coord-seq)))

(defun draw-polygon (sheet point-seq
		     &rest args
		     &key (filled t) (closed t)
			  ink clipping-region transformation line-style line-thickness
			  line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (loop for point in point-seq
	  nconcing (multiple-value-bind (x y) (point-position point)
		     (list x y)) into coord-seq
	  finally (medium-draw-polygon* medium coord-seq closed filled))))

(defun draw-polygon* (sheet coord-seq
		    &rest args
		    &key (filled t) (closed t)
			 ink clipping-region transformation line-style line-thickness
			 line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-polygon* medium coord-seq closed filled)))

(defun draw-rectangle (sheet point1 point2
			&rest args
			&key (filled t)
			     ink clipping-region transformation line-style line-thickness
			     line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (x1 y1) (point-position point1)
      (multiple-value-bind (x2 y2) (point-position point2)
	(medium-draw-rectangle* medium x1 y1 x2 y2 filled)))))

(defun draw-rectangle* (sheet x1 y1 x2 y2
			&rest args
			&key (filled t)
			     ink clipping-region transformation line-style line-thickness
			     line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))

(defun draw-ellipse (sheet
		     center-point
		     radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		     &rest args
		     &key (filled t) (start-angle 0) (end-angle (* 2 pi))
			  ink clipping-region transformation
			  line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (center-x center-y) (point-position center-point)
      (medium-draw-ellipse* medium
			    center-x center-y
			    radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			    start-angle end-angle filled))))

(defun draw-ellipse* (sheet
		      center-x center-y
		      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		      &rest args
		      &key (filled t) (start-angle 0) (end-angle (* 2 pi))
			   ink clipping-region transformation
			   line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-ellipse* medium
			  center-x center-y
			  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			  start-angle end-angle filled)))

(defun draw-circle (sheet
		    center-point radius
		    &rest args
		    &key (filled t) (start-angle 0) (end-angle (* 2 pi))
			 ink clipping-region transformation
			 line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (center-x center-y) (point-position center-point)
      (medium-draw-ellipse* medium
			    center-x center-y
			    radius 0 0 radius
			    start-angle end-angle filled))))

(defun draw-circle* (sheet
		     center-x center-y radius
		     &rest args
		     &key (filled t) (start-angle 0) (end-angle (* 2 pi))
			  ink clipping-region transformation
			  line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-ellipse* medium
			  center-x center-y
			  radius 0 0 radius
			  start-angle end-angle filled)))

(defun draw-text (sheet string point
		   &rest args
		   &key (start 0) (end nil)
			(align-x :left) (align-y :baseline)
			towards-x towards-y transform-glyphs
			ink clipping-region transformation
			text-style text-family text-face text-size)
  (declare (ignore ink clipping-region transformation
		   text-style text-family text-face text-size))
  (with-medium-options (sheet args)
    (multiple-value-bind (x y) (point-position point)
      (medium-draw-text* medium string x y
			 start end
			 align-x align-y
			 towards-x towards-y transform-glyphs))))

(defun draw-text* (sheet string x y
		   &rest args
		   &key (start 0) (end nil)
			(align-x :left) (align-y :baseline)
			towards-x towards-y transform-glyphs
			ink clipping-region transformation
			text-style text-family text-face text-size)
  (declare (ignore ink clipping-region transformation
		   text-style text-family text-face text-size))
  (with-medium-options (sheet args)
    (medium-draw-text* medium string x y
		       start end
		       align-x align-y
		       towards-x towards-y transform-glyphs)))

(defun draw-arrow (sheet point-1 point-2
		   &rest args
		   &key ink clipping-region transformation
			line-style line-thickness
			line-unit line-dashes line-cap-shapeto-head
			from-head head-length head-width)
  (declare (ignore sheet point-1 point-2 args
		   ink clipping-region transformation
		   line-style line-thickness
		   line-unit line-dashes line-cap-shapeto-head
		   from-head head-length head-width))
  (error "DRAW-ARROW is not implemented"))

(defun draw-arrow* (sheet x1 y1 x2 y2
		   &rest args
		   &key ink clipping-region transformation
			line-style line-thickness
			line-unit line-dashes line-cap-shapeto-head
			from-head head-length head-width)
  (declare (ignore sheet x1 y1 x2 y2 args
		   ink clipping-region transformation
		   line-style line-thickness
		   line-unit line-dashes line-cap-shapeto-head
		   from-head head-length head-width))
  (error "DRAW-ARROW* is not implemented"))

(defun draw-oval (sheet center-pt x-radius y-radius
		  &rest args
		  &key (filled t) ink clipping-region transformation
		       line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore sheet center-pt x-radius y-radius args
		   filled ink clipping-region transformation
		   line-style line-thickness line-unit line-dashes line-cap-shape))
  (error "DRAW-OVAL is not implemented"))

(defun draw-oval* (sheet center-x center-y x-radius y-radius
		  &rest args
		  &key (filled t) ink clipping-region transformation
		       line-style line-thickness line-unit line-dashes line-cap-shape)
  (declare (ignore sheet center-x center-y x-radius y-radius args
		   filled ink clipping-region transformation
		   line-style line-thickness line-unit line-dashes line-cap-shape))
  (error "DRAW-OVAL* is not implemented"))


;;; Pixmap functions

(defclass pixmap ()
  ((sheet :initarg :sheet
	  :reader pixmap-sheet)
   (width :initarg :width
	  :reader pixmap-width)
   (height :initarg :height
	   :reader pixmap-height)
   ))

(defmethod allocate-pixmap ((sheet sheet) width height)
  (port-allocate-pixmap (port sheet) sheet width height))

(defmethod allocate-pixmap ((sheet medium) width height)
  (port-allocate-pixmap (port sheet) sheet width height))

(defmethod allocate-pixmap ((sheet stream) width height)
  (port-allocate-pixmap (port sheet) sheet width height))

(defmethod deallocate-pixmap ((pixmap pixmap))
  (port-deallocate-pixmap (port (pixmap-sheet pixmap)) pixmap))

(defmethod copy-to-pixmap ((sheet sheet) sheet-x sheet-y width height
			   &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (if (null pixmap)
      (setq pixmap (allocate-pixmap sheet (+ pixmap-x width) (+ pixmap-y height))))
  (port-copy-to-pixmap (port sheet) sheet sheet-x sheet-y width height
		       pixmap pixmap-x pixmap-y)
  pixmap)

(defmethod copy-to-pixmap ((sheet medium) sheet-x sheet-y width height
			   &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (if (null pixmap)
      (setq pixmap (allocate-pixmap sheet (+ pixmap-x width) (+ pixmap-y height))))
  (port-copy-to-pixmap (port sheet) sheet sheet-x sheet-y width height
		       pixmap pixmap-x pixmap-y)
  pixmap)

(defmethod copy-to-pixmap ((sheet stream) sheet-x sheet-y width height
			   &optional pixmap (pixmap-x 0) (pixmap-y 0))
  (if (null pixmap)
      (setq pixmap (allocate-pixmap sheet (+ pixmap-x width) (+ pixmap-y height))))
  (port-copy-to-pixmap (port sheet) sheet sheet-x sheet-y width height
		       pixmap pixmap-x pixmap-y)
  pixmap)

(defmethod copy-area ((sheet sheet) from-x from-y width height to-x to-y)
  (port-copy-area (port sheet) sheet from-x from-y width height to-x to-y))

(defmacro with-output-to-pixmap ((medium-var sheet &key width height) &body body)
  `(let* ((pixmap (allocate-pixmap ,sheet ,width ,height))
	  (,medium-var (make-medium (port sheet) pixmap))
	  (old-medium (sheet-medium sheet)))
     (setf (sheet-medium sheet) ,medium-var)
     (unwind-protect
	 (progn ,@body)
       (setf (sheet-medium sheet) old-medium))
     pixmap))


;;; Generic graphic operation methods

(defmacro def-graphic-op (name (&rest args))
  (let ((method-name (intern (format nil "MEDIUM-~A*" name))))
    `(eval-when (eval load compile)
       (defmethod ,method-name ((stream sheet) ,@args)
	 (with-sheet-medium (medium stream)
	   (,method-name medium ,@args))))))

(def-graphic-op draw-point (x y))
(def-graphic-op draw-points (coord-seq))
(def-graphic-op draw-line (x1 y1 x2 y2))
(def-graphic-op draw-lines (coord-seq))
(def-graphic-op draw-polygon (coord-seq closed filled))
(def-graphic-op draw-rectangle (left top right bottom filled))
(def-graphic-op draw-ellipse (center-x center-y
				  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				  start-angle end-angle filled))
(def-graphic-op draw-text (string x y
			       start end
			       align-x align-y
			       toward-x toward-y transform-glyphs))
