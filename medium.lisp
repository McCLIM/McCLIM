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

;;; MEDIUM class

(defclass medium ()
  ((port :initarg :port
	 :accessor port)
   (graft :initarg :graft
	  :accessor graft)
   (foreground :initarg :foreground
	       :initform +black+
	       :accessor medium-foreground)
   (background :initarg :background
	       :initform +white+
	       :accessor medium-background)
   (ink :initarg :ink
	:initform +foreground-ink+
	:accessor medium-ink)
   (transformation :type transformation
		   :initarg :transformation
		   :initform +identity-transformation+ 
		   :accessor medium-transformation)
   (clipping-region :type region
		    :initarg :clipping-region
		    :initform +everywhere+
		    :documentation "Clipping region in the SHEET coordinates.")
   ;; always use this slot through its accessor, since there may
   ;; be secondary methods on it -RS 2001-08-23
   (line-style :initarg :line-style
	       :initform (make-line-style)
	       :accessor medium-line-style)
   ;; always use this slot through its accessor, since there may
   ;; be secondary methods on it -RS 2001-08-23
   (text-style :initarg :text-style
	       :initform (make-text-style :fix :roman :normal)
	       :accessor medium-text-style)
   (default-text-style :initarg :default-text-style
                       :initform (make-text-style :fix :roman :normal)
		       :accessor medium-default-text-style)
   (sheet :initarg :sheet
	  :initform nil ; this means that medium is not linked to a sheet
	  :accessor medium-sheet)
   ))

(defun mediump (x)
  (typep x 'medium))

(defmethod initialize-instance :after ((medium medium) &rest args)
  (declare (ignore args))
  ;; Initial CLIPPING-REGION is in coordinates, given by initial
  ;; TRANSFORMATION, but we store it in SHEET's coords.
  (with-slots (clipping-region) medium
    (setf clipping-region (transform-region (medium-transformation medium)
                                            clipping-region))))

(defmethod medium-clipping-region ((medium medium))
  (untransform-region (medium-transformation medium)
                    (slot-value medium 'clipping-region)))

(defmethod (setf medium-clipping-region) (region (medium medium))
  (setf (slot-value medium 'clipping-region)
        (transform-region (medium-transformation medium)
                            region)))

(defmethod (setf medium-clipping-region) :after (region (medium medium))
  (declare (ignore region))
  (let ((sheet (medium-sheet medium)))
    (when sheet
      (invalidate-cached-regions sheet))))

(defmethod (setf medium-transformation) :after (transformation (medium medium))
  (declare (ignore transformation))
  (let ((sheet (medium-sheet medium)))
    (when sheet
      (invalidate-cached-transformations sheet))))

(defmethod medium-merged-text-style ((medium medium))
  (merge-text-styles (medium-text-style medium) (medium-default-text-style medium)))

(defmacro with-sheet-medium ((medium sheet) &body body)
  (let ((old-medium (gensym))
	(old-sheet (gensym)))
    `(let* ((,old-medium (sheet-medium ,sheet))
	    (,medium (or ,old-medium (make-medium (port ,sheet) ,sheet)))
	    (,old-sheet (medium-sheet ,medium)))
       (setf (sheet-medium ,sheet) ,medium)
       (setf (medium-sheet ,medium) ,sheet)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (sheet-medium ,sheet) ,old-medium)
	 (setf (medium-sheet ,medium) ,old-sheet)))))

(defmacro with-sheet-medium-bound ((sheet medium) &body body)
  (let ((old-medium (gensym))
	(old-sheet (gensym)))
    `(let* ((,old-medium (sheet-medium ,sheet))
	    (medium (or ,old-medium ,medium (make-medium (port sheet) sheet)))
	    (,old-sheet (medium-sheet ,medium)))
       (if (null ,old-medium)
	   (setf (sheet-medium ,sheet) ,medium))
       (setf (medium-sheet ,medium) ,sheet)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (sheet-medium ,sheet) ,old-medium)
	 (setf (medium-sheet ,medium) ,old-sheet)))))

(defmacro with-pixmap-medium ((medium pixmap) &body body)
  (let ((old-medium (gensym))
	(old-pixmap (gensym)))
    `(let* ((,old-medium (pixmap-medium ,pixmap))
	    (,medium (or ,old-medium (make-medium (port ,pixmap) ,pixmap)))
	    (,old-pixmap (medium-sheet ,medium)))
       (setf (pixmap-medium ,pixmap) ,medium)
       (setf (medium-sheet ,medium) ,pixmap)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (pixmap-medium ,pixmap) ,old-medium)
	 (setf (medium-sheet ,medium) ,old-pixmap)))))

;;; Medium Device functions

(defmethod medium-device-transformation ((medium medium))
  (sheet-device-transformation (medium-sheet medium)))

(defmethod medium-device-region ((medium medium))
  (sheet-device-region (medium-sheet medium)))


;;; Text-Style class

(eval-when (eval load compile)

(defclass text-style ()
  ())

(defun text-style-p (x)
  (typep x 'text-style))

(defclass standard-text-style (text-style)
  ((family :initarg :text-family
	   :initform :fix
	   :reader text-style-family)
   (face :initarg :text-face
	 :initform :roman
	 :reader text-style-face)
   (size :initarg :text-size
	 :initform :normal
	 :reader text-style-size)))

(defun family-key (family)
  (ecase family
    ((nil) 0)
    ((:fix :fixed) 1)
    ((:serif) 2)
    ((:sans-serif) 3)))

(defun face-key (face)
  (if (equal face '(:bold :italic))
      4
      (ecase face
	((nil) 0)
	((:roman) 1)
	((:bold) 2)
	((:italic) 3))))

(defun size-key (size)
  (if (numberp size)
      (+ 10 (round (* 256 size)))
      (ecase size
	((nil) 0)
	((:tiny) 1)
	((:very-small) 2)
	((:small) 3)
	((:normal) 4)
	((:large) 5)
	((:very-large) 6)
	((:huge) 7)
	((:smaller 8))
	((:larger 9)))))

(defun text-style-key (family face size)
  (+ (* 256 (size-key size))
     (* 16 (face-key face))
     (family-key family)))

(defvar *text-style-hash-table* (make-hash-table :test #'eql))

(defun make-text-style (family face size)
  (let ((key (text-style-key family face size)))
    (declare (type fixnum key))
    (or (gethash key *text-style-hash-table*)
	(setf (gethash key *text-style-hash-table*)
	      (make-instance 'standard-text-style
			     :text-family family
			     :text-face face
			     :text-size size)))))
) ; end eval-when

(defconstant *default-text-style* (make-text-style :fix :roman :normal))

(defconstant *smaller-sizes* '(:huge :very-large :large :normal
			       :small :very-small :tiny :tiny))

(defun find-smaller-size (size)
  (if (numberp size)
      (max (round (* size 0.75)) 6)
    (cadr (member size *smaller-sizes*))))

(defconstant *larger-sizes* '(:tiny :very-small :small :normal
			      :large :very-large :huge :huge))

(defun find-larger-size (size)
  (if (numberp size)
      (max (round (* size 4/3)) 6)
    (cadr (member size *larger-sizes*))))

(defmethod text-style-components ((text-style standard-text-style))
  (values (text-style-family text-style)
          (text-style-face text-style)
          (text-style-size text-style)))

;;; Device-Font-Text-Style class

(defclass device-font-text-style (text-style)
  ())

(defun device-font-text-style-p (s)
  (typep s 'device-font-text-style))

(defun make-device-font-text-style (display-device device-font-name)
  (port-make-font-text-style (port display-device) device-font-name))

;;; Text-style utilities

(defun merge-text-styles (s1 s2)
  (if (and (not (device-font-text-style-p s1))
	   (not (device-font-text-style-p s2)))
      (let ((new-style (make-text-style (or (text-style-family s1)
					    (text-style-family s2))
					(or (text-style-face s1)
					    (text-style-face s2))
					(or (text-style-size s1)
					    (text-style-size s2)))))
	(with-slots (size) new-style
	  (case size
	    (:smaller
	     (setq size (find-smaller-size (text-style-size s2))))
	    (:larger
	     (setq size (find-larger-size (text-style-size s2))))))
	new-style)
      s1))

(defmethod invoke-with-text-style ((sheet sheet) continuation text-style)
  (invoke-with-text-style (sheet-medium sheet) continuation text-style))

(defmethod invoke-with-text-style ((medium medium) continuation text-style)
  (let ((old-style (medium-text-style medium)))
    (setf (medium-text-style medium)
      (merge-text-styles text-style (medium-merged-text-style medium)))
    (unwind-protect
	(funcall continuation)
      (setf (medium-text-style medium) old-style))))

(defun parse-text-style (style)
  (if (text-style-p style)
      style
    (let ((family nil)
	  (face nil)
	  (size nil))
      (loop for item in style
	    do (cond
		((member item '(fix :serif :sans-serif))
		 (setq family item))
		((or (member item '(:roman :bold :italic))
		     (listp item))
		 (setq face item))
		((or (member item '(:tiny :very-small :small :normal
				    :large :very-large :huge))
		     (numberp item))
		 (setq size item))))
      (make-text-style family face size))))

(defmacro with-text-style ((medium text-style) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  `(flet ((continuation ()
	    ,@body))
     #-clisp (declare (dynamic-extent #'continuation))
     (invoke-with-text-style ,medium #'continuation (parse-text-style ,text-style))))

(defmacro with-text-family ((medium family) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  `(flet ((continuation ()
	    ,@body))
     #-clisp (declare (dynamic-extent #'continuation))
     (invoke-with-text-style ,medium #'continuation (make-text-style ,family nil nil))))

(defmacro with-text-face ((medium face) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  `(flet ((continuation ()
	    ,@body))
     #-clisp (declare (dynamic-extent #'continuation))
     (invoke-with-text-style ,medium
                             #'continuation (make-text-style nil ,face nil))))

(defmacro with-text-size ((medium size) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  `(flet ((continuation ()
	    ,@body))
     #-clisp (declare (dynamic-extent #'continuation))
     (invoke-with-text-style ,medium #'continuation (make-text-style nil nil ,size))))


;;; Line-Style class

(defclass line-style ()
  ((unit :initarg :line-unit
	 :initform :normal
	 :reader line-style-unit)
   (thickness :initarg :line-thickness
	      :initform 1
	      :reader line-style-thickness)
   (joint-shape :initarg :line-joint-shape
		:initform :miter
		:reader line-style-joint-shape)
   (cap-shape :initarg :line-cap-shape
	      :initform :butt
	      :reader line-style-cap-shape)
   (dashes :initarg :line-dashes
	   :initform nil
	   :reader line-style-dashes)
  ))

(defun line-style-p (x)
  (typep x 'line-style))

(defclass standard-line-style (line-style)
  ())

(defun make-line-style (&key (unit :normal) (thickness 1)
			     (joint-shape :miter) (cap-shape :butt)
			     (dashes nil))
  (make-instance 'standard-line-style
    :line-unit unit
    :line-thickness thickness
    :line-joint-shape joint-shape
    :line-cap-shape cap-shape
    :line-dashes dashes))


;;; Graphics ops

(defgeneric medium-draw-point* (medium x y))
(defgeneric medium-draw-points* (medium coord-seq))
(defgeneric medium-draw-line* (medium x1 y1 x2 y2))
(defgeneric medium-draw-lines* (medium coord-seq))
(defgeneric medium-draw-polygon* (medium coord-seq closed filled))
(defgeneric medium-draw-rectangle* (medium left top right bottom filled))
(defgeneric medium-draw-ellipse* (medium center-x center-y
				  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				  start-angle end-angle filled))
(defgeneric medium-draw-text* (medium string x y
			       start end
			       align-x align-y
			       toward-x toward-y transform-glyphs))


;;; Misc ops

(defmacro with-output-buffered ((medium &optional (buffer-p t)) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  (let ((old-buffer (gensym)))
    `(let ((,old-buffer (medium-buffering-output-p ,medium)))
       (setf (medium-buffering-output-p ,medium) ,buffer-p)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (medium-buffering-output-p ,medium) ,old-buffer)))))


;;; BASIC-MEDIUM class

(defclass basic-medium (medium)
  ()
  (:documentation "The basic class, on which all CLIM mediums are built."))

(defmacro with-transformed-position ((transformation x y) &body body)
  `(multiple-value-bind (,x ,y) (transform-position ,transformation ,x ,y)
     ,@body))

(defmacro with-transformed-distance ((transformation dx dy) &body body)
  `(multiple-value-bind (,dx ,dy) (transform-distance ,transformation ,dx ,dy)
     ,@body))

(defmacro with-transformed-positions ((transformation coord-seq) &body body)
  `(let ((,coord-seq (transform-positions ,transformation ,coord-seq)))
     ,@body))


;;; Pixmaps

(defmethod medium-copy-area ((from-drawable basic-medium) from-x from-y width height
                             to-drawable to-x to-y)
  (declare (ignore from-x from-y width height to-drawable to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))

(defmethod medium-copy-area (from-drawable from-x from-y width height
                             (to-drawable basic-medium) to-x to-y)
  (declare (ignore from-drawable from-x from-y width height to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))


;;; Medium-specific Drawing Functions

(defmethod medium-draw-point* :around ((medium basic-medium) x y)
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x y)
                               (call-next-method medium x y))))

(defmethod medium-draw-points* :around ((medium basic-medium) coord-seq)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
                                (call-next-method medium coord-seq))))

(defmethod medium-draw-line* :around ((medium basic-medium) x1 y1 x2 y2)
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x1 y1)
                               (with-transformed-position (tr x2 y2)
                                                          (call-next-method medium x1 y1 x2 y2)))))

(defmethod medium-draw-lines* :around ((medium basic-medium) coord-seq)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
                                (call-next-method medium coord-seq))))

(defmethod medium-draw-polygon* :around ((medium basic-medium) coord-seq closed filled)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
                                (call-next-method medium coord-seq closed filled))))

(defmethod medium-draw-rectangle* :around ((medium basic-medium) left top right bottom filled)
  (let ((tr (medium-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (multiple-value-bind (left top right bottom)
            (transform-rectangle* tr left top right bottom)
          (call-next-method medium left top right bottom filled))
      (medium-draw-polygon* medium (list left top
                                         left bottom
                                         right bottom
                                         right top)
                            t filled))))

(defmethod medium-draw-rectangles* :around ((medium basic-medium) position-seq filled)
  (let ((tr (medium-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (loop for (left top right bottom) on position-seq by #'cddddr
              nconcing (multiple-value-list
                        (transform-rectangle* tr left top right bottom)) into position-seq
              finally (call-next-method medium position-seq filled))
        (loop for (left top right bottom) on position-seq by #'cddddr
              do (medium-draw-polygon* medium (list left top
                                                    left bottom
                                                    right bottom
                                                    right top)
                                       t filled)))))


(defmethod medium-draw-ellipse* :around ((medium basic-medium) center-x center-y
                                         radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                         start-angle end-angle filled)
  (let* ((ellipse (make-elliptical-arc* center-x center-y
                                        radius-1-dx radius-1-dy
                                        radius-2-dx radius-2-dy
                                        :start-angle start-angle
                                        :end-angle end-angle))
         (transformed-ellipse (transform-region (medium-transformation medium)
                                                ellipse))
         (start-angle (ellipse-start-angle transformed-ellipse))
         (end-angle (ellipse-end-angle transformed-ellipse)))
    (multiple-value-bind (center-x center-y) (ellipse-center-point* transformed-ellipse)
      (multiple-value-bind (radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
          (ellipse-radii transformed-ellipse)
        (call-next-method medium center-x center-y
                          radius-1-dx radius-1-dy
                          radius-2-dx radius-2-dy
                          start-angle end-angle filled)))))

(defmethod medium-draw-text* :around ((medium basic-medium) string x y
                                      start end
                                      align-x align-y
                                      toward-x toward-y transform-glyphs)
  ;;!!! FIX ME!
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x y)
      (call-next-method medium string x y
                        start end
                        align-x align-y
                        toward-x toward-y transform-glyphs))))

(defmethod medium-draw-glyph :around ((medium basic-medium) element x y
                                      align-x align-y toward-x toward-y
                                      transform-glyphs)
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x y)
      (call-next-method medium element x y
                        align-x align-y toward-x toward-y
                        transform-glyphs))))

(defmethod medium-copy-area :around ((from-drawable basic-medium)
                                     from-x from-y width height
                                     to-drawable to-x to-y)
  (with-transformed-position ((medium-transformation from-drawable)
                              from-x from-y)
    (call-next-method from-drawable from-x from-y width height
                      to-drawable to-x to-y)))

(defmethod medium-copy-area :around (from-drawable from-x from-y width height
                                     (to-drawable  basic-medium)
                                     to-x to-y)
  (with-transformed-position ((medium-transformation to-drawable)
                              to-x to-y)
    (call-next-method from-drawable from-x from-y width height
                      to-drawable to-x to-y)))

;;; Fall-through Methods For Multiple Objects Drawing Functions

(defmethod medium-draw-points* ((medium basic-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (loop for (x y) on coord-seq by #'cddr
            do (medium-draw-point* medium x y)))))

(defmethod medium-draw-lines* ((medium basic-medium) position-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr position-seq)
      (loop for (x1 y1 x2 y2) on position-seq by #'cddddr
            do (medium-draw-line* medium x1 y1 x2 y2)))))

(defmethod medium-draw-rectangles* ((medium basic-medium) coord-seq filled)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (loop for (x1 y1 x2 y2) on coord-seq by #'cddddr
            do (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium basic-medium))
  nil)

(defmethod medium-force-output ((medium basic-medium))
  nil)

(defmethod medium-clear-area ((medium basic-medium) left top right bottom)
  (draw-rectangle* medium left top right bottom :ink +background-ink+))

(defmethod medium-beep ((medium basic-medium))
  nil)
