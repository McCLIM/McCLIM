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
	:initform +black+
	:accessor medium-ink)
   (transformation :initarg :transformation
		   :initform +identity-transformation+ 
		   :accessor medium-transformation)
   (clipping-region :initarg :clipping-region
		    :initform +everywhere+
		    :accessor medium-clipping-region)
   (line-style :initarg :line-style
	       :initform (make-line-style)
	       :accessor medium-line-style)
   (text-style :initarg :text-style
	       :initform (make-text-style :fix :roman :normal)
	       :accessor medium-text-style)
   (default-text-style :initarg :default-text-style
                       :initform (make-text-style :fix :roman :normal)
		       :accessor medium-default-text-style)
   (sheet :initarg :sheet
	  :accessor medium-sheet)
   ))

(defun mediump (x)
  (typep x 'medium))

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


;;; Text-Style class

(eval-when (eval load compile)

(defclass text-style ()
  ((family :initarg :text-family
	   :initform :fix
	   :reader text-style-family)
   (face :initarg :text-face
	 :initform :roman
	 :reader text-style-face)
   (size :initarg :text-size
	 :initform :normal
	 :reader text-style-size)
   ))

(defun text-style-p (x)
  (typep x 'text-style))

(defclass standard-text-style (text-style)
  ())

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
)

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

(defun merge-text-styles (s1 s2)
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
    new-style))

(defmethod invoke-with-text-style ((sheet sheet) continuation text-style)
  (invoke-with-text-style (sheet-medium sheet) continuation text-style))

(defmethod invoke-with-text-style ((medium medium) continuation text-style)
  (let ((old-style (medium-text-style medium)))
    (setf (slot-value medium 'text-style)
      (merge-text-styles text-style (medium-merged-text-style medium)))
    (unwind-protect
	(funcall continuation)
      (setf (slot-value medium 'text-style) old-style))))

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
  `(flet ((continuation ()
	    ,@body))
     #-clisp (declare (dynamic-extent #'continuation))
     (invoke-with-text-style ,medium #'continuation (parse-text-style ,text-style))))

(defmacro with-text-family ((medium family) &body body)
  `(flet ((continuation ()
	    ,@body))
     #-clisp (declare (dynamic-extent #'continuation))
     (invoke-with-text-style ,medium #'continuation (make-text-style ,family nil nil))))

(defmacro with-text-face ((medium face) &body body)
  `(flet ((continuation ()
	    ,@body))
     #-clisp (declare (dynamic-extent #'continuation))
     (invoke-with-text-style ,medium #'continuation (make-text-style nil ,face nil))))

(defmacro with-text-size ((medium size) &body body)
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
  (let ((old-buffer (gensym)))
    `(let ((,old-buffer (medium-buffering-output-p ,medium)))
       (setf (medium-buffering-output-p ,medium) ,buffer-p)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (medium-buffering-output-p ,medium) ,old-buffer)))))
