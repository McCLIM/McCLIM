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

;;; SHEET class

(defclass sheet ()
  ((port :initform nil
	 :initarg :port
	 :accessor port)
   (graft :initform nil
	  :initarg :graft
	  :accessor graft)
   (enabled :initform t
	    :accessor sheet-enabled-p)
   (region :initform (make-bounding-rectangle 0 0 100 100)
	   :accessor sheet-region)
   ))

(defun sheetp (x)
  (typep x 'sheet))

(defmethod sheet-parent ((sheet sheet))
  nil)

(defmethod set-sheets-parent ((sheet sheet) (parent sheet))
  (error "Attempting to set the parent of a sheet that has no parent"))

(defmethod sheet-children ((sheet sheet))
  nil)

(defmethod sheet-transformation ((sheet sheet))
  (error "Attempting to get the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod (setf sheet-transformation) (transformation (sheet sheet))
  (declare (ignore transformation))
  (error "Attempting to set the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod sheet-medium ((sheet sheet))
  (error "Attempting to get the MEDIUM of a SHEET that doesn't contain one"))

(defmethod (setf sheet-medium) (medium (sheet sheet))
  (declare (ignore medium))
  (error "Attempting to set the MEDIUM of a SHEET that doesn't contain one"))

;;; SHEET error conditions

(define-condition sheet-already-has-parent (error)
  (
   ))

(define-condition sheet-is-not-child (error)
  (
   ))

;;; SHEET relationship functions

(defmethod sheet-adopt-child ((sheet sheet) (child sheet))
  (error "SHEET attempting to adopt a child"))

(defmethod sheet-disown-child ((sheet sheet) (child sheet) &key (errorp t))
  (cond
   ((eq (sheet-parent child) sheet)
    (set-sheets-parent child nil)
    (with-slots (children) sheet
      (setf children (remove child children)))
    )
   (errorp
    (error 'sheet-is-not-child)))
  child)

(defmethod sheet-siblings ((sheet sheet))
  (remove sheet (sheet-children (sheet-parent sheet))))

(defmethod sheet-enabled-children ((sheet sheet))
  (loop for child in (sheet-children sheet)
       if (sheet-enabled-p child)
	  collect child))

(defmethod sheet-ancestor-p ((sheet sheet) (putative-ancestor sheet))
  (loop for s = (sheet-parent sheet) then (sheet-parent s)
       until (null s)
       if (eq s putative-ancestor)
	  return t
       finally (return nil)))

(defmethod raise-sheet ((sheet sheet))
  (with-slots (children) (sheet-parent sheet)
    (setf children (cons sheet (remove sheet children))))
  sheet)

(defmethod bury-sheet ((sheet sheet))
  (with-slots (children) (sheet-parent sheet)
    (setf children (nconc (remove sheet children) (list sheet))))
  sheet)

(define-condition sheet-ordering-underspecified (error)
  (
   ))

(defmethod reorder-sheets ((sheet sheet) new-ordering)
  (with-slots (children) sheet
    (if (set-difference children new-ordering)
	(error 'sheet-ordering-underspecified))
    (if (set-difference new-ordering children)
	(error 'sheet-is-not-child))
    (setf children new-ordering))
  sheet)

(defmethod sheet-viewable-p ((sheet sheet))
  (and (sheet-enabled-p sheet)
       (or (null (sheet-parent sheet))
	   (sheet-viewable-p (sheet-parent sheet)))))

(defmethod sheet-occluding-sheets ((sheet sheet) (child sheet))
  (let ((childs-region (sheet-region child))
	(results nil))
    (do ((children (sheet-children sheet) (cdr children)))
	((or (null children)
	     (eq (first children) child)))
      (if (and (sheet-enabled-p (first children))
	       (region-intersects-region-p childs-region (sheet-region (first children))))
	  (push (first children) results)))
    results))

;;; SHEET mixin classes

(defclass sheet-parent-mixin ()
  ((parent :initform nil
	   :reader sheet-parent)
   ))

(defmethod set-sheets-parent ((child sheet-parent-mixin) (parent sheet))
  (setf (slot-value child 'parent) parent))

(defclass sheet-leaf-mixin ()
  (
   ))

(defmethod sheet-adopt-child ((sheet sheet-leaf-mixin) (child sheet))
  (error "Leaf sheet attempting to adopt a child"))

(defclass sheet-single-child-mixin ()
  ((children :initform nil
	     :reader sheet-children)
   ))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin) (child sheet))
  (if (sheet-parent child)
      (error 'sheet-already-has-parent))
  (if (sheet-children sheet)
      (error "Single Child Sheet attempting to adopt a second child"))
  (set-sheets-parent child sheet)
  (with-slots (children) sheet
    (setf children (list child)))
  child)

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil
	     :reader sheet-children)
   ))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin) (child sheet))
  (if (sheet-parent child)
      (error 'sheet-already-has-parent))
  (set-sheets-parent child sheet)
  (with-slots (children) sheet
    (setf children (append children (list child))))
  child)

;;; SHEET geometry functions

(defmethod map-sheet-position-to-parent ((sheet sheet) x y)
  (transform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-position-to-child ((sheet sheet) x y)
  (transform-position (invert-transformation (sheet-transformation sheet)) x y))

(defmethod map-sheet-rectangle*-to-parent ((sheet sheet) x1 y1 x2 y2)
  (transform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

(defmethod map-sheet-rectangle*-to-child ((sheet sheet) x1 y1 x2 y2)
  (transform-rectangle* (invert-transformation (sheet-transformation sheet)) x1 y1 x2 y2))

(defmethod child-containing-position ((sheet sheet) x y)
  (loop for child in (sheet-children sheet)
      do (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
	   (if (and (sheet-enabled-p child)
		    (region-contains-position-p (sheet-region child) tx ty))
	       (return child)))))

(defmethod children-overlapping-region ((sheet sheet) (region region))
  (loop for child in (sheet-children sheet)
      if (and (sheet-enabled-p child)
	      (region-intersects-region-p region (transform-region (sheet-transformation child) (sheet-region child))))
      collect child))

(defmethod children-overlapping-rectangle* ((sheet sheet) x1 y1 x2 y2)
  (loop with region = (make-rectangle* x1 y1 x2 y2)
      for child in (sheet-children sheet)
      if (and (sheet-enabled-p child)
	      (region-intersects-region-p region (transform-region (sheet-transformation child) (sheet-region child))))
      collect child))

(defmethod sheet-delta-transformation ((sheet sheet) (ancestor (eql nil)))
  (if (sheet-parent sheet)
      (compose-transformations (sheet-transformation sheet)
			       (sheet-delta-transformation
				(sheet-parent sheet) ancestor))
      (sheet-transformation sheet)))
  
(define-condition sheet-is-not-ancestor (error) ())

(defmethod sheet-delta-transformation ((sheet sheet) (ancestor sheet))
  (cond ((eq sheet ancestor) +identity-transformation+)
	((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t (error 'sheet-is-not-ancestor))))

(defmethod sheet-allocated-region ((sheet sheet) (child sheet))
  (loop with region = (sheet-region child)
      with inverse-transform = (invert-transformation (sheet-transformation child))
      for sibling in (sheet-siblings child)
      for siblings-region = (transform-region inverse-transform (transform-region (sheet-transformation sibling) (sheet-region sibling)))
      do (if (region-intersects-region-p region siblings-region)
	     (setq region (region-difference region siblings-region)))
      finally (return region)))

;;; SHEET geometry classes

(defclass sheet-identity-transformation-mixin ()
  ((transformation :initform (make-instance 'transformation)
		   :initarg :transformation
		   :accessor sheet-transformation)
   ))

(defmethod (setf sheet-transformation) :before ((transformation transformation) (sheet sheet-identity-transformation-mixin))
  (if (not (identity-transformation-p transformation))
      (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-IDENTITY-TRANSFORMATION-MIXIN to a non identity transformation")))

(defclass sheet-translation-transformation-mixin ()
  ((transformation :initform (make-instance 'transformation)
		   :initarg :transformation
		   :accessor sheet-transformation)
   ))

(defmethod (setf sheet-transformation) :before ((transformation transformation) (sheet sheet-translation-transformation-mixin))
  (if (not (translation-transformation-p transformation))
      (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-TRANSLATION-TRANSFORMATION-MIXIN to a non translation transformation")))

(defclass sheet-y-inverting-transformation-mixin ()
  ((transformation :initform (make-instance 'transformation :myy -1.0)
		   :initarg :transformation
		   :accessor sheet-transformation)
   ))

(defmethod (setf sheet-transformation) :before ((transformation transformation) (sheet sheet-y-inverting-transformation-mixin))
  (if (not (y-inverting-transformation-p transformation))
      (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-Y-INVERTING-TRANSFORMATION-MIXIN to a non Y inverting transformation")))

(defclass sheet-transformation-mixin ()
  ((transformation :initform (make-instance 'transformation)
		   :initarg :transformation
		   :accessor sheet-transformation)
   ))

;;; Mirrored-Sheet class

(defclass mirrored-sheet (sheet)
  ()
  )

(defmethod sheet-direct-mirror ((sheet sheet))
  nil)

(defmethod sheet-direct-mirror ((sheet mirrored-sheet))
  (port-lookup-mirror (port sheet) sheet))

(defmethod sheet-mirrored-ancestor ((sheet sheet))
  (if (sheet-parent sheet)
      (sheet-mirrored-ancestor (sheet-parent sheet))))

(defmethod sheet-mirrored-ancestor ((sheet mirrored-sheet))
  sheet)

(defmethod sheet-mirror ((sheet sheet))
  (let ((mirrored-ancestor (sheet-mirrored-ancestor sheet)))
    (if mirrored-ancestor
	(sheet-direct-mirror mirrored-ancestor))))

;;; REALIZE-MIRROR and UNREALIZE-MIRROR have been moved to ports.lisp because
;;; they require the PORT class to be defined

;;; Repaint protocol

(defclass standard-repaint-mixin () ())

(defmethod dispatch-repaint ((sheet standard-repaint-mixin) region)
  (queue-repaint sheet region))

(defclass immediate-repaint-mixin () ())

(defmethod dispatch-repaint ((sheet immediate-repaint-mixin) region)
  (handle-repaint sheet nil region))

(defclass mute-repaint-mixin () ())

(defmethod dispatch-repaint ((sheet mute-repaint-mixin) region)
  (handle-repaint sheet nil region))

(defmethod dispatch-repaint ((sheet standard-repaint-mixin) region)
  (queue-repaint sheet region))

(defmethod repaint-sheet ((sheet mute-repaint-mixin) region)
  (declare (ignore region))
  (values))
