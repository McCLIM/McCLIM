;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com),
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The sheet protocol

(in-package :clim-internals)

(defgeneric raise-sheet-internal (sheet parent))
(defgeneric bury-sheet-internal (sheet parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input protocol

(defgeneric dispatch-event (client event))
(defgeneric queue-event (client event))
(defgeneric schedule-event (client event delay))
(defgeneric handle-event (client event))
(defgeneric event-read (client))
(defgeneric event-read-no-hang (client))
(defgeneric event-peek (client &optional event-type))
(defgeneric event-unread (client event))
(defgeneric event-listen (client))
;(defgeneric sheet-direct-mirror (sheet))
;(defgeneric sheet-mirrored-ancestor (sheet))
;(defgeneric sheet-mirror (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; repaint protocol

(defgeneric dispatch-repaint (sheet region))
;(defgeneric queue-repaint (sheet region))
;(defgeneric handle-repaint (sheet region))
;(defgeneric repaint-sheet (sheet region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

(defgeneric note-sheet-grafted (sheet))
(defgeneric note-sheet-degrafted (sheet))
(defgeneric note-sheet-adopted (sheet))
(defgeneric note-sheet-disowned (sheet))
(defgeneric note-sheet-enabled (sheet))
(defgeneric note-sheet-disabled (sheet))
(defgeneric note-sheet-region-changed (sheet))
(defgeneric note-sheet-transformation-changed (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; sheet protocol class

(defclass basic-sheet (sheet)
  ((region :type region
	   :initarg :region
	   :initform (make-bounding-rectangle 0 0 100 100)
	   :accessor sheet-region)
   (native-transformation :type (or null transformation)
			  ;:initform nil
			  :initform +identity-transformation+
                          :writer %%set-sheet-native-transformation
                          :reader %%sheet-native-transformation)
   (native-region :type (or null region)
		  :initform nil)
   (device-transformation :type (or null transformation)
			  :initform nil)
   (device-region :type (or null region)
		  :initform nil)
   (pointer-cursor :accessor sheet-pointer-cursor
                   :initarg  :pointer-cursor
                   :initform :default)
   (enabled-p :type boolean
	      :initarg :enabled-p
              :initform t
              :accessor sheet-enabled-p)))
; Native region is volatile, and is only computed at the first request when it's equal to nil.
; Invalidate-cached-region method sets the native-region to nil.

(defmethod sheet-parent ((sheet basic-sheet))
  nil)

(defmethod sheet-children ((sheet basic-sheet))
  nil)

(defmethod sheet-adopt-child ((sheet basic-sheet) (child sheet))
  (error "~S attempting to adopt ~S" sheet child))

(defmethod sheet-adopt-child :after ((sheet basic-sheet) (child sheet))
  (note-sheet-adopted child)
  (when (sheet-grafted-p sheet)
    (note-sheet-grafted child)))

(define-condition sheet-is-not-child (error) ())

(defmethod sheet-disown-child :before ((sheet basic-sheet) (child sheet) &key (errorp t))
  (when (and (not (member child (sheet-children sheet))) errorp)
    (error 'sheet-is-not-child)))

(defmethod sheet-disown-child :after ((sheet basic-sheet) (child sheet) &key (errorp t))
  (declare (ignore errorp))
  (note-sheet-disowned child)
  (when (sheet-grafted-p sheet)
    (note-sheet-degrafted child)))

(defmethod sheet-siblings ((sheet basic-sheet))
  (when (not (sheet-parent sheet))
    (error 'sheet-is-not-child))
  (remove sheet (sheet-children (sheet-parent sheet))))

(defmethod sheet-enabled-children ((sheet basic-sheet))
  (delete-if-not #'sheet-enabled-p (copy-list (sheet-children sheet))))

(defmethod sheet-ancestor-p ((sheet basic-sheet)
			     (putative-ancestor sheet))
  (or (eq sheet putative-ancestor)
      (and (sheet-parent sheet)
	   (sheet-ancestor-p (sheet-parent sheet) putative-ancestor))))

(defmethod raise-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(defmethod bury-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(define-condition sheet-ordering-underspecified (error) ())

(defmethod reorder-sheets ((sheet basic-sheet) new-ordering)
  (when (set-difference (sheet-children sheet) new-ordering)
    (error 'sheet-ordering-underspecified))
  (when (set-difference new-ordering (sheet-children sheet))
    (error 'sheet-is-not-child))
  (setf (sheet-children sheet) new-ordering)
  sheet)

(defmethod sheet-viewable-p ((sheet basic-sheet))
  (and (sheet-parent sheet)
       (sheet-viewable-p (sheet-parent sheet))
       (sheet-enabled-p sheet)))

(defmethod sheet-occluding-sheets ((sheet basic-sheet) (child sheet))
  (labels ((fun (l)
		(cond ((eq (car l) child) '())
		      ((and (sheet-enabled-p (car l))
                            (region-intersects-region-p
                             (sheet-region (car l)) (sheet-region child)))
		       (cons (car l) (fun (cdr l))))
		      (t (fun (cdr l))))))
    (fun (sheet-children sheet))))

(defmethod map-over-sheets (function (sheet basic-sheet))
  (funcall function sheet)
  (mapc #'(lambda (child) (map-over-sheets function child))
        (sheet-children sheet))
  nil)

(defmethod (setf sheet-enabled-p) :after (enabled-p (sheet basic-sheet))
  (if enabled-p
      (note-sheet-enabled sheet)
      (note-sheet-disabled sheet)))

(defmethod sheet-transformation ((sheet basic-sheet))
  (error "Attempting to get the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod (setf sheet-transformation) (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (error "Attempting to set the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod move-sheet ((sheet basic-sheet) x y)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (setf (sheet-transformation sheet)
            (compose-translation-with-transformation
             transform (- x old-x) (- y old-y))))))

(defmethod resize-sheet ((sheet basic-sheet) width height)
  (setf (sheet-region sheet)
        (make-bounding-rectangle 0 0 width height)))

(defmethod move-and-resize-sheet ((sheet basic-sheet) x y width height)
  (move-sheet sheet x y)
  (resize-sheet sheet width height))

(defmethod map-sheet-position-to-parent ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-position-to-child ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-parent ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-child ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-over-sheets-containing-position (function (sheet basic-sheet) x y)
  (map-over-sheets #'(lambda (child)
                       (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
                         (when (region-contains-position-p (sheet-region child) tx ty)
                           (funcall function child))))
                   sheet))


(defmethod map-over-sheets-overlapping-region (function (sheet basic-sheet) region)
  (map-over-sheets #'(lambda (child)
                       (when (region-intersects-region-p
                              region
                              (transform-region
                               (if (eq child sheet)
                                   +identity-transformation+
                                   (sheet-transformation child))
                               (sheet-region child)))
                         (funcall function child)))
                   sheet))

(defmethod child-containing-position ((sheet basic-sheet) x y)
  (loop for child in (sheet-children sheet)
      do (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
	    (if (and (sheet-enabled-p child)
		     (region-contains-position-p (sheet-region child) tx ty))
		(return child)))))

(defmethod children-overlapping-region ((sheet basic-sheet) (region region))
  (loop for child in (sheet-children sheet)
      if (and (sheet-enabled-p child)
	      (region-intersects-region-p
	       region
	       (transform-region (sheet-transformation child)
				 (sheet-region child))))
      collect child))

(defmethod children-overlapping-rectangle* ((sheet basic-sheet) x1 y1 x2 y2)
  (children-overlapping-region sheet (make-rectangle* x1 y1 x2 y2)))

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor (eql nil)))
  (cond ((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t +identity-transformation+)))

(define-condition sheet-is-not-ancestor (error) ())

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor sheet))
  (cond ((eq sheet ancestor) +identity-transformation+)
	((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t (error 'sheet-is-not-ancestor))))

(defmethod sheet-allocated-region ((sheet basic-sheet) (child sheet))
  (reduce #'region-difference
	  (mapcar #'(lambda (child)
                      (transform-region (sheet-transformation child)
                                        (sheet-region child)))
                  (cons child (sheet-occluding-sheets sheet child)))))

(defmethod sheet-direct-mirror ((sheet basic-sheet))
  nil)

(defmethod sheet-mirrored-ancestor ((sheet basic-sheet))
  (if (sheet-parent sheet)
      (sheet-mirrored-ancestor (sheet-parent sheet))))

(defmethod sheet-mirror ((sheet basic-sheet))
  (let ((mirrored-ancestor (sheet-mirrored-ancestor sheet)))
    (if mirrored-ancestor
	(sheet-direct-mirror mirrored-ancestor))))

(defmethod graft ((sheet basic-sheet))
  nil)

(defmethod note-sheet-grafted ((sheet basic-sheet))
  (mapc #'note-sheet-grafted (sheet-children sheet)))

(defmethod note-sheet-degrafted ((sheet basic-sheet))
  (mapc #'note-sheet-degrafted (sheet-children sheet)))

(defmethod note-sheet-adopted ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disowned ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-enabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-region-changed ((sheet basic-sheet))
  nil) ;have to change

(defmethod note-sheet-transformation-changed ((sheet basic-sheet))
  nil)

(defmethod sheet-native-transformation ((sheet basic-sheet))
  (with-slots (native-transformation) sheet
    (unless native-transformation
        (setf native-transformation
              (let ((parent (sheet-parent sheet)))
                 (if parent
                     (compose-transformations
                      (sheet-native-transformation parent)
                      (sheet-transformation sheet))
                     +identity-transformation+))))
    native-transformation))

(defmethod sheet-native-region ((sheet basic-sheet))
  (with-slots (native-region) sheet
    (unless native-region
      (let ((this-native-region (transform-region
				 (sheet-native-transformation sheet)
				 (sheet-region sheet)))
	    (parent (sheet-parent sheet)))
	(setf native-region (if parent
				(region-intersection this-native-region
						     (sheet-native-region
						      parent))
				this-native-region))))
    native-region))

(defmethod sheet-device-transformation ((sheet basic-sheet))
  (with-slots (device-transformation) sheet
    (unless device-transformation
      (setf device-transformation
            (let ((medium (sheet-medium sheet)))
              (compose-transformations
               (sheet-native-transformation sheet)
               (if medium
                   (medium-transformation medium)
                   +identity-transformation+)))))
    device-transformation))

(defmethod sheet-device-region ((sheet basic-sheet))
  (with-slots (device-region) sheet
    (unless device-region
      (setf device-region
            (let ((medium (sheet-medium sheet)))
              (region-intersection
               (sheet-native-region sheet)
               (if medium
                   (transform-region
                    (sheet-device-transformation sheet)
                    (medium-clipping-region medium))
                   +everywhere+)))))
    device-region))

(defmethod invalidate-cached-transformations ((sheet basic-sheet))
  (with-slots (native-transformation device-transformation) sheet
    (setf native-transformation nil
          device-transformation nil))
  (loop for child in (sheet-children sheet)
        do (invalidate-cached-transformations child)))

(defmethod invalidate-cached-regions ((sheet basic-sheet))
  (with-slots (native-region device-region) sheet
    (setf native-region nil
          device-region nil))
  (loop for child in (sheet-children sheet)
        do (invalidate-cached-regions child)))

(defmethod (setf sheet-transformation) :after (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (note-sheet-transformation-changed sheet)
  (invalidate-cached-transformations sheet)
  (invalidate-cached-regions sheet))

(defmethod (setf sheet-region) :after (region (sheet basic-sheet))
  (declare (ignore region))
  (note-sheet-region-changed sheet)
  (invalidate-cached-regions sheet))

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet basic-sheet))
  (set-sheet-pointer-cursor (port sheet) sheet cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet parent mixin


(defclass sheet-parent-mixin ()
  ((parent :initform nil :accessor sheet-parent)))

(define-condition sheet-already-has-parent (error) ())
(define-condition sheet-is-ancestor (error) ())

(defmethod sheet-adopt-child :before (sheet (child sheet-parent-mixin))
  (when (sheet-parent child) (error 'sheet-already-has-parent))
  (when (sheet-ancestor-p sheet child) (error 'sheet-is-ancestor)))

(defmethod sheet-adopt-child :after (sheet (child sheet-parent-mixin))
  (setf (sheet-parent child) sheet))

(defmethod sheet-disown-child :after (sheet
				      (child sheet-parent-mixin)
				      &key (errorp t))
  (declare (ignore sheet errorp))
  (setf (sheet-parent child) nil))

(defmethod raise-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (raise-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (raise-mirror (port sheet) sheet)))

(defmethod bury-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (bury-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (bury-mirror (port sheet) sheet)))

(defmethod graft ((sheet sheet-parent-mixin))
  (and (sheet-parent sheet) (graft (sheet-parent sheet))))

(defmethod (setf sheet-transformation) :after (newvalue (sheet sheet-parent-mixin))
  (declare (ignore newvalue))
  #+nil(note-sheet-transformation-changed sheet))

(defmethod map-sheet-position-to-parent ((sheet sheet-parent-mixin) x y)
  (transform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-position-to-child ((sheet sheet-parent-mixin) x y)
  (untransform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-rectangle*-to-parent ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (transform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

(defmethod map-sheet-rectangle*-to-child ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (untransform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet leaf mixin

(defclass sheet-leaf-mixin () ())

(defmethod sheet-children ((sheet sheet-leaf-mixin))
  nil)

(defmethod sheet-adopt-child ((sheet sheet-leaf-mixin) (child sheet))
  (error "Leaf sheet attempting to adopt a child"))

(defmethod sheet-disown-child ((sheet sheet-leaf-mixin) (child sheet) &key (errorp t))
  (declare (ignorable errorp))
  (error "Leaf sheet attempting to disown a child"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet single child mixin

(defclass sheet-single-child-mixin ()
  ((child :initform nil :accessor sheet-child)))

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (and (sheet-child sheet) (list (sheet-child sheet))))

(define-condition sheet-supports-only-one-child (error) ())

(defmethod sheet-adopt-child :before ((sheet sheet-single-child-mixin)
				      (child sheet-parent-mixin))
  (when (sheet-child sheet)
    (error 'sheet-supports-only-one-child)))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin)
			      (child sheet-parent-mixin))
  (setf (sheet-child sheet) child))

(defmethod sheet-disown-child ((sheet sheet-single-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-child sheet) nil))

(defmethod raise-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))

(defmethod bury-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet multiple child mixin

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil :initarg :children :accessor sheet-children)))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin)
			      (child sheet-parent-mixin))
  (push child (sheet-children sheet)))

(defmethod sheet-disown-child ((sheet sheet-multiple-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-children sheet) (delete child (sheet-children sheet))))

(defmethod raise-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
	(cons sheet (delete sheet (sheet-children parent)))))

(defmethod bury-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
	(append (delete sheet (sheet-children parent)) (list  sheet))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet geometry classes

(defclass sheet-identity-transformation-mixin ()
  ())

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defclass sheet-transformation-mixin ()
  ((transformation :initform +identity-transformation+
		   :initarg :transformation
		   :accessor sheet-transformation)))

(defclass sheet-translation-transformation-mixin (sheet-transformation-mixin)
  ())

(defmethod (setf sheet-transformation) :before ((transformation transformation)
						(sheet sheet-translation-transformation-mixin))
  (if (not (translation-transformation-p transformation))
      (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-TRANSLATION-TRANSFORMATION-MIXIN to a non translation transformation")))

(defclass sheet-y-inverting-transformation-mixin (sheet-transformation-mixin)
  ()
  (:default-initargs :transformation (make-transformation 1 0 0 -1 0 0)))

(defmethod (setf sheet-transformation) :before ((transformation transformation)
						(sheet sheet-y-inverting-transformation-mixin))
  (if (not (y-inverting-transformation-p transformation))
      (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-Y-INVERTING-TRANSFORMATION-MIXIN to a non Y inverting transformation")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirrored sheet


;; We assume the following limitations of the host window systems:
;;
;;  mirror transformations:
;;   . can only be translations
;;   . are limited to 16-bit signed integer deltas
;;
;;  mirror regions:
;;   . can only be axis-aligend rectangles
;;   . min-x = min-y = 0
;;   . max-x, max-y < 2^16
;;
;; These are the limitations of the X Window System.
;;

(defclass mirrored-sheet-mixin ()
  ((port :initform nil :initarg :port :accessor port)

   (mirror-transformation
    :documentation "Our idea of the current mirror transformation. Might not
                    be correct if a foreign application changes our mirror's geometry."
    :initform +identity-transformation+
    :accessor %sheet-mirror-transformation)

   (mirror-region
    :documentation "Our idea of the current mirror region. Might not be
correct if a foreign application changes our mirror's geometry. Also note
that this might be different from the sheet's native region."
    :initform nil
    :accessor %sheet-mirror-region)))

(defmethod sheet-direct-mirror ((sheet mirrored-sheet-mixin))
  (port-lookup-mirror (port sheet) sheet))

(defmethod (setf sheet-direct-mirror) (mirror (sheet mirrored-sheet-mixin))
  (port-register-mirror (port sheet) sheet mirror))

(defmethod sheet-mirrored-ancestor ((sheet mirrored-sheet-mixin))
  sheet)

(defmethod sheet-mirror ((sheet mirrored-sheet-mixin))
  (sheet-direct-mirror sheet))

(defmethod note-sheet-grafted :before ((sheet mirrored-sheet-mixin))
  (unless (port sheet)
    (error "~S called on sheet ~S, which has no port?!" 'note-sheet-grafted sheet))
  (realize-mirror (port sheet) sheet))

(defmethod note-sheet-degrafted :after ((sheet mirrored-sheet-mixin))
  (destroy-mirror (port sheet) sheet))

(defmethod (setf sheet-region) :after (region (sheet mirrored-sheet-mixin))
  (declare (ignore region))
  #+nil(port-set-sheet-region (port sheet) sheet region)
  (update-mirror-geometry sheet)
  )

(defmethod note-sheet-transformation-changed ((sheet mirrored-sheet-mixin))
  (update-mirror-geometry sheet))

(defmethod sheet-native-region ((sheet mirrored-sheet-mixin))
  (with-slots (native-region) sheet     
    (unless native-region      
      (let ((this-region (transform-region (sheet-native-transformation sheet)
					   (sheet-region sheet)))
	    (parent (sheet-parent sheet)))
	(setf native-region
	      (if parent
		  (region-intersection this-region
				       (transform-region
					(invert-transformation
					 (%sheet-mirror-transformation sheet))
					(sheet-native-region parent)))
		  this-region))))
    native-region))

(defmethod (setf sheet-enabled-p) :after (new-value (sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)     ;only do this if the sheet actually has a mirror
    (if new-value
        (port-enable-sheet (port sheet) sheet)
        (port-disable-sheet (port sheet) sheet))))

;;; Reflecting a Sheet's Geometry to the Mirror

(defmethod sheet-mirror-region ((sheet mirrored-sheet-mixin))
  (cond
    ;; for grafts or top-level-sheet's always read the mirror region from
    ;; the server, since it is not under our control.
    ((or (null (sheet-parent sheet))
         (null (sheet-parent (sheet-parent sheet))))
     (make-rectangle* 0 0 #x10000 #x10000)
     #+nil
     (make-rectangle* 0 0
                      (port-mirror-width (port sheet) sheet)
                      (port-mirror-height (port sheet) sheet)))
    (t
     ;; For other sheets just use the calculated value, saves a round trip.
     (or (%sheet-mirror-region sheet)
         ;; XXX what to do if the sheet has no idea about its region?
         ;; XXX can we consider calling sheet-mirror-region then an error?
         (make-rectangle* 0 0 #x10000 #x10000) ))))

(defmethod sheet-native-transformation ((sheet mirrored-sheet-mixin))
  ;; XXX hm...
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
            (compose-transformations
             (invert-transformation
              (%sheet-mirror-transformation sheet))
             (compose-transformations
              (sheet-native-transformation (sheet-parent sheet))
              (sheet-transformation sheet)))))
      native-transformation))

(defmethod invalidate-cached-transformations ((sheet mirrored-sheet-mixin))
  (with-slots (native-transformation device-transformation) sheet
    (setf ;; native-transformation nil XXX hm...
          device-transformation nil))
  (loop for child in (sheet-children sheet)
        do (invalidate-cached-transformations child)))

(defmethod effective-mirror-region ((sheet mirrored-sheet-mixin))
  ;; XXX is this really needed, can't we deduce this information more easily?
  (let* ((parent (sheet-parent sheet))
         (ancestor (and parent (sheet-mirrored-ancestor parent))))
    (if ancestor
        (region-intersection (sheet-mirror-region sheet)
                             (untransform-region (%sheet-mirror-transformation sheet)
                                                 (effective-mirror-region ancestor)))
      (sheet-mirror-region sheet))))

;;; Internal interface for enabling/disabling motion hints

(defgeneric sheet-motion-hints (sheet)
  (:documentation "Returns t if motion hints are enabled for this sheet"))

(defmethod sheet-motion-hints ((sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (port-motion-hints (port sheet) sheet)))

(defgeneric (setf sheet-motion-hints) (val sheet))

(defmethod (setf sheet-motion-hints) (val (sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (setf (port-motion-hints (port sheet) sheet) val)))

;;;; Coordinate Swizzling

;; This implements what I call "coordinate swizzling", the illusion that
;; sheets can be arbitrary large. The key idea here is that there is a
;; certain kind freedom in choosing the native transformation. A little
;; diagram to illustrate the involved transformations:

;;
;;                  NT                           NT = native transformation
;;    sheet  ---------------->  mirror          PNT = parent's NT
;;      |                         |              MT = mirror transformation
;;      |                         |               T = sheet transformation
;;      |                         |
;;    T |                         | MT
;;      |                         |
;;      |                         |
;;      |                         |
;;      v          PNT            v
;;    parent ----------------> parent
;;                             mirror
;;

;; To setup both the mirror transformation (MR) and the mirror region (MR),
;; we start with the mirror region. The window systems limitations are here:
;; We can only have a certain size and its upper-left corner must be at the
;; origin.

;; Now the parent already has a mirror region (PMR) assigned, which obeys to
;; the very same size restrictions. Since every part of MR outside of (PMR o
;; MT^1) is not visible, the first idea is to just clip it by the visible
;; part:

;;  MR_1 = intersection (SR o NT, PMR o MT^-1)             [mirror space]

;; Since both NT and MT^-1 are not yet known let us reformulate that region
;; in the parent mirror space:

;;  MR_2 = MR_1 o MT                                       [parent mirror space]
;;       = intersection (SR o NT, PMR o MT^-1) o MT
;;       = intersection (SR o NT o MT, PMR o MT^-1 o MT)
;;       = intersection (SR o (T o PNT o MT^-1) o MT, PMR)
;;       = intersection (SR o T o PNT, PMR)

;; MR_2 now is a good candidate for a mirror region. Unfortunately it is
;; still in parent mirror space, so we transform it back, yielding MR_3:

;;  MR_3 = MR_2 o MT^-1
;;       = intersection (SR o T o PNT, PMR) o MT^-1

;; Here the only unknown is the mirror transformation MT, we can still
;; choose any as long as the window system limitations are met for both MR
;; and MT.

;; 1. MT should be a translation, whose delta x and y components are within
;;    limits.

;; 2. The size limitation of MR is already met, since MR_3's size is no
;;    larger than PMR's size (which mets the limitations). [Remember that MT
;;    was defined to be some translation].

;; 3. MR_3's upper left corner should also be at the origin which nicely
;;    defines MT^-1: Just choose this upper left corner coordinates as MT's x
;;    and y deltas.

;; So we can meet all criteria. The NT can easily be set up by the identity:

;;    NT = T o PNT o MT^-1

;;; Notes

;; . when the native transformation changes, we need to:

;;  a. Redraw the mirror's contents since the mapping from the sheet space
;;     to the mirror space (that is the native transformation) just changed. 
;;     Translational changes in the native transformation can be catered by
;;     blittering, but then have a nice synchronization problem: Suppose
;;     a repaint event is underway as we blitter from some region R_1 to
;;     region R_2. Say the repaint event's region intersects with R_1. In
;;     this case we just blittered pixels which were considered dirty into
;;     R_2. Redrawing R_1 now does not repair the defect, since R_2 now also
;;     contains dirty pixels. => oops, redraw error.
;;
;;  b. Since the above above calculation took the parent's native
;;     transformation into account, (and even the naively wanted mirror
;;     region depends on the parent's native transformation), we need to
;;     redo mirror geometry calculation for any child.
;;
;;  c. I imagine more aggressive output records which remember the actual
;;     octets which need to be send to the X server. These would contain
;;     mirror coordinates and will need to be recalculated, when the native
;;     transformation changes.

;; => Changing the native transformation can be expensive, so we want a way
;;    to minimize changes to the native transformation.

;;

;; What did we do? We clipped the wanted mirror region, SR o NT, inside the
;; parent's mirror region to meet the window system limitations. We can make
;; this clip region larger as long as we still come up with an mirror
;; region, which meets the limits.

(defun update-mirror-geometry (sheet &key)
  "This function reflects the current sheet region and sheet transformation
to the mirror. It also sets up the native transformation. This function is
supposed to be called whenever one of the following happens:

  - the sheet's transformation changed
  - the sheet's region changed
  - the parent's native transformation changed
  - the parent's transformation changed
  - the parent's mirror region changed

Also if the sheet's native transformation changes the mirror's contents need
to be redrawn, which is achieved by calling PORT-DIRTY-MIRROR-REGION.

Since changing the sheet's native transformation might thus be expensive,
this function tries to minimize changes to it. (although it does not try
very hard)."
  (let ((old-native-transformation (%%sheet-native-transformation sheet)))
    (cond ((null (sheet-parent sheet))
           ;; Ugh, we have no parent, this must be the graft, we cannot resize it can we?
           nil)
          ;;
          ;; Otherwise, the native transformation has to changed or needs to be computed initially
          ;;
          (t
           (let* ((parent (sheet-parent sheet))
                  (sheet-region-in-native-parent
                   ;; this now is the wanted sheet mirror region
                   (transform-region (sheet-native-transformation parent)
                                     (transform-region (sheet-transformation sheet)
                                                       (sheet-region sheet)))))

             (when (region-equal sheet-region-in-native-parent +nowhere+)
               ;; hmm
               (setf (%sheet-mirror-transformation sheet) (make-translation-transformation -5 -5))
               (setf (%sheet-mirror-region sheet) (make-rectangle* 0 0 1 1))
               (when (sheet-direct-mirror sheet)
                 (port-set-mirror-region (port sheet) (sheet-direct-mirror sheet)
                                         (%sheet-mirror-region sheet))
                 (port-set-mirror-transformation (port sheet)
                                                 (sheet-direct-mirror sheet)
                                                 (%sheet-mirror-transformation sheet)))
               (return-from update-mirror-geometry))
             
             ;; mx1 .. my2 are is now the wanted mirror region in the parent
             ;; coordinate system.
             (with-bounding-rectangle* (mx1 my1 mx2 my2) sheet-region-in-native-parent
               (let (;; pw, ph is the width/height of the parent
                     (pw  (bounding-rectangle-width (sheet-mirror-region parent)))
                     (ph  (bounding-rectangle-height (sheet-mirror-region parent))))
                 (labels ((choose (MT)
                            ;; -> fits-p mirror-region
                            (multiple-value-bind (x1 y1) (transform-position MT 0 0)
                              (let ((x2  (if (<= mx2 pw)
                                             mx2
                                             (floor (+ pw (min mx2 (+ #x8000 x1) #x8000)) 2)))
                                    (y2  (if (<= my2 ph)
                                             my2
                                             (floor (+ ph (min my2 (+ #x8000 y1) #x8000)) 2))))
                                (when (and (< (- x2 x1) #x8000)
                                           (or (<= (max (- pw #x8000) mx1) x1 0) (coordinate= x1 mx1))
                                           (< (- y2 y1) #x8000)
                                           (or (<= (max (- pw #x8000) my1) y1 0) (coordinate= y1 my1))
                                           (> (round (- x2 x1)) 0)
                                           (> (round (- y2 y1)) 0))
                                  (values t (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1)))))))))
                   ;;
                   ;; Try reusing the native transformation:
                   ;;
                   (when old-native-transformation
                     (let ((MT (compose-transformations
                                (compose-transformations
                                 (sheet-native-transformation (sheet-parent sheet))
                                 (sheet-transformation sheet))
                                (invert-transformation old-native-transformation))))
                       (multiple-value-bind (fits-p MR) (choose MT)
                         (when fits-p
                           (setf (%sheet-mirror-region sheet) MR)
                           (setf (%sheet-mirror-transformation sheet) MT)
                           (when (sheet-direct-mirror sheet)
                             (let ((port (port sheet))
                                   (mirror (sheet-direct-mirror sheet)))
                               (port-set-mirror-region port mirror MR)
                               (port-set-mirror-transformation port mirror MT)))
                           (return-from update-mirror-geometry nil) ))))

                   ;;
                   ;; Try reusing the mirror transformation:
                   ;;
                   '
                   (let ((MT (%sheet-mirror-transformation sheet)))
                     (when MT
                       (multiple-value-bind (fits-p MR) (choose MT)
                         (when fits-p
                           (let ((native-transformation
                                  ;; NT = T o PNT o -MT
                                  (compose-transformations
                                   (invert-transformation MT)
                                   (compose-transformations
                                    (sheet-native-transformation (sheet-parent sheet))
                                    (sheet-transformation sheet)))))
                             ;; finally reflect the change to the host window system
                             (setf (%sheet-mirror-region sheet) MR)
                             (setf (%sheet-mirror-transformation sheet) MT)
                             (when (sheet-direct-mirror sheet)
                               (let ((port (port sheet))
                                     (mirror (sheet-direct-mirror sheet)))
                                 (port-set-mirror-region port mirror MR)
                                 (port-set-mirror-transformation port mirror MT)))
                             ;; update the native transformation if neccessary.
                             (unless (and old-native-transformation
                                          (transformation-equal native-transformation old-native-transformation))
                               (invalidate-cached-transformations sheet)
                               (%%set-sheet-native-transformation native-transformation sheet)
                               (when old-native-transformation
                                 (care-for-new-native-transformation
                                  sheet old-native-transformation native-transformation))))
                           (return-from update-mirror-geometry nil)
                           ))))

                   ;; Otherwise just choose

                   ;; Conditions to be met:
                   ;;  x2 < #x8000 + x1
                   ;;  x1 in [max(pw - #x8000, mx1), 0] u {mx1}
                   ;;  x2 in [pw, min (#x8000, mx2)]    u {mx2}
                   ;;
                   ;; It can still happend, that we cannot meet the
                   ;; window system limitations => the sheet is
                   ;; unvisible.
                   (let* ((x1 (if (>= mx1 0) (round mx1) (floor (max (- pw #x8000) mx1) 2)))
                          (y1 (if (>= my1 0) (round my1) (floor (max (- ph #x8000) my1) 2)))
                          (x2 (if (<= mx2 pw) mx2 (floor (+ pw (min mx2 (- #x8000 x1))) 2)))
                          (y2 (if (<= my2 ph) my2 (floor (+ ph (min my2 (- #x8000 y1))) 2)))
                          (MT (make-translation-transformation x1 y1))
                          (MR (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1))))
                          (native-transformation
                           ;; NT = T o PNT o -MT
                           (compose-transformations
                            (invert-transformation MT)
                            (compose-transformations
                             (sheet-native-transformation (sheet-parent sheet))
                             (sheet-transformation sheet))))
                          (old-native-transformation
                           (%%sheet-native-transformation sheet)))

                     (cond ((and (> (round (- x2 x1)) 0)
                                 (> (round (- y2 y1)) 0))
                            ;; finally reflect the change to the host window system
                            (setf (%sheet-mirror-region sheet) MR)
                            (setf (%sheet-mirror-transformation sheet) MT)
                            (when (sheet-direct-mirror sheet)
                              (let ((port (port sheet))
                                    (mirror (sheet-direct-mirror sheet)))
                                (port-set-mirror-region port mirror MR)
                                (port-set-mirror-transformation port mirror MT)))
                            ;; update the native transformation if neccessary.
                            (unless (and old-native-transformation
                                         (transformation-equal native-transformation old-native-transformation))
                              (invalidate-cached-transformations sheet)
                              (%%set-sheet-native-transformation native-transformation sheet)
                              (when old-native-transformation
                                (care-for-new-native-transformation
                                 sheet old-native-transformation native-transformation))))

                           (t
                            (setf (%sheet-mirror-transformation sheet) (make-translation-transformation -5 -5))
                            (setf (%sheet-mirror-region sheet) (make-rectangle* 0 0 1 1))
                            (when (sheet-direct-mirror sheet)
                              (port-set-mirror-region (port sheet) (sheet-direct-mirror sheet)
                                                      (%sheet-mirror-region sheet))
                              (port-set-mirror-transformation (port sheet)
                                                              (sheet-direct-mirror sheet)
                                                              (%sheet-mirror-transformation sheet)))) ))))))))))

(defun care-for-new-native-transformation (sheet old-native-transformation native-transformation)
  "Internal and helper for UPDATE-MIRROR-GEOMETRY. This is called in
   case the native transformation changed and takes care that the
   sheet contents get redrawn as appropriate. It also attempts to
   save some redraws by blittering."
  ;;
  ;; compute D := -NT_old o NT_new
  ;;
  ;; if D is a translation then
  ;;    blitter from: (MR o -D) ^ MR  to: (MR o D) ^ MR
  ;;    clear MR \ (MR o -D)
  ;; else
  ;;    clear MR
  ;;
  (let* (;; Compute the transformation to get from an old coordinate in
         ;; the mirror coordinate system to its new location.
         (delta (compose-transformations
                 native-transformation
                 (invert-transformation old-native-transformation)))
         ;;
         (MR (effective-mirror-region sheet)))
    (declare (ignorable delta))
    ;; When this delta transformation is a translation, we can
    ;; possibly blitter the pixels. Otherwise not, since blittering
    ;; cannot account for say scaling or rotation.
    (cond 
;;;          <-- please leave this code commented out for now -->
;;;          ;; Blittering will never work reliable soon.
;;;          ;; --GB
;;;          ((translation-transformation-p delta)
;;;           ;; We want to bitter. So compute, dMR, the region in mirror
;;;           ;; coordinate space where MR should end up. Clip it to the actual
;;;           ;; mirror, which gives us the destination rectangle. Transform this
;;;           ;; destination back to the old space to get the source rectangle.
;;;           ;; Finally compute the region, which is not occupied by the
;;;           ;; destination and thus must be redrawn.
;;;           ;;
;;;           ;; Note that by using region operations, we automatically take care
;;;           ;; for the case that the window was scrolled too far to reuse any
;;;           ;; pixels.
;;;           (let* ((dMR  (transform-region delta MR))
;;;                  (dest (region-intersection dMR MR))
;;;                  (src  (untransform-region delta dest))
;;;                  (lack (region-difference MR dMR)))
;;;             ;; Now actually blitter, take care for empty regions.
;;;             (unless (or (region-equal src +nowhere+)
;;;                         (region-equal dest +nowhere+))
;;;               (let ((gc (xlib:create-gcontext :drawable (sheet-direct-mirror sheet))))
;;;                 (xlib:copy-area (sheet-direct-mirror sheet) gc
;;;                                 (floor (bounding-rectangle-min-x src))
;;;                                 (floor (bounding-rectangle-min-y src))
;;;                                 (floor (bounding-rectangle-width src))
;;;                                 (floor (bounding-rectangle-height src))
;;;                                 (sheet-direct-mirror sheet)
;;;                                 (floor (bounding-rectangle-min-x dest))
;;;                                 (floor (bounding-rectangle-min-y dest)))) )
;;;             ;; And handle the exposure
;;;             (unless (region-equal lack +nowhere+)
;;;               (xlib:clear-area (sheet-direct-mirror sheet)
;;;                                :x (floor (bounding-rectangle-min-x lack))
;;;                                :y (floor (bounding-rectangle-min-y lack))
;;;                                :width (floor (bounding-rectangle-width lack))
;;;                                :height (floor (bounding-rectangle-height lack))
;;;                                :exposures-p nil)
;;;               (handle-repaint sheet (untransform-region native-transformation lack)))))
          (t
           ;; Full sheet contents need to be redrawn, since transformation is no
           ;; translation.
           (dispatch-repaint sheet
                             (untransform-region native-transformation MR)) ))))


;;; Sheets as bounding rectangles

;; Somewhat hidden in the spec, we read (section 4.1.1 "The Bounding
;; Rectangle Protocol")
;;

;; | bounding-rectangle* region [Generic Function]
;; |
;; |      [...] The argument region must be either a bounded region [...] or
;; |      some other object that obeys the bounding rectangle protocol, such
;; |      as a sheet or an output record. [...]

(defmethod bounding-rectangle* ((sheet sheet))
  (bounding-rectangle* (sheet-region sheet)))

;;; The null sheet

(defclass null-sheet (basic-sheet) ())

