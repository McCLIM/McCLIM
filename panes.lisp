;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2001 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)

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

;;; GENERIC FUNCTIONS

(defgeneric compose-space (pane))
(defgeneric allocate-space (pane width height))
(defgeneric change-space-requirements (pane &rest rest))
(defgeneric note-space-requirements-changed (sheet pane))

;;; PANES

(defconstant +fill+ (expt 10 (floor (log most-positive-fixnum 10))))

(defclass space-requirement () ())

(defclass standard-space-requirement ()
  ((width :initform 1
	  :initarg :width
	  :reader space-requirement-width)
   (max-width :initform 1
	      :initarg :max-width
	      :reader space-requirement-max-width)
   (min-width :initform 1
	      :initarg :min-width
	      :reader space-requirement-min-width)
   (height :initform 1
	   :initarg :height
	   :reader space-requirement-height)
   (max-height :initform 1
	       :initarg :max-height
	       :reader space-requirement-max-height)
   (min-height :initform 1
	       :initarg :min-height
	       :reader space-requirement-min-height) ) )

(defmethod print-object ((space space-requirement) stream)
  (with-slots (width height min-width max-width min-height max-height) space
    (print-unreadable-object (space stream :type t :identity nil)
      (format stream "width: ~S [~S,~S] height: ~S [~S,~S]"
              width
              min-width
              max-width
              height
              min-height
              max-height))))

(defun make-space-requirement (&key (width 1) (height 1)
				    (min-width 0) (min-height 0)
				    (max-width +fill+) (max-height +fill+))
  (assert (<= 0 min-width width max-width) (min-width width max-width))
  (assert (<= 0 min-height height max-height) (min-height height max-height))
  (make-instance 'standard-space-requirement
    :width width
    :max-width max-width
    :min-width min-width
    :height height
    :max-height max-height
    :min-height min-height))

(defmethod space-requirement-components ((space-req standard-space-requirement))
  (with-slots (width min-width max-width height min-height max-height) space-req
    (values width min-width max-width height min-height max-height)))

(defun space-requirement-combine* (function sr1 &key (width 0) (min-width 0) (max-width 0)
                                                (height 0) (min-height 0) (max-height 0))
  (apply #'make-space-requirement
         (mapcan #'(lambda (c1 c2 keyword)
                     (list keyword (funcall function c1 c2)))
                 (multiple-value-list (space-requirement-components sr1))
                 (list width min-width max-width height min-height max-height)
                 '(:width :min-width :max-width :height :min-height :max-height))))

(defun space-requirement-combine (function sr1 sr2)
  (multiple-value-bind (width min-width max-width height min-height max-height)
      (space-requirement-components sr2)
    (space-requirement-combine* function
                                sr1
                                :width width :min-width min-width :max-width max-width
                                :height height :min-height min-height :max-height max-height)))

(defun space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

(defun space-requirement+* (space-req &key (width 0) (min-width 0) (max-width 0)
                                      (height 0) (min-height 0) (max-height 0))
  (space-requirement-combine* #'+
                              space-req
                              :width width :min-width min-width :max-width max-width
                              :height height :min-height min-height :max-height max-height))
  
;; Macros for quick access to space-requirement slots.
(defmacro sr-width (pane)
  `(space-requirement-width (pane-space-requirement ,pane)))
(defmacro sr-height (pane)
  `(space-requirement-height (pane-space-requirement ,pane)))
(defmacro sr-max-width (pane)
  `(space-requirement-max-width (pane-space-requirement ,pane)))
(defmacro sr-max-height (pane)
  `(space-requirement-max-height (pane-space-requirement ,pane)))
(defmacro sr-min-width (pane)
  `(space-requirement-min-width (pane-space-requirement ,pane)))
(defmacro sr-min-height (pane)
  `(space-requirement-min-height (pane-space-requirement ,pane)))

(defclass pane (standard-sheet-input-mixin
		temporary-medium-sheet-output-mixin
		sheet-transformation-mixin basic-sheet)
  (
   #+ignore(foreground :initarg :foreground
		       :initform +black+
		       :reader pane-foreground)
   #+ignore(background :initarg :background
		       :initform +white+
		       :reader pane-background)
   (text-style :initarg :text-style :initform nil :reader pane-text-style)
   (name :initarg :name :initform "(Unnamed Pane)" :reader pane-name)
   (manager :initarg :manager)
   (port :initarg :port)
   (frame :initarg :frame :initform *application-frame* :reader pane-frame)
   (enabledp :initform nil :initarg :enabledp :accessor pane-enabledp)
   (sr-width :initform nil :initarg :width)
   (sr-height :initform nil :initarg :height)
   (sr-max-width :initform nil :initarg :max-width)
   (sr-max-height :initform nil :initarg :max-height)
   (sr-min-width :initform nil :initarg :min-width)
   (sr-min-height :initform nil :initarg :min-height)
   (space-requirement :initform nil :accessor pane-space-requirement)
   ;; New sizes, for allocating protocol
   (new-width :initform nil)
   (new-height :initform nil)
   )
  (:documentation ""))

(defun panep (x)
  (typep x 'pane))

(defun make-pane (type &rest args)
  (apply #'make-pane-1 *pane-realizer* *application-frame* type args))

#||
;; dummy but useful
(defmacro do-in (size min max)
  `(max ,min (min ,max ,size)))
;; moved as clamp into utils.lisp
||#

(defun get-pane-width (pane)
  ;; Give the region size if already set, otherwise, give the space-requirement
  (let ((width (rectangle-width (sheet-region pane))))
    (if (or (not width) (zerop width)) (sr-width pane) width)))

(defun get-pane-height (pane)
  ;; Give the region size if already set, otherwise, give the space-requirement
  (let ((height (rectangle-height (sheet-region pane))))
    (if (or (not height) (zerop height)) (sr-height pane) height)))

(defun make-extremums-children-ratio-width (pane children)
  (with-slots (sorted-max-w-r sorted-min-w-r) pane
    (loop for child in children
	  for max-width-ratio = (/ (sr-width child) (sr-max-width child))
	  for min-width-ratio = (/ (sr-width child) (sr-min-width child))
	  collect (cons max-width-ratio child) into max-w-ratios
	  collect (cons min-width-ratio child) into min-w-ratios
	  finally (setf sorted-max-w-r (sort max-w-ratios #'> :key #'car)
			sorted-min-w-r (sort min-w-ratios #'< :key #'car)))))

(defun make-extremums-children-ratio-height (pane children)
  (with-slots (sorted-max-h-r sorted-min-h-r) pane
    (loop for child in children
	  for max-height-ratio = (/ (sr-height child) (sr-max-height child))
	  for min-height-ratio = (/ (sr-height child) (sr-min-height child))
	  collect (cons max-height-ratio child) into max-h-ratios
	  collect (cons min-height-ratio child) into min-h-ratios
	  finally (setf sorted-max-h-r (sort max-h-ratios #'> :key #'car)
			sorted-min-h-r (sort min-h-ratios #'< :key #'car)))))

(defun allocate-space-internal-width (pane size)
  (loop with sum = (slot-value pane 'sum-width)
	and maxs = (slot-value pane 'sorted-max-w-r)
	and mins = (slot-value pane 'sorted-min-w-r)
	for (nil . child) in (if (>= size sum) maxs mins)
	and box-size = sum then (- box-size (get-pane-width child))
	for tmp = (round (/ (* size (get-pane-width child)) box-size))
	for new-size = (clamp tmp (sr-min-width child) (sr-max-width child))
	for new-sum = new-size then (+ new-sum new-size)
	do (decf size new-size)
	   (setf (slot-value child 'new-width) new-size)
	finally
	   (setf (slot-value pane 'sum-width) new-sum)))

(defun allocate-space-internal-height (pane size)
  (loop with sum = (slot-value pane 'sum-height) 
	and maxs = (slot-value pane 'sorted-max-h-r)
	and mins = (slot-value pane 'sorted-min-h-r)
	for (nil . child) in (if (>= size sum) maxs mins)
	and box-size = sum then (- box-size (get-pane-height child))
	for tmp = (round (/ (* size (get-pane-height child)) box-size))
	for new-size = (clamp tmp (sr-min-height child) (sr-max-height child))
	for new-sum = new-size then (+ new-sum new-size)
	do (decf size new-size)
	   (setf (slot-value child 'new-height) new-size)
	finally
	   (setf (slot-value pane 'sum-height) new-sum)))

(defmethod medium-foreground ((pane pane))
  (medium-foreground (sheet-medium pane)))

(defmethod (setf medium-foreground) (ink (pane pane))
  (setf (medium-foreground (sheet-medium pane)) ink))

(defmethod medium-background ((pane pane))
  (medium-background (sheet-medium pane)))

(defmethod (setf medium-background) (ink (pane pane))
  (setf (medium-background (sheet-medium pane)) ink))

(defmethod compose-space ((pane pane))
  (make-space-requirement :width 200
			  :height 200))

(defmethod compose-space :around ((pane pane))
  (with-slots (sr-width sr-height sr-max-width
                        sr-max-height sr-min-width sr-min-height
                        space-requirement) pane
    (unless space-requirement
      (let ((request (call-next-method)))
	(when (spacing-p pane)
	  (with-slots (margin-width margin-height
                                    margin-max-width margin-max-height
                                    margin-min-width margin-min-height) pane
	    (let ((child (first (sheet-children pane))))
	      (setf sr-width (+ margin-width (sr-width child))
		    sr-height (+ margin-height (sr-height child))
		    sr-max-width (+ margin-max-width (sr-max-width child))
		    sr-max-height (+ margin-max-height (sr-max-height child))
		    sr-min-width (+ margin-min-width (sr-min-width child))
		    sr-min-height (+ margin-min-height (sr-min-height child)) ))))
        (let* ((max-width (or sr-max-width
                              sr-width
                              (space-requirement-max-width request)))
               (max-height (or sr-max-height
                               sr-height
                               (space-requirement-max-height request)))
               (min-width (or sr-min-width
                              sr-width
                              (space-requirement-min-width request)))
               (min-height (or sr-min-height
                               sr-height
                               (space-requirement-min-height request)))
               (width (clamp (or sr-width (space-requirement-width request))
                             min-width
                             max-width))
               (height (clamp (or sr-height (space-requirement-height request))
                              min-height
                              max-height)))
          (setf space-requirement
                (make-space-requirement
                 :width width
                 :height height
                 :max-width max-width
                 :max-height max-height
                 :min-width min-width
                 :min-height min-height))))
      (compute-extremum pane))
    space-requirement))

(defmethod allocate-space ((pane pane) width height)
  (resize-sheet pane width height))

(defmethod compute-extremum ((pane pane))
  (declare (ignorable pane))
  nil)

(defmethod change-space-requirements ((pane pane) &rest rest)
  (declare (ignore rest))
  (values))

(defmethod note-space-requirements-changed (sheet (pane pane))
  (declare (ignore sheet))
  (setf (pane-space-requirement pane) nil)
  (compose-space pane)
  (if (or (top-level-sheet-pane-p pane) 
	  (restraining-pane-p pane)
	  (and (slot-value pane 'sr-width) 
	       (slot-value pane 'sr-height)))
      (allocate-space pane (sr-width pane) (sr-height pane))
      (note-space-requirements-changed (sheet-parent pane) pane)))

;;; WINDOW STREAM

(defclass window-stream (standard-extended-output-stream 
			 standard-extended-input-stream)
  (
   )
  )

;;; BASIC-PANE

(defclass basic-pane (window-stream
		      sheet-parent-mixin
		      mirrored-sheet-mixin
		      permanent-medium-sheet-output-mixin
		      pane)
  ()
  )

(defmethod initialize-instance :after ((pane basic-pane) &rest args
				       &key (background nil)
					    (foreground nil)
				       &allow-other-keys)
  (declare (ignore args))
  (when background
    (setf (medium-background pane) background))
  (when foreground
    (setf (medium-foreground pane) foreground)))

;;; COMPOSITE PANE

(defclass composite-pane (sheet-multiple-child-mixin
			  basic-pane)
  (
   ;; Caution :
   ;; For respect of children wishes when increasing or decreasing size.
   ;; Children are sort by (/ max-size size) and (/min-size size)
   (sorted-max-w-r :initform nil)
   (sorted-max-h-r :initform nil)
   (sorted-min-w-r :initform nil)
   (sorted-min-h-r :initform nil)
   (sum-width :initform nil)
   (sum-height :initform nil))
  (:documentation "protocol class"))


(defmethod initialize-instance :after ((pane composite-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (if contents
      (unless (typep (first contents) 'list) ;; temporary medecine
	(loop for child in (reverse contents)
	      do (sheet-adopt-child pane child)))))


(defmacro changing-space-requirement (&body body &key resize-frame)
  (declare (ignore resize-frame))
  `(progn
     ,@body))

(defmethod change-space-requirements ((pane composite-pane)
				      &key resize-frame
				      (width nil width-p)
				      (min-width nil min-width-p)
				      (max-width nil max-width-p)
				      (height nil height-p)
				      (min-height nil min-height-p)
				      (max-height nil max-height-p))
  (with-slots (sr-width sr-height sr-max-width sr-max-height
	       sr-min-width sr-min-height) pane
    (when width-p (setf sr-width width))
    (when min-width-p (setf sr-min-width min-width))
    (when max-width-p (setf sr-max-width max-width))
    (when height-p (setf sr-height height))
    (when min-height-p (setf sr-min-height min-height))
    (when max-height-p (setf sr-max-height max-height)))
  (if resize-frame
      ;; we didn't find the :resize-frame option in define-application-frame
      (layout-frame (pane-frame pane))
      (note-space-requirements-changed (sheet-parent pane) pane)))

;;; SINGLE-CHILD-COMPOSITE PANE

(defclass single-child-composite-pane (sheet-single-child-mixin
				       basic-pane)
  (
   )
  )


(defmethod initialize-instance :after ((pane single-child-composite-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (when contents
    (sheet-adopt-child pane (first contents))))

(defmethod compose-space ((pane single-child-composite-pane))
  (if (sheet-children pane)
      (compose-space (first (sheet-children pane)))
      (make-space-requirement)))

(defmethod allocate-space ((pane single-child-composite-pane) width height)
  (when (first (sheet-children pane))
    (resize-sheet pane width height)
    (allocate-space (first (sheet-children pane)) width height)))

;;; TOP-LEVEL-SHEET

(defclass top-level-sheet-pane (composite-pane)
  ()
  (:documentation "For the first pane in the architecture"))

(defun top-level-sheet-pane-p (pane)
  (typep pane 'top-level-sheet-pane))

(defmethod compose-space ((pane top-level-sheet-pane))
  (compose-space (first (sheet-children pane))))

(defmethod allocate-space ((pane top-level-sheet-pane) width height)
  (when (first (sheet-children pane))
    (allocate-space 
        (first (sheet-children pane))
	(clamp width (sr-min-width pane) (sr-max-width pane))
	(clamp height (sr-min-height pane) (sr-max-height pane)))))

(defmethod dispatch-event ((pane top-level-sheet-pane) event)
  (handle-event pane event))

(defmethod handle-event ((pane top-level-sheet-pane)
			 (event window-configuration-event))
  (let ((x (window-configuration-event-x event))
	(y (window-configuration-event-y event))
	(width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    ;; avoid goint into an infinite loop by not using (setf sheet-transformation)
    (setf (slot-value pane 'transformation)
	  (make-translation-transformation x y))
    ;; avoid goint into an infinite loop by not using (setf sheet-region)
    (setf (slot-value pane 'region)
	  (make-bounding-rectangle 0 0 width height))
    (allocate-space pane width height)))

(defclass unmanaged-top-level-sheet-pane (top-level-sheet-pane)
  ()
  (:documentation "Top-level sheet without window manager intervention"))

(defmethod sheet-native-transformation ((sheet top-level-sheet-pane))
  +identity-transformation+)

;;; SHEET 

;; FIXME: Should it exists ???
(defmethod note-space-requirements-changed ((sheet sheet) (pane composite-pane))
  (values))


;;; HBOX-PANE

(defclass hbox-pane (composite-pane)
  ()
  (:documentation ""))

(defmacro horizontally ((&rest options
			 &key (equalize-height t)
			 &allow-other-keys) &body contents)
  (remf options :equalize-height)
  (if equalize-height
      `(make-pane 'hrack-pane ,@options :contents (list ,@contents))
      `(make-pane 'hbox-pane ,@options :contents (list ,@contents))))

(defmethod compose-space ((box hbox-pane))
  (loop with space = (make-space-requirement)
	for child in (sheet-children box)	
	for request = (compose-space child)
	sum (get-pane-width child) into sum-width
	do (setq space (space-requirement+* space
                         :width (space-requirement-width request)
                         :max-width (space-requirement-max-width request)
                         :min-height (space-requirement-min-width request)))
         (setq space (space-requirement-combine* #'max space
                         :height (space-requirement-height request)
                         :max-height (space-requirement-max-height request)
                         :min-height (space-requirement-min-height request)))
	finally (progn		  
		  (setf (slot-value box 'sum-width) sum-width)
		  (return space))))

(defmethod compute-extremum ((box hbox-pane))
  (make-extremums-children-ratio-width box (sheet-children box)))

(defmethod allocate-space ((box hbox-pane) width height)
  (resize-sheet box width height)
  (allocate-space-internal-width box width)
  (loop for child in (sheet-children box)
	for nw = (slot-value child 'new-width)
	and x = 0 then (+ x nw)
	and nh = (/ (* height (get-pane-height child)) (get-pane-height box))
        do (move-sheet child x 0)
	   (setf nh (clamp nh (sr-min-height child) (sr-max-height child)))
	   (allocate-space child nw (round nh))))

;;; VBOX-PANE

(defclass vbox-pane (composite-pane)
  ()
  (:documentation ""))

(defmacro vertically ((&rest options
		       &key (equalize-width t)
		       &allow-other-keys) &body contents)
  (remf options :equalize-width)
  (if equalize-width
      `(make-pane 'vrack-pane ,@options :contents (list ,@contents))
      `(make-pane 'vbox-pane ,@options :contents (list ,@contents))))

(defmethod compose-space ((box vbox-pane))
  (loop with space = (make-space-requirement)
	for child in (sheet-children box)
	for request = (compose-space child)
	sum (get-pane-height child) into sum-height
	do (setq space (space-requirement-combine* #'max space
                         :width (space-requirement-width request)
                         :max-width (space-requirement-max-width request)
                         :min-height (space-requirement-min-width request)))
         (setq space (space-requirement+* space
                         :height (space-requirement-height request)
                         :max-height (space-requirement-max-height request)
                         :min-height (space-requirement-min-height request)))
	finally (progn 
		  (setf (slot-value box 'sum-height) sum-height)
		  (return space))))
	   
(defmethod compute-extremum ((box vbox-pane))
  (make-extremums-children-ratio-height box (sheet-children box)))

(defmethod allocate-space ((box vbox-pane) width height)
  (resize-sheet box width height)
  (allocate-space-internal-height box height)
  (loop for child in (sheet-children box)
	  for nh = (slot-value child 'new-height)
	  and y = 0 then (+ y nh)
	  and nw = (/ (* width (get-pane-width child)) (get-pane-width box))
	  do (move-sheet child 0 y)
	     (setf nw (clamp nw (sr-min-width child) (sr-max-width child)))
	     (allocate-space child (round nw) nh)))
  
;;; HRACK-PANE

(defclass hrack-pane (composite-pane)
  ()
  (:documentation ""))

(defmethod compose-space ((rack hrack-pane))
  (loop for child in (sheet-children rack)
        for sr = (compose-space child)
        maximize (space-requirement-height sr) into height
        maximize (space-requirement-min-height sr) into min-height
        minimize (space-requirement-max-height sr) into max-height
        sum (space-requirement-width sr) into width
        sum (space-requirement-min-width sr) into min-width
        sum (space-requirement-max-width sr) into max-width
        finally
        (progn
          (setf (slot-value rack 'sum-width) (max min-width width))
          (return (make-space-requirement
                   :min-height  min-height
                   :height      (max min-height height)
                   :max-height  (max max-width height)
                   :min-width min-width
                   :width     (max min-width width)
                   :max-width (max max-width width))) )))

(defmethod compute-extremum ((rack hrack-pane))
  (make-extremums-children-ratio-width rack (sheet-children rack)))

(defmethod allocate-space ((rack hrack-pane) width height)
  (resize-sheet rack width height)
  (allocate-space-internal-width rack width)
  (loop for child in (sheet-children rack)
	for nw = (slot-value child 'new-width)
	and x = 0 then (+ x (round nw))
        do (move-sheet child x 0)
	   (allocate-space child nw height)))

;;; VRACK-PANE

(defclass vrack-pane (composite-pane)
  ()
  (:documentation ""))

(defmethod compose-space ((rack vrack-pane))
  (loop for child in (sheet-children rack)
        for sr = (compose-space child)
        maximize (space-requirement-width sr) into width
        maximize (space-requirement-min-width sr) into min-width
        minimize (space-requirement-max-width sr) into max-width
        sum (space-requirement-height sr) into height
        sum (space-requirement-min-height sr) into min-height
        sum (space-requirement-max-height sr) into max-height
        finally
        (progn
          (setf (slot-value rack 'sum-height) (max min-height height))
          (return (make-space-requirement
                   :min-width  min-width
                   :width      (max min-width width)
                   :max-width  (max max-height width)
                   :min-height min-height
                   :height     (max min-height height)
                   :max-height (max max-height height))) )))

(defmethod compute-extremum ((rack vrack-pane))
  (make-extremums-children-ratio-height rack (sheet-children rack)))
  
(defmethod allocate-space ((rack vrack-pane) width height)
  (resize-sheet rack width height)
  (allocate-space-internal-height rack height)
  (loop for child in (sheet-children rack)
	for nh = (slot-value child 'new-height)
	and y = 0 then (+ y nh)
        do (move-sheet child 0 y)
	   (allocate-space child width nh)))

;;; TABLE PANE

(defclass table-pane (composite-pane)
  ((number-per-line :type integer :initform 0 :accessor table-pane-number)
   ;; the same list that is given during creation
   (logical-children-layout :initform nil :reader format-children)
   )
  (:documentation "The table layout implies that each colums has the same width
 and each lines has the same height - same rules for max and min -"))

(defmethod initialize-instance :before ((table table-pane)
					&rest args
					&key contents
					&allow-other-keys)
  (declare (ignore args))
  (unless (apply #'= (mapcar #'length contents))
    (error "The variable contents hasn't the good format")))

(defmethod initialize-instance :after ((table table-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (loop for children in (reverse contents)
	do (loop for child in (reverse children)
		 do (sheet-adopt-child table child)))
  (setf (table-pane-number table) (length (first contents))
	(slot-value table 'logical-children-layout) contents))

(defun table-pane-p (pane)
  (typep pane 'table-pane))

(defmacro tabling ((&rest options &key (grid t) &allow-other-keys) &body contents)
  (if grid
      `(make-pane 'grid-pane ,@options :contents (list ,@contents))
      `(make-pane 'table-pane ,@options :contents (list ,@contents))))

(defmethod compose-space ((table table-pane))
  (mapc #'compose-space (sheet-children table))
  (let* ((space (make-space-requirement))
	 (nb-children-p-l (table-pane-number table))
	 (nb-children-p-c (/ (length (sheet-children table)) nb-children-p-l))
	 (max-size (max nb-children-p-l nb-children-p-c))
	 (space-vec (make-array (list 6 max-size) :initial-element 0)))
    (loop for i from 0 below (second (array-dimensions space-vec))
	  do (incf (aref space-vec 2 i) (if (< i nb-children-p-l) 500000 0))
	     (incf (aref space-vec 3 i) 500000))
    (loop for child in (sheet-children table) and i from 0
	  for c = (mod i nb-children-p-l)
	  and l = 0 then (incf l (if (zerop c) 1 0)) 
	  when (and (zerop i) (zerop l)) sum (get-pane-width child) into sum-width
	  when (zerop c) sum (get-pane-height child) into sum-height 
	  do (setf (aref space-vec 0 c)
		   (max (sr-width child) (aref space-vec 0 c))
		   (aref space-vec 2 c)
		   (min (sr-max-width child) (aref space-vec 2 c))
		   (aref space-vec 4 c)
		   (max (sr-min-width child) (aref space-vec 4 c))
		   (aref space-vec 1 l)
		   (max (sr-height child) (aref space-vec 1 l))
		   (aref space-vec 3 l)
		   (min (sr-max-height child) (aref space-vec 3 l))
		   (aref space-vec 5 l)
		   (max (sr-min-height child) (aref space-vec 5 l)))
	  finally (setf (slot-value table 'sum-width) sum-width
			(slot-value table 'sum-height) sum-height))
    (loop for i from 0 below (second (array-dimensions space-vec))
	  do (incf (space-requirement-width space) (aref space-vec 0 i))
	     (incf (space-requirement-height space) (aref space-vec 1 i))
	     (incf (space-requirement-max-width space) (aref space-vec 2 i))
	     (incf (space-requirement-max-height space) (aref space-vec 3 i))
	     (incf (space-requirement-min-width space) (aref space-vec 4 i))
	     (incf (space-requirement-min-height space) (aref space-vec 5 i)))
    space))

(defmethod compute-extremum ((table table-pane))
  ;; Because in a table-pane each columns cells has the same width
  ;; and each lines cells has the same height, will make a line-reference
  ;; and a column reference. (first line and column of the table.
  ;; (ie. They will represent the size reference for lines and columns)
  (make-extremums-children-ratio-height
       table (mapcar #'car (format-children table)))
  (make-extremums-children-ratio-width
       table (car (format-children table))))

(defmethod allocate-space ((table table-pane) width height)
  (resize-sheet table width height)
  (allocate-space-internal-height table height)
  (allocate-space-internal-width table width)
  (loop for children in (format-children table)
	for line-ref in (mapcar #'car (format-children table))
	for new-height = (slot-value line-ref 'new-height)
	and y = 0 then (+ y new-height)
	do (loop for child in children
		 for ref in (car (format-children table))
		 for new-width = (slot-value ref 'new-width)
		 and x = 0 then (+ x new-width)
                 do (move-sheet child x y)
		    (allocate-space child new-width new-height))))

;(defmethod sheet-adopt-child :before ((table table-pane) child)
;  (declare (ignore child))
;  (when (= (length (sheet-children table)) (table-pane-number table))
;    (error "The table can't adopt more childs than specified by the table-number")))

(defmethod sheet-disowned-child :before ((table table-pane) child
					 &key (error-p t))
  (declare (ignore child error-p))
  (error "The table pane can't disown one of its child"))


;;; GRID PANE

(defclass grid-pane (table-pane) 
  ()
  (:documentation 
   "Be careful : each cells has the same size in the two dimentions.
 In other words : if the cell sizes are width, height then
  width  = grid-width / number of children per line
  height = grid-height / number of children per column.
=====> this is for all cells."))

(defun grid-p (pane)
  (typep pane 'grid-pane))

(defmethod compose-space ((grid grid-pane))
  (mapc #'compose-space (sheet-children grid))
  (loop with nb-children-pl = (table-pane-number grid)
	with nb-children-pc = (/ (length (sheet-children grid)) nb-children-pl)
	for child in (sheet-children grid)
	and width = 0 then (max width (sr-width child))
	and height = 0 then (max height (sr-height child))
	and max-width = 5000000 then (min max-width (sr-min-width child))
	and max-height = 5000000 then (min max-height (sr-max-height child))
	and min-width = 0 then (max min-width (sr-min-width child))
	and min-height = 0 then (max min-height (sr-min-height child))
	finally (return 
		 (make-space-requirement
		  :width (* width nb-children-pl)
		  :height (* height nb-children-pc)
		  :max-width (* width nb-children-pl)
		  :max-height (* max-height nb-children-pc)
		  :min-width (* min-width nb-children-pl)
		  :min-height (* min-height nb-children-pc)))))
     
(defmethod allocate-space ((grid grid-pane) width height)
  (resize-sheet grid width height)
  (loop with nb-kids-p-l = (table-pane-number grid)
	with nb-kids-p-c = (/ (length (sheet-children grid)) nb-kids-p-l)
	for children in (format-children grid) 
	for c from nb-kids-p-c downto 1
	for tmp-height = height then (decf tmp-height new-height)
	for new-height = (/ tmp-height c)
	for y = 0 then (+ y new-height)
	do (loop for child in children
		 for l from nb-kids-p-l downto 1
		 for tmp-width = width then (decf tmp-width new-width)
		 for new-width = (/ tmp-width l)
		 for x = 0 then (+ x new-width)
		 do (move-sheet child x y)
		    (allocate-space child (round new-width) (round new-height)))))

;;; SPACING PANE

(defclass spacing-pane (composite-pane)
  ((margin-width :initform nil :initarg :width)
   (margin-height :initform nil :initarg :height)
   (margin-max-width :initform nil :initarg :max-width)
   (margin-max-height :initform nil :initarg :max-height)
   (margin-min-width :initform nil :initarg :min-width)
   (margin-min-height :initform nil :initarg :min-height))
  (:documentation "The spacing pane will create a margin for his child.
The margin sizes (w h) are given with the :width and :height initargs.
During realization the child of the spacing will have as cordinates
 x = w/2 , y = h/2."))

(defmethod initialize-instance :after ((spacing spacing-pane) &rest ignore)
  (declare (ignore ignore))
  (with-slots (margin-width margin-height
	       margin-max-width margin-max-height
	       margin-min-width margin-min-height) spacing
    (setf margin-width (or margin-width 0)
	  margin-height (or margin-height 0)
	  margin-max-width (or margin-max-width margin-width)
	  margin-max-height (or margin-max-height margin-height)
	  margin-min-width (or margin-min-width margin-width)
	  margin-min-height (or margin-min-height margin-height))))

(defun spacing-p (pane)
  (typep pane 'spacing-pane))

(defmacro spacing ((&rest options) &body contents)
  `(make-pane 'spacing-pane ,@options :contents (list ,@contents)))

(defmethod compose-space ((spacing spacing-pane))
  (compose-space (first (sheet-children spacing))))

(defmethod allocate-space ((spacing spacing-pane) width height)
  (resize-sheet spacing width height)
  (let* ((child (first (sheet-children spacing)))
	 (margin-width (- (sr-width spacing) (sr-width child)))
	 (margin-height (- (sr-height spacing) (sr-height child))))
    (setf margin-width
	  (clamp (round (/ (* width margin-width) (get-pane-width spacing)))
		 (slot-value spacing 'margin-min-width)
		 (slot-value spacing 'margin-max-width))
	  margin-height
	  (clamp (round (/ (* height margin-height) (get-pane-height spacing)))
		 (slot-value spacing 'margin-min-height)
		 (slot-value spacing 'margin-max-height))
	  (sheet-transformation child)
	  (make-translation-transformation (/ margin-width 2)
					   (/ margin-height 2)))
    (allocate-space child (- width margin-width) (- height margin-height))))

;;; BORDER-PANE

(defclass border-pane (spacing-pane)
  ((border-width :initarg :border-width :initform 1 :reader border-pane-width)
   )
  (:documentation ""))

(defmethod initialize-instance :after ((bp border-pane) &rest ignore)
  (declare (ignore ignore))
  (with-slots (border-width) bp
    (let ((2*border-width (* 2 border-width)))
      (setf (slot-value bp 'margin-width) 2*border-width
	    (slot-value bp 'margin-height) 2*border-width 
	    (slot-value bp 'margin-max-width) 2*border-width
	    (slot-value bp 'margin-max-height) 2*border-width
	    (slot-value bp 'margin-min-width) 2*border-width
	    (slot-value bp 'margin-min-height) 2*border-width))))

(defmacro bordering ((&rest options) &body contents)
  `(make-pane 'border-pane ,@options :contents (list ,@contents)))

(defmethod allocate-space ((bp border-pane) width height)
  (resize-sheet bp width height)
  (when (first (sheet-children bp))
    (let ((border-width (border-pane-width bp)))
      (setf (sheet-transformation (first (sheet-children bp)))
	    (make-translation-transformation border-width border-width))
      (allocate-space (first (sheet-children bp))
		      (- width (* 2 border-width))
		      (- height (* 2 border-width))))))

;;; RAISED PANE

(defclass raised-pane (border-pane) ())

(defmacro raising ((&rest options) &body contents)
  `(make-pane 'raised-pane ,@options :contents (list ,@contents)))

(defmethod dispatch-repaint ((raised-pane raised-pane) region)
  (repaint-sheet raised-pane region))

(defmethod handle-event ((pane raised-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod repaint-sheet ((pane raised-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
      (draw-edges-lines* pane 0 0 (- x2 x1 1) (- y2 y1 1)))))

;;; RESTRAINING PANE

(defclass restraining-pane (composite-pane) ())

(defun restraining-pane-p (pane)
  (typep pane 'restraining-pane))

(defmacro restraining ((&rest options) &body contents)
  `(make-pane 'restraining-pane ,@options :contents (list ,@contents)))

;;; BBOARD PANE

(defclass bboard-pane (composite-pane) ())

(defmethod compose-space ((bboard bboard-pane))
  (make-space-requirement :width 300 :height 300))

;;; VIEWPORT

(defclass viewport-pane (single-child-composite-pane) ())


;;; SCROLLER-PANE

(defparameter *scrollbar-thickness* 16)

(defclass scroller-pane (composite-pane)
  ((scroll-bar :type (member '(t :vertical :horizontal))
	       :initform t
	       :initarg :scroll-bar
	       :accessor scroller-pane-scroll-bar)
   (viewport :initform nil)
   (vscrollbar :initform nil)
   (hscrollbar :initform nil)))

(defun scroll-page-callback (scroll-bar direction)
  (with-slots (client orientation) scroll-bar
    (multiple-value-bind (old-x old-y)
        (untransform-position (sheet-transformation client)
                              0 0)
      (if (eq orientation :vertical)
          (scroll-extent client
                         old-x
                         (- old-y
                            (* direction
                               (bounding-rectangle-height
                                (pane-viewport-region client)))))
          (scroll-extent client
                         (- old-x
                            (* direction
                               (bounding-rectangle-width
                                (pane-viewport-region client))))
                         old-y)))))

(defun scroll-line-callback (scroll-bar direction)
  (with-slots (client orientation) scroll-bar
    (multiple-value-bind (old-x old-y)
        (untransform-position (sheet-transformation client)
                              0 0)
      (if (eq orientation :vertical)
          (scroll-extent client
                         old-x
                         (- old-y
                            (* direction
                               (stream-line-height client))))
          (scroll-extent client
                         (- old-x
                            (* direction
                               (stream-line-height client)))
                         old-y)))))

(defmethod initialize-instance :after ((pane scroller-pane) &rest args)
  (declare (ignore args))
  (with-slots (scroll-bar viewport vscrollbar hscrollbar) pane
    (setq viewport (first (sheet-children pane)))
    (when (not (eq scroll-bar :horizontal))
      (setq vscrollbar
            (make-pane 'scroll-bar-pane
                       :orientation :vertical
                       :client (first (sheet-children viewport))
                       :drag-callback
                       #'(lambda (gadget new-value)
                           (let ((old-x (bounding-rectangle-min-x
                                         (pane-viewport-region
                                          (gadget-client gadget)))))
                             (scroll-extent (gadget-client gadget)
                                            old-x new-value)))
                       :scroll-up-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar 1))
                       :scroll-down-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar -1))
                       :scroll-up-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar 1))
                       :scroll-down-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar -1))
                       :foreground +grey+
                       :background +grey40+))
      (sheet-adopt-child pane vscrollbar))
    (when (not (eq scroll-bar :vertical))
      (setq hscrollbar
            (make-pane 'scroll-bar-pane
                       :orientation :horizontal
                       :length (bounding-rectangle-width (sheet-region viewport))
                       :client (first (sheet-children viewport))
                       :drag-callback
                       #'(lambda (gadget new-value)
                           (let ((old-y (bounding-rectangle-min-y
                                         (pane-viewport-region
                                          (gadget-client gadget)))))
                             (scroll-extent (gadget-client gadget)
                                            new-value old-y)))
                       :scroll-up-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar 1))
                       :scroll-down-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar -1))
                       :scroll-up-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar 1))
                       :scroll-down-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar -1))
                       :foreground +grey+
                       :background +grey40+))
      (sheet-adopt-child pane hscrollbar))))
    
(defmacro scrolling ((&rest options) &body contents)
  `(let ((viewport (make-pane 'viewport-pane :contents (list ,@contents))))
     (make-pane 'scroller-pane ,@options :contents (list viewport))))

(defmethod compose-space ((pane scroller-pane))
  (with-slots (viewport vscrollbar hscrollbar) pane
    (if viewport
        (let ((req (compose-space viewport)))
          (when vscrollbar
            (setq req (space-requirement+* req
                                           :height *scrollbar-thickness*
                                           :min-height *scrollbar-thickness*
                                           :max-height *scrollbar-thickness*)))
          (when hscrollbar
            (setq req (space-requirement+* req
                                           :width *scrollbar-thickness*
                                           :min-width *scrollbar-thickness*
                                           :max-width *scrollbar-thickness*)))
          req)
        (make-space-requirement))))

(defmethod allocate-space ((pane scroller-pane) width height)
  (resize-sheet pane width height)
  (with-slots (viewport vscrollbar hscrollbar) pane
    (when viewport
      (setf (sheet-transformation viewport)
	(make-translation-transformation (if vscrollbar *scrollbar-thickness* 0) 0))
      (allocate-space viewport
		      (if vscrollbar (- width *scrollbar-thickness*) width)
		      (if hscrollbar (- height *scrollbar-thickness*) height)))
    (when vscrollbar
      (setf (sheet-transformation vscrollbar)
	(make-translation-transformation 0 0))
      (allocate-space vscrollbar
		      *scrollbar-thickness*
		      (if hscrollbar (- height *scrollbar-thickness*) height)))
    (when hscrollbar
      (setf (sheet-transformation hscrollbar)
	(make-translation-transformation (if vscrollbar
                                             *scrollbar-thickness*
                                             0)
                                         (- height *scrollbar-thickness*)))
      (allocate-space hscrollbar
		      (if vscrollbar (- width *scrollbar-thickness*) width)
		      *scrollbar-thickness*))))

(defun is-in-scroller-pane (pane)
  (let ((parent (sheet-parent pane)))
    (and (typep parent 'viewport-pane)
         (typep (sheet-parent parent) 'scroller-pane))))

(defmethod pane-viewport ((pane basic-pane))
  (when (is-in-scroller-pane pane)
    (sheet-parent pane)))

(defmethod pane-viewport-region ((pane basic-pane))
  (when (is-in-scroller-pane pane)
    (sheet-region pane)))

(defmethod pane-scroller ((pane basic-pane))
  (when (is-in-scroller-pane pane)
    (sheet-parent (sheet-parent pane))))

(defun update-scroll-bars (pane entire-region x y)
  (multiple-value-bind (min-x min-y max-x max-y) (bounding-rectangle* entire-region)
    (with-slots (vscrollbar hscrollbar viewport) (pane-scroller pane)
      (when vscrollbar
	(with-slots (value) vscrollbar
	  (setf value y))
	(setf (gadget-min-value vscrollbar) min-y
	      (gadget-max-value vscrollbar) max-y)
	(dispatch-repaint vscrollbar (sheet-region vscrollbar)))
      (when hscrollbar
	(with-slots (value) hscrollbar
	  (setf value x))
	(setf (gadget-min-value hscrollbar) min-x
	      (gadget-max-value hscrollbar) max-x)
	(dispatch-repaint hscrollbar (sheet-region hscrollbar))))))

(defmethod scroll-extent ((pane basic-pane) x y)
  (when (is-in-scroller-pane pane)
    (move-sheet pane x y)
    (update-scroll-bars pane (sheet-region pane) x y)
    (dispatch-repaint pane (sheet-region pane))))

;;; LABEL PANE

(defclass label-pane (composite-pane)
  ((label :type string :initarg :label :accessor label-pane-label)
   )
  (:documentation ""))

(defmacro labelling ((&rest options) &body contents)
  `(make-instance 'label-pane ,@options :contents (list ,@contents)))


;;; GENERIC FUNCTIONS

(defgeneric window-clear (clim-stream-pane))
(defgeneric window-refresh (clim-stream-pane))
(defgeneric window-viewport (clim-stream-pane))
(defgeneric window-erase-viewport (clim-stream-pane))
(defgeneric window-viewport-position (clim-stream-pane))
(defgeneric* (setf window-viewport-position) (x y clim-stream-pane))


;;; CLIM-STREAM-PANE

(defclass clim-stream-pane (standard-output-recording-stream sheet-leaf-mixin basic-pane)
  ((display-time :initform nil
		 :initarg :display-time
		 :accessor pane-display-time)
   (incremental-redisplay :type (member '(t nil))
			  :initform nil
			  :initarg :incremental-redisplay 
			  :accessor pane-incremental-redisplay)
   (scroll-bars :type (member '(t :vertical :horizontal nil))
		:initform nil
		:initarg :scroll-bars
		:accessor pane-scroll-bars)
   (display-function :initform 'default-frame-top-level
		     :initarg :display-function
		     :accessor pane-display-function)
   ; Should inherit from label-pane for this one ??
   (label :type string :initform nil
	  :initarg :label
	  :reader pane-label)
   (text-margin :initarg :text-margin
		:reader pane-text-margin)
   (vertical-spacing :initarg :vertical-spacing
		     :reader pane-vertical-spacing)
   (end-of-line-action :initform :wrap
		       :initarg :end-of-line-action
		       :reader pane-end-of-line-action)
   (end-of-page-action :initform :scroll
		       :initarg :end-of-line-action
		       :reader pane-end-of-page-action)
   (output-history :initform (make-instance 'standard-tree-output-history)
		   :initarg :output-history
		   :accessor pane-output-history)))

(defmethod handle-event ((pane clim-stream-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod compose-space ((pane clim-stream-pane))
  (make-space-requirement :width 300 :height 300))

(defmethod dispatch-repaint ((pane clim-stream-pane) region)
  (repaint-sheet pane region))

(defmethod window-clear ((pane clim-stream-pane))
  (let ((output-history (pane-output-history pane)))
    (with-bounding-rectangle* (left top right bottom) output-history
      (medium-clear-area (sheet-medium pane) left top right bottom))
    (clear-output-record output-history))
  (window-erase-viewport pane)
  (let ((cursor (stream-text-cursor pane)))
    (when cursor
      (setf (cursor-position cursor) (values 0 0))))
  (scroll-extent pane 0 0))

(defmethod window-refresh ((pane clim-stream-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+))
  (stream-replay pane))

(defmethod window-viewport ((pane clim-stream-pane))
  (pane-viewport-region pane))

(defmethod window-erase-viewport ((pane clim-stream-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (pane-viewport-region pane)
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+)))

(defmethod window-viewport-position ((pane clim-stream-pane))
  (multiple-value-bind (x y) (bounding-rectangle* (pane-output-history pane))
    (values x y)))

(defmethod* (setf window-viewport-position) (x y (pane clim-stream-pane))
  (scroll-extent pane x y))

(defun scroll-area (pane dx dy)
  (let ((transform (sheet-transformation pane)))
    ;; Region has been "scrolled" already.
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (multiple-value-bind (destx desty)
	  (transform-position transform x1 y1)
	(multiple-value-bind (srcx srcy)
	    (transform-position transform (- x1 dx) (- y1 dy))
	  (format *debug-io* "dx ~S dy ~S srcx ~S srcy ~S destx ~S desty ~S~%"
		  dx dy srcx srcy destx desty)
	  (copy-area pane  srcx srcy (- x2 x1) (- y2 y1) destx desty))))))

(defmethod scroll-extent ((pane clim-stream-pane) x y)
  (when (is-in-scroller-pane pane)
    (let ((new-x (max x 0))
	  (new-y (max y 0))
	  (output-history (pane-output-history pane)))
      (let ((entire-region
	     (make-bounding-rectangle 0 0
				      (bounding-rectangle-max-x output-history)
				      (bounding-rectangle-max-y output-history)))
	    dx dy)
	(set-bounding-rectangle-position (sheet-region pane) new-x new-y)
	;; find out the coordinates, in the coordinates system of
	;; pane, of the upper-left corner, i.e. the one with
	;; coordinates (0,0) in the viewport
	(multiple-value-bind (x0 y0)
	    (untransform-position (sheet-transformation pane) 0 0)
	  (setq dx (- x0 new-x)
		dy (- y0 new-y))
	  ;; alter the sheet transformation to reflect the new position
	  (setf (sheet-transformation pane)
	    (compose-translation-with-transformation
	     (sheet-transformation pane) dx dy))
          (update-scroll-bars pane entire-region new-x new-y)
	  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
	    (cond
	     ((and (zerop dx)
		   (< (abs dy) (- y2 y1)))
	      (copy-area pane 0 0 (- x2 x1) (- y2 y1) 0 dy)
	      #+nil
	      (scroll-area pane 0 dy)
	      (cond
	       ((< dy 0)
		(draw-rectangle* (sheet-medium pane) x1 (+ y2 dy) x2 y2 :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle x1 (+ y2 dy) x2 y2)))
	       (t
		(draw-rectangle* (sheet-medium pane) x1 y1 x2 (+ y1 dy) :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle x1 y1 x2 (+ y1 dy)))))
	      )
	     ((and (zerop dy)
		   (< (abs dx) (- x2 x1)))
	      (copy-area pane 0 0 (- x2 x1) (- y2 y1) dx 0)
	      #+nil
	      (scroll-area pane dx 0)
	      (cond
	       ((< dx 0)
		(draw-rectangle* (sheet-medium pane) (+ x2 dx) y1 x2 y2 :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle (+ x2 dx) y1 x2 y2)))
	       (t
		(draw-rectangle* (sheet-medium pane) x1 y1 (+ x1 dx) y2 :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle x1 y1 (+ x1 dx) y2))))
	      )
	     (t
	      (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+)
	      (stream-replay pane (sheet-region pane))))))))))

;;; INTERACTOR PANES 

(defclass interactor-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((interactor interactor-pane) &rest args)
  (declare (ignore args))
  (setf (pane-scroll-bars interactor) :vertical))

(defmethod initialize-instance :after ((pane interactor-pane) &rest args)
  (declare (ignore args))
#+ignore  (let ((cursor (stream-text-cursor pane)))
    (setf (cursor-visibility cursor) t)))


;;; APPLICATION PANES

(defclass application-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((application application-pane) &rest args)
  (declare (ignore args))
  (setf (pane-display-time application) :command-loop
	(pane-scroll-bars application) t))
	

;;; COMMAND-MENU PANE

(defclass command-menu-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((command-menu command-menu-pane) &rest ignore)
  (declare (ignore ignore))
  (setf (pane-display-time command-menu) :command-loop
	(pane-incremental-redisplay command-menu) t
	(pane-scroll-bars command-menu) t
	(pane-display-function command-menu) 'diplay-command-menu))


;;; TITLE PANE

(defclass title-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((title title-pane) &rest args)
  (declare (ignore args))
  (setf (pane-display-time title) t))


;;; POINTER DOCUMENTATION PANE

(defclass pointer-documentation-pane (clim-stream-pane)
  ())


;;; CONSTRUCTORS

(defun make-clim-stream-pane (&rest options 
				    &key (type 'clim-stream-pane)
				         (scroll-bars :vertical)
					 (border-width 1)
				    &allow-other-keys)
  (declare (ignorable scroll-bars))
  (loop for key in '(:type :scroll-bars :border-width)
	do (remf options key))
  (let ((pane (apply #'make-pane type options)))
    (when scroll-bars
      (setq pane (make-pane 'scroller-pane 
			    :scroll-bar scroll-bars
			    :contents (list (make-pane 'viewport-pane 
						       :contents (list pane))))))
    (when (and border-width (> border-width 0))
      (setq pane (make-pane 'border-pane 
			    :border-width border-width 
			    :contents (list pane))))
    pane))

(defun make-clim-interactor-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'interactor-pane options))

(defun make-clim-application-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'application-pane options))
