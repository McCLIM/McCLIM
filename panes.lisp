;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

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

;;; GENERIC FUNCTIONS

(defgeneric compose-space (pane))
(defgeneric allocate-space (pane width height))
(defgeneric change-space-requirements (pane &rest rest))
(defgeneric note-space-requirements-changed (sheet pane))

;;; PANES

(defconstant +fill+ :fill)

(defclass space-requirement ()
  ((width :initform 1
	  :initarg :width
	  :accessor space-requirement-width)
   (max-width :initform 1
	      :initarg :max-width
	      :accessor space-requirement-max-width)
   (min-width :initform 1
	      :initarg :min-width
	      :accessor space-requirement-min-width)
   (height :initform 1
	   :initarg :height
	   :accessor space-requirement-height)
   (max-height :initform 1
	       :initarg :max-height
	       :accessor space-requirement-max-height)
   (min-height :initform 1
	       :initarg :min-height
	       :accessor space-requirement-min-height)
   )
  )

(defmethod print-object ((space space-requirement) stream)
  (with-slots (width height) space
    (print-unreadable-object (space stream :type t :identity t)
      (format stream "width: ~S height: ~S" width height))))

(defun make-space-requirement (&key (width 1) (max-width 1) (min-width 1)
				    (height 1) (max-height 1) (min-height 1))
  (make-instance 'space-requirement
    :width width
    :max-width max-width
    :min-width min-width
    :height height
    :max-height max-height
    :min-height min-height))

;; Set of macro for accessing quickly to the space-requirement slots.
(defmacro get-width (pane)
  `(space-requirement-width (pane-space-requirement ,pane)))
(defmacro get-height (pane)
  `(space-requirement-height (pane-space-requirement ,pane)))
(defmacro get-max-width (pane)
  `(space-requirement-max-width (pane-space-requirement ,pane)))
(defmacro get-max-height (pane)
  `(space-requirement-max-height (pane-space-requirement ,pane)))
(defmacro get-min-width (pane)
  `(space-requirement-min-width (pane-space-requirement ,pane)))
(defmacro get-min-height (pane)
  `(space-requirement-min-height (pane-space-requirement ,pane)))
(defmacro get-max (pane &optional (width-p t))
  `(if ,width-p (get-max-width ,pane) (get-max-height ,pane)))
(defmacro get-min (pane &optional (width-p t))
  `(if ,width-p (get-min-width ,pane) (get-min-height ,pane)))

(defclass pane (standard-sheet-input-mixin
		temporary-medium-sheet-output-mixin
		sheet-transformation-mixin sheet)
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

;; dummy but useful
(defmacro do-in (size min max)
  `(max ,min (min ,max ,size)))

(defun get-size (pane &optional (width-p t))
  ;; Give the region size if already set, otherwise, give the space-requirement
  (multiple-value-bind (width height) (rectangle-size (sheet-region pane))
    (if width-p
	(if (or (not width) (zerop width)) (get-width pane) width)
        (if (or (not height) (zerop height)) (get-height pane) height))))

(defun set-width-and-height (rectangular-sheet width height)
  (multiple-value-bind (x1 y1)
      (rectangle-edges* (sheet-region rectangular-sheet))
    (setf (sheet-region rectangular-sheet)
	  (make-rectangle* x1 y1 (+ x1 width) (+ y1 height)))))

(defun make-extremums-children-ratio (pane children &key (width t) (height t))
  (with-slots (sorted-max-w-r sorted-max-h-r sorted-min-w-r sorted-min-h-r) pane
    (loop for child in children
	  for max-width-ratio = (/ (get-width child) (get-max-width child))
	  for min-width-ratio = (/ (get-width child) (get-min-width child))
	  for max-height-ratio = (/ (get-height child) (get-max-height child))
	  for min-height-ratio = (/ (get-height child) (get-min-height child))
	  collect (cons max-width-ratio child) into max-w-ratios
	  collect (cons min-width-ratio child) into min-w-ratios
	  collect (cons max-height-ratio child) into max-h-ratios
	  collect (cons min-height-ratio child) into min-h-ratios
	  finally 
	    (flet ((test1 (c1 c2) (> (car c1) (car c2)))
		   (test2 (c1 c2) (< (car c1) (car c2))))
	      (when width
		(setf sorted-max-w-r (sort max-w-ratios #'test1)
		      sorted-min-w-r (sort min-w-ratios #'test2)))
	      (when height
		(setf sorted-max-h-r (sort max-h-ratios #'test1)
		      sorted-min-h-r (sort min-h-ratios #'test2)))))))

(defun allocate-space-internal (pane size &optional (w-p t))
  (loop with sum = (slot-value pane (if w-p 'sum-width 'sum-height)) 
	and maxs = (slot-value pane (if w-p 'sorted-max-w-r 'sorted-max-h-r))
	and mins = (slot-value pane (if w-p 'sorted-min-w-r 'sorted-min-h-r))
	for (r . child) in (if (>= size sum) maxs mins)
	and box-size = sum then (- box-size (get-size child w-p))
	for tmp = (round (/ (* size (get-size child w-p)) box-size))
	for new-size = (do-in tmp (get-min child w-p) (get-max child w-p))
	for new-sum = new-size then (+ new-sum new-size)
	do (decf size new-size)
	   (setf (slot-value child (if w-p 'new-width 'new-height)) new-size)
	finally
	   (setf (slot-value pane (if w-p 'sum-width 'sum-height)) new-sum)))

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
			  :max-width 200
			  :height 200
			  :max-height 200))

(defmethod compose-space :around ((pane pane))
  (with-slots (sr-width sr-height sr-max-width
	       sr-max-height sr-min-width sr-min-height
	       space-requirement) pane
    (unless space-requirement
      (let ((request (call-next-method)))
	(when (spacer-p pane)
	  (with-slots (margin-width margin-height
		       margin-max-width margin-max-height
		       margin-min-width margin-min-height) pane
	    (let ((child (first (sheet-children pane))))
	      (setf sr-width (+ margin-width (get-width child))
		    sr-height (+ margin-height (get-height child))
		    sr-max-width (+ margin-max-width (get-max-width child))
		    sr-max-height (+ margin-max-height (get-max-height child))
		    sr-min-width (+ margin-min-width (get-min-width child))
		    sr-min-height (+ margin-min-height (get-min-height child))
		    ))))
	(setf space-requirement (make-space-requirement))
	(with-slots (width height max-width max-height min-width min-height)
	    space-requirement
	  (setf width (or sr-width (space-requirement-width request))
		height (or sr-height (space-requirement-height request))
		max-width (or sr-max-width
			      sr-width
			      (space-requirement-max-width request))
		max-height (or sr-max-height
			       sr-height
			       (space-requirement-max-height request))
		min-width (or sr-min-width
			      sr-width
			      (space-requirement-min-width request))
		min-height (or sr-min-height
			       sr-height
			       (space-requirement-min-height request))
		width (do-in (or sr-width (space-requirement-width request))
			     min-width
			     max-width)
		height (do-in (or sr-height (space-requirement-height request))
			      min-height
			      max-height))))
      (compute-extremum pane))
    space-requirement))

(defmethod allocate-space ((pane pane) width height)
  (set-width-and-height pane width height))

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
      (allocate-space pane (get-width pane) (get-height pane))
      (note-space-requirements-changed (sheet-parent pane) pane)))

;;; WINDOW STREAM

(defclass window-stream (extended-output-stream standard-input-stream)
  (
   )
  )

;;; BASIC-PANE

(defclass basic-pane (window-stream
		      sheet-parent-mixin
		      mirrored-sheet
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
    (set-width-and-height pane width height)
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
	(do-in width (get-min-width pane) (get-max-width pane))
	(do-in height (get-min-height pane) (get-max-height pane)))))

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
	sum (get-size child) into sum-width
	do (incf (space-requirement-width space)
		 (space-requirement-width request))
	   (incf (space-requirement-max-width space)
		 (space-requirement-max-width request))
	   (incf (space-requirement-min-width space)
		 (space-requirement-min-width request))
	   (setf (space-requirement-height space)
		 (max (space-requirement-height space)
		      (space-requirement-height request)))
	   (setf (space-requirement-max-height space)
		 (max (space-requirement-max-height space)
		      (space-requirement-max-height request)))
	   (setf (space-requirement-min-height space)
		 (max (space-requirement-min-height space)
		      (space-requirement-min-height request)))
	finally (progn		  
		  (setf (slot-value box 'sum-width) sum-width)
		  (return space))))

(defmethod compute-extremum ((box hbox-pane))
  (make-extremums-children-ratio box (sheet-children box) :height nil))

(defmethod allocate-space ((box hbox-pane) width height)
  (set-width-and-height box width height)
  (allocate-space-internal box width)
  (loop for child in (sheet-children box)
	for nw = (slot-value child 'new-width)
	and x = 0 then (+ x nw)
	and nh = (/ (* height (get-size child nil)) (get-size box nil))
	do (multiple-value-bind (x1 y1)
	       (bounding-rectangle* (sheet-region child))
	     (setf (sheet-transformation child)
		   (make-translation-transformation (- x x1) (- y1))))
	   (setf nh (do-in nh (get-min-height child) (get-max-height child)))
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
	sum (get-size child nil) into sum-height
	do (setf (space-requirement-width space)
		 (max (space-requirement-width space)
		      (space-requirement-width request)))
	   (setf (space-requirement-max-width space)
		 (max (space-requirement-max-width space)
		      (space-requirement-max-width request)))
	   (setf (space-requirement-min-width space)
		 (max (space-requirement-min-width space)
		      (space-requirement-min-width request)))
	   (incf (space-requirement-height space)
		 (space-requirement-height request))
	   (incf (space-requirement-max-height space)
		 (space-requirement-max-height request))
	   (incf (space-requirement-min-height space)
		 (space-requirement-min-height request))
	finally (progn 
		  (setf (slot-value box 'sum-height) sum-height)
		  (return space))))
	   
(defmethod compute-extremum ((box vbox-pane))
  (make-extremums-children-ratio box (sheet-children box) :width nil))

(defmethod allocate-space ((box vbox-pane) width height)
  (set-width-and-height box width height)
  (allocate-space-internal box height nil)
  (loop for child in (sheet-children box)
	  for nh = (slot-value child 'new-height)
	  and y = 0 then (+ y nh)
	  and nw = (/ (* width (get-size child)) (get-size box))
	  do (multiple-value-bind (x1 y1)
	         (bounding-rectangle* (sheet-region child))
	       (setf (sheet-transformation child)
		     (make-translation-transformation (- x1) (- y y1))))
	     (setf nw (do-in nw (get-min-width child) (get-max-width child)))
	     (allocate-space child (round nw) nh)))
  
;;; HRACK-PANE

(defclass hrack-pane (composite-pane)
  ()
  (:documentation ""))

(defmethod compose-space ((rack hrack-pane))
  (mapc #'compose-space (sheet-children rack))
  (loop with space = (make-space-requirement
		      :max-height (get-max-height (car (sheet-children rack))))
	for child in (sheet-children rack)
	for request = (pane-space-requirement child)
	sum (get-size child) into sum-width
	do (incf (space-requirement-width space)
		 (space-requirement-width request))
	   (incf (space-requirement-max-width space)
		 (space-requirement-max-width request))
	   (incf (space-requirement-min-width space)
		 (space-requirement-min-width request))
	   (setf (space-requirement-height space)
		 (max (space-requirement-height space)
		      (space-requirement-height request)))
	   (setf (space-requirement-max-height space)
		 (min (space-requirement-max-height space)
		      (space-requirement-max-height request)))
	   (setf (space-requirement-min-height space)
		 (max (space-requirement-min-height space)
		      (space-requirement-min-height request)))
	finally (progn
		  (setf (slot-value rack 'sum-width) sum-width)
		  (return space))))

(defmethod compute-extremum ((rack hrack-pane))
  (make-extremums-children-ratio rack (sheet-children rack) :height nil))

(defmethod allocate-space ((rack hrack-pane) width height)
  (set-width-and-height rack width height)
  (allocate-space-internal rack width)
  (loop for child in (sheet-children rack)
	for nw = (slot-value child 'new-width)
	and x = 0 then (+ x (round nw))
	do (multiple-value-bind (x1 y1)
	       (bounding-rectangle* (sheet-region child))
	     (setf (sheet-transformation child)
		   (make-translation-transformation (- x x1) (- y1))))
	   (allocate-space child nw height)))

;;; VRACK-PANE

(defclass vrack-pane (composite-pane)
  ()
  (:documentation ""))

(defmethod compose-space ((rack vrack-pane))
  (mapc #'compose-space (sheet-children rack))
  (loop with space = (make-space-requirement 
		      :max-width (get-max-width (car (sheet-children rack))))
	for child in (sheet-children rack)
	for request = (pane-space-requirement child)
	sum (get-size child nil) into sum-height
	do (setf (space-requirement-width space)
		 (max (space-requirement-width space)
		      (space-requirement-width request)))
	   (setf (space-requirement-max-width space)
		 (min (space-requirement-max-width space)
		      (space-requirement-max-width request)))
	   (setf (space-requirement-min-width space)
		 (max (space-requirement-min-width space)
		      (space-requirement-min-width request)))
	   (incf (space-requirement-height space)
		 (space-requirement-height request))
	   (incf (space-requirement-max-height space)
		 (space-requirement-max-height request))
	   (incf (space-requirement-min-height space)
		 (space-requirement-min-height request))
	finally (progn
		  (setf (slot-value rack 'sum-height) sum-height)
		  (return space))))

(defmethod compute-extremum ((rack vrack-pane))
  (make-extremums-children-ratio rack (sheet-children rack) :width nil))
  
(defmethod allocate-space ((rack vrack-pane) width height)
  (set-width-and-height rack width height)
  (allocate-space-internal rack height nil)
  (loop for child in (sheet-children rack)
	for nh = (slot-value child 'new-height)
	and y = 0 then (+ y nh)
	do (multiple-value-bind (x1 y1)
	       (bounding-rectangle* (sheet-region child))
	     (setf (sheet-transformation child)
		   (make-translation-transformation (- x1) (- y y1))))
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
	  when (and (zerop i) (zerop l)) sum (get-size child) into sum-width
	  when (zerop c) sum (get-size child nil) into sum-height 
	  do (setf (aref space-vec 0 c)
		   (max (get-width child) (aref space-vec 0 c))
		   (aref space-vec 2 c)
		   (min (get-max-width child) (aref space-vec 2 c))
		   (aref space-vec 4 c)
		   (max (get-min-width child) (aref space-vec 4 c))
		   (aref space-vec 1 l)
		   (max (get-height child) (aref space-vec 1 l))
		   (aref space-vec 3 l)
		   (min (get-max-height child) (aref space-vec 3 l))
		   (aref space-vec 5 l)
		   (max (get-min-height child) (aref space-vec 5 l)))
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
  ;; and each lines cells has the same height, will make a line-refernce
  ;; and a column reference. (first line and column of the table.
  ;; (ie. They will represent the size reference for lines and columns)
  (make-extremums-children-ratio 
       table (mapcar #'car (format-children table)) :width nil)
  (make-extremums-children-ratio
       table (car (format-children table)) :height nil))

(defmethod allocate-space ((table table-pane) width height)
  (set-width-and-height table width height)
  (allocate-space-internal table height nil)
  (allocate-space-internal table width)
  (loop for children in (format-children table)
	for line-ref in (mapcar #'car (format-children table))
	for new-height = (slot-value line-ref 'new-height)
	and y = 0 then (+ y new-height)
	do (loop for child in children
		 for ref in (car (format-children table))
		 for new-width = (slot-value ref 'new-width)
		 and x = 0 then (+ x new-width)
		 do (multiple-value-bind (x1 y1)
			(bounding-rectangle* (sheet-region child))
		      (setf (sheet-transformation child)
			    (make-translation-transformation (- x x1) (- y y1))))
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
	and width = 0 then (max width (get-width child))
	and height = 0 then (max height (get-height child))
	and max-width = 5000000 then (min max-width (get-min-width child))
	and max-height = 5000000 then (min max-height (get-max-height child))
	and min-width = 0 then (max min-width (get-min-width child))
	and min-height = 0 then (max min-height (get-min-height child))
	finally (return 
		 (make-space-requirement
		  :width (* width nb-children-pl)
		  :height (* height nb-children-pc)
		  :max-width (* width nb-children-pl)
		  :max-height (* max-height nb-children-pc)
		  :min-width (* min-width nb-children-pl)
		  :min-height (* min-height nb-children-pc)))))
     
(defmethod allocate-space ((grid grid-pane) width height)
  (set-width-and-height grid width height)
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
		 do (multiple-value-bind (x1 y1)
			(bounding-rectangle* (sheet-region child))
		      (setf (sheet-transformation child)
			    (make-translation-transformation (- x x1) (- y y1))))
		    (allocate-space child (round new-width) (round new-height)))))

;;; SPACER PANE

(defclass spacer-pane (composite-pane)
  ((margin-width :initform nil :initarg :width)
   (margin-height :initform nil :initarg :height)
   (margin-max-width :initform nil :initarg :max-width)
   (margin-max-height :initform nil :initarg :max-height)
   (margin-min-width :initform nil :initarg :min-width)
   (margin-min-height :initform nil :initarg :min-height))
  (:documentation "The spacer pane will create a margin for his child.
The margin sizes (w h) are given with the :width and :height initargs.
During realization the child of the spacer will have as cordinates
 x = w/2 , y = h/2."))

(defmethod initialize-instance :after ((spacer spacer-pane) &rest ignore)
  (declare (ignore ignore))
  (with-slots (margin-width margin-height
	       margin-max-width margin-max-height
	       margin-min-width margin-min-height) spacer
    (setf margin-width (or margin-width 0)
	  margin-height (or margin-height 0)
	  margin-max-width (or margin-max-width margin-width)
	  margin-max-height (or margin-max-height margin-height)
	  margin-min-width (or margin-min-width margin-width)
	  margin-min-height (or margin-min-height margin-height))))

(defun spacer-p (pane)
  (typep pane 'spacer-pane))

(defmacro spacing ((&rest options) &body contents)
  `(make-pane 'spacer-pane ,@options :contents (list ,@contents)))

(defmethod compose-space ((spacer spacer-pane))
  (compose-space (first (sheet-children spacer))))

(defmethod allocate-space ((spacer spacer-pane) width height)
  (set-width-and-height spacer width height)
  (let* ((child (first (sheet-children spacer)))
	 (margin-width (- (get-width spacer) (get-width child)))
	 (margin-height (- (get-height spacer) (get-height child))))
    (setf margin-width
	  (do-in (round (/ (* width margin-width) (get-size spacer)))
		 (slot-value spacer 'margin-min-width)
		 (slot-value spacer 'margin-max-width))
	  margin-height
	  (do-in (round (/ (* height margin-height) (get-size spacer nil)))
		 (slot-value spacer 'margin-min-height)
		 (slot-value spacer 'margin-max-height))
	  (sheet-transformation child)
	  (make-translation-transformation (/ margin-width 2)
					   (/ margin-height 2)))
    (allocate-space child (- width margin-width) (- height margin-height))))

;;; BORDER-PANE

(defclass border-pane (spacer-pane)
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
  (set-width-and-height bp width height)
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
  (with-double-buffering (pane)
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
  (make-space-requirement :width 300 :max-width 300 :min-width 300
			  :height 300 :max-height 300 :min-height 300))

;;; VIEWPORT

(defclass viewport-pane (single-child-composite-pane) ())


;;; SCROLLER-PANE

(defparameter *scrollbar-thickness* 12)

(defclass scroller-pane (composite-pane)
  ((scroll-bar :type (member '(t :vertical :horizontal))
	       :initform t
	       :initarg :scroll-bar
	       :accessor scroller-pane-scroll-bar)
   (viewport :initform nil)
   (vscrollbar :initform nil)
   (hscrollbar :initform nil)))

(defmethod initialize-instance :after ((pane scroller-pane) &rest args)
  (declare (ignore args))
  (with-slots (scroll-bar viewport vscrollbar hscrollbar) pane
    (setq viewport (first (sheet-children pane)))
    (when (not (eq scroll-bar :horizontal))
      (setq vscrollbar (make-pane 'scroll-bar-pane
				  :orientation :vertical
				  :foreground +grey40+
				  :background +grey+))
      (sheet-adopt-child pane vscrollbar))
    (when (not (eq scroll-bar :vertical))
      (setq hscrollbar (make-pane 'scroll-bar-pane
				  :orientation :horizontal
				  :foreground +grey40+
				  :background +grey+))
      (sheet-adopt-child pane hscrollbar))))
    
(defmacro scrolling ((&rest options) &body contents)
  `(let ((viewport (make-pane 'viewport-pane :contents (list ,@contents))))
     (make-pane 'scroller-pane ,@options :contents (list viewport))))

(defmethod compose-space ((pane scroller-pane))
  (with-slots (viewport) pane
    (if viewport
	(let ((req (compose-space viewport)))
	  (incf (space-requirement-width req) 10)
	  (incf (space-requirement-min-width req) 10)
	  (incf (space-requirement-max-width req) 10)
	  (incf (space-requirement-height req) 10)
	  (incf (space-requirement-min-height req) 10)
	  (incf (space-requirement-max-height req) 10)
	  req)
        (make-space-requirement))))

(defmethod allocate-space ((pane scroller-pane) width height)
  (set-width-and-height pane width height)
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
      (setf (sheet-transformation vscrollbar)
	(make-translation-transformation (if vscrollbar *scrollbar-thickness* 0)
					 (- height *scrollbar-thickness*)))
      (allocate-space hscrollbar
		      (if vscrollbar (- width *scrollbar-thickness*) width)
		      *scrollbar-thickness*))))

(defmethod pane-viewport ((scroller scroller-pane))
  (first (sheet-children scroller)))

(defmethod pane-viewport-region ((scroller scroller-pane))
  (sheet-region (pane-viewport scroller)))

(defmethod pane-scroller ((scroller scroller-pane))
  (scroller-pane-scroll-bar scroller))

(defmethod update-scrollbars ((pane basic-pane))
  (update-scrollbars (sheet-parent pane)))

(defmethod update-scrollbars ((pane scroller-pane))
  (with-slots (viewport vscrollbar hscrollbar) pane
    (with-bounding-rectangle* (hminx hminy hmaxx hmaxy)
	(stream-output-history (first (sheet-children viewport)))
      (with-bounding-rectangle* (rminx rminy rmaxx rmaxy)
	  (sheet-region (first (sheet-children viewport)))
	(when vscrollbar
          (let ((delta-y (max (- hmaxy hminy) 1)))
            (setf (scrollbar-offset vscrollbar) (/ (- rminy hminy) delta-y))
            (setf (scrollbar-length vscrollbar) (/ (- rmaxy rminy) delta-y)))
	  (window-refresh vscrollbar))
	(when hscrollbar
          (let ((delta-x (max (- hmaxx hminx) 1)))
            (setf (scrollbar-offset hscrollbar) (/ (- rminx hminx) delta-x))
            (setf (scrollbar-length hscrollbar) (/ (- rmaxx rminx) delta-x)))
	  (window-refresh hscrollbar))))))


;;; LABEL PANE

(defclass label-pane (composite-pane)
  ((label :type string :initarg :label :accessor label-pane-label)
   )
  (:documentation ""))

(defmacro labelled ((&rest options) &body contents)
  `(make-instance 'label-pane ,@options :contents (list ,@contents)))


;;; GENERIC FUNCTIONS

(defgeneric window-clear (clim-stream-pane))
(defgeneric window-refresh (clim-stream-pane))
(defgeneric window-viewport (clim-stream-pane))
(defgeneric window-erase-viewport (clim-stream-pane))
(defgeneric window-viewport-position (clim-stream-pane))
;(defgeneric (setf window-viewport-position) (x y clim-stream-pane))


;;; CLIM-STREAM-PANE

(defclass clim-stream-pane (output-recording-stream sheet-leaf-mixin basic-pane)
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

(defmethod compose-space ((pane clim-stream-pane))
  (make-space-requirement :width 300 :height 300
			  :max-width 300 :max-height 300))

(defmethod window-clear ((pane clim-stream-pane))
  (let ((output-history (pane-output-history pane)))
    (with-bounding-rectangle* (x1 y1 x2 y2) output-history
      (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+))
    (clear-output-record output-history))
  (let ((cursor (stream-text-cursor pane)))
    (when cursor
      (setf*-cursor-position 0 0 cursor)))
;      (setf* (cursor-position cursor) (values 0 0))))
  (scroll-extent pane 0 0))

(defmethod window-refresh ((pane clim-stream-pane))
  (window-clear pane)
  (stream-replay pane))

(defmethod window-viewport ((pane clim-stream-pane))
  (sheet-region pane))

(defmethod window-erase-viewport ((pane clim-stream-pane))
  (window-clear pane)
  (dispatch-repaint pane (sheet-region pane)))

(defmethod window-viewport-position ((pane clim-stream-pane))
  (multiple-value-bind (x1 y1)
      (bounding-rectangle* (sheet-region pane))
    (values x1 y1)))

(defmethod (setf window-viewport-position) (x y (pane clim-stream-pane))
  (let ((region (sheet-region pane)))
    (setf (slot-value region 'x1) x
	  (slot-value region 'y1) y)))

(defmethod scroll-extent ((pane clim-stream-pane) x y)
; (setf (sheet-transformation pane)
;	(make-translation-transformation (- x) (- y)))
  (set-bounding-rectangle-position (sheet-region pane) x y)
; (format *debug-io*
;	  "set region position ~D,~D for ~S~%"
;	  x y (sheet-region pane))
  (update-scrollbars (sheet-parent pane))
  (clear-area pane)
  (replay (stream-output-history pane) pane)
  )

		    

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
