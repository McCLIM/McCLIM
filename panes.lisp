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

;; Generic Functions

(defgeneric compose-space (pane))
(defgeneric allocate-space (pane width height))
(defgeneric change-space-requirements (pane &rest res))
(defgeneric note-space-requirements-changed (sheet pane))

;; Panes

(defclass space-requirement ()
  ((width :initform 0
	  :initarg :width
	  :accessor space-requirement-width)
   (max-width :initform 0
	      :initarg :max-width
	      :accessor space-requirement-max-width)
   (min-width :initform 0
	      :initarg :min-width
	      :accessor space-requirement-min-width)
   (height :initform 0
	   :initarg :height
	   :accessor space-requirement-height)
   (max-height :initform 0
	       :initarg :max-height
	       :accessor space-requirement-max-height)
   (min-height :initform 0
	       :initarg :min-height
	       :accessor space-requirement-min-height)
   )
  )

(defmethod print-object ((space space-requirement) stream)
  (with-slots (width height) space
    (print-unreadable-object (space stream :type t :identity t)
      (format stream "width: ~S height: ~S" width height))))

(defun make-space-requirement (&key (width 0) (max-width 0) (min-width 0)
				    (height 0) (max-height 0) (min-height 0))
  (make-instance 'space-requirement
    :width width
    :max-width max-width
    :min-width min-width
    :height height
    :max-height max-height
    :min-height min-height))

(defclass pane (standard-sheet-input-mixin temporary-medium-sheet-output-mixin
		sheet-transformation-mixin sheet)
  ((foreground :initarg :foreground
	       :initform +black+
	       :reader pane-foreground)
   (background :initarg :background
	       :initform +white+
	       :reader pane-background)
   (text-style :initarg :text-style
	       :initform nil
	       :reader pane-text-style)
   (name :initarg :name
	 :initform "(Unnamed Pane)"
	 :reader pane-name)
   (manager :initarg :manager)
   (port :initarg :port)
   (frame :initarg :frame
	  :initform *application-frame*
	  :reader pane-frame)
   (enabledp :initform nil
	     :initarg :enabledp
	     :accessor pane-enabledp)
   (space-requirement :initarg :space-requirement
		      :initform (make-space-requirement :width 300 :max-width 300
							:height 300 :max-height 300)
		      :accessor pane-space-requirement)
   )
  )

(defmethod compose-space ((pane pane))
  (or (pane-space-requirement pane)
      (make-space-requirement :width 300 :max-width 300
			      :height 300 :max-height 300)))


(defun panep (x)
  (typep x 'pane))

(defun make-pane (type &rest args)
  (apply #'make-pane-1 (find-frame-manager) *application-frame* type args))

(defmethod compose-space ((pane pane))
  (setf (pane-space-requirement pane)
    (make-space-requirement :width 200
			    :max-width 200
			    :height 200
			    :max-height 200)))

(defun set-width-and-height (rectangular-sheet width height)
  (multiple-value-bind (x1 y1)
      (rectangle-edges* (sheet-region rectangular-sheet))
    (setf (sheet-region rectangular-sheet)
	  (make-rectangle* x1 y1 (+ x1 width) (+ y1 height)))))

;;; FIXME: this should be done for each type of pane
(defmethod allocate-space ((pane pane) width height)
  (set-width-and-height pane width height))

(defmethod change-space-requirements ((pane pane) &rest rest)
  (declare (ignore rest))
  (values))

(defmethod note-space-requirements-changed (sheet (pane pane))
  (declare (ignore sheet))
  (values))


(defclass window-stream (extended-output-stream standard-input-stream)
  (
   )
  )


;;; Basic-Pane class

(defclass basic-pane (window-stream
		      sheet-parent-mixin
		      mirrored-sheet
		      permanent-medium-sheet-output-mixin
		      pane)
  ()
  )

(defmethod compose-space ((basic basic-pane))
  (ask-space-children basic)
  (compute-space basic))


;;; COMPOSITE PANE

(defclass composite-pane (sheet-multiple-child-mixin
			  basic-pane)
  (
   )
  )


(defmethod initialize-instance :after ((pane composite-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (if contents
      (unless (typep (first contents) 'list) ;; temporary medecine
	(loop for child in (reverse contents)
	      do (sheet-adopt-child pane child)))))


(defmethod ask-space-children ((composite composite-pane))
  (mapcar #'compose-space (sheet-children composite)))

;;; FIXME: this one shoud be removed
(defmethod compute-space ((composite composite-pane))
  (let ((space (make-space-requirement)))
    (loop for child in (sheet-children composite)
	  for request = (pane-space-requirement child)
	  do (progn (incf (space-requirement-width space)
			  (space-requirement-width request))
		    (incf (space-requirement-max-width space)
			  (space-requirement-max-width request))
		    (incf (space-requirement-min-width space)
			  (space-requirement-min-width request))
		    (incf (space-requirement-height space)
			  (space-requirement-height request))
		    (incf (space-requirement-max-height space)
			  (space-requirement-max-height request))
		    (incf (space-requirement-min-height space)
			  (space-requirement-min-height request))))
    (setf (pane-space-requirement composite) space)
    space))

(defmacro changing-space-requirement (&body body &key resize-frame)
  `(progn
     ,@body))

(defmethod change-space-requirements ((pane composite-pane)
				      &rest space-req-keys
				      &key resize-frame
				      space-requirement-width
				      space-requirement-min-width
				      space-requirement-max-width
				      space-requirement-height
				      space-requirement-min-height
				      space-requirement-max-height)
  (declare (ignore space-req-keys))
  (when space-requirement-width
    (setf (space-requirement-width pane) space-requirement-width))
  (when space-requirement-min-width
    (setf (space-requirement-min-width pane) space-requirement-min-width))
  (when space-requirement-max-width
    (setf (space-requirement-max-width pane) space-requirement-max-width))
  (when space-requirement-height
    (setf (space-requirement-height pane) space-requirement-height))
  (when space-requirement-min-height
    (setf (space-requirement-min-height pane) space-requirement-min-height))
  (when space-requirement-max-height
    (setf (space-requirement-max-height pane) space-requirement-max-height))
  (if resize-frame
      (layout-frame (pane-frame pane))
      ; we didn't find the :resize-frame option in define-application-frame
    (note-space-requirements-changed (sheet-parent pane) pane)))

(defmethod note-space-requirements-changed ((sheet composite-pane) (pane composite-pane))
  (if (eq sheet (sheet-parent pane))
      (compose-space pane)
      (compute-space sheet))
  (note-space-requirements-changed (sheet-parent sheet) pane))

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
  (if contents
      (sheet-adopt-child pane (first contents))))

(defmethod compose-space ((pane single-child-composite-pane))
  (if (sheet-children pane)
      (compose-space (first (sheet-children pane)))
    (make-space-requirement)))

(defmethod allocate-space ((pane single-child-composite-pane) width height)
  (set-width-and-height pane width height)
  (let ((child (first (sheet-children pane))))
    (when child
      (setf (sheet-region child)
	(make-bounding-rectangle 0 0 width height))
      (allocate-space child width height))))

;; TOP-LEVEL-SHEET

(defclass top-level-sheet-pane (composite-pane)
  ()
  (:documentation "For the first pane in the architecture"))

(defmethod allocate-space ((pane top-level-sheet-pane) width height)
  (allocate-space (car (sheet-children pane)) width height))

(defmethod dispatch-event ((pane top-level-sheet-pane) event)
  (handle-event pane event))

(defmethod handle-event ((pane top-level-sheet-pane) (event window-configuration-event))
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

;; SHEET 

(defmethod note-space-requirements-changed ((sheet sheet) (pane composite-pane))
  (values))


;;; HBOX-PANE class

(defclass hbox-pane (composite-pane)
  ()
  )

(defmacro horizontally ((&rest options
			 &key (equalize-height t)
			 &allow-other-keys) &body contents)
  (remf options :equalize-height)
  (if equalize-height
      `(make-pane 'hrack-pane ,@options :contents (list ,@contents))
    `(make-pane 'hbox-pane ,@options :contents (list ,@contents))))

(defmethod compose-space ((box hbox-pane))
  (let ((space (make-space-requirement)))
    (loop for child in (sheet-children box)
	for request = (compose-space child)
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
		  (space-requirement-min-height request))))
    (setf (pane-space-requirement box) space)
    space))

(defmethod allocate-space ((box hbox-pane) width height)
  (set-width-and-height box width height)
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement box))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement box))))
	(x 0))
    (loop for child in (sheet-children box)
	  for request = (pane-space-requirement child)
	  for new-width = (floor (* h-percent (space-requirement-width request)))
	  for new-height = (floor (* v-percent (space-requirement-height request)))
	  do (progn (multiple-value-bind (x1 y1)
			(bounding-rectangle* (sheet-region child))
		      (setf (sheet-transformation child)
			    (make-translation-transformation (- x x1) (- y1))))
		    (allocate-space child new-width new-height)
		    (incf x new-width)))))


;;; VBOX-PANE class

(defclass vbox-pane (composite-pane)
  ()
  )

(defmacro vertically ((&rest options
		       &key (equalize-width t)
		       &allow-other-keys) &body contents)
  (remf options :equalize-width)
  (if equalize-width
      `(make-pane 'vrack-pane ,@options :contents (list ,@contents))
    `(make-pane 'vbox-pane ,@options :contents (list ,@contents))))

(defmethod compose-space ((box vbox-pane))
  (let ((space (make-space-requirement)))
    (loop for child in (sheet-children box)
	for request = (compose-space child)
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
		 (space-requirement-min-height request)))
    (setf (pane-space-requirement box) space)
    space))

(defmethod allocate-space ((box vbox-pane) width height)
  (set-width-and-height box width height)
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement box))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement box))))
	(y 0))
    (loop for child in (sheet-children box)
	  for request = (pane-space-requirement child)
	  for new-width = (floor (* h-percent (space-requirement-width request)))
	  for new-height = (floor (* v-percent (space-requirement-height request)))
	  do (progn (multiple-value-bind (x1 y1)
			(bounding-rectangle* (sheet-region child))
		      (setf (sheet-transformation child)
			    (make-translation-transformation (- x1) (- y y1))))
		    (allocate-space child new-width new-height)
		    (incf y new-height)))))


;;; HRACK-PANE class

(defclass hrack-pane (composite-pane)
  ()
  )

(defmethod compose-space ((rack hrack-pane))
  (let ((space (make-space-requirement)))
    (loop for child in (sheet-children rack)
	for request = (compose-space child)
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
		  (space-requirement-min-height request))))
    (setf (pane-space-requirement rack) space)
    space))

(defmethod allocate-space ((rack hrack-pane) width height)
  (set-width-and-height rack width height)
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement rack))))
	(x 0))
    (loop for child in (sheet-children rack)
	  for request = (pane-space-requirement child)
	  for new-width = (floor (* h-percent (space-requirement-width request)))
	  do (progn (multiple-value-bind (x1 y1)
			(bounding-rectangle* (sheet-region child))
		      (setf (sheet-transformation child)
			    (make-translation-transformation (- x x1) (- y1))))
		    (allocate-space child new-width height)
		    (incf x new-width)))))


;;; VRACK-PANE class

(defclass vrack-pane (composite-pane)
  ()
  )

(defmethod compose-space ((rack vrack-pane))
  (let ((space (make-space-requirement)))
    (loop for child in (sheet-children rack)
	for request = (compose-space child)
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
		 (space-requirement-min-height request)))
    (setf (pane-space-requirement rack) space)
    space))

(defmethod allocate-space ((rack vrack-pane) width height)
  (set-width-and-height rack width height)
  (let ((v-percent (/ height (space-requirement-height (pane-space-requirement rack))))
	(y 0))
    (loop for child in (sheet-children rack)
	  for request = (pane-space-requirement child)
	  for new-height = (floor (* v-percent (space-requirement-height request)))
	  do (progn (multiple-value-bind (x1 y1)
			(bounding-rectangle* (sheet-region child))
		      (setf (sheet-transformation child)
			    (make-translation-transformation (- x1) (- y y1))))
		    (allocate-space child width new-height)
		    (incf y new-height)))))


;; TABLE PANE

(defclass table-pane (composite-pane)
  ((number-per-line :type integer :initform 0 :accessor table-pane-number)))

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
  (setf (table-pane-number table) (length (first contents))))

(defmacro tabling ((&rest options) &body contents)
  `(if (pane-grid-p (list ,@contents))
       (make-pane 'grid-pane ,@options :contents (list ,@contents))
       (make-pane 'table-pane ,@options :contents (list ,@contents))))

(defmacro change-formated-spaces (spaces space-direction-accessor)
  `(setf (,space-direction-accessor space)
	 (reduce #'max (mapcar #'(lambda (l)
				  (reduce #'+ (mapcar #',space-direction-accessor l)))
			      ,spaces))))

(defun format-children (children number-per-line)
  (let (formated aux)
    (loop for child in children
	  for i from 1
	  do (progn (push child aux)
		    (when (= (mod i number-per-line) 0)
		      (push (nreverse aux) formated)
		      (setf aux nil))))
    (nreverse formated)))


(defmethod compute-space ((table table-pane))
  (let* ((space (make-space-requirement))
	 (h-spaces (format-children (mapcar #'pane-space-requirement (sheet-children table))
				    (table-pane-number table)))
	 (v-spaces (apply #'mapcar #'list h-spaces)))
    (change-formated-spaces h-spaces space-requirement-width)
    (change-formated-spaces h-spaces space-requirement-min-width)
    (change-formated-spaces h-spaces space-requirement-max-width)
    (change-formated-spaces v-spaces space-requirement-height)
    (change-formated-spaces v-spaces space-requirement-min-height)
    (change-formated-spaces v-spaces space-requirement-max-height)
    (setf (pane-space-requirement table) space)
    space))

(defun find-collums-and-rows-size (table)
  (let* ((number-per-line (table-pane-number table))
         (children-request (mapcar #'pane-space-requirement (sheet-children table)))
         (width-requests (mapcar #'space-requirement-width children-request))
         (height-requests (mapcar #'space-requirement-height children-request))
         (height-requests (format-children height-requests number-per-line))
         (width-requests (apply #'mapcar #'list 
                                (format-children width-requests number-per-line))))
    (list
     (mapcar #'(lambda (l) (reduce #'max l)) width-requests)
     (mapcar #'(lambda (l) (reduce #'max l)) height-requests))))

(defmethod allocate-space ((table table-pane) width height)
  (set-width-and-height table width height)
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement table))))
        (v-percent (/ height (space-requirement-height (pane-space-requirement table))))
        (table-cell-sizes (find-collums-and-rows-size table))
        (contents (format-children (sheet-children table) (table-pane-number table)))
	(x 0)
        (y 0))
    (loop for line-children in contents
          for height in (second table-cell-sizes)
          for new-height = (floor (* h-percent height))
          do (progn (loop for child in line-children
			  for width in (first table-cell-sizes)
			  for new-width = (floor (* v-percent width))
			  do (progn (multiple-value-bind (x1 y1)
					(bounding-rectangle* (sheet-region child))
				      (setf (sheet-transformation child)
					    (make-translation-transformation (- x x1) (- y y1))))
				    (allocate-space child new-width new-height)
				    (incf x new-width)))
		    (setf x 0)
		    (incf y new-height)))))

;(defmethod sheet-adopt-child :before ((table table-pane) child)
;  (declare (ignore child))
;  (when (= (length (sheet-children table)) (table-pane-number table))
;    (error "The table can't adopt more childs than specified by the table-number")))

(defmethod sheet-disowned-child :before ((table table-pane) child &key (error-p t))
  (declare (ignore child error-p))
  (error "The table pane can't disown one of its child"))


;; GRID PANE

(defclass grid-pane (table-pane) ()
  (:documentation "Be careful : each cell has its width equal to its height"))

(defun pane-grid-p (contents)
  (let ((spaces (mapcar #'pane-space-requirement (apply #'append contents))))
    (apply #'= (mapcan #'(lambda (pane)
			    (list (space-requirement-width pane)
				  (space-requirement-height pane)))
					       
			spaces))))
	    


(defmethod allocate-space ((grid grid-pane) width height)
  (set-width-and-height grid width height)
  (let* ((new-height (/ height (/ (length (sheet-children grid)) (table-pane-number grid))))
	 (new-width (/ width (table-pane-number grid)))
	 (contents (format-children (sheet-children grid) (table-pane-number grid)))
	 (x 0)
	 (y 0))
    (loop for children in contents
	  do (progn (loop for child in children
			  do (progn (multiple-value-bind (x1 y1)
					(bounding-rectangle* (sheet-region child))
				      (setf (sheet-transformation child)
					    (make-translation-transformation (- x x1) (- y y1))))
				    (allocate-space child new-width new-height)
				    (incf x new-width)))
		    (setf x 0)
		    (incf y new-height)))))


;; SPACER PANE

(defclass spacer-pane (composite-pane) ())

(defmacro spacing ((&rest options) &body contents)
  `(make-pane 'spacer-pane ,@options :contents (list ,@contents)))

(defmethod compute-space ((spacer spacer-pane))
  (let ((space (make-space-requirement))
	(margin-space (pane-space-requirement spacer))
	(child-space (pane-space-requirement (first (sheet-children spacer)))))
    (setf (space-requirement-width space) 
	  (+ (space-requirement-width margin-space)
	     (space-requirement-width child-space))
	  (space-requirement-min-width space) 
	  (+ (space-requirement-min-width margin-space)
	     (space-requirement-min-width child-space))
	  (space-requirement-max-width space) 
	  (+ (space-requirement-max-width margin-space)
	     (space-requirement-max-width child-space))
	  (space-requirement-height space) 
	  (+ (space-requirement-height margin-space)
	     (space-requirement-height child-space))
	  (space-requirement-min-height space) 
	  (+ (space-requirement-min-height margin-space)
	     (space-requirement-min-height child-space))
	  (space-requirement-max-height space) 
	  (+ (space-requirement-max-height margin-space)
	     (space-requirement-max-height child-space))
	  (pane-space-requirement spacer) space)
    space))

;;; FIXME: I don't see how this one could possibly work
(defmethod allocate-space ((spacer spacer-pane) width height)
  (set-width-and-height spacer width height)
  (let* ((h-percent (/ width (space-requirement-width (pane-space-requirement spacer))))
	 (v-percent (/ height (space-requirement-height (pane-space-requirement spacer))))
	 (child (first (sheet-children spacer)))
	 (request (pane-space-requirement child))
	 (new-width (floor (* h-percent (space-requirement-width request))))
	 (new-height (floor (* v-percent (space-requirement-height request)))))
    (multiple-value-bind (x1 y1)
	(bounding-rectangle* (sheet-region child))
      (setf (sheet-transformation child)
	    (make-translation-transformation (- x1) (- y1))))
    (allocate-space child new-width new-height)))


;;; BORDER-PANE class

(defclass border-pane (single-child-composite-pane)
  ((border-width :initarg :border-width
		 :initform 1
		 :reader border-pane-width)
   (background :initarg :background :initform nil))
  )

(defmacro bordering ((&rest options
		      &key background &allow-other-keys) contents)
  `(make-pane 'border-pane ,@options :contents (list ,contents)))

(defmethod compose-space ((bp border-pane))
  (let ((space (make-space-requirement))
	(request (compose-space (first (sheet-children bp))))
	(border-width*2 (* 2 (border-pane-width bp))))
    (setf (space-requirement-width space)
	  (+ border-width*2 (space-requirement-width request)))
    (setf (space-requirement-max-width space)
	  (+ border-width*2 (space-requirement-max-width request)))
    (setf (space-requirement-min-width space)
	  (+ border-width*2 (space-requirement-min-width request)))
    (setf (space-requirement-height space)
	  (+ border-width*2 (space-requirement-height request)))
    (setf (space-requirement-max-height space)
	  (+ border-width*2 (space-requirement-max-height request)))
    (setf (space-requirement-min-height space)
	  (+ border-width*2 (space-requirement-min-height request)))
    (setf (pane-space-requirement bp) space)
    space))

(defmethod allocate-space ((bp border-pane) width height)
  (set-width-and-height bp width height)
  (let ((border-width (border-pane-width bp))
	(child (first (sheet-children bp))))
    (setf (sheet-region child)
      (make-bounding-rectangle border-width border-width
			       (- width border-width) (- height border-width)))
    (allocate-space child (- width (* 2 border-width)) (- height (* 2 border-width)))))

;; RESTRAINING PANE

(defclass restraining-pane (composite-pane) ())

(defmacro restraining ((&rest options) &body contents)
  `(make-pane 'restraining-pane ,@options :contents (list ,@contents)))

(defmethod note-space-requirements-changed (sheet (restraining restraining-pane))
  (declare (ignore sheet))
  (values))


;; BBOARD PANE

(defclass bboard-pane (composite-pane) ())

(defmethod ask-space-children ((bboard bboard-pane))
  (values))

(defmethod compute-space ((bboard bboard-pane))
  (pane-space-requirement bboard))


;; VIEWPORT

(defclass viewport-pane (single-child-composite-pane) ())


;; SCROLLER-PANE

(defclass scroller-pane (composite-pane)
  ((scroll-bar :type (member '(t :vertical :horizontal))
	       :initform t
	       :initarg :scroll-bar
	       :accessor scroller-pane-scroll-bar)))

(defmacro scrolling ((&rest options) &body contents)
  `(let ((viewport (make-pane 'viewport-pane :contents (list ,@contents))))
     (make-pane 'scroller-pane ,@options :contents (list viewport))))

(defmethod compose-space ((pane scroller-pane))
  (if (sheet-children pane)
      (compose-space (first (sheet-children pane)))
    (make-space-requirement)))

(defmethod allocate-space ((pane scroller-pane) width height)
  (set-width-and-height pane width height)
  (let ((child (first (sheet-children pane))))
    (when child
      (setf (sheet-region child)
	(make-bounding-rectangle 0 0 width height))
      (allocate-space child width height))))

(defmethod pane-viewport ((scroller scroller-pane))
  (first (sheet-children scroller)))

(defmethod pane-viewport-region ((scroller scroller-pane))
  (sheet-region (pane-viewport scroller)))

(defmethod pane-scroller ((scroller scroller-pane))
  (scroller-pane-scroll-bar scroller))

(defmethod update-scrollbars ((pane basic-pane))
  (update-scrollbars (sheet-parent pane)))

(defmethod update-scrollbars ((pane scroller-pane))
  )

(defmethod scroll-extent ((pane clim-stream-pane) x y)
  (setf (sheet-transformation pane) (make-translation-transformation (- x) (- y)))
  (set-bounding-rectangle-position (sheet-region pane) x y)
  (update-scrollbars (sheet-parent pane))
  (clear-area pane)
  (replay (stream-output-history pane) pane)
  )


;; LABEL PANE

(defclass label-pane (composite-pane)
  ((label :type string
	  :initarg :label
	  :accessor label-pane-label)))

(defmacro labelled ((&rest options) &body contents)
  `(make-instance 'label-pane ,@options :contents (list ,@contents)))


;; FUNCTIONS

(defgeneric window-clear (clim-stream-pane))
(defgeneric window-refresh (clim-stream-pane))
(defgeneric window-viewport (clim-stream-pane))
(defgeneric window-erase-viewport (clim-stream-pane))
(defgeneric window-viewport-position (clim-stream-pane))
;(defgeneric (setf window-viewport-position) (x y clim-stream-pane))


;; CLIM-STREAM-PANE

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
   ; pour le label, il derive peut-etre de label-pane
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

(defmethod ask-space-children ((pane clim-stream-pane))
  (values))

(defmethod compute-space ((pane clim-stream-pane))
  (pane-space-requirement pane))

(defmethod window-clear ((pane clim-stream-pane))
;  (setf (pane-output-history pane) (make-instance 'standard-tree-output-history))
  (dispatch-repaint pane (sheet-region pane)))
    ;(let ((cursor (stream-text-cursor pane))) 
   ; (when cursor
    ;  (setf (cursor-position cursor) 0 0))))

(defmethod window-refresh ((pane clim-stream-pane))
  (window-clear pane)
  (when (typep pane 'output-recording-stream)
    (funcall (pane-display-function pane))))

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

		    

;; INTERACTOR PANES 

(defclass interactor-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((interactor interactor-pane) &rest args)
  (declare (ignore args))
  (setf (pane-scroll-bars interactor) :vertical))

(defmethod initialize-instance :after ((pane interactor-pane) &rest args)
  (declare (ignore args))
#+ignore  (let ((cursor (stream-text-cursor pane)))
    (setf (cursor-visibility cursor) t)))


;; APPLICATION PANES

(defclass application-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((application application-pane) &rest args)
  (declare (ignore args))
  (setf (pane-display-time application) :command-loop
	(pane-scroll-bars application) t))
	

;; COMMAND-MENU PANE

(defclass command-menu-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((command-menu command-menu-pane) &rest args)
  (declare (ignore args))
  (setf (pane-display-time command-menu) :command-loop
	(pane-incremental-redisplay command-menu) t
	(pane-scroll-bars command-menu) t
	(pane-display-function command-menu) 'diplay-command-menu))


;; TITLE PANE

(defclass title-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((title title-pane) &rest args)
  (declare (ignore args))
  (setf (pane-display-time title) t))


;; POINTER DOCUMENTATION PANE

(defclass pointer-documentation-pane (clim-stream-pane)
  ())


;; CONSTRUCTORS

(defun make-clim-stream-pane (&rest options 
				    &key (type 'clim-stream-pane)
				         (scroll-bars :vertical)
					 (border-width 1)
				    &allow-other-keys)
  (declare (ignorable scroll-bars))
  (loop for key in '(:type :scroll-bars :border-width)
	do (remf options key))
  (let ((pane (apply #'make-pane type options)))
    (if scroll-bars
	(setq pane (make-pane 'scroller-pane :scroll-bar scroll-bars
			      :contents (list (make-pane 'viewport-pane :contents (list pane))))))
    (if (and border-width
	     (> border-width 0))
	(setq pane (make-pane 'border-pane :border-width border-width :contents (list pane))))
    pane))

(defun make-clim-interactor-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'interactor-pane options))

(defun make-clim-application-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'application-pane options))


