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
   (space-requirement :initform (make-space-requirement :width 100 :max-width 100
							:height 100 :max-height 100)
		      :accessor pane-space-requirement)
   )
  )

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

(defmethod allocate-space ((pane pane) width height)
  (declare (ignore width height)))



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


;;; Composite-Pane class

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
      (loop for child in contents
	  do (sheet-adopt-child pane child))))

(defmethod compose-space ((box composite-pane))
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

(defmethod allocate-space ((box composite-pane) width height)
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement box))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement box))))
	(y 0))
    (loop for child in (sheet-children box)
	for request = (pane-space-requirement child)
	for new-width = (floor (* h-percent (space-requirement-width request)))
	for new-height = (floor (* v-percent (space-requirement-height request)))
	do (setf (sheet-region child)
	     (make-bounding-rectangle 0 y new-width (+ y new-height)))
	   (allocate-space child new-width new-height)
	   (incf y new-height))))


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
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement box))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement box))))
	(x 0))
    (loop for child in (sheet-children box)
	for request = (pane-space-requirement child)
	for new-width = (floor (* h-percent (space-requirement-width request)))
	for new-height = (floor (* v-percent (space-requirement-height request)))
	do (setf (sheet-region child)
	     (make-bounding-rectangle x 0 (+ x new-width) new-height))
	   (allocate-space child new-width new-height)
	   (incf x new-width))))


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
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement box))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement box))))
	(y 0))
    (loop for child in (sheet-children box)
	for request = (pane-space-requirement child)
	for new-width = (floor (* h-percent (space-requirement-width request)))
	for new-height = (floor (* v-percent (space-requirement-height request)))
	do (setf (sheet-region child)
	     (make-bounding-rectangle 0 y new-width (+ y new-height)))
	   (allocate-space child new-width new-height)
	   (incf y new-height))))


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
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement rack))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement rack))))
	(x 0))
    (loop for child in (sheet-children rack)
	for request = (pane-space-requirement child)
	for new-width = (floor (* h-percent (space-requirement-width request)))
	for new-height = (floor (* v-percent (space-requirement-height request)))
	do (setf (sheet-region child)
	     (make-bounding-rectangle x 0 (+ x new-width) new-height))
	   (allocate-space child new-width new-height)
	   (incf x new-width))))


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
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement rack))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement rack))))
	(y 0))
    (loop for child in (sheet-children rack)
	for request = (pane-space-requirement child)
	for new-width = (floor (* h-percent (space-requirement-width request)))
	for new-height = (floor (* v-percent (space-requirement-height request)))
	do (setf (sheet-region child)
	     (make-bounding-rectangle 0 y width (+ y new-height)))
	   (allocate-space child new-width new-height)
	   (incf y new-height))))


;;; INTERACTOR-PANE class

(defclass clim-stream-pane (output-recording-stream basic-pane)
  ()
  )

(defmethod compose-space ((pane clim-stream-pane))
  (setf (pane-space-requirement pane)
    (make-space-requirement :width 400
			    :max-width 800
			    :height 200
			    :max-height 200)))


;;; INTERACTOR-PANE class

(defclass interactor-pane (clim-stream-pane)
  ()
  )

(defmethod initialize-instance :after ((pane interactor-pane) &rest args)
  (declare (ignore args))
#+ignore  (let ((cursor (stream-text-cursor pane)))
    (setf (cursor-visibility cursor) t)))


;;; APPLICATION-PANE class

(defclass application-pane (clim-stream-pane)
  ((incremental-redisplay :initarg :incremental-redisplay
			  :initform nil)
   (display-function :initarg :display-function
		     :initform nil)
   )
  )


;;; BORDER-PANE class

(defclass border-pane (composite-pane)
  ((border-width :initarg :border-width
		 :initform 1
		 :reader border-pane-width))
  )

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
  (let ((h-percent (/ width (space-requirement-width (pane-space-requirement bp))))
	(v-percent (/ height (space-requirement-height (pane-space-requirement bp))))
	(y 0))
    (loop for child = (first (sheet-children bp))
	for request = (pane-space-requirement child)
	for new-width = (floor (* h-percent (space-requirement-width request)))
	for new-height = (floor (* v-percent (space-requirement-height request)))
	do (setf (sheet-region child)
	     (make-bounding-rectangle 0 y width (+ y new-height)))
	   (allocate-space child new-width new-height)
	   (incf y new-height))))

