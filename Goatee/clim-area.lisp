;;; -*- Mode: Lisp; Package: GOATEE -*-

;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)
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

(in-package :goatee)

;;; Need to support replay and redisplay (buffer has changed).  Redisplay needs
;;; to have the idea of incremental redisplay (update screen directly) and
;;; start over from scratch.  We won't hook into the CLIM idea of
;;; incremental redisplay just yet as it isn't implemented in McCLIM.

;;; cheat and use this McCLIM internal class :)
(defclass screen-area-cursor (clim-internals::cursor-mixin)
  ((screen-line :accessor screen-line :initarg :screen-line)))

(defmethod* (setf cursor-position) (nx ny (cursor screen-area-cursor))
  (when (cursor-visibility cursor)
    (error "screen-area-cursor ~S must not be visible when position is
  set"
	   cursor))
  (call-next-method))

(defmethod climi::cursor-height ((cursor screen-area-cursor))
  (let ((line (screen-line cursor)))
    (+ (ascent line) (descent line))))


(defclass screen-line (editable-area-line displayed-output-record rectangle)
  ((current-contents :accessor current-contents :initarg :current-contents
		     :initform (make-array '(1)
					   :adjustable t
					   :fill-pointer 0
					   :element-type 'character)
		     :documentation "A representation of what is, or soon will
   be, on the screen.  This does not include the buffer line's newline")
   (ascent :accessor ascent :initarg :ascent)
   (descent :accessor descent :initarg :descent)
   (baseline :accessor baseline :initarg :baseline)
   (x :initarg :x-position :initform 0)
   (y :initarg :y-position :initform 0)
   (parent :initarg :parent :initform nil :reader output-record-parent)
   (width :accessor width :initarg :width)
   (cursor :accessor cursor :initarg :cursor :initform nil)))

(defun line-contents-sans-newline (buffer-line &key destination)
  (let* ((line-size (size buffer-line)))
    (if (zerop line-size)
	(if destination
	    (progn
	      (setf (fill-pointer destination) 0)
	      destination)
	    "")
	(let* ((last-char (char-ref buffer-line (1- line-size)))
	       (contents-size (if (char= last-char #\Newline )
				  (1- line-size)
				  line-size)))
	  (if destination
	      (progn
		(adjust-array destination contents-size
			      :fill-pointer contents-size)
		(flexivector-string-into buffer-line destination
					 :end2 contents-size))
	      (flexivector-string buffer-line :end contents-size))))))


(defmethod initialize-instance :after
    ((obj screen-line) &key (current-contents nil current-contents-p))
  (declare (ignore current-contents))
  (when (and (not current-contents-p) (slot-boundp obj 'buffer-line))
    (line-contents-sans-newline (buffer-line obj)
				:destination (current-contents obj)))
  (unless (slot-boundp obj 'width)
    (let ((stream (stream (output-record-parent obj))))
     (setf (width obj) (text-size stream (current-contents obj)))))
  (unless (slot-boundp obj 'baseline)
    (multiple-value-bind (x y)
	(output-record-position obj)
      (declare (ignore x))
      (setf (baseline obj) (+ y (ascent obj))))))


(defmethod output-record-position ((record screen-line))
  (values (slot-value record 'x) (slot-value record 'y)))

(defmethod* (setf output-record-position) (nx ny (record screen-line))
  (setf (values (slot-value record 'x) (slot-value record 'y))
	(values nx ny)))

(defmethod bounding-rectangle* ((record screen-line))
  (let ((x (slot-value record 'x))
	(y (slot-value record 'y)))
    (values x
	    y
	    (+ x (slot-value record 'width))
	    (+ y (slot-value record 'ascent) (slot-value record 'descent)))))

;;; Implement the rectangle protocol; now region stuff should work.
(defmethod rectangle-edges* ((record screen-line))
  (bounding-rectangle* record))

(defmethod map-over-output-records (function (record screen-line)
				    &optional (x-offset 0) (y-offset 0)
				    &rest function-args)
  (declare (ignore function x-offset y-offset function-args))
  nil)

(defmethod map-over-output-records-overlapping-region
	   (function (line screen-line) region
	    &optional x-offset y-offset &rest continuation-args)
  (declare (ignore function region x-offset y-offset continuation-args))
  nil)

(defmethod map-over-output-records-containing-position
	   (function (line screen-line) x y
	    &optional x-offset y-offset &rest continuation-args)
  (declare (ignore function x y x-offset y-offset continuation-args))
  nil)

(defmethod replay-output-record ((record screen-line) stream
				 &optional region (x-offset 0) (y-offset 0))
  (let ((medium (sheet-medium stream))
	(cursor (cursor record)))
    (letf (((medium-text-style medium)
	    (text-style (output-record-parent record)))
	   ((medium-transformation medium)
	    (make-translation-transformation x-offset y-offset)))
      (when (and cursor (cursor-visibility cursor))
	(climi::display-cursor cursor :erase))
      (multiple-value-bind (x y) (output-record-position record)
	(declare (ignore y))
	;; Is this necessary?
	(with-output-recording-options (stream :record nil)
	  (draw-text* stream (current-contents record)
		      x (slot-value record 'baseline))))
      (when (and cursor (cursor-visibility cursor))
	(climi::flip-screen-cursor cursor)))))

(defclass simple-screen-area (editable-area standard-sequence-output-record)
  ((text-style :accessor text-style :initarg :text-style)
   (vertical-spacing :accessor vertical-spacing :initarg :vertical-spacing)
   (cursor :accessor cursor)
   (stream :accessor stream :initarg :stream)))

(defmethod initialize-instance :after ((area simple-screen-area)
				       &key stream)
  (when (not (slot-boundp area 'text-style))
    (if stream
	(setf (text-style area) (medium-text-style stream))
	(error "One of :text-style or :stream must be specified.")))
  (when (not (slot-boundp area 'vertical-spacing))
    (if stream
	(setf (vertical-spacing area) (stream-vertical-spacing stream))
	(error "One of :vertical-spacing or :stream must be specified.")))
  (when (not (slot-boundp area 'cursor))
    (multiple-value-bind (x y)
	(output-record-position area)
      (setf (cursor area)
	  (make-instance 'screen-area-cursor
			 :sheet (stream area)
			 :x-position x
			 :y-position y))))
  (initialize-area-from-buffer area (buffer area))
  (setf (cursor-visibility (cursor area)) t)
  (tree-recompute-extent area))

(defmethod output-record-children ((area simple-screen-area))
  (loop for line = (area-first-line area) then (next line)
	while line
	collect line))

(defmethod map-over-output-records (function (record simple-screen-area)
				    &optional (x-offset 0) (y-offset 0)
				    &rest function-args)
  (declare (ignore x-offset y-offset))
  (loop for line = (area-first-line record) then (next line)
	while line
	do (apply function line function-args)))

(defmethod initialize-area-from-buffer ((area simple-screen-area) buffer)
  ;; XXX Stupid, but eventually will be different per line.
  (with-slots (vertical-spacing)
      area
    (multiple-value-bind (parent-x parent-y)
	(output-record-position area)
      (let* ((stream (stream area))
	     (ascent (text-style-ascent (text-style area) stream))
	     (descent (text-style-descent (text-style area) stream)))
	 (loop for buffer-line = (dbl-head (lines buffer))
	       then (and buffer-line (next buffer-line))
	       for prev-area-line = (lines area) then area-line
	       for y = parent-y then (+ y ascent descent vertical-spacing)
	       for area-line = (and buffer-line
				    (make-instance 'screen-line
						   :x-position parent-x
						   :y-position y
						   :parent area
						   :buffer-line buffer-line
						   :last-tick -1
						   :editable-area area
						   :ascent ascent
						   :descent descent))
	       while buffer-line
	       do (progn
		    (dbl-insert-after area-line prev-area-line)
		    (line-update-cursor area-line (stream area)))))))
  area)

(defgeneric redisplay-area (area))

(defmethod redisplay-area ((area simple-screen-area))
  (let ((stream (stream area)))
    (loop for line = (area-first-line area) then (next line)
	  while line
	  do (multiple-value-bind (line-changed dimensions-changed)
		 (maybe-update-line-dimensions line)
	       (declare (ignore dimensions-changed)) ;XXX
	       (when line-changed
		 (redisplay-line line stream))))))

(defmethod get-line-differences ((line screen-line))
  (with-slots (current-contents
	       buffer-line)
      line
    (let* ((current-length (length current-contents))
	   (line-length (size buffer-line))
	   (min-length (min current-length line-length)))
      (multiple-value-bind (unchanged common-beginning)
	  (loop for i from 0 below min-length
		while (char= (char current-contents i)
			     (char-ref buffer-line i))
		finally (return (values (and (eql current-length line-length)
					     (eql min-length current-length)
					     (eql i min-length))
					i)))
	(when unchanged
	  (return-from get-line-differences
	    (values t current-length 0 line-length 0)))
	;; Determine the common string at the line end
	(loop for i downfrom (1- current-length)
	      for j downfrom (1- line-length)
	      while (and (>= i 0) (>= j 0) (char= (char current-contents i)
						  (char-ref buffer-line j)))
	      finally (return (values nil
					common-beginning
					(1+ i)
					common-beginning
					(1+ j))))))))


;;; Two steps to redisplaying a line: figure out if the
;;; ascent/descent/baseline have changed, then render the line, incrementally
;;; or not.

(defmethod maybe-update-line-dimensions ((line screen-line))
  "returns 2 values: contents changed, dimensions changed"
  (if (eql (last-tick line) (tick (buffer-line line)))
      (values nil nil)
      (values t nil)))

;;; Line's coordinates are now correct.
;;;
;;; Strategy: Split the line up into 3 parts: unchanged text at beginning of
;;; line, changed text in middle, unchanged text line at end of line.  Any of
;;; these may be empty.  To redisplay, move the unchanged text at the end into
;;; position, then erase and display the middle text.
(defmethod redisplay-line ((line screen-line) stream)
  (let* ((medium (sheet-medium stream))
	 (style (text-style (output-record-parent line)))
	 (cursor-visible nil))
    (with-slots (current-contents ascent descent baseline cursor buffer-line)
	line
      (multiple-value-bind (unchanged
			    current-unchanged-from-start
			    current-unchanged-from-end
			    line-unchanged-from-start
			    line-unchanged-from-end)
	  (get-line-differences line)
	(when (and cursor (setq cursor-visible (cursor-visibility cursor)))
	  (setf (cursor-visibility cursor) nil))
	(unless unchanged
	  (let* ((start-width (if (> current-unchanged-from-start 0)
				  (text-size medium current-contents
					     :text-style style
					     :end current-unchanged-from-start)
				  0))
		 (line-end (text-size medium current-contents))
		 (current-unchanged-left
		  (if (< current-unchanged-from-end (length current-contents))
		      (text-size medium current-contents
				 :text-style style
				 :end current-unchanged-from-end)
		      line-end))
		 (new-line-size (size buffer-line)))
	    ;; Having all we need from the old contents of the line, update
	    ;; with the new contents
	    (when (> new-line-size (car (array-dimensions current-contents)))
	      (adjust-array current-contents (list new-line-size)))
	    (setf (fill-pointer current-contents) new-line-size)
	    (flexivector-string-into buffer-line current-contents)
	    (let* ((new-line-end (text-size medium current-contents))
		   (new-unchanged-left
		    (if (< line-unchanged-from-end (length current-contents))
			(text-size medium current-contents
				   :text-style style
				   :end line-unchanged-from-end)
			new-line-end)))
	      (multiple-value-bind (x y)
		  (output-record-position line)
		;; Move unchanged text at the end of line, if needed
		(when (and (not (eql line-unchanged-from-end new-line-size))
			   (not (eql current-unchanged-left
				     new-unchanged-left)))
		  (copy-area medium
			     (+ current-unchanged-left x)
			     y
			     (- line-end current-unchanged-left)
			     (+ ascent descent)
			     (+ new-unchanged-left x)
			     y))
		;; If the line is now shorter, erase the old end of line.
		(erase-line line medium new-line-end line-end)
		;; Erase the changed middle
		(erase-line line medium start-width new-unchanged-left)
		;; Draw the middle
		(when (< line-unchanged-from-start line-unchanged-from-end)
		  (draw-text* medium current-contents
			      (+ x start-width) baseline
			      :start line-unchanged-from-start
			      :end line-unchanged-from-end)))
	      ;; Old, wrong, bounding rectangle
	      (with-bounding-rectangle* (old-min-x old-min-y old-max-x old-max-y)
		  line
		(setf (width line) new-line-end)
		(recompute-extent-for-changed-child (output-record-parent line)
						    line
						    old-min-x old-min-y
						    old-max-x old-max-y)))))
	;; Now deal with the cursor
	(line-update-cursor line stream)
	(when cursor
	  (setf (cursor-visibility cursor) t))))))

(defmethod line-update-cursor ((line screen-line) stream)
  (multiple-value-bind (point-line point-pos)
      (point* (buffer (editable-area line)))
    (with-slots (cursor baseline ascent current-contents x) line
      (if (eq point-line (buffer-line line))
	  (setf cursor (cursor (editable-area line)))
	  (setf cursor nil))
      (when cursor
	(let ((cursor-x (+ x
			   (stream-string-width
			    stream
			    current-contents
			    :end point-pos
			    :text-style (text-style (editable-area line))))))
	  (setf (screen-line cursor) line)
	  (setf (cursor-position cursor)
		(values cursor-x
			(- baseline ascent))))))))

(defmethod erase-line ((line screen-line) medium left right)
  "Erase line from left to right (which are relative to the line
origin)"
  (when (< left right)
    (multiple-value-bind (x y)
	(output-record-position line)
      (with-slots (ascent descent)
	  line
	(draw-rectangle* medium
			 (+ left x) y
			 (+ x right) (+ y ascent descent)
			 :ink (medium-background medium)
			 :filled t)))))
