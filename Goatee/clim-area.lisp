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
;;; start over from scratch.


(defclass screen-line (editable-area-line displayed-output-record)
  ((current-contents :accessor current-contents :initarg :current-contents
		     :initform (make-array '(1)
					   :adjustable t
					   :fill-pointer 0)
		     :documentation "A representation of what is, or soon will
be, on the screen")
   (ascent :accessor ascent :initarg :ascent)
   (decent :accessor decent :initarg :decent)
   (baseline :accessor baseline :initarg :baseline)))


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
  (let ((medium (sheet-medium stream)))
    (letf (((medium-text-style medium)
	    (text-style (output-record-parent record)))
	   ((medium-transformation medium)
	    (make-translation-transformation x-offset y-offset)))
      (multiple-value-bind (x y) (output-record-position record)
	(declare (ignore y))
	;; Is this necessary?
	(with-output-recording-options (stream :record nil)
	  (draw-text* stream (current-contents record) x baseline))))))

(defclass simple-screen-area (editable-area output-record)
  ((text-style :accessor text-style :initarg :text-style)
   (vertical-spacing :accessor vertical-spacing :initarg :vertical-spacing)))


(defmethod initialize-area-from-buffer ((area simple-screen-area) buffer)
  ;; XXX Stupid, but eventually will be different per line.
  (with-slots (vertical-spacing)
      area
    (multiple-value-bind (parent-x parent-y)
	(output-record-position area)
      (let ((ascent (text-style-ascent (text-style area)))
	    (descent (text-style-descent (text-style area)))
	    (loop for buffer-line = (next (lines buffer))
		  then (and buffer-line (next buffer-line))
		  for prev-area-line = (lines area) then area-line
		  for y = parent-y then (+ y ascent descent vertical-spacing)
		  for area-line = (and buffer-line
				       (make-instance 'screen-line
						      :x parent-x
						      :y y
						      :parent area
						      :buffer-line buffer-line
						      :last-tick -1
						      :editable-area area
						      :ascent ascent
						      :descent descent))
		  while buffer-line
		  do (dbl-insert-after area-line prev-area-line))))))
  area)

(defmethod compare-contents ((line screen-line))
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
	  (return-from compare-contents
	    (values t current-length 0 line-length 0)))
	;; Determine the common string at the line end
	(loop for i downfrom (1- current-length) to 0
	      for j downfrom (1- line-length) to 0
	      while (char= (char current-contents i)
			   (char-ref buffer-line j))
	      finally (return (values nil
				      common-beginning
				      i
				      common-beginning
				      j)))))))


;;; Two steps to redisplaying a line: figure out if the
;;; ascent/decent/baseline have changed, then render the line, incrementally
;;; or not.

(defmethod maybe-update-line-dimensions ((line screen-line) stream)
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
	 (style (text-style (output-record-parent line))))
    (with-slots (current-contents ascent descent baseline)
	line
      (multiple-value-bind (unchanged
			    current-unchanged-from-start
			    current-unchanged-from-end
			    line-unchanged-from-start
			    line-unchanged-from-end)
	  (compare-contents line)
	(when unchanged
	  (return-from redisplay-line nil))
	(let* ((start-width (if (> current-unchanged-from-start 0)
				(text-size medium current-contents
					   :text-style style
					   :end current-unchanged-from-start)
				0))
	       (line-end (text-size medium current-contents))
	       (current-unchanged-left
		(if (< current-unchanged-from-end
		       (length current-contents))
		    (text-size medium current-contents
			       :text-style style
			       :end current-unchanged-from-end)
		    line-end))
	       (new-line-size (size line)))
	  ;; Having all we need from the old contents of the line, update
	  ;; with the new contents
	  (when (> new-line-size (car (array-dimensions current-contents)))
	    (adjust-array current-contents (list new-line-size)))
	  (setf (fill-pointer current-contents) new-line-size)
	  (flexivector-string-into line current-contents)
	  (let* ((new-line-end (text-size medium current-contents))
		 (new-unchanged-left
		  (if (< line-unchanged-from-end (length current-contents))
		      (text-size medium current-contents
				 :text-style style
				 :end line-unchanged-from-end)
		      new-line-end)))
	    (multiple-value-bind (x y)
		(output-record-position line)
	      (when (and (not (eql line-unchanged-from-end new-line-size ))
			 (not (eql current-unchanged-left new-unchanged-left)))
		(copy-area medium
			   (+ current-unchanged-left x)
			   y
			   (- line-end current-unchanged-left)
			   (- descent ascent)
			   (+ new-unchanged-left x)
			   y)
		;; If the line is now shorter, erase the old end of line.
		(erase-line line medium new-line-end line-end)
		;; Erase the changed middle
		(erase-line line medium start-width new-unchanged-left)
		;; Draw the middle
		(when (< line-unchanged-from-start line-unchanged-from-end)
		  (draw-text* medium current-contents
			      (+ x start-width) baseline
			      :start line-unchanged-from-start
			      :end line-unchanged-from-end))))))))))


(defmethod erase-line ((line screen-line) medium left right)
  "Erase line from left to right (which are relative to the line
origin)"
  (when (< left right)
    (multiple-value-bind (x y)
	(output-record-position line)
      (with-slots (ascent descent)
	  line
	(draw-rectangle* medium (+ left x) y (- right left) (- ascent descent)
			 :ink (medium-background medium)
			 :filled t)))))


(defmethod redisplay-window ((pane goatee-pane) region)
  (declare (ignore region))
  (with-sheet-medium (medium pane)
    (let ((line-dy (+ (text-style-height (text-style pane) medium)
			     (vertical-spacing pane)))
	  (region (sheet-region pane)))
      (multiple-value-bind (x1 y1 x2 y2)
	  (bounding-rectangle* (sheet-region pane))
	(draw-rectangle* pane 0 0 (- x2 x1) (- y2 y1)
			 :ink (gadget-current-color pane) :filled t)
	(loop for line-dbl = (dbl-head (lines pane)) then (next line-dbl)
	      while line-dbl
	      for line = (contents line-dbl)
	      for y = 0 then (+ y line-dy)
	      do (draw-text* pane (chars line)
			     0 y
			     :align-y :top))))))
