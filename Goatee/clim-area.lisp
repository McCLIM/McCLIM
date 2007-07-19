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
;;; start over from scratch.  Note that this is different from the
;;; CLIM concept of incremental redisplay, which happens when commands
;;; are executed (usually).

;;; cheat and use this McCLIM internal class :)
(defclass screen-area-cursor (clim-internals::cursor-mixin cursor)
  ((screen-line :accessor screen-line :initarg :screen-line))
  (:default-initargs :appearance :solid))

(defmethod* (setf cursor-position) (nx ny (cursor screen-area-cursor))
  (declare (ignore nx ny))
  (when (and (cursor-state cursor)
	     (stream-drawing-p (cursor-sheet cursor)))
    (error "screen-area-cursor ~S must not be visible when position is
  set"
	   cursor))
  (call-next-method))

(defmethod climi::cursor-height ((cursor screen-area-cursor))
  (let ((line (screen-line cursor)))
    (+ (ascent line) (descent line))))

(defgeneric line-text-width (area line &key start end)
  (:documentation "The width text in line's current-contents from START to END,
  NOT including line wrap."))

(defclass simple-screen-area (editable-area standard-sequence-output-record)
  ((text-style :accessor text-style :initarg :text-style)
   (vertical-spacing :accessor vertical-spacing :initarg :vertical-spacing)
   (cursor :accessor cursor)
   (area-stream :accessor area-stream :initarg :area-stream)
   (max-width :accessor max-width :initarg :max-width :initform nil
	      :documentation "Maximum available width for area.")
   (gutter-width :accessor gutter-width :initarg :gutter-width :initform 12
		 :documentation "Width of gutter at end of line")
   (foreground-ink :accessor foreground-ink :initarg :foreground-ink
		   :documentation "Default foreground color (ink) for area")
   (background-ink :accessor background-ink :initarg :background-ink
		   :documentation "Default background color (ink) for area"))
  (:documentation "A Goatee editable area implemented as an output record."))

(defmethod initialize-instance :after ((area simple-screen-area)
				       &key area-stream
				       (cursor-visibility :on))
  (when (not (slot-boundp area 'text-style))
    (if area-stream
	(setf (text-style area) (medium-text-style area-stream))
	(error "One of :text-style or :area-stream must be specified.")))
  (when (not (slot-boundp area 'vertical-spacing))
    (if area-stream
	(setf (vertical-spacing area) (stream-vertical-spacing area-stream))
	(error "One of :vertical-spacing or :stream must be specified.")))
  (when (not (slot-boundp area 'cursor))
    (multiple-value-bind (x y)
	(output-record-position area)
      (setf (cursor area)
	  (make-instance 'screen-area-cursor
			 :sheet (area-stream area)
			 :x-position x
			 :y-position y))))
  (when (not (slot-boundp area 'max-width))
    (setf (max-width area) (if area-stream
			       (- (stream-text-margin area-stream)
				  (output-record-position area)) ; x
			       (* 80 9))))
  (when (not (slot-boundp area 'foreground-ink))
    (setf (foreground-ink area) (medium-foreground area-stream)))
  (when (not (slot-boundp area 'background-ink))
    (setf (background-ink area) (medium-background area-stream)))
  (initialize-area-from-buffer area (buffer area))
  (setf (cursor-visibility (cursor area)) cursor-visibility)
  (tree-recompute-extent area))

(defmethod cursor-visibility ((area simple-screen-area))
  (cursor-visibility (cursor area)))

(defmethod (setf cursor-visibility) (vis (area simple-screen-area))
  (setf (cursor-visibility (cursor area)) vis))

(defmethod line-text-width ((area simple-screen-area)
			    ;; XXX need a less implementation-dependent class
			    (line extent-buffer-line) 
			    &key (start 0) (end (line-last-point line)))
  "Compute the width of a buffer line if it were to be displayed."
  (let ((stream (area-stream area))
	(text-style (text-style area)))
    (loop for i from start below end
	  for char = (char-ref line i)
	  sum (text-size stream char :text-style text-style))))

(defmethod* (setf output-record-position) :around
  (nx ny (record simple-screen-area))
  (multiple-value-bind (x y)
      (output-record-position record)
    (multiple-value-prog1
	(call-next-method)
      (let ((cursor (cursor record)))
	(multiple-value-bind (cx cy)
	    (cursor-position cursor)
	  (setf (cursor-position cursor)
		(values (+ cx (- nx x))
			(+ cy (- ny y)))))))))

(defclass screen-line (editable-area-line displayed-output-record
					  climi::basic-output-record)
  ((current-contents :accessor current-contents :initarg :current-contents
		     :initform (make-array '(1)
					   :adjustable t
					   :fill-pointer 0
					   :element-type 'character)
		     :documentation "A representation of what is, or soon will
   be, on the screen.  This does not include the buffer line's newline")
   (ascent :accessor ascent :initarg :ascent)
   (descent :accessor descent :initarg :descent)
   (baseline :accessor baseline :initarg :baseline
	     :documentation "The y coordinate of the line's
 baseline. This is an absolute coordinate, not relative to the output record.")
   (width :accessor width :initarg :width)
   (cursor :accessor cursor :initarg :cursor :initform nil)
   (line-breaks :accessor line-breaks :initform nil)))

(defmethod print-object ((obj screen-line) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-bounding-rectangle* (x1 y1 x2 y2)
	obj
      (format stream "X ~S:~S Y ~S:~S " x1 x2 y1 y2)
      (write (current-contents obj) :stream stream))))

(defmethod (setf output-record-position) :around
    (nx ny (record screen-line))
  (declare (ignore nx))
  (multiple-value-bind (x y)
      (output-record-position record)
    (declare (ignore x))
    (multiple-value-prog1
	(call-next-method)
      (incf (baseline record) (- ny y)))))

(defmethod (setf width) :after (width (line screen-line))
  (climi::with-standard-rectangle* (:x1 x1 :y1 y1 :y2 y2)
      line
    (setf (rectangle-edges* line) (values x1 y1 (+ x1 width) y2))))

(defmethod (setf ascent) :after (ascent (line screen-line))
    (climi::with-standard-rectangle* (:x1 x1 :y1 y1 :x2 x2)
	line
      (setf (rectangle-edges* line) (values x1 y1 x2 (+ y1 ascent)))))

(defmethod (setf descent) :after (descent (line screen-line))
  (climi::with-standard-rectangle* (:x1 x1 :y1 y1 :x2 x2)
      line
    (setf (rectangle-edges* line) (values x1 y1 x2 (+ y1 descent)))))

(defun line-contents-sans-newline (buffer-line &key destination)
  (let* ((contents-size (line-last-point buffer-line)))
    ;; XXX Should check entire string for "non-printable" characters
    (when (and (> contents-size 0)
	       (eql (char-ref buffer-line (1- contents-size)) #\Newline))
      (decf contents-size))
    (if (zerop contents-size)
	(if destination
	    (progn
	      (setf (fill-pointer destination) 0)
	      destination)
	    "")
	(if destination
	    (progn
	      (adjust-array destination contents-size
			    :fill-pointer contents-size)
	      (flexivector-string-into buffer-line destination
				       :end2 contents-size))
	    (flexivector-string buffer-line :end contents-size)))))

(defmethod line-text-width ((area simple-screen-area) (line screen-line)
			    &key (start 0)
			    (end (length (current-contents line))))
  (text-size (area-stream area) (current-contents line)
                               :start start
                               :end end))

(defmethod initialize-instance :after
    ((obj screen-line) &key (current-contents nil current-contents-p))
  (declare (ignore current-contents))
  (when (and (not current-contents-p) (slot-boundp obj 'buffer-line))
    (line-contents-sans-newline (buffer-line obj)
				:destination (current-contents obj)))
  (unless (slot-boundp obj 'width)
    (setf (width obj) (line-text-width (editable-area obj) obj)))
  (unless (slot-boundp obj 'baseline)
    (climi::with-standard-rectangle* (:x1 x1 :y1 y1 :x2 x2)
	obj
      (setf (rectangle-edges* obj)
	    (values x1 y1 x2 (+ y1 (ascent obj) (descent obj))))
      (setf (baseline obj) (+ y1 (ascent obj))))))

(defmethod bounding-rectangle* ((record screen-line))
  (let ((cursor (cursor record)))
    (multiple-value-bind (x1 y1 x2 y2) (call-next-method)
      (values x1 y1
              (if (and cursor (eq (cursor-visibility cursor) :on))
                  (with-slots (climi::x climi::width) cursor
                     (max x2 (+ climi::x climi::width)))
                  x2)
              (if (and cursor (eq (cursor-visibility cursor) :on))
                  (max y2 (+ y1 (climi::cursor-height cursor)))
                  y2)))))

(defmethod climi::map-over-output-records-1 (function (record screen-line)
				      function-args)
  (declare (ignore function function-args))
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

(defmethod foreground-ink ((line screen-line))
  (foreground-ink (output-record-parent line)))

(defmethod background-ink ((line screen-line))
  (background-ink (output-record-parent line)))

(defmethod replay-output-record ((record screen-line) stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region x-offset y-offset))
  (let ((medium (sheet-medium stream))
	(cursor (cursor record)))
    (letf (((medium-text-style medium)
	    (text-style (output-record-parent record)))
	   ((medium-transformation medium)
            +identity-transformation+) ; Is it necessary?
           )
      (when (and cursor (cursor-state cursor))
	(climi::display-cursor cursor :erase))
      (multiple-value-bind (x y) (output-record-position record)
	(declare (ignore y))
	(draw-text* stream (current-contents record)
                    x (slot-value record 'baseline)
		    :ink (foreground-ink record)))
      (when (and cursor (cursor-state cursor))
	(climi::flip-screen-cursor cursor)))))

(defmethod output-record-refined-position-test ((record screen-line) x y)
  (declare (ignore x y))
  t)

(defgeneric max-text-width (area)
  (:documentation "The width available for text in an area."))

(defmethod max-text-width ((area simple-screen-area))
  (- (max-width area) (gutter-width area)))

(defmethod output-record-children ((area simple-screen-area))
  (loop for line = (area-first-line area) then (next line)
	while line
	collect line))

(defmethod add-output-record (child (area simple-screen-area))
  (declare (ignore child))
  (error "add-output-record shouldn't be called on simple-screen-area"))

(defmethod delete-output-record (child (record simple-screen-area)
				 &optional (errorp t))
  (declare (ignore child errorp))
  (error "delete-output-record shouldn't be called on simple-screen-area"))

(defmethod clear-output-record ((record simple-screen-area))
  (error "clear-output-record shouldn't be called on simple-screen-area"))

(defmethod climi::map-over-output-records-1 (function (record simple-screen-area)
				      function-args)
  (if function-args
      (loop for line = (area-first-line record) then (next line)
	 while line
	 do (apply function line function-args))
      (loop for line = (area-first-line record) then (next line)
	while line
	do (funcall function line))))

;;; Since lines don't overlap, we can use the same order for
;;; map-over-output-records-containing-position and
;;; map-over-output-records-overlapping-region.

(defmethod map-over-output-records-containing-position
    (function (record simple-screen-area) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (flet ((mapper (child)
	   (multiple-value-bind (min-x min-y max-x max-y)
	       (output-record-hit-detection-rectangle* child)
	     (when (and (<= min-x x max-x)
			(<= min-y y max-y)
			(output-record-refined-position-test child
							     x y))
	       (apply function child function-args)))))
    (declare (dynamic-extent #'mapper))
    (map-over-output-records #'mapper record)))

(defmethod map-over-output-records-overlapping-region
    (function (record simple-screen-area) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (flet ((mapper (child)
	   (when (region-intersects-region-p region child)
	       (apply function child function-args))))
    (declare (dynamic-extent #'mapper))
    (map-over-output-records #'mapper record)))

(defmethod initialize-area-from-buffer ((area simple-screen-area) buffer)
  (setf (area-bp-start area) (copy-location (buffer-start buffer)))
  (setf (area-bp-end area) (copy-location (buffer-end buffer)))
  ;; XXX Stupid, but eventually will be different per line.
  (with-slots (vertical-spacing)
      area
    (multiple-value-bind (parent-x parent-y)
	(output-record-position area)
      (let* ((stream (area-stream area))
	     (ascent (text-style-ascent (text-style area) stream))
	     (descent (text-style-descent (text-style area) stream))
	     (last-buffer-line (line (area-bp-end area))))
	 (loop for buffer-line = (line (area-bp-start area))
	         then (next buffer-line)
	       for prev-area-line = (lines area) then area-line
	       for y = parent-y then (+ y ascent descent vertical-spacing)
	       for area-line = (make-instance 'screen-line
					      :x-position parent-x
					      :y-position y
					      :parent area
					      :buffer-line buffer-line
					      :last-tick (tick buffer-line)
					      :editable-area area
					      :ascent ascent
					      :descent descent)
	       do (progn
		    (dbl-insert-after area-line prev-area-line)
		    (line-update-cursor area-line (area-stream area)))
	       until (eq buffer-line last-buffer-line)
	       finally (setf (last-line area) area-line)))))
  area)

;;; Redisplay consists of two parts.  First, the buffer is examined for new
;;; lines, deleted lines, or scrolling (eventually).  Lines are moved to the
;;; right location.  Any new lines are rendered.  Then, individual lines are
;;; examined and incrementally updated.
;;;
;;; For these two operations we use a simple strategy.  Divide the thing being
;;; updated -- area or individual line -- into unchanged stuff at its
;;; beginning, a changed middle, and unchanged stuff at the end.  Then move the
;;; unchanged end into its new position, erase the middle and any of the end
;;; left behind, and draw the new middle.

(defgeneric redisplay-all (area)
  (:documentation "Reinitialize the area's screen state, clear the area and
  redraw everything."))

(defmethod redisplay-all ((area simple-screen-area))
  (dbl-kill-after (lines area))
  (setf (line (area-bp-start area)) nil)
  (setf (line (area-bp-end area)) nil)
  (letf (((cursor-visibility (cursor area)) :off))
    (initialize-area-from-buffer area (buffer area)))
  (with-bounding-rectangle* (x1 y1 x2 y2)
      area
    (let* ((stream (area-stream area))
	   (medium (sheet-medium stream)))
      (draw-rectangle* medium x1 y1 x2 y2
			 :ink (background-ink area)
			 :filled t)))
  (replay area (area-stream area)))

(defgeneric redisplay-area (area))

(defmethod get-area-differences ((area simple-screen-area))
  (let ((buf-start (line (area-bp-start area)))
	(buf-end (line (area-bp-end area))))
    (multiple-value-bind (unchanged area-beginning-end buffer-beginning-end)
	(loop for line = (area-first-line area) then (next line)
	      for prev-line = nil then line
	      for buffer-line = buf-start then (next buffer-line)
	      for prev-buffer-line = nil then buffer-line
	      if (or (null line)
		     (not (eq (buffer-line line) buffer-line)))
	        return (values nil prev-line prev-buffer-line)
              do (progn nil)            ;XXX workaround CMUCL bug
                                        ; Is it still necessary? -- APD, 2002-06-25
	      until (eq buffer-line buf-end)
	      ;; If there are still lines in the area list, then there was a
	      ;; change.
	      finally (return (values (null (next line)) line buffer-line)))
      (when unchanged
	(return-from get-area-differences
	  (values t
		  area-beginning-end (area-first-line area)
		  buffer-beginning-end buf-start)))
      (loop for line = (last-line area) then (prev line)
	      for prev-line = nil then line
	      for buffer-line = buf-end then (prev buffer-line)
	      for prev-buffer-line = nil then buffer-line
	      if (or (eq line (lines area))
		     (not (eq (buffer-line line) buffer-line)))
	        return (values nil
			       area-beginning-end prev-line
			       buffer-beginning-end prev-buffer-line)
              do (progn nil)            ;XXX workaround CMUCL bug
                                        ; Is it still necessary? -- APD, 2002-06-25
	      until (eq buffer-line buf-start)
	      finally (return (values nil
				      area-beginning-end line
				      buffer-beginning-end buffer-line))))))

(defmethod redisplay-area ((area simple-screen-area))
  (let ((stream (area-stream area)))
    (multiple-value-bind (area-unchanged
			  area-beginning-end area-finish-start
			  buffer-beginning-end buffer-finish-start)
	(get-area-differences area)
      (declare (ignore area-beginning-end area-finish-start
		       buffer-beginning-end buffer-finish-start))
      ;; XXX big old hack for now.
      (unless area-unchanged
	(tree-recompute-extent area)
	(redisplay-all area)
	(return-from redisplay-area t)))
    (loop for line = (area-first-line area) then (next line)
	  while line
	  do (multiple-value-bind (line-changed dimensions-changed)
		 (maybe-update-line-dimensions line)
	       (declare (ignore dimensions-changed)) ;XXX
	       (when line-changed
		 (redisplay-line line stream))))))

(defmethod get-line-differences ((line screen-line))
  "Returns: line is different (t or nil)
    end of current (screen) unchanged beginning
    start of current unchanged end
    end of buffer line unchanged beginning
    start of buffer line unchanged end."
  (with-slots (current-contents
	       buffer-line)
      line
    (let* ((current-length (length current-contents))
	   (line-length (line-last-point buffer-line))
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

(defgeneric compute-line-breaks (area line))

(defmethod compute-line-breaks ((area simple-screen-area) line)
  (let ((max-text-width (max-text-width area)))
    (when (<= (line-text-width area line) max-text-width)
      (return-from compute-line-breaks nil))
    (loop with line-width = 0
	  for i from 0 below (length (current-contents line))
	  for char-width = (line-text-width area line :start i :end (1+ i))
	  if (> (+ line-width char-width) max-text-width)
	    collect i
	    and do (setq line-width 0)
	  else
	   do (incf line-width char-width)
	  end))
  )

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
	 (style (text-style (output-record-parent line))))
    (declare (ignorable style))
    (with-slots (current-contents ascent descent baseline cursor buffer-line)
	line
      (multiple-value-bind (unchanged
			    current-unchanged-from-start
			    current-unchanged-from-end
			    line-unchanged-from-start
			    line-unchanged-from-end)
	  (get-line-differences line)
	(when (and cursor (cursor-state cursor))
	  (setf (cursor-visibility cursor) :off))
	(unless unchanged
	  (let* ((area (editable-area line))
		 (start-width (if (> current-unchanged-from-start 0)
				  (line-text-width
				   area line
				   :end current-unchanged-from-start)
				  0))
		 (line-end (line-text-width area line))
		 (current-unchanged-left
		  (if (< current-unchanged-from-end (length current-contents))
		      (line-text-width area line
				       :end current-unchanged-from-end)
		      line-end))
		 (new-line-size (line-last-point buffer-line)))
	    ;; Having all we need from the old contents of the line, update
	    ;; with the new contents
	    (when (> new-line-size (car (array-dimensions current-contents)))
	      (adjust-array current-contents  new-line-size))
	    (setf (fill-pointer current-contents) new-line-size)
	    (flexivector-string-into buffer-line current-contents)
	    (let* ((new-line-end (line-text-width area line))
		   (new-unchanged-left
		    (if (< line-unchanged-from-end (length current-contents))
			(line-text-width area line
					 :end line-unchanged-from-end)
			new-line-end)))
	      (when (stream-drawing-p stream)
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
				:end line-unchanged-from-end
				:ink (foreground-ink line)))))
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
	  (setf (cursor-visibility cursor) :on))))))

(defun maybe-scroll (cursor)
  (let ((pane (cursor-sheet cursor))
        (cwidth (slot-value cursor 'climi::width))
        (cheight (climi::cursor-height cursor)))
    (when (and (typep pane 'pane)
               (pane-viewport-region pane))
      (multiple-value-bind (x y) (cursor-position cursor)
        (unless (region-contains-position-p (pane-viewport-region pane)
                                            (+ x cwidth -1)
                                            (+ y cheight -1))
          (multiple-value-bind (vw vh)              
              (bounding-rectangle-size (pane-viewport-region pane))
            (let ((max-width  (max (+ x cwidth)
                                   (bounding-rectangle-width pane)))
                  (max-height (max (+ y cheight)
                                   (bounding-rectangle-height pane))))              
              (change-space-requirements pane :width  max-width
                                              :height max-height))
            (scroll-extent pane (max 0 (+ x cwidth (- vw)))
                           (max 0 (+ y cheight (- vh))))))))))

(defmethod line-update-cursor ((line screen-line) stream)
  (multiple-value-bind (point-line point-pos)
      (point* (buffer (editable-area line)))
    (with-slots (cursor baseline ascent current-contents) line
      (let ((x (output-record-position line)))
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
	    (letf (((cursor-visibility cursor) :off))
	      (when (and (slot-boundp cursor 'screen-line)
			 (screen-line cursor)
			 (not (eq line (screen-line cursor))))
		(setf (cursor (screen-line cursor)) nil))
	      (setf (screen-line cursor) line)
	      (setf (cursor-position cursor)
		    (values cursor-x
			    (- baseline ascent)))
	      (maybe-scroll cursor))))))))


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
			 :ink (background-ink line)
			 :filled t)))))
