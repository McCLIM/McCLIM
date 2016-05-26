(in-package :goatee)

;;; A mixin for buffers that can be displayed in editable areas
(defclass displayable-buffer ()
  ((editable-areas :accessor editable-areas :initarg :editable-areas)))


(defclass editable-area ()
  ((buffer :reader buffer :initarg :buffer)
   (frame-begin-mark :accessor frame-begin-mark) ;XXX obsolete
   (last-tick :accessor last-tick :initarg :last-tick
	      :documentation "buffer tick")
   (lines :accessor lines :initarg :lines
	  :initform (make-instance 'dbl-list-head)
	  :documentation "Lines in the area, as opposed to the lines
  in the buffer.")
   (area-bp-start :accessor area-bp-start :initarg :area-bp-start
		  :documentation "buffer pointer to line in buffer
  that's at the top of the area.  The bp is not necessarily at the
  beginning of the line.")
   (area-bp-end :accessor area-bp-end :initarg :area-bp-end
		  :documentation "buffer pointer to line in buffer
  that's at the bottom of the area.  The bp is not necessarily at the
  beginning of the line.")
   (last-line :accessor last-line :initarg :last-line :initform nil)
   (last-command :accessor last-command :initform nil)
   (goal-column :accessor goal-column :initform nil
		:documentation "Goal column for next-line command when
  moving over a short line"))
  (:documentation "An abstract superclass for the on-screen area
  devoted to Goatee editing.  Roughly equivalent to a window in GNU Emacs."))

(defmethod initialize-instance :after ((obj editable-area)
				       &key initial-contents)
  (when initial-contents
    (if (slot-boundp obj 'buffer)
	(error "Only one of :buffer and :initial-contents may be supplied")
	(setf (slot-value obj 'buffer)
	      (make-instance 'editable-buffer
			     :initial-contents initial-contents)))))

(defgeneric area-first-line (area))

(defmethod area-first-line ((area editable-area))
  (dbl-head (lines area)))

#+nil(progn
(defmethod (setf buffer) ((new-buf displayable-buffer) (win editable-area))
  (when (slot-boundp win 'buffer)
    (remove-mark frame-begin-mark))
  (setf (slot-value win 'buffer) new-buf)
  (pushnew win (editable-areas new-buf))
  (frame-window-to-point win)
  (add-new-lines win)
  new-buf)

(defconstant +point-frame-ratio+ 1/2)

(defgeneric frame-window-to-point (window))

;;;XXX How to deal with line wrap?
(defmethod frame-window-to-point ((win window))
  (with-accessors ((buffer buffer)
		   (frame-begin-mark frame-begin-mark))
    win
    (let* ((lines-to-start (floor (* (rows win) +point-frame-ratio+)))
	   (window-start (loop for lines from 0
			       for prev-eol = (point buffer)
			         then (position-backward buffer
							 #\newline
							 prev-eol )
			       while (and prev-eol
					  (<= lines lines-to-start))
			       finally (return (or prev-eol 0)))))
      (if (and (slot-boundp win 'frame-begin-mark) frame-begin-mark)
	  (progn
	    (remove-mark frame-begin-mark)
	    (setf frame-begin-mark
		  (insert-mark buffer frame-begin-mark window-start)))
	  (setf frame-begin-mark (insert-mark-using-class buffer
							  'fixed-mark
							  window-start))))))

(defclass line-mark (fixed-mark)
  ((last-update :accessor last-update :initarg :last-update)))
)

(defclass editable-area-line (dbl-list)
  ((buffer-line :accessor buffer-line :initarg :buffer-line)
   (last-tick :accessor last-tick :initarg :last-tick)
   (editable-area :accessor editable-area :initarg :editable-area
		  :documentation "backpointer")))

;;; XXX mostly garbage at the moment...
#+nil
(defmethod add-new-lines ((win window))
  (with-accessors ((buf buffer)
		   (line-marks line-marks)
		   (lines lines))
    win
    (setf (dbl-head line-marks) nil)
    (setf (dbl-head lines) nil)
    (let ((start-line-pos (at (frame-begin-mark win))))
      (loop for line-count from 0 below (rows win)
	    for line-pos = start-line-pos then (position-forward
						buffer
						#\Newline
						(1+ prev-line-pos))
	    for prev-line-pos = 0 then line-pos
	    while line-pos
	    for line-mark = (insert-mark-using-class
			     'line-mark
			     (1+ line-pos)
			     :last-update (update-counter win))
	    for prev-line-mark-dbl = line-marks then line-mark-dbl
	    for line-mark-dbl = (insert-obj-after line-mark prev-line-mark-dbl)
	    for line = (make-instance 'line
				      :mark line-mark
				      :last-update (update-counter win))
	    for prev-line-dbl = lines then line-dbl
	    for line-dbl = (insert-obj-after line prev-line-dbl)))
    (loop for line-dbl = (dbl-head lines) then (next line-dbl)
	  while line-dbl
	  for line = (contents line-dbl)
	  do (let* ((start-line (at (mark line)))
		    (end-line (if (next line-dbl)
				  (1- (at (mark (contents (next line-dbl)))))
				  (position-forward buffer
						    #\Newline
						    (1+ start-line))))
		    (line-length (- start-line end-line))
		    (chars (make-array line-length
				       :element-type 'character
				       :adjustable t
				       :fill-pointer line-length)))
	       (buffer-string-into buffer chars
				   :start2 start-line :end2 end-line)
	       (setf (chars line) chars)))))
