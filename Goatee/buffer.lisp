(in-package :goatee)

(defclass location ()
  ((line :accessor line :initarg :line)
   (pos :accessor pos :initarg :pos)))

(defclass basic-buffer ()
  ((lines :accessor lines :initarg :lines
	  :initform (make-instance 'dbl-list-head))
   (tick :accessor tick :initarg :tick :initform 0)
   (point :documentation "A location that maintains the current
insertion point.  Clients are given copies of the point and are not
allowed to manipulate it directly.")
   (size :reader size :initform 0)))

(define-condition buffer-bounds-error (error)
  ((buffer :reader buffer-bounds-error-buffer :initarg :buffer :initform nil)
   (line :reader buffer-bounds-error-line :initarg :line :initform nil)
   (pos :reader buffer-bounds-error-pos :initarg :pos :initform nil))
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "In buffer ~S, position line ~S pos ~S is ~
out of bounds"
		     (buffer-bounds-error-buffer condition)
		     (buffer-bounds-error-line condition)
		     (buffer-bounds-error-pos condition)))))

(defmethod point ((buf basic-buffer))
  (let ((point (slot-value buf 'point)))
    (make-instance 'location :line (line point) :pos (pos point))))

(defmethod (setf point) (new-val (buf basic-buffer))
  (let ((point (slot-value buf 'point)))
    ;; If there's a next line, the point can't be moved past the
    ;; newline at the end of the line.
    (with-slots (line pos) new-val
      (when (and (next line)
		 (>= pos (1- (size line))))
	(error 'buffer-bounds-error :buffer buf :line line :pos pos))
      (setf (line point) line)
      (setf (pos point) pos)
      ;; XXX Is this right for the semantics of SETF?
      new-val)))


(defclass buffer-line (flexivector dbl-list)
  ((buffer :accessor buffer :initarg :buffer)
   (tick :accessor tick :initarg :tick :initform 0)))

(defgeneric make-buffer-line (buffer &rest initargs)
  (:documentation "Creates line instances for a buffer"))

(defmethod make-buffer-line ((buffer basic-buffer) &rest initargs)
  (apply #'make-instance 'buffer-line :buffer buffer initargs))


(defmethod initialize-instance :after ((obj basic-buffer)
				       &key initial-contents
				       (start 0)
				       (end (when initial-contents
					      (length initial-contents))))
    (if initial-contents
	(progn
	  (loop for start-line = start then (1+ end-line)
		for end-line = (position #\Newline initial-contents
					 :start start-line :end end)
		for prev-line = (lines obj) then line
		for line = (when end-line
			     (dbl-insert-after
			      (make-buffer-line obj
						:initial-contents
						initial-contents
						:start start-line
						:end (1+ end-line)
						:tick (incf (tick obj)))
			      prev-line)) 
		while end-line
		finally (unless (> start-line end)
			  ;; Handle lack of newline at end of file
			  (dbl-insert-after
			   (make-buffer-line obj
					     :initial-contents initial-contents
					     :start start-line
					     :end end
					     :tick (incf (tick obj)))
			   prev-line)))
	  (setf (slot-value obj 'size) (length initial-contents)))
	(dbl-insert-after (make-buffer-line obj :tick (incf (tick obj)))
			  (lines obj)))
    (setf (slot-value obj 'point) (make-instance 'location
						 :line (dbl-head (lines obj))
						 :pos 0)))

(defgeneric char-ref (buffer position))

(defmethod char-ref ((buf basic-buffer) position)
  (fref (line position) (pos position)))

(defgeneric open-line (line pos)
  (:documentation "Insert a newline at POS in LINE, creating a new line that
contains LINEs contents from POS to the end of LINE.  Returns the new line."))

(defmethod open-line ((line buffer-line) pos)
  (let ((len (size line))
	(buf (buffer line)))
    (if (<= pos len)
	(let ((new-line (make-instance
			 'buffer-line
			 :buffer buf
			 :initial-store (flexivector-string line :start pos)
			 :tick (incf (tick buf)))))
	  (delete-char line (- (size line) pos) pos) ;delete to end of line
	  (insert line pos #\newline)
	  (setf (tick line) (incf (tick buf)))
	  (dbl-insert-after line new-line)))))

(defmethod insert ((buffer basic-buffer) (c character)
		   &optional position &key)
  (when position
    (setf (point buffer) position))
  (let* ((pt (slot-value buffer 'point))
	 (line (line pt))
	 (pos (pos pt)))
    (if (eql c #\Newline)
	(progn
	  (setf (line pt) (open-line line pos))
	  (setf (pos pt) 0))
	(progn
	  (insert line c pos)
	  (incf (pos pt))))
    (incf (slot-value buffer 'size))
    (setf (tick line) (incf (tick buffer)))
    nil))

(defmethod insert ((buffer basic-buffer) (s string) &optional position
		   &key (start 0) (end (length s)))
  (when position
    (setf (point buffer) position))
  (with-slots (line pos) (slot-value buffer 'point)
    (loop for search-start = start then (1+ found-newline)
	  for found-newline = (position #\Newline s
					:start search-start
					:end end)
	  while found-newline
	  do (progn
	       (insert line s pos :start search-start :end found-newline)
	       ;; open-line increments the line tick.
	       (setf line (open-line line (+ pos
					     (- found-newline search-start))))
	       (setf pos 0))
	  finally (progn
		    (insert line s pos :start search-start :end end)
		    (incf pos (- end search-start))
		    (setf (tick line) (incf (tick buffer)))))
    (incf (slot-value buffer 'size) (length s))
    nil))

(defgeneric close-line (line)
  (:documentation "Delete the newline at the end of line, bring the
following line's contents onto line, and delete the following line"))

(defmethod close-line (line)
  (let ((next-line (next line))
	(line-size (size line)))
    (when (eql (char-ref line (1- line-size)) #\Newline)
      (delete-char line (1- (size line)))
      (decf (slot-value buffer 'size)))
    (when next-line
      (loop for i from 0 below (size next-line)
	    for j from (size line)
	    do (insert line (fref next-line i) j))
      (dbl-remove next-line))))

(defgeneric delete-char (buf &optional n position))

(defmethod delete-char ((buf basic-buffer) &optional (n 1) position)
  (when position
    (setf (point buf) position))
  (with-slots (point) buf
    (if (> n 0)
	(with-slots (line pos) point
	  (loop for c = (char-ref line pos)
		repeat n
		do (if (eql pos (1- (size line)))
		       (close-line line)
		       (delete-char line pos)))
	  (setf (tick line) (incf (tick buf))))
	(with-slots (line pos) point
	  (loop repeat n
		do (if (zerop pos)
		       (progn
			 (unless (prev line)
			   (loop-finish))
			 (setf line (prev line))
			 (setf pos (1- (size line)))
			 (close-line line))
		       (progn
			 (delete-char line pos -1)
			 (decf pos))))
	  (setf (tick line) (incf (tick buf))))))
  (decf (slot-value buf 'size) (abs n))
  nil)

(defgeneric buffer-string (buffer &key start end))

(defmethod buffer-string ((buf basic-buffer) &key start end)
  (declare (ignore start end))
  (let ((result (make-string (size buf))))
    (loop for line-in-string = 0 then (+ line-in-string (size line))
	  for line = (dbl-head (lines buf)) then (next line)
	  while line
	  do (flexivector-string-into line result :start1 line-in-string))
    result))
