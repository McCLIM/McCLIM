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

(defclass location ()
  ((line :accessor line :initarg :line)
   (pos :accessor pos :initarg :pos))
  (:documentation "A location in a buffer."))

(defmethod location* ((bp location))
  (values (line bp) (pos bp)))

(defgeneric* (setf location*) (line pos bp))

(defmethod* (setf location*) (line pos (bp location))
  (setf (values (line bp) (pos bp)) (values line pos)))

;;; Is this too massive a hack?

(defgeneric copy-location (location))

(defmethod copy-location ((loc location))
  (make-instance (class-of loc) :line (line loc) :pos (pos loc)))

;;; basic-buffer must implement:
;;; lines - returns the lines of a buffer
;;; tick - a counter incremented after *any* change to a buffer
;;; size
;;; buffer-insert*
;;; buffer-delete-char*
;;; buffer-open-line*
;;; buffer-close-line*
;;;
;;; All modifications to a buffer are done through those 4 generic
;;; functions, so subclasses can update themselves via
;;;;before/after/around methods on them.
(defclass basic-buffer ()
  ()
  (:documentation "basic-buffer is a protocol class that specifies basic insert
  and delete operations on a buffer of text (generally)."))

(defclass basic-buffer-mixin (basic-buffer)
  ((lines :accessor lines :initarg :lines)
   (tick :accessor tick :initarg :tick)
   (size :reader size :initform 0)
   (newline-character :accessor newline-character :initarg :newline-character
		      :documentation "The character that ends a line. NIL means
   that the buffer will only have one line."))
  (:default-initargs
    :lines (make-instance 'dbl-list-head)
    :tick 0
    :newline-character #\Newline))

(define-condition buffer-bounds-error (goatee-error)
  ((buffer :reader buffer-bounds-error-buffer :initarg :buffer :initform nil)
   (line :reader buffer-bounds-error-line :initarg :line :initform nil)
   (pos :reader buffer-bounds-error-pos :initarg :pos :initform nil))
  (:report (lambda (condition stream)
	     (format stream "In buffer ~S, position line ~S pos ~S is ~
                             out of bounds"
		     (buffer-bounds-error-buffer condition)
		     (buffer-bounds-error-line condition)
		     (buffer-bounds-error-pos condition)))))


(defclass buffer-line (flexivector dbl-list)
  ((buffer :accessor buffer :initarg :buffer)
   (tick :accessor tick :initarg :tick :initform 0)))

(defgeneric make-buffer-line (buffer &rest initargs)
  (:documentation "Creates line instances for a buffer"))

(defmethod make-buffer-line ((buffer basic-buffer-mixin) &rest initargs)
  (apply #'make-instance 'buffer-line :buffer buffer initargs))

(defmethod initialize-instance :after ((obj basic-buffer-mixin) &key)
  (dbl-insert-after (make-buffer-line obj :tick (incf (tick obj))) (lines
								    obj)))

(defgeneric first-line-p (line)
  (:documentation "Returns true if line is the first line in a buffer"))

(defmethod first-line-p ((line buffer-line))
  (not (typep (prev line) 'buffer-line)))

(defgeneric last-line-p (line)
  (:documentation "Returns true if line is the last line in a buffer"))

(defmethod last-line-p ((line buffer-line))
  (null (next line)))

(defgeneric char-ref (buffer position))

(defmethod char-ref ((buf basic-buffer) position)
  (char-ref (line position) (pos position)))

(defmethod char-ref* ((buf basic-buffer) line pos)
  (char-ref line pos))

(defgeneric buffer-open-line* (buffer line pos)
  (:documentation "Insert a newline at POS in LINE, creating a new line that
  contains LINEs contents from POS to the end of LINE.  Returns the position
  (spread) of the beginning of the new line"))

(defmethod buffer-open-line*
    ((buf basic-buffer-mixin) (line buffer-line) pos)
  (let ((len (size line)))
    (if (<= pos len)			;XXX throw an error?
	(let ((new-line (make-buffer-line
			 buf
			 :buffer buf
			 :initial-store (flexivector-string line :start pos)
			 :tick (incf (tick buf)))))
	  ;; delete to end of line
	  (delete-char line (- (size line) pos) :position pos)
	  (insert line (newline-character buf) :position pos)
	  (setf (tick line) (incf (tick buf)))
	  (dbl-insert-after new-line line)
	  (incf (slot-value buf 'size))
	  (values new-line 0))
	(error 'buffer-bounds-error :buffer buf :line line :pos pos))))

(defgeneric buffer-insert* (buffer thing line pos &key)
  (:documentation "Insert a THING (character or string) into BUFFER."))

(defmethod buffer-insert* ((buffer basic-buffer-mixin) (c character) line pos
			   &key)
  (when (or (> pos (line-last-point line))
	    (< pos 0))
    (error 'buffer-bounds-error :buffer buffer :line line :pos pos))
  (insert line c :position pos)
  (incf (slot-value buffer 'size))
  (setf (tick line) (incf (tick buffer)))
  (values line (1+ pos)))

(defmethod buffer-insert* ((buffer basic-buffer-mixin) (s string) line pos
			   &key (start 0) (end (length s)))
  (when (or (> pos (line-last-point line))
	    (< pos 0))
    (error 'buffer-bounds-error :buffer buffer :line line :pos pos))
  (let ((len (- end start)))
    (insert line s :position pos :start start :end end)
    (incf (slot-value buffer 'size) len)
    (setf (tick line) (incf (tick buffer)))
    (values line (+ pos len))))



(defgeneric buffer-close-line* (buffer line direction)
  (:documentation "If DIRECTION is positive, delete the newline at the
  end of line, bring the following line's contents onto line, and
  delete the following line.  If DIRECTION is negative, first move back
  one line, then do the deletion." ))

(defmethod buffer-close-line* ((buffer basic-buffer-mixin) line direction)
  (multiple-value-bind (this-line next-line)
      (if (< 0 direction)
	  (values line (next line))
	  (values (prev line) line))
    (unless (typep this-line 'dbl-list)
      (error 'buffer-bounds-error :buffer buffer :line nil :pos 0))
    (let ((line-size (size this-line))
	  (newline-character (newline-character buffer)))
      (if (eql (char-ref this-line (1- line-size)) newline-character)
	  (progn
	    (delete-char this-line 1 :position (1- line-size))
	    (decf (slot-value buffer 'size))
	    (when next-line
	      (loop for i from 0 below (size next-line)
		    for j from (1- line-size)
		    do (insert this-line (char-ref next-line i) :position j))
	      (dbl-remove next-line))
	    (setf (tick this-line) (incf (tick buffer)))
	    (values this-line (1- line-size)))
	  (error 'buffer-bounds-error
		 :buffer buffer :line this-line :pos line-size)))))

(defgeneric buffer-delete-char* (buffer line pos n)
  (:documentation "Delete characters from a line.  Can not delete the final
  newline or characters before the beginning of the line"))

(defmethod buffer-delete-char* ((buffer basic-buffer-mixin) line pos n)
  (multiple-value-prog1
        (if (>= n 0)
	    (progn
	      (when (> (+ pos n) (line-last-point line))
		(error 'buffer-bounds-error :buffer buffer :line line :pos pos))
	      (delete-char line n :position pos)
	      (decf (slot-value buffer 'size) n)
	      (values line pos))
	    (progn
	      (when (< (+ pos n) 0)
		(error 'buffer-bounds-error :buffer buffer :line line :pos pos))
	      (delete-char line n :position pos)
	      (incf (slot-value buffer 'size) n)
	      (values line (+ pos n))))
    (setf (tick line) (incf (tick buffer)))))


;;; The need for these should go away once we have real buffer pointers...
;;; ... but we'll keep 'em around; these might be useful for basic-buffers even
;;; though they're overridden by methods on editable-buffer.
(defmethod beginning-of-buffer* ((buf basic-buffer))
  (values (dbl-head (lines buf))
	  0))

(defmethod end-of-buffer* ((buf basic-buffer))
  (loop for line = (dbl-head (lines buf)) then (next line)
	while (next line)
	finally (return (end-of-line* buf :line line))))

;;; Buffer pointers and the bp-buffer-mixin that maintains them.

(defclass bp-buffer-mixin ()
  ())

(defclass bp-buffer-line (buffer-line)
  ((bps :accessor bps :initarg :bps :initform nil)))

(defmethod make-buffer-line ((buffer bp-buffer-mixin) &rest initargs)
  (apply #'make-instance 'bp-buffer-line :buffer buffer initargs))

(defclass buffer-pointer (location)
  ()
  (:documentation "Buffer pointer that moves with insertions at its location"))

(defmethod initialize-instance :after ((obj buffer-pointer) &key)
  (when (slot-boundp obj 'line)
    (push obj (bps (line obj)))))

(defmethod (setf line) (new-line (bp buffer-pointer))
  (let* ((was-bound (slot-boundp bp 'line))
	 (old-line (and was-bound (line bp))))
    (when (and was-bound old-line (not (eq old-line new-line)))
      (setf (bps old-line) (delete bp (bps old-line))))
    (prog1
	(call-next-method)
      (when (and new-line
		 (not (and was-bound (eq old-line new-line))))
	(push bp (bps (line bp)))))))

(defgeneric update-for-insert (bp pos delta))

(defmethod update-for-insert ((bp buffer-pointer) pos delta)
  (when (>= (pos bp) pos)
    (incf (pos bp) delta)))

(defclass fixed-buffer-pointer (buffer-pointer)
  ()
  (:documentation "Buffer pointer that doesn't move with insertions at its location"))

(defmethod update-for-insert ((bp fixed-buffer-pointer) pos delta)
  (when (> (pos bp) pos)
    (incf (pos bp) delta)))

(defmethod buffer-insert* :after
    ((buffer basic-buffer-mixin) (c character) line pos &key)
  (loop for bp in (bps line)
	do (update-for-insert bp pos 1)))
  
(defmethod buffer-insert* :after
    ((buffer basic-buffer-mixin) (s string) line pos
     &key (start 0) (end (length s)))
  (loop with len = (- end start)
	for bp in (bps line)
	do (update-for-insert bp pos len)))

(defmethod buffer-delete-char* :after ((buffer bp-buffer-mixin) line pos n)
  (cond ((> n 0)
	 (loop for bp in (bps line)
	       when (> (pos bp) pos)
	       do (setf (pos bp) (max pos (- (pos bp) n)))))
	((< n 0)
	 (loop with new-pos = (+ pos n)
	       for bp in (bps line)
	       do (cond ((>= (pos bp) pos)
			 (incf (pos bp) n))
			((> (pos bp) new-pos)
			 (setf (pos bp) new-pos)))))))

(defmethod buffer-open-line* :around
    ((buf bp-buffer-mixin) (line bp-buffer-line) pos)
  (multiple-value-bind (new-line new-pos)
      (call-next-method)
    (loop for bp in (bps line)
	  if (typecase bp
	       (fixed-buffer-pointer
		(> (pos bp) pos))
	       (t (>= (pos bp) pos)))
	    do (setf (line bp) new-line
		     (pos bp) (- (pos bp) pos))
	    and collect bp into new-line-bps
	  else
	    collect bp into old-line-bps
	  end
	  finally (setf (bps line) old-line-bps
			(bps new-line) new-line-bps))
    (values new-line new-pos)))

(defmethod buffer-close-line* :around
    ((buffer bp-buffer-mixin) (line bp-buffer-line) direction)
  (multiple-value-bind (this-line next-line)
      (if (< 0 direction)
	  (values line (next line))
	  (values (prev line) line))
    (multiple-value-bind (line new-pos)
	(call-next-method)
      (loop for bp in (bps next-line)
	    do (progn
		 (incf (pos bp) new-pos)
		 (setf (line bp) this-line)))
      (values line new-pos))))


(defmacro with-buffer-pointer* ((bp-var line pos &key (class ''buffer-pointer))
				&body body)
  "Like with-buffer-pointer, but takes line and pos as initialization
  arguments."
  (let ((bp-temp (gensym)))
     `(let* ((,bp-temp (make-instance ,class :line ,line :pos ,pos))
	     (,bp-var ,bp-temp))
	(unwind-protect
	     (progn
	       ,@body)
	  (setf (line ,bp-temp) nil)))))

(defmacro with-buffer-pointer ((bp-var location &key (class ''buffer-pointer))
			       &body body)
  "binds bp-var to a buffer pointer initialized to location.  Class is the
   class of the buffer pointer, defaulting to 'buffer-pointer. bp-var is
   deallocated when at exit from the form."
  (let ((line-var (gensym "LINE"))
	(pos-var (gensym "POS")))
     `(multiple-value-bind (,line-var ,pos-var)
	  (location* ,location)
       (with-buffer-pointer* (,bp-var ,line-var ,pos-var :class ,class)
	 ,@body))))

(defclass extent ()
  ((bp-start :reader bp-start)
   (bp-end :reader bp-end))
  (:documentation "A delimited region in a buffer.  The concept follows extents
  in XEmacs, though the interface is more in line with Common Lisp."))

(defmethod initialize-instance :after ((obj extent) 
				       &key start-line 
				       start-pos
				       (end-line start-line) 
				       (end-pos start-pos)
				       (start-state :closed)
				       (end-state :closed))
  ;; Subclasses could provide values for bp-start and bp-end through
  ;; initargs.
  (unless (slot-boundp obj 'bp-start)
    (setf (slot-value obj 'bp-start)
	  (make-instance (if (eq start-state :open)
			     'buffer-pointer
			     'fixed-buffer-pointer)
			 :line start-line :pos start-pos)))
  (unless (slot-boundp obj 'bp-end)
    (setf (slot-value obj 'bp-end)
	  (make-instance (if (eq end-state :open)
			     'fixed-buffer-pointer
			     'buffer-pointer)
			 :line end-line :pos end-pos)))
  (when (and start-line end-line)
    (record-extent-lines obj)))

(defclass extent-buffer-mixin (bp-buffer-mixin)
  ()
  (:documentation "Buffer class that maintains extents."))

(defclass extent-buffer-line (bp-buffer-line)
  ((extents :accessor extents :initarg :extents :initform nil
	    :documentation "Holds any extents that contain this line, including
  ones that start and finish on it.  Eventually the extents in this list will
  be kept in \"display\" order." )))

(defmethod make-buffer-line ((buffer extent-buffer-mixin) &rest initargs)
  (apply #'make-instance 'extent-buffer-line :buffer buffer initargs))

(defmethod record-extent-lines ((extent extent))
  (loop for line = (line (bp-start extent)) then (next line)
	until (eq line (line (bp-end extent)))
	do (push extent (extents line))
	finally (push extent (extents line))))

(defmethod detach-extent ((extent extent))
  (loop for line = (line (bp-start extent)) then (next line)
	until (eq line (lines (bp-end extent)))
	do (setf (extents line) (delete extent (extents line)))
	finally (setf (extents line) (delete extent (extents line))))
  (setf (line (bp-start extent)) nil)
  (setf (line (bp-end extent)) nil))

(defmethod start-state ((extent extent))
  (if (typep (bp-start extent) 'fixed-buffer-pointer)
      :closed
      :open))

(defmethod (setf start-state) (new-val (extent extent))
  (with-slots (bp-start)
      extent
    (if (eq new-val :open)
	(when (typep bp-start 'fixed-buffer-pointer)
	  (setf bp-start (change-class bp-start 'buffer-pointer)))
	(when (not (typep bp-start 'fixed-buffer-pointer))
	  (setf bp-start (change-class bp-start 'fixed-buffer-pointer))))))

(defmethod end-state ((extent extent))
  (if (typep (bp-end extent) 'fixed-buffer-pointer)
      :open
      :closed))

(defmethod (setf end-state) (new-val (extent extent))
  (with-slots (bp-end)
      extent
    (if (eq new-val :open)
	(when (not (typep bp-end 'fixed-buffer-pointer))
	  (setf bp-end (change-class bp-end 'fixed-buffer-pointer)))
	(when (typep bp-end 'fixed-buffer-pointer)
	  (setf bp-end (change-class bp-end 'buffer-pointer))))))

(defmethod buffer-open-line* :around
    ((buf extent-buffer-mixin) (line extent-buffer-line) pos)
  (declare (ignore pos))
  (multiple-value-bind (new-line new-pos)
      (call-next-method)
    (loop for extent in (extents line)
	  for bp-start = (bp-start extent)
	  for bp-end = (bp-end extent)
	  if (or (not (eq (line bp-start) line))
		 (eq (line bp-end) new-line))
	    collect extent into new-line-extents
	  end
	  if (not (eq (line bp-start) new-line))
	    collect extent into old-line-extents
	  end
	  finally (setf (extents line) old-line-extents
			(extents new-line) new-line-extents))
    (values new-line new-pos)))
 
(defmethod buffer-close-line* :around
    ((buffer extent-buffer-mixin) (line extent-buffer-line) direction)
  (multiple-value-bind (this-line next-line)
      (if (< 0 direction)
	  (values line (next line))
	  (values (prev line) line))
      (multiple-value-bind (line new-pos)
	  (call-next-method)
	(let ((this-line-extents (extents this-line))
	      (next-line-extents (extents next-line)))
	  (loop for extent in next-line-extents
	      if (not (member extent this-line-extents :test #'eq))
	      collect extent into new-extents
	      end
	      finally (setf (extents line)
			(nconc this-line-extents new-extents)))
	  (values line new-pos)))))

(defun line-last-point (line)
  "Returns the last legal value for a position on a line, which is either
  before the newline, if there is one, or after the last character."
  (let* ((size (size line))
	 (last-char (if (> size 0)
			(char-ref line (1- size))
			nil))
	 (newline-char (newline-character (buffer line))))
    (cond ((and last-char newline-char (char= last-char newline-char))
	   (1- size))
	  (t size))))

(defmethod map-over-extents-at-location* (func (line extent-buffer-line) pos
					  &key
					  (start-state nil start-statep)
					  (end-state nil end-statep))
  (loop for extent in (extents line)
	for bp-start = (bp-start extent)
	for bp-end = (bp-end extent)
	for extent-start-state = (or (and start-statep start-state)
				     (start-state extent))
	for extent-end-state = (or (and end-statep end-state)
				   (end-state extent))
	do (let* ((start-test (if (eq extent-start-state :open)
				  #'>
				  #'>=))
		  (end-test (if (eq extent-end-state :open)
				  #'<
				  #'<=))
		  (do-func (cond ((and (eq (line bp-start) line)
				       (eq (line bp-end) line))
				  (and (funcall start-test pos (pos bp-start))
				       (funcall end-test pos (pos bp-end))))
				 ((eq (line bp-start) line)
				  (funcall start-test pos (pos bp-start)))
				 ((eq (line bp-end) line)
				  (funcall end-test pos (pos bp-end)))
				 (t t))))
	     (when do-func
	       (funcall func extent line pos)))))
