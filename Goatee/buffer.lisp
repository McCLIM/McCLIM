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

(defclass buffer-pointer ()
  ((line :accessor line :initarg :line)
   (pos :accessor pos :initarg :pos))
  (:documentation "A location in a buffer."))

(defmethod location* ((bp buffer-pointer))
  (values (line bp) (pos bp)))

(defgeneric* (setf location*) (line pos bp))

(defmethod* (setf location*) (line pos (bp buffer-pointer))
  (setf (values (line bp) (pos bp)) (values line pos)))

;;; basic-buffer must implement:
;;; lines
;;; tick
;;; size
;;; buffer-insert*
;;; buffer-delete*
;;; buffer-open-line*
;;; buffer-close-line*

(defclass basic-buffer ()
  ())

(defclass basic-buffer-mixin (basic-buffer)
  ((lines :accessor lines :initarg :lines
	  :initform (make-instance 'dbl-list-head))
   (tick :accessor tick :initarg :tick :initform 0)
   (point :accessor point :documentation "A buffer-pointer that
  maintains the current insertion point.")
   (size :reader size :initform 0)))

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


;;; A moment of convenience, a lifetime of regret...

(defmethod point* ((buf basic-buffer-mixin))
  (let ((point (point buf)))
    (values (line point) (pos point))))

(defgeneric* (setf point*) (line pos buffer))

(defmethod* (setf point*) (line pos (buf basic-buffer-mixin))
  (let ((point (slot-value buf 'point)))
    ;; If there's a next line, the point can't be moved past the
    ;; newline at the end of the line.
    (when (and (next line)
	       (>= pos (1- (size line))))
      (error 'buffer-bounds-error :buffer buf :line line :pos pos))
    (when (not (eq (line point) line))
      (setf (tick line) (incf (tick buf))))
    (setf (location* point) (values line pos))
    (setf (tick (line point)) (incf (tick buf)))
    (values line pos)))


(defclass buffer-line (flexivector dbl-list)
  ((buffer :accessor buffer :initarg :buffer)
   (tick :accessor tick :initarg :tick :initform 0)))

(defgeneric make-buffer-line (buffer &rest initargs)
  (:documentation "Creates line instances for a buffer"))

(defmethod make-buffer-line ((buffer basic-buffer-mixin) &rest initargs)
  (apply #'make-instance 'buffer-line :buffer buffer initargs))

(defmethod initialize-instance :after ((obj basic-buffer-mixin)
				       &key initial-contents
				       (start 0)
				       (end (when initial-contents
					      (length initial-contents))))
  (dbl-insert-after (make-buffer-line obj :tick (incf (tick obj))) (lines obj))
  (setf (point obj) (make-instance 'buffer-pointer
				   :line (dbl-head (lines obj))
				   :pos 0))
  (when initial-contents
    (insert obj initial-contents :start start :end end)))


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
	  (insert line #\newline :position pos)
	  (setf (tick line) (incf (tick buf)))
	  (dbl-insert-after new-line line)
	  (incf (slot-value buf 'size))
	  (values new-line 0)))))

(defgeneric buffer-insert* (buffer thing line pos &key))

(defmethod buffer-insert* ((buffer basic-buffer-mixin) (c character) line pos
			   &key)
  (insert line c :position pos)
  (incf (slot-value buffer 'size))
  (setf (tick line) (incf (tick buffer)))
  (values line (1+ pos)))

(defmethod buffer-insert* ((buffer basic-buffer-mixin) (s string) line pos
			   &key (start 0) (end (length s)))
  (let ((len (- end start)))
    (insert line s :position pos :start start :end end)
    (incf (slot-value buffer 'size) len)
    (setf (tick line) (incf (tick buffer)))
    (values line (+ pos len))))

;;; Insert is the convenience function; thing can contain newline(s).
;;;
;;; Soon insert, delete-char et al. won't need to maintain the point; should
;;; they set it at all?

(defmethod insert ((buffer basic-buffer) (c character)
		   &key position line (pos 0))
  (cond (position
	 (setf (point buffer) position))
	(line
	 (setf (point* buffer) (values line pos))))
  (let* ((pt (slot-value buffer 'point))
	 (line (line pt))
	 (pos (pos pt)))
    (setf (location* pt)
	  (if (eql c #\Newline)
	      (buffer-open-line* buffer line pos)
	      (buffer-insert* buffer c line pos)))
    nil))

(defmethod insert ((buffer basic-buffer) (s string)
		   &key position line (pos 0) (start 0) (end (length s)))
  (cond (position
	 (setf (point buffer) position))
	(line
	 (setf (point* buffer) (values line pos))))
  (multiple-value-bind (line pos)
      (point* buffer)
    (loop for search-start = start then (1+ found-newline)
	  for found-newline = (position #\Newline s
					:start search-start
					:end end)
	  while found-newline
	  do (progn
	       (setf (values line pos)
		     (buffer-insert* buffer s line pos
				     :start search-start
				     :end found-newline))
	       (setf (values line pos)
		     (buffer-open-line* buffer line pos)))
	  finally (setf (values line pos)
			(buffer-insert* buffer s line pos
					:start search-start :end end)))
    (setf (point* buffer) (values line pos)))
  nil)

(defgeneric buffer-close-line* (buffer line)
  (:documentation "Delete the newline at the end of line, bring the
  following line's contents onto line, and delete the following line"))

(defmethod buffer-close-line* ((buffer basic-buffer-mixin) line)
  (unless (typep line 'dbl-list)
    (error 'buffer-bounds-error :buffer buffer :line nil :pos 0))
  (let ((next-line (next line))
	(line-size (size line)))
    (if (eql (char-ref line (1- line-size)) #\Newline)
	(progn
	  (delete-char line 1 :position (1- (size line)))
	  (decf (slot-value buffer 'size))
	  (when next-line
	    (loop for i from 0 below (size next-line)
		  for j from (size line)
		  do (insert line (char-ref next-line i) :position j))
	    (dbl-remove next-line))
	  (setf (tick line) (incf (tick buffer)))
	  (values line (1- line-size)))
	(error 'buffer-bounds-error
	       :buffer buffer :line line :pos line-size))))

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

(defmethod delete-char ((buf basic-buffer) &optional (n 1)
			&key position line (pos 0))
  (cond (position
	 (setf (point buf) position))
	(line
	 (setf (point* buf) (values line pos))))
  (multiple-value-bind (line pos)
      (point* buf)
    (if (>= n 0)
	(loop with remaining = n
	      for last-point = (line-last-point line)
	      while (> (+ remaining pos) last-point)
	      do (let ((del-chars (- last-point pos)))
		   (when (> del-chars 0)
		     (buffer-delete-char* buf line pos (1- del-chars)))
		   ;; Up against the end, this should signal an error
		   (buffer-close-line* buf line)
		   (decf remaining del-chars))
	      finally (buffer-delete-char* buf line pos remaining))
	(loop with remaining = (- n)
	      while (< (- pos remaining) 0)
	      do (progn
		   (buffer-delete-char* buf line pos (- pos))
		   (decf remaining pos)
		   (setf (values line pos)
			 (buffer-close-line* buf (prev line))))
	      finally (setf (values line pos)
			    (buffer-delete-char* buf line pos (- remaining)))))
    (setf (point* buf) (values line pos)))
  nil)

(defun adjust-fill (array new-fill)
  (if (> new-fill (car (array-dimensions array)))
      (adjust-array array new-fill :fill-pointer new-fill)
      (setf (fill-pointer array) new-fill))
  array)

(defgeneric buffer-string (buffer &key start end))

(defmethod buffer-string ((buf basic-buffer) &key start end result)
  (declare (ignore start end))
  (let ((result (if result
		    (adjust-fill result (size buf))
		    (make-string (size buf)))))
    (loop for line-in-string = 0 then (+ line-in-string (size line))
	  for line = (dbl-head (lines buf)) then (next line)
	  while line
	  do (flexivector-string-into line result :start1 line-in-string))
    result))

(defun line-last-point (line)
  "Returns the last legal value for a position on a line, which is either
  before the newline, if there is one, or after the last character."
  (let* ((size (size line))
	 (last-char (if (> size 0)
			(char-ref line (1- size))
			nil)))
    (cond ((and last-char (char= last-char #\Newline))
	   (1- size))
	  (t size))))

(defmethod forward-char* ((buf basic-buffer) n
			  &key (position (point buf)) line (pos 0))
  (multiple-value-bind (line pos)
      (if line
	  (values line pos)
	  (location* position))
    (if (>= n 0)
	(loop for current-line = line then (next current-line)
	      for current-pos = pos then 0
	      for current-line-size = (or (and current-line (size current-line))
					  0)
	      for ends-in-nl = (and current-line
				    (char= (char-ref current-line
						     (1-
						      current-line-size))
					   #\Newline))
	      for chars-to-eol = (- current-line-size
				    (if ends-in-nl 1 0)
				    current-pos)
	      ;; point goes before #\newline, then to the next line.
	      for remaining = n then (- remaining chars-to-eol)
	      until (or (null current-line) (<= remaining chars-to-eol))
	      finally (if (null current-line)
			  (error 'buffer-bounds-error :buffer buf)
			  (return (values current-line
					  (+ current-pos remaining)))))
	(loop for current-line = line then (and (typep (prev current-line)
						       'dbl-list)
						(prev current-line))
	      for current-line-size = (or (and current-line (size current-line))
					  0)
	      for current-pos = pos then (1- current-line-size)
	      for remaining = (- n) then (- remaining current-pos)
	      until (and current-line (<= remaining current-pos))
	      finally (if (null current-line)
			  (error 'buffer-bounds-error :buffer buf)
			  (return (values current-line
					  (- current-pos remaining))))))))

(defmethod forward-char ((buf basic-buffer) n &rest key-args)
  (multiple-value-bind (new-line new-pos)
      (apply #'forward-char* buf n key-args)
    (make-instance 'buffer-pointer :line new-line :pos new-pos)))

(defgeneric end-of-line* (buffer &key position line pos))

(defgeneric end-of-line (buffer &rest key-args))

(defmethod end-of-line* ((buf basic-buffer)
			 &key (position (point buf)) line (pos 0))
  (multiple-value-bind (line pos)
      (if line
	  (values line pos)
	  (location* position))
    (values line (line-last-point line))))

(defmethod end-of-line ((buf basic-buffer) &rest key-args)
  (multiple-value-bind (new-line new-pos)
      (apply #'end-of-line* buf key-args)
    (make-instance 'buffer-pointer :line new-line :pos new-pos)))

(defgeneric beginning-of-line* (buffer &key position line pos))

(defmethod beginning-of-line* ((buf basic-buffer)
			       &key (position (point buf)) line (pos 0))
  (multiple-value-bind (line pos)
      (if line
	  (values line pos)
	  (location* position))
    (values line 0)))

;;; The need for these should go away once we have real buffer pointers...

(defmethod beginning-of-buffer* ((buf basic-buffer))
  (values (dbl-head (lines buf))
	  0))

(defmethod end-of-buffer* ((buf basic-buffer))
  (loop for line = (dbl-head (lines buf)) then (next line)
	while (next line)
	finally (return (end-of-line* buf :line line))))

;;; In the next go-around, this will have buffer pointers, including the point
;;;which will be auto-updated.

(defclass editable-buffer (basic-buffer-mixin)
  ())
