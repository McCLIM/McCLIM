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

(defclass basic-buffer ()
  ((lines :accessor lines :initarg :lines
	  :initform (make-instance 'dbl-list-head))
   (tick :accessor tick :initarg :tick :initform 0)
   (point :accessor point :documentation "A buffer-pointer that
  maintains the current insertion point.")
   (size :reader size :initform 0)))

(define-condition buffer-bounds-error (error)
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

(defmethod point* ((buf basic-buffer))
  (let ((point (point buf)))
    (values (line point) (pos point))))

(defgeneric* (setf point*) (line pos buffer))

(defmethod* (setf point*) (line pos (buf basic-buffer))
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
    (setf (point obj) (make-instance 'buffer-pointer
				     :line (dbl-head (lines obj))
				     :pos 0)))

(defgeneric char-ref (buffer position))

(defmethod char-ref ((buf basic-buffer) position)
  (char-ref (line position) (pos position)))

(defmethod char-ref* ((buf basic-buffer) line pos)
  (char-ref line pos))

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
	  ;; delete to end of line
	  (delete-char line (- (size line) pos) :position pos)
	  (insert line #\newline :position pos)
	  (setf (tick line) (incf (tick buf)))
	  (dbl-insert-after line new-line)))))

(defmethod insert ((buffer basic-buffer) (c character)
		   &key position line (pos 0))
  (cond (position
	 (setf (point buffer) position))
	(line
	 (setf (point* buffer) (values line pos))))
  (let* ((pt (slot-value buffer 'point))
	 (line (line pt))
	 (pos (pos pt)))
    (if (eql c #\Newline)
	(setf (location* pt) (values (open-line line pos) 0))
	(progn
	  (insert line c :position pos)
	  (incf (pos pt))))
    (incf (slot-value buffer 'size))
    (setf (tick line) (incf (tick buffer)))
    nil))

(defmethod insert ((buffer basic-buffer) (s string)
		   &key position line (pos 0) (start 0) (end (length s)))
  (cond (position
	 (setf (point buffer) position))
	(line
	 (setf (point* buffer) (values line pos))))
  (with-slots (line pos) (slot-value buffer 'point)
    (loop for search-start = start then (1+ found-newline)
	  for found-newline = (position #\Newline s
					:start search-start
					:end end)
	  while found-newline
	  do (progn
	       (insert line s
		       :position pos :start search-start :end found-newline)
	       ;; open-line increments the line tick.
	       (setf line (open-line line (+ pos
					     (- found-newline search-start))))
	       (setf pos 0))
	  finally (progn
		    (insert line s :position pos :start search-start :end end)
		    (incf pos (- end search-start))
		    (setf (tick line) (incf (tick buffer)))))
    (incf (slot-value buffer 'size) (length s))
    nil))

(defgeneric close-line (line) 
  (:documentation "Delete the newline at the end of line, bring the
  following line's contents onto line, and delete the following line"))

(defmethod close-line (line)
  (let ((next-line (next line))
	(line-size (size line))
	(buffer (buffer line)))
    (when (eql (char-ref line (1- line-size)) #\Newline)
      (delete-char line 1 :position (1- (size line)))
      (decf (slot-value buffer 'size)))
    (when next-line
      (loop for i from 0 below (size next-line)
	    for j from (size line)
	    do (insert line (char-ref next-line i) :position j))
      (dbl-remove next-line))))

(defmethod delete-char ((buf basic-buffer) &optional (n 1)
			&key position line (pos 0))
  (cond (position
	 (setf (point buf) position))
	(line
	 (setf (point* buf) (values line pos))))
  (with-slots (point) buf
    (if (> n 0)
	(with-slots (line pos) point
	  (loop for c = (char-ref line pos)
		repeat n
		do (if (eql pos (1- (size line)))
		       (close-line line)
		       (delete-char line 1 :position pos)))
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
			 (delete-char line -1 :position pos)
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
	      for chars-to-eol = (- current-line-size current-pos)
	      ;; point goes before #\newline, then to the next line.
	      for remaining = n then (- remaining chars-to-eol)
	      until (or (null current-line) (< remaining (- chars-to-eol 1)))
	      finally (if (null current-line)
			  (error 'buffer-bounds-error :buffer buf)
			  (return (values current-line
					  (+ current-pos remaining)))))
	(loop for current-line = line then (prev current-line)
	      for current-line-size = (or (and current-line (size current-line))
					  0)
	      for current-pos = pos then (1- current-line-size)
	      for remaining = n then (- remaining current-pos)
	      until (< remaining current-pos)
	      finally (if (null current-line)
			  (error 'buffer-bounds-error :buffer buf)
			  (return (values current-line
					  (- current-pos remaining))))))))

(defmethod forward-char ((buf basic-buffer) n &rest key-args)
  (multiple-value-bind (new-line new-pos)
      (apply #'forward-char* buf n key-args)
    (make-instance 'buffer-pointer :line new-line :pos new-pos)))

