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

;;; An editable-buffer implements the semantics one would expect of an
;;; Emacs buffer.  It maintains a point, where insertions and
;;; deletions take place, and provides functions to move the point around.

(defclass editable-buffer (extent-buffer-mixin basic-buffer-mixin)
  ((point :accessor point :documentation "A buffer-pointer that
  maintains the current insertion point.")
   (buffer-start :accessor buffer-start
		 :documentation "A fixed buffer pointer at the start of the buffer.")
   (buffer-end :accessor buffer-end
	       :documentation "A buffer pointer at the end of the buffer.")))

(defmethod initialize-instance :after ((obj editable-buffer)
				       &key initial-contents
				       (start 0)
				       (end (when initial-contents
					      (length initial-contents))))
  (setf (point obj) (make-instance 'buffer-pointer
				   :line (dbl-head (lines obj))
				   :pos 0))
  (setf (buffer-start obj) (make-instance 'fixed-buffer-pointer
					  :line (dbl-head (lines obj))
					  :pos 0))
  (setf (buffer-end obj) (make-instance 'buffer-pointer
				     :line (dbl-head (lines obj))
				     :pos 0))
  (when initial-contents
    (insert obj initial-contents :start start :end end)))

;;; A moment of convenience, a lifetime of regret...

(defmethod point* ((buf editable-buffer))
  (let ((point (point buf)))
    (values (line point) (pos point))))

(defgeneric* (setf point*) (line pos buffer))

(defmethod* (setf point*) (line pos (buf editable-buffer))
  (let ((point (slot-value buf 'point)))
    ;; If there's a next line, the point can't be moved past the
    ;; newline at the end of the line.
    (when (or (> pos (line-last-point line))
	      (< pos 0))
      (error 'buffer-bounds-error :buffer buf :line line :pos pos))
    (when (not (eq (line point) line))
      (setf (tick line) (incf (tick buf))))
    (setf (location* point) (values line pos))
    (setf (tick (line point)) (incf (tick buf)))
    (values line pos)))

(defmacro with-point ((buffer) &body body)
  "Saves and restores the point of buffer around body."
  (let ((bp-var (gensym "BP"))
	(buffer-var (gensym "BUFFER")))
    `(let ((,buffer-var ,buffer))
       (with-buffer-pointer (,bp-var (point ,buffer-var))
	 (unwind-protect
	      (progn
		,@body)
	   (setf (location* (point ,buffer-var)) (location* ,bp-var)))))))

;;; Insert is the convenience function; thing can contain newline(s).

(defmethod insert ((buffer editable-buffer) (c character)
		   &key position line (pos 0))
  (cond (position
	 (setf (point* buffer) (location* position)))
	(line
	 (setf (point* buffer) (values line pos))))
  (let* ((pt (slot-value buffer 'point))
	 (line (line pt))
	 (pos (pos pt)))
    ;; point is updated by bp-buffer-mixin methods.
    (if (eql c (newline-character buffer))
	      (buffer-open-line* buffer line pos)
	      (buffer-insert* buffer c line pos))))

(defmethod insert ((buffer editable-buffer) (s string)
		   &key position line (pos 0) (start 0) (end (length s)))
  (cond (position
	 (setf (point* buffer) (location* position)))
	(line
	 (setf (point* buffer) (values line pos))))
  (multiple-value-bind (line pos)
      (point* buffer)
    (loop
       with newline-character = (newline-character buffer)
       for search-start = start then (1+ found-newline)
       for found-newline = (position newline-character s
				     :start search-start
				     :end end)
       while found-newline
       do (progn
	    (setf (values line pos)
		  (buffer-insert* buffer s line pos
				  :start search-start
				  :end found-newline))
	    (setf (values line pos) (buffer-open-line* buffer line pos)))
       finally (return (buffer-insert* buffer s line pos
				       :start search-start :end end)))))

(defmethod delete-char ((buf editable-buffer) &optional (n 1)
			&key position line (pos 0))
  (cond (position
	 (setf (point* buf) (location* position)))
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
		   (setf (values line pos) (buffer-close-line* buf line 1)) 
		   (decf remaining (1+ del-chars)))
	    finally (return (buffer-delete-char* buf line pos remaining)))
	(loop with remaining = (- n)
	      while (< (- pos remaining) 0)
	      do (progn
		   (buffer-delete-char* buf line pos (- pos))
		   (decf remaining (1+ pos))
		   (setf (values line pos)
			 (buffer-close-line* buf line -1)))
	      finally (return
			(buffer-delete-char* buf line pos (- remaining)))))))


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

(defmethod forward-char* ((buf editable-buffer) n
			  &key (position (point buf)) line (pos 0))
  (multiple-value-bind (line pos)
      (if line
	  (values line pos)
	  (location* position))
    (if (>= n 0)
	(loop with remaining = n
	      for current-line = line then (next current-line)
	      for current-pos = pos then 0
	      for last-point = (or (and current-line
					(line-last-point current-line))
				   0)
	      until (or (null current-line)
			(<= remaining (- last-point current-pos)))
	      do (decf remaining (max 1 (- last-point current-pos)))
	      finally (if (null current-line)
			  (error 'buffer-bounds-error :buffer buf)
			  (return (values current-line
					  (+ current-pos remaining)))))
	(loop for current-line = line then (and (typep (prev current-line)
						       'dbl-list)
						(prev current-line))
	      for last-point = (or (and current-line
					(line-last-point current-line))
				   0)
	      ;; previous current-pos
	      for remaining = (- n) then (- remaining current-pos 1)
	      for current-pos = pos then last-point
	      until (or (null current-line) (<= remaining current-pos))
	      finally (if (null current-line)
			  (error 'buffer-bounds-error :buffer buf)
			  (return (values current-line
					  (- current-pos remaining))))))))

(defmethod forward-char ((buf basic-buffer) n &rest key-args)
  (multiple-value-bind (new-line new-pos)
      (apply #'forward-char* buf n key-args)
    (make-instance 'location :line new-line :pos new-pos)))

(defgeneric end-of-line* (buffer &key position line pos))

(defgeneric end-of-line (buffer &rest key-args))

(defmethod end-of-line* ((buf editable-buffer)
			 &key (position (point buf)) line (pos 0))
  (multiple-value-bind (line pos)
      (if line
	  (values line pos)
	  (location* position))
    (declare (ignore pos))
    (values line (line-last-point line))))

(defmethod end-of-line ((buf editable-buffer) &rest key-args)
  (multiple-value-bind (new-line new-pos)
      (apply #'end-of-line* buf key-args)
    (make-instance 'location :line new-line :pos new-pos)))

(defgeneric beginning-of-line* (buffer &key position line pos))

(defmethod beginning-of-line* ((buf editable-buffer)
			       &key (position (point buf)) line (pos 0))
  (multiple-value-bind (line pos)
      (if line
	  (values line pos)
	  (location* position))
    (declare (ignore pos))
    (values line 0)))

(defmethod beginning-of-buffer* ((buf editable-buffer))
  (location* (buffer-start buf)))

(defmethod end-of-buffer* ((buf editable-buffer))
  (location* (buffer-end buf)))

(defgeneric next-line (buffer &optional n &key position line pos)
  (:documentation "Return the line N lines from LINE, and a POS in it.
If LINE is not given, uses the BUFFER's current line.
If N is negative, goes backwards.
POS is the position in the line to return as the second value (trimmed
if beyond the actual line's maximum)."))

(defmethod next-line ((buf editable-buffer) &optional (n 1)
		      &key (position (point buf)) line (pos 0))
  (let ((line (or line (location* position)))
	(forward (> n 0))
	(times (abs n)))
    (loop for i upto times
	  for cur-line = line then (if forward (next line) (prev line))
	  when (or (not (typep cur-line 'buffer-line)) (null cur-line))
	    do (error 'buffer-bounds-error :buffer buf :line line)
	  finally
	  (return (values cur-line (min pos (line-last-point cur-line)))))))

;;; These iteration constructs need a bit more thought.
;;; map-over-region in its current state may not do the right thing if
;;; the buffer is modified in the region, but what is the right thing?
;;; Behave as if we're moving a point?

(defun map-over-region (func buf start end)
  (multiple-value-bind (line pos)
      (location* start)
    (loop until (and (eq line (line end)) (eql pos (pos end)))
	  do (progn
	       (funcall func line pos)
	       (setf (values line pos)
		     (forward-char* buf 1 :line line :pos pos))))))

;;; This is cheesy, but I don't feel like optimizing the delete right now.

;; antifuchs->Moore: is this more optimized? What was the extents
;; stuff you hinted at?

;; Had to fix a bug here - would always delete to the end of line, even if
;; start/end was a region within one line. Hopefully didn't screw anything up.
;;                                                           --Hefner

(defun delete-region (buf start end)  
  (if (eql (line start) (line end))
      (buffer-delete-char* buf (line start) (pos start)
			   (- (pos end) (pos start)))
    (progn
      (buffer-delete-char* buf (line start) (pos start)
			   (- (line-last-point (line start)) (pos start)))
      ;; didn't test this piece of code very well...
      (loop until (eql (line start) (line end))
        do (progn
             (buffer-close-line* buf (line start) 1)
             (buffer-delete-char* buf (line start) (pos start)
				  (- (line-last-point (line start))
				     (pos start)))))
      (buffer-delete-char* buf (line start) (pos start) (pos end)))))

(defmethod clear-buffer ((buf editable-buffer))
  (delete-region buf (buffer-start buf) (buffer-end buf))
  (setf (extents (line (buffer-start buf))) nil))
