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

(defclass flexivector ()
  ((store :accessor store)
   (size :accessor size)
   (gap :accessor gap)
   (gap-size :accessor gap-size)
   (size-increment :accessor size-increment :initarg :size-increment
		   :initform 32)))

(defmethod initialize-instance :after ((obj flexivector)
				       &key initial-store
				       initial-contents
				       (start 0)
				       (end (when initial-contents
					      (length initial-contents))))
  (when (and initial-store initial-contents)
    (error "Only one of initial-store and initial-contents may be supplied."))
  (if (or initial-store initial-contents)
      (let ((len (if initial-store
		     (length initial-store)
		     (- start end)) ))
	(cond ((and initial-contents (eql start 0) (eql end len))
	       (setf (store obj) (make-array len
					     :initial-contents initial-contents
					     :element-type 'character)))
	      (initial-store
	       (setf (store obj) initial-store))
	      (t (setf (store obj)
		       (make-array len
				   :initial-contents initial-contents
				   :element-type 'character))))
	(setf (size obj) 0)
	(setf (gap obj) len)
	(setf (gap-size obj) 0))
      (progn
	(setf (store obj) (make-array (size-increment obj)
				      :element-type 'character))
	(setf (size obj) 0)
	(setf (gap obj) 0)
	(setf (gap-size obj) (size-increment obj)))))

(define-condition flexivector-bounds-error (error)
  ((flexivector :reader flexivector-bounds-error-flexivector
		:initarg :flexivector :initform nil)
   (pos :reader flexivector-bounds-error-pos :initarg :pos :initform nil))
  (:report (lambda (condition stream)
	     (format stream "Position ~S is out of bounds for flexivector ~S~
of size ~S"
		     (pos condition)
		     (flexivector condition)
		     (size (flexivector condition))))))

(defmethod char-ref ((fv flexivector) pos)
  (when (or (>= pos (size fv))
	    (< pos 0))
    (error 'flexivector-bounds-error :flexivector fv :pos pos))
  (if (< pos (gap fv))
      (char (store fv) pos)
      (char (store fv) (+ pos (gap-size fv)))))

(defun gap-to-insertion-point (buf point)
  (cond ((eql point (gap buf))
	 (return-from gap-to-insertion-point nil))
	((> (gap buf) point)
	 (replace (store buf) (store buf)
		  :start1 (+ point (gap-size buf))
		  :end1 (+ (gap buf) (gap-size buf))
		  :start2 point
		  :end2 (gap buf)))
	(t (let ((point-in-store (if (<= point (gap buf))
				     point
				     (+ point (gap-size buf)))))
	     (replace (store buf) (store buf)
		      :start1 (gap buf)
		      :end1 (- point-in-store (gap-size buf))
		      :start2 (+ (gap buf) (gap-size buf))
		      :end2 point-in-store))))
  (setf (gap buf) point))

(defun ensure-point-gap (buf position len)
  (when (not (eql position (gap buf)))
    (gap-to-insertion-point buf position))
  (unless (<= len (gap-size buf))
    (let* ((new-gap-size (max len (size-increment buf)))
	   (new-store-size (+ (size buf) new-gap-size))
	   (new-store (make-array new-store-size :element-type 'character)))
      (replace new-store (store buf) :end2 (gap buf))
      (replace new-store (store buf)
	       :start1 (gap buf) :start2 (+ (gap buf) (gap-size buf)))
      (setf (store buf) new-store)
      (setf (gap-size buf) new-gap-size))))

(defun update-flexivector-for-insertion (buf len)
  (incf (size buf) len)
  (incf (gap buf) len)
  (decf (gap-size buf) len))

(defgeneric insert (vector thing &optional position &key))

(defmethod insert ((buf flexivector) (c character) &optional (position 0)
		    &key)
  (ensure-point-gap buf position 1)
  (setf (schar (store buf) position) c)
  (update-flexivector-for-insertion buf 1)
  buf)

(defmethod insert ((buf flexivector) (str string) &optional (position 0)
		    &key (start 0) (end (length str)))
  (let ((len (length str)))
    (ensure-point-gap buf position len)
    (replace (store buf) str :start1 position :start2 start :end2 end)
    (update-flexivector-for-insertion buf len))
  buf)

(defgeneric delete-char (buf &optional n position))

(defmethod delete-char ((buf flexivector)  &optional (n 1) (position 0))
  (ensure-point-gap buf position 0)
  (if (> n 0)
      (progn
	(incf (gap-size buf) n)
	(decf (size buf) n))
      (update-flexivector-for-insertion buf n)))

(defgeneric flexivector-string (buf &key start end))

(defmethod flexivector-string ((buf flexivector)
			       &key (start 0) (end (size buf)))
  (let* ((str-size (- end start))
	 (string (make-array  str-size :element-type 'character)))
    (when (< start (gap buf))
      (replace string (store buf)
	       :start2 start :end2 (min end (gap buf))))
    (when (> end (gap buf))
      (replace string (store buf)
	       :start1 (max 0 (- (gap buf) start))
	       :start2 (+ (max (gap buf) start) (gap-size buf))
	       :end2 (+ end (gap-size buf))))
    string))

(defgeneric flexivector-string-into (buf string &key start1 end1 start2 end2))

(defmethod flexivector-string-into ((buf flexivector) string &key
			       (start1 0) (end1 (length string))
			       (start2 0) (end2 (size buf)))
  (when (< start2 (gap buf))
      (replace string (store buf)
	       :start1 start1 :end1 end1
	       :start2 start2 :end2 (min end2 (gap buf))))
    (when (> end2 (gap buf))
      (replace string (store buf)
	       :start1 (max start1 (- (gap buf) start2))
	       :end1 end1
	       :start2 (+ (max (gap buf) start2) (gap-size buf))
	       :end2 (+ end2 (gap-size buf))))
    string)

(defgeneric position-forward (buffer char position &optional bound))

(defmethod position-forward ((buf flexivector) char position
			     &optional(bound (size buf)))
  (loop for pos from position below (min bound (gap buf))
	do (when (eql char (schar (store buf) pos))
	     (return-from position-forward pos)))
  (when (<= bound (gap buf))
    (return-from position-forward nil))
  (loop for pos from (+ (gap buf) (gap-size buf)) below (+ (min bound
								(size buf))
							   (gap-size buf))
	do (when (eql char (schar (store buf) pos))
	     (return-from position-forward (- pos (gap-size buf)))))
  nil)

(defgeneric position-backward (buffer char position &optional bound))


(defmethod position-backward ((buf flexivector) char position
			      &optional (bound 0))
  ;; First loop only happens when (> position gap)
  (loop for pos from (1- (+ position (gap-size buf))) downto (+ (gap buf)
								(gap-size buf))
	do (when (eql char (schar (store buf) pos))
	     (return-from position-backward (- pos (gap-size buf)))))
  (when (> bound (gap buf))
    (return-from position-backward nil))
  (loop for pos from (1- (min position (gap buf))) downto bound
	do (when (eql char (schar (store buf) pos))
	     (return-from position-backward pos)))
  nil)
