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

;;; Much implementation is done in flexivector-base so we can easily
;;; create flexivectors with general elements

(defclass flexivector-base ()
  ((store :accessor store)
   (size :accessor size)
   (gap :accessor gap)
   (gap-size :accessor gap-size)
   (size-increment :accessor size-increment :initarg :size-increment
		   :initform 32)))

(defmethod initialize-instance :after ((obj flexivector-base)
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
		     (- end start)) ))
	(cond (initial-contents
	       (setf (store obj) (make-array len
					     :element-type 'character))
	       (replace (store obj) initial-contents :start2 start :end2 end))
	      (initial-store
	       (setf (store obj) initial-store)))
	(setf (size obj) len)
	(setf (gap obj) len)
	(setf (gap-size obj) 0))
      (progn
	(setf (store obj) (make-array (size-increment obj)
				      :element-type 'character))
	(setf (size obj) 0)
	(setf (gap obj) 0)
	(setf (gap-size obj) (size-increment obj)))))

(define-condition flexivector-bounds-error (goatee-error)
  ((flexivector :reader flexivector-bounds-error-flexivector
		:initarg :flexivector :initform nil)
   (pos :reader flexivector-bounds-error-pos :initarg :pos :initform nil))
  (:report (lambda (condition stream)
	     (format stream "Position ~S is out of bounds for flexivector ~S~
of size ~S"
		     (flexivector-bounds-error-pos condition)
		     (flexivector-bounds-error-flexivector condition)
		     (size (flexivector-bounds-error-flexivector condition))))))

(defclass flexivector (flexivector-base)
  ()
  (:documentation "A flexivector that stores characters"))

(defmethod initialize-instance :after ((obj flexivector)
				       &key initial-contents
				       (start 0)
				       (end (when initial-contents
					      (length initial-contents))))
  (if initial-contents
      (let ((len (- end start)))
	(setf (store obj) (make-array (- end start) :element-type 'character))
	(replace (store obj) initial-contents :start2 start :end2 end)
	(setf (size obj) len)
	(setf (gap obj) len)
	(setf (gap-size obj) 0))
      (progn
	(setf (store obj) (make-array (size-increment obj)
				      :element-type 'character))
	(setf (size obj) 0)
	(setf (gap obj) 0)
	(setf (gap-size obj) (size-increment obj)))))

(defmethod print-object ((object flexivector) stream)
  (print-unreadable-object (object stream :type t)
    (write-char #\" stream)
    (loop for i from 0 below (size object)
	  do (write-char (char-ref object i) stream))
    (write-char #\" stream)))


(defmethod char-ref ((fv flexivector) pos)
  (when (or (>= pos (size fv))
	    (< pos 0))
    (error 'flexivector-bounds-error :flexivector fv :pos pos))
  (if (< pos (gap fv))
      (schar (store fv) pos)
      (schar (store fv) (+ pos (gap-size fv)))))

(defgeneric gap-to-insertion-point (buf point))

(defmethod gap-to-insertion-point ((buf flexivector-base) point)
  (let ((gap (gap buf))
	(store (store buf))
	(gap-size (gap-size buf)))
    (cond ((eql point gap)
	   (return-from gap-to-insertion-point nil))
	  ((> gap point)
	   (replace store store
		    :start1 (+ point gap-size) :end1 (+ gap gap-size)
		    :start2 point :end2 gap))
	  (t (let ((point-in-store (+ point gap-size)))
	       (replace store store
			:start1 gap :end1 (- point-in-store gap-size)
			:start2 (+ gap gap-size) :end2 point-in-store)))))
  (setf (gap buf) point))

(defgeneric ensure-point-gap (buf position len))

(defmethod ensure-point-gap ((buf flexivector-base) position len)
  (when (not (eql position (gap buf)))
    (gap-to-insertion-point buf position))
  (unless (<= len (gap-size buf))
    (let* ((new-gap-size (max len (size-increment buf)))
	   (new-store-size (+ (size buf) new-gap-size))
	   (new-store (make-array new-store-size :element-type 'character)))
      (replace new-store (store buf) :end2 (gap buf))
      (replace new-store (store buf)
	       :start1 (+ (gap buf) new-gap-size)
	       :start2 (+ (gap buf) (gap-size buf)))
      (setf (store buf) new-store)
      (setf (gap-size buf) new-gap-size))))

(defgeneric update-flexivector-for-insertion (buf len)
  (:documentation "Update the flexivector for an insertion at the gap (LEN is 
 positive) or a backwards deletion at the gap (LEN is negative)."))

(defmethod update-flexivector-for-insertion ((buf flexivector-base) len)
  (incf (size buf) len)
  (incf (gap buf) len)
  (decf (gap-size buf) len))

(defgeneric insert (vector thing &key position)
  (:documentation "Generalized insertion of THING into an ordered
 CONTAINTER at the generalized POSITION."))

(defmethod insert ((buf flexivector) (c character) &key (position 0))
  "Insert character C into flexivector BUF at POSITION."
  (ensure-point-gap buf position 1)
  (setf (schar (store buf) position) c)
  (update-flexivector-for-insertion buf 1)
  buf)

(defmethod insert ((buf flexivector) (str string)
		    &key (position 0) (start 0) (end (length str)))
  "Insert string STR into flexivector BUF at POSITION."
  (let ((len (length str)))
    (ensure-point-gap buf position len)
    (replace (store buf) str :start1 position :start2 start :end2 end)
    (update-flexivector-for-insertion buf len))
  buf)

(defgeneric delete-element (buf &optional n &key position))


(defmethod delete-element ((buf flexivector-base)  &optional (n 1)
			   &key (position 0))
  (ensure-point-gap buf position 0)
  (if (> n 0)
      (progn
	(incf (gap-size buf) n)
	(decf (size buf) n))
      (update-flexivector-for-insertion buf n)))

(defgeneric delete-char (buf &optional n &key position))

(defmethod delete-char ((buf flexivector) &optional (n 1) &key (position 0))
  (delete-element buf n :position position))

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

(defgeneric position-forward (buffer char &key position bound))

(defmethod position-forward ((buf flexivector) char
			     &key (position 0) (bound (size buf)))
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

(defgeneric position-backward (buffer char &key position bound))


(defmethod position-backward ((buf flexivector) char
			      &key (position (1- (size buf))) (bound 0))
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
