;;;  (c) copyright 1998 by Gilbert Baumann
;;;
;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; (Hacked for inclusion into McCLIM by David Lichteblau.)

(in-package :climi)


;;; ARGB image data represented as an (unsigned-byte 32) array

(defclass rgb-image ()
    ((width :initarg :width :accessor image-width)
     (height :initarg :height :accessor image-height)
     (data :initarg :data
	   :accessor image-data
	   :type (or null (simple-array (unsigned-byte 32) (* *))))
     (alphap :initarg :alphap
	     :initform nil
	     :accessor image-alpha-p)))

;;; Applications (closure in particular) might want to cache any
;;; backend-specific data required to draw an RGB-IMAGE.
;;;
;;; To implement this caching, designs must be created separately for each
;;; medium, so that mediums can put their own data into them.

(defclass rgb-image-design (design)
    ((medium :initform nil :initarg :medium)
     (image :reader image
            :initarg :image)
     (medium-data :initform nil)))

(defun make-rgb-image-design (image)
  (make-instance 'rgb-image-design :image image))


;;; Protocol to free cached data

(defgeneric medium-free-image-design (medium design))

(defmethod medium-free-image-design ((sheet sheet-with-medium-mixin) design)
  (medium-free-image-design (sheet-medium sheet) design))

(defun free-image-design (design)
  (medium-free-image-design (slot-value design 'medium) design))


;;; Drawing protocol

(defgeneric medium-draw-image-design* (medium design x y))

(defmethod medium-draw-image-design* :before (current-medium design x y)
  (with-slots (medium medium-data) design
    (unless (eq medium current-medium)
      (when medium
	(medium-free-image-design medium design))
      (setf medium current-medium)
      (setf medium-data nil))))

(defmethod medium-draw-image-design*
    ((medium sheet-with-medium-mixin) design x y)
  (medium-draw-image-design* (sheet-medium medium) design x y))

;;; Output recording stuff, this was copied from the pattern code.

(def-grecording draw-image-design (() image-design x y) ()
  (let ((width (image-width (image image-design)))
        (height (image-height (image image-design)))
	(transform (medium-transformation medium)))
    (setf (values x y) (transform-position transform x y))
    (values x y (+ x width) (+ y height))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-image-design-output-record))
(with-standard-rectangle* (:x1 x1 :y1 y1)
    record
  (with-slots (x y)
      record
    (let ((dx (- nx x1))
	  (dy (- ny y1)))
      (multiple-value-prog1
	  (call-next-method)
	(incf x dx)
	(incf y dy))))))

(defrecord-predicate draw-image-design-output-record (x y image-design)
  (and (if-supplied (x coordinate)
	 (coordinate= (slot-value record 'x) x))
       (if-supplied (y coordinate)
	 (coordinate= (slot-value record 'y) y))
       (if-supplied (image-design rgb-image-design)
         (eq (slot-value record 'image-design) image-design))))

;;; Fetching protocol

(defun sheet-rgb-image (sheet &key x y width height)
  (multiple-value-bind (data alphap)
      (sheet-rgb-data (port sheet)
		      sheet
		      :x x
		      :y y
		      :width width
		      :height height)
    (destructuring-bind (height width)
	(array-dimensions data)
      (make-instance 'rgb-image
	:width width
	:height height
	:data data
	:alphap alphap))))

(defgeneric sheet-rgb-data (port sheet &key x y width height))


(defmethod draw-design
    (medium (design rgb-image-design) &rest options
     &key (x 0) (y 0) &allow-other-keys)
  (with-medium-options (medium options)
    (medium-draw-image-design* medium design x y)))
