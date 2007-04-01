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
    ((medium :initarg :medium)
     (image :initarg :image)
     (medium-data :initform nil)))

(defun make-rgb-image-design (medium image)
  (make-instance 'rgb-image-design
    :medium medium
    :image image))


;;; Protocol to free cached data

(defgeneric medium-free-image-design (medium design))

(defun free-image-design (design)
  (medium-free-image-design (slot-value design 'medium) design))


;;; Drawing protocol

(defgeneric medium-draw-image-design* (medium design x y))

(defmethod medium-draw-image-design* :before (medium design x y)
  (assert (eq medium (slot-value design 'medium))))


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
