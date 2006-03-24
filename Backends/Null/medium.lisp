;;; -*- Mode: Lisp; Package: CLIM-NULL -*-

;;; (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-null)

(defclass null-medium (basic-medium)
  ((buffering-output-p :accessor medium-buffering-output-p)))

(defmethod (setf medium-text-style) :before (text-style (medium null-medium))
  ())

(defmethod (setf medium-line-style) :before (line-style (medium null-medium))
  ())

(defmethod (setf medium-clipping-region) :after (region (medium null-medium))
  ())

(defmethod medium-copy-area ((from-drawable null-medium)
			     from-x from-y width height
                             (to-drawable null-medium)
			     to-x to-y)
  nil)

#+nil ; FIXME: PIXMAP class
(progn
  (defmethod medium-copy-area ((from-drawable null-medium)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    nil)
  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable null-medium)
			       to-x to-y)
    ())
  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    ()))

(defmethod medium-draw-point* ((medium null-medium) x y)
  ())

(defmethod medium-draw-points* ((medium null-medium) coord-seq)
  ())

(defmethod medium-draw-line* ((medium null-medium) x1 y1 x2 y2)
  ())

;; FIXME: Invert the transformation and apply it here, as the :around
;; methods on transform-coordinates-mixin will cause it to be applied
;; twice, and we need to undo one of those. The
;; transform-coordinates-mixin stuff needs to be eliminated.
(defmethod medium-draw-lines* ((medium null-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (declare (ignore tr))
    nil))

(defmethod medium-draw-polygon* ((medium null-medium) coord-seq closed filled)
  ())

(defmethod medium-draw-rectangle* ((medium null-medium) left top right bottom filled)
  ())
(defmethod medium-draw-rectangles* ((medium null-medium) position-seq filled)
  ())

(defmethod medium-draw-ellipse* ((medium null-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  ())

(defmethod medium-draw-circle* ((medium null-medium)
				center-x center-y radius start-angle end-angle
				filled)
  ())

(defmethod text-style-ascent (text-style (medium null-medium))
  1)
(defmethod text-style-descent (text-style (medium null-medium))
  1)
(defmethod text-style-height (text-style (medium null-medium))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))
(defmethod text-style-character-width (text-style (medium null-medium) char)
  1)
;;; FIXME: this one is nominally backend-independent
(defmethod text-style-width (text-style (medium null-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size
    ((medium null-medium) string &key text-style (start 0) end)
  (setf string (etypecase string
		 (character (string string))
		 (string string)))
  (let ((width 0)
	(height (text-style-height text-style medium))
	(x (- (or end (length string)) start))
	(y 0)
	(baseline (text-style-ascent text-style medium)))
    (do ((pos (position #\Newline string :start start :end end)
	      (position #\Newline string :start (1+ pos) :end end)))
	((null pos) (values width height x y baseline))
      (let ((start start)
	    (end pos))
	(setf x (- end start))
	(setf y (+ y (text-style-height text-style medium)))
	(setf width (max width x))
	(setf height (+ height (text-style-height text-style medium)))
	(setf baseline (+ baseline (text-style-height text-style medium)))))))

(defmethod medium-draw-text* ((medium null-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  ())

#+nil
(defmethod medium-buffering-output-p ((medium null-medium))
  t)
#+nil
(defmethod (setf medium-buffering-output-p) (buffer-p (medium null-medium))
  buffer-p)

(defmethod medium-draw-glyph ((medium null-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  ())

(defmethod medium-finish-output ((medium null-medium))
  ())
(defmethod medium-force-output ((medium null-medium))
  ())

(defmethod medium-clear-area ((medium null-medium) left top right bottom)
  ())

(defmethod medium-beep ((medium null-medium))
  ())

(defmethod invoke-with-special-choices (continuation (medium null-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

(defmethod medium-miter-limit ((medium null-medium))
  0)
