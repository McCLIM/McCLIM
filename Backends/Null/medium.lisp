;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2005 by Christophe Rhodes <c.rhodes@gold.ac.uk>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-null)

(defclass null-medium (basic-medium)
  ())

(defmethod (setf medium-text-style) :before (text-style (medium null-medium))
  (declare (ignore text-style))
  nil)

(defmethod (setf medium-line-style) :before (line-style (medium null-medium))
  (declare (ignore line-style))
  nil)

(defmethod (setf medium-clipping-region) :after (region (medium null-medium))
  (declare (ignore region))
  nil)

(defclass null-pixmap ()
  ((width  :initarg :width  :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   (depth  :initarg :depth  :reader pixmap-depth)))

(defmethod allocate-pixmap ((medium null-medium) width height)
  (make-instance 'null-pixmap :width width :height height :depth 32))

(defmethod deallocate-pixmap ((pixmap null-pixmap))
  nil)

(macrolet ((frob (from-class to-class)
             `(defmethod medium-copy-area ((from-drawable ,from-class)
                                           from-x from-y width height
                                           (to-drawable ,to-class)
                                           to-x to-y))))
  (frob null-medium null-medium)
  (frob null-medium null-pixmap)
  (frob null-pixmap null-medium)
  (frob null-pixmap null-pixmap))

(defmethod medium-draw-point* ((medium null-medium) x y)
  (declare (ignore x y))
  nil)

(defmethod medium-draw-points* ((medium null-medium) coord-seq)
  (declare (ignore coord-seq))
  nil)

(defmethod medium-draw-line* ((medium null-medium) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  nil)

;; FIXME: Invert the transformation and apply it here, as the :around
;; methods on transform-coordinates-mixin will cause it to be applied
;; twice, and we need to undo one of those. The
;; transform-coordinates-mixin stuff needs to be eliminated.
(defmethod medium-draw-lines* ((medium null-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (declare (ignore tr))
    nil))

(defmethod medium-draw-polygon* ((medium null-medium) coord-seq closed filled)
  (declare (ignore coord-seq closed filled))
  nil)

(defmethod medium-draw-rectangle* ((medium null-medium) left top right bottom filled)
  (declare (ignore left top right bottom filled))
  nil)

(defmethod medium-draw-rectangles* ((medium null-medium) position-seq filled)
  (declare (ignore position-seq filled))
  nil)

(defmethod medium-draw-ellipse* ((medium null-medium) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  (declare (ignore center-x center-y
                   radius-1-dx radius-1-dy
                   radius-2-dx radius-2-dy
                   start-angle end-angle filled))
  nil)

(defmethod text-style-ascent (text-style (medium null-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-descent (text-style (medium null-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-height (text-style (medium null-medium))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-character-width (text-style (medium null-medium) char)
  (declare (ignore text-style char))
  1)

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

(defmethod climb:text-bounding-rectangle*
    ((medium null-medium) string &key text-style (start 0) end align-x align-y direction)
  (declare (ignore align-x align-y direction)) ; implement me!
  (multiple-value-bind (width height x y baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore baseline))
    (values x y (+ x width) (+ y height))))

(defmethod medium-draw-text* ((medium null-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore string x y start end align-x align-y toward-x toward-y transform-glyphs))
  nil)

(defmethod medium-finish-output ((medium null-medium))
  nil)

(defmethod medium-force-output ((medium null-medium))
  nil)

(defmethod medium-clear-area ((medium null-medium) left top right bottom)
  (declare (ignore left top right bottom))
  nil)

(defmethod medium-beep ((medium null-medium))
  nil)

(defmethod medium-miter-limit ((medium null-medium))
  0)
