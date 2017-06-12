;;; -*- Mode: Lisp; Package: CLIM-PDF -*-

;;;  (c) copyright 2017 by
;;;           Cyrus Harmon (cyrus@bobobeach.com)

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

(in-package :clim-pdf)

(defun put-line* (x1 y1 x2 y2)
  (pdf:move-to x1 y1)
  (pdf:line-to x2 y2)
  (pdf:stroke))

(defun put-circle* (x y radius)
  (pdf:circle x y radius)
  (pdf:close-fill-and-stroke))

(defmethod medium-draw-line* ((medium pdf-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf-actualize-graphics-state medium :line-style :color)
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (put-line* x1 y1 x2 y2)))))

(defmethod medium-draw-lines* ((medium pdf-medium) coord-seq)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf-actualize-graphics-state medium :line-style :color)
    (map-repeated-sequence 'nil 4
                           (lambda (x1 y1 x2 y2)
                             (with-transformed-position (tr x1 y1)
                               (with-transformed-position (tr x2 y2)
                                 (put-line* x1 y1 x2 y2))))
                           coord-seq)))


(defmethod medium-draw-point* ((medium pdf-medium) x y)
  (let ((tr (sheet-native-transformation (medium-sheet medium)))
        (radius (/ (medium-line-thickness medium) 2)))
    (pdf-actualize-graphics-state medium :line-style :color)
    (with-transformed-position (tr x y)
      (put-circle* x y radius))))

(defmethod medium-draw-points* ((medium pdf-medium) coord-seq)
  (let ((tr (sheet-native-transformation (medium-sheet medium)))
        (radius (/ (medium-line-thickness medium) 2)))
    (pdf-actualize-graphics-state medium :line-style :color)
    (map-repeated-sequence 'nil 2
                           (lambda (x y)
                             (with-transformed-position (tr x y)
                               (put-circle* x y radius)))
                           coord-seq)))

(defmethod medium-draw-polygon* ((medium pdf-medium) coord-seq closed filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf-actualize-graphics-state medium :line-style :color)
    (pdf:polyline
     (map-repeated-sequence 'list 2
                            (lambda (x y)
                              (with-transformed-position (tr x y)
                                (list x y)))
                            coord-seq))
    (cond
      ((and closed filled)
       (pdf:close-fill-and-stroke))
      (closed
       (pdf:close-and-stroke))
      (filled
       (pdf:fill-and-stroke))
      (t
       (pdf:stroke)))))

(defmethod medium-draw-rectangle* ((medium pdf-medium) x1 y1 x2 y2 filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf-actualize-graphics-state medium :line-style :color)
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (pdf:rectangle x1 y1 (- x2 x1) (- y2 y1))
        (if filled
            (pdf:fill-path)
            (pdf:stroke))))))

;;;
;;; this is brokem. i don't understand why.
#+nil
(defmethod medium-draw-rectangles* ((medium pdf-medium) position-seq filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (map-repeated-sequence 'nil 4
                           (lambda (x1 y1 x2 y2)
                             (with-transformed-position (tr x1 y1)
                               (with-transformed-position (tr x2 y2)
                                 (pdf:rectangle x1 y1 (- x2 x1) (- y2 y1))
                                 (if filled
                                     (pdf:fill-path)
                                     (pdf:stroke)))))
                           position-seq)))

;;; Graphics state

(defgeneric pdf-set-graphics-state (medium kind))

(defvar *pdf-graphics-states*
  '((:line-style . medium-line-style)
    (:color . medium-ink)
    (:clipping-region . medium-clipping-region)
    (:text-style . medium-text-style)))

(defun pdf-current-state (medium kind)
  (funcall (cdr (assoc kind *pdf-graphics-states*))
           medium))

(defmacro pdf-saved-state (medium kind)
  `(getf (pdf-medium-graphics-state ,medium) ,kind))

(defun pdf-actualize-graphics-state (medium &rest kinds)
  "Sets graphics parameters named in STATES."
  (loop for kind in (cons :clipping-region kinds)
     ;; every drawing function depends on clipping region
     ;;
     ;; KLUDGE: clipping-region MUST be actualized first due to its
     ;; dirty dealing with graphics state. -- APD, 2002-02-11
     unless (eql (pdf-current-state medium kind)
                 (pdf-saved-state medium kind))
     do (pdf-set-graphics-state medium kind)))

;;; Line style
(defconstant +pdf-line-joints+ '(:miter 0
                                        :round 1
                                        :bevel 2
                                        :none 0))

(defconstant +pdf-line-caps+ '(:butt 0
                                      :round 1
                                      :square 2 ; extended butt caps
                                      :No-end-point 0))

(defconstant +pdf-default-line-dashes+ '(30 30))

(defconstant +normal-line-width+ (/ 2.0 3.0))

(defun line-style-scale (line-style)
  (let ((unit (line-style-unit line-style)))
    (ecase unit
      (:normal +normal-line-width+)
      (:point 1)
      (:coordinate (error ":COORDINATE line unit is not implemented.")))))

(defmethod line-style-effective-thickness
    (line-style (medium pdf-medium))
  (* (line-style-thickness line-style)
     (line-style-scale line-style)))

(defun medium-line-thickness (medium)
  (line-style-effective-thickness (medium-line-style medium) medium))

(defmethod pdf-set-graphics-state (medium (kind (eql :line-style)))
  (let* ((line-style (medium-line-style medium))
         (scale (line-style-scale line-style)))
    (pdf:set-line-width (* scale (line-style-thickness line-style)))
    (pdf:set-line-join (getf +pdf-line-joints+
                             (line-style-joint-shape line-style)))
    (pdf:set-line-cap (getf +pdf-line-caps+
                  (line-style-cap-shape line-style)))
    ;; FIXME!!! dashes not yet implemented!
    ))

;;; Color
(defgeneric medium-color-rgb (medium ink))

(defmethod medium-color-rgb (medium (ink (eql +foreground-ink+)))
  (medium-color-rgb medium (medium-foreground medium)))

(defmethod medium-color-rgb (medium (ink (eql +background-ink+)))
  (medium-color-rgb medium (medium-background medium)))

(defmethod medium-color-rgb (medium (ink color))
  (declare (ignore medium))
  (color-rgb ink))

(defmethod pdf-set-graphics-state (medium (kind (eql :color)))
  (multiple-value-bind (r g b)
      (medium-color-rgb medium (medium-ink medium))
    (pdf:set-rgb-fill r g b)
    (pdf:set-rgb-stroke r g b)))

;;; Clipping region
(defgeneric pdf-set-clipping-region (region))

(defmethod pdf-set-clipping-region (region)
  (pdf-add-path region)
  (pdf:clip-path))

(defmethod pdf-set-clipping-region ((region (eql +everywhere+))))

(defmethod pdf-set-clipping-region ((region (eql +nowhere+)))
  (pdf:basic-rect 0 0 0 0)
  (pdf:clip-path))

(defmethod pdf-set-graphics-state (medium (kind (eql :clipping-region)))
  ;; FIXME: There is no way to enlarge clipping path. Current code
  ;; does only one level of saving graphics state, so we can restore
  ;; and save again GS to obtain an initial CP. It is ugly, but I see
  ;; no other way now. -- APD, 2002-02-11
  (pdf-set-clipping-region (medium-clipping-region medium)))
