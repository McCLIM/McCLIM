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
    (pdf:with-saved-state
      (pdf-actualize-graphics-state medium :line-style :color)
      (with-transformed-position (tr x1 y1)
        (with-transformed-position (tr x2 y2)
          (put-line* x1 y1 x2 y2))))))

(defmethod medium-draw-lines* ((medium pdf-medium) coord-seq)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf:with-saved-state
      (pdf-actualize-graphics-state medium :line-style :color)
      (map-repeated-sequence 'nil 4
                             (lambda (x1 y1 x2 y2)
                               (with-transformed-position (tr x1 y1)
                                 (with-transformed-position (tr x2 y2)
                                   (put-line* x1 y1 x2 y2))))
                             coord-seq))))


(defmethod medium-draw-point* ((medium pdf-medium) x y)
  (let ((tr (sheet-native-transformation (medium-sheet medium)))
        (radius (/ (medium-line-thickness medium) 2)))
    (pdf:with-saved-state
      (pdf-actualize-graphics-state medium :line-style :color)
      (with-transformed-position (tr x y)
        (put-circle* x y radius)))))

(defmethod medium-draw-points* ((medium pdf-medium) coord-seq)
  (let ((tr (sheet-native-transformation (medium-sheet medium)))
        (radius (/ (medium-line-thickness medium) 2)))
    (pdf:with-saved-state
      (pdf-actualize-graphics-state medium :line-style :color)
      (map-repeated-sequence 'nil 2
                             (lambda (x y)
                               (with-transformed-position (tr x y)
                                 (put-circle* x y radius)))
                             coord-seq))))

(defmethod medium-draw-polygon* ((medium pdf-medium) coord-seq closed filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf:with-saved-state
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
         (pdf:stroke))))))

(defmethod medium-draw-rectangle* ((medium pdf-medium) x1 y1 x2 y2 filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf:with-saved-state
      (pdf-actualize-graphics-state medium :line-style :color)
      (with-transformed-position (tr x1 y1)
        (with-transformed-position (tr x2 y2)
          (pdf:rectangle x1 y1 (- x2 x1) (- y2 y1))
          (if filled
              (pdf:fill-path)
              (pdf:stroke)))))))


(defmethod medium-draw-rectangles* ((medium pdf-medium) position-seq filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (pdf:with-saved-state
      (pdf-actualize-graphics-state medium :line-style :color)
      (map-repeated-sequence 'nil 4
                             (lambda (x1 y1 x2 y2)
                               (with-transformed-position (tr x1 y1)
                                 (with-transformed-position (tr x2 y2)
                                   (pdf:rectangle x1 y1 (- x2 x1) (- y2 y1))
                                   (if filled
                                       (pdf:fill-path)
                                       (pdf:stroke)))))
                             position-seq))))

(defun put-ellipse (center-x center-y
                    radius1-dx radius1-dy radius2-dx radius2-dy
                    start-angle end-angle filled)
  (declare (ignore start-angle end-angle filled))
  (let* ((kappa (* 4 (/ (- (sqrt 2) 1) 3.0)))
         (radius-dx (abs (+ radius1-dx radius2-dx)))
         (radius-dy (abs (+ radius1-dy radius2-dy))))
    (pdf:move-to (+ center-x radius-dx) center-y)
    (pdf:bezier-to (+ center-x radius-dx) (+ center-y (* kappa radius-dy))
                   (+ center-x (* kappa radius-dx)) (+ center-y radius-dy)
                   center-x (+ center-y radius-dy))
    (pdf:bezier-to (- center-x (* kappa radius-dx)) (+ center-y radius-dy)
                   (- center-x radius-dx) (+ center-y (* kappa radius-dy))
                   (- center-x radius-dx) center-y)
    (pdf:bezier-to (- center-x radius-dx) (- center-y (* kappa radius-dy))
                   (- center-x (* kappa radius-dx)) (- center-y radius-dy)
                   center-x (- center-y radius-dy))
    (pdf:bezier-to (+ center-x (* kappa radius-dx)) (- center-y radius-dy)
                   (+ center-x radius-dx) (- center-y (* kappa radius-dy))
                   (+ center-x radius-dx) center-y)))

(defmethod medium-draw-ellipse* ((medium pdf-medium) center-x center-y
                                 radius1-dx radius1-dy radius2-dx radius2-dy
                                 start-angle end-angle filled)
  (unless (or (= radius2-dx radius1-dy 0) (= radius1-dx radius2-dy 0))
    (error "PDF Backend MEDIUM-DRAW-ELLIPSE* not yet implemented for
    non axis-aligned ellipses."))
  (pdf:with-saved-state
    (let ((tr (sheet-native-transformation (medium-sheet medium))))
      (pdf-actualize-graphics-state medium :line-style :color)
      (with-transformed-position (tr center-x center-y)
        (put-ellipse center-x center-y
                     radius1-dx radius1-dy radius2-dx radius2-dy
                     start-angle end-angle filled))
      (if filled
          (pdf:close-fill-and-stroke)
          (pdf:stroke)))))

(defmethod text-size ((medium pdf-medium) string
                      &key text-style (start 0) end)
  (when (characterp string) (setq string (string string)))
  (unless end (setq end (length string)))
  (let* ((font-name (text-style-mapping (port medium)
                                        (merge-text-styles text-style
                                                           (medium-merged-text-style medium))))
         (size (clim-postscript-font:font-name-size font-name))
         (metrics-key (clim-postscript-font:font-name-metrics-key font-name)))
    (clim-postscript-font:text-size-in-font metrics-key size
                                            string start (or end (length string)))))

(defun medium-font (medium)
  (text-style-mapping (port medium) (medium-merged-text-style medium)))

(defmethod medium-draw-text* ((medium pdf-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              transformation)
  (multiple-value-bind (x y)
      (transform-position transformation x y)
    (pdf:with-saved-state
      (pdf:in-text-mode
        (pdf-actualize-graphics-state medium :text-style :color)
        (let ((tr (sheet-native-transformation (medium-sheet medium))))
          (multiple-value-bind (total-width total-height
                                final-x final-y baseline)
              (let* ((font-name (medium-font medium))
                     (font (clim-postscript-font:font-name-metrics-key font-name))
                     (size (clim-postscript-font:font-name-size font-name)))
                (clim-postscript-font:text-size-in-font font size string 0 nil))
            (declare (ignore final-x final-y))
            (let  ((x (ecase align-x
                        (:left x)
                        (:center (- x (/ total-width 2)))
                        (:right (- x total-width))))
                   (y (ecase align-y
                        (:baseline y)
                        (:top (+ y baseline))
                        (:center (- y (- (/ total-height 2)
                                         baseline)))
                        (:bottom (- y (- total-height baseline))))))
              (with-transformed-position (tr x y)
                (pdf:move-text x y)
                (pdf:draw-text string)))))))))


;;; Postscript path functions

(defgeneric pdf-add-path (medium region)
  (:documentation
   "Adds REGION (if it is a path) or its boundary (if it is an area)
   to the current path of MEDIUM."))

(defmethod pdf-add-path (medium (region (eql +nowhere+)))
  (declare (ignore medium)))

(defmethod pdf-add-path (medium (region standard-region-union))
  (map-over-region-set-regions (lambda (region)
                                 (pdf-add-path medium region))
                               region))

(defmethod pdf-add-path (medium (region standard-region-intersection))
  (loop for subregion in (region-set-regions region)
     do (pdf-add-path medium subregion)))

;;; Primitive paths
(defmethod pdf-add-path (medium (polygon polygon))
  (let ((points (polygon-points polygon))
        (tr (sheet-native-transformation (medium-sheet medium))))
    (let ((x0 (point-x (first points)))
          (y0 (point-y (first points))))
      (with-transformed-position (tr x0 y0)
        (pdf:move-to x0 y0)
        (loop for point in (rest points)
           do
             (let ((x1 (point-x point))
                   (y1 (point-y point)))
               (with-transformed-position (tr x1 y1)
                 (cl-pdf:line-to x1 y1))))
        ))))

(defmethod pdf-add-path (medium (ellipse ellipse))
  #+(or)
  (progn
    (put-ellipse medium ellipse t)
    (pdf:clip-path)
    (pdf:end-path-no-op)))

(defmethod pdf-add-path (medium (rs climi::standard-rectangle-set))
  (map-over-region-set-regions
   (lambda (r) (pdf-add-path medium r))
   rs))


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
(defgeneric pdf-set-clipping-region (medium region))

(defmethod pdf-set-clipping-region (medium region)
  (pdf-add-path medium region)
  (pdf:clip-path)
  (pdf:end-path-no-op))

(defmethod pdf-set-clipping-region (medium (region (eql +everywhere+))))

(defmethod pdf-set-clipping-region (medium (region (eql +nowhere+)))
  (pdf:basic-rect 0 0 0 0)
  (pdf:clip-path)
  (pdf:end-path-no-op))

(defmethod pdf-set-graphics-state (medium (kind (eql :clipping-region)))
  (pdf-set-clipping-region medium (medium-clipping-region medium)))

(defun %font-name-pdf-name (font-name)
  (etypecase font-name
    (clim-postscript-font:postscript-device-font-name
     (let ((font-info (clim-postscript-font:get-font-info font-name)))
       (unless font-info
         (error "Unknown font: ~S" font-info))
       (clim-postscript-font:font-info-name font-info)))
    (cons (coerce (car font-name) 'string))))

(defmethod pdf-set-graphics-state (medium (kind (eql :text-style)))
  (let* ((font-name (medium-font medium))
         (font (%font-name-pdf-name font-name))
         (size (clim-postscript-font:font-name-size font-name)))
    (pushnew font (slot-value (medium-sheet medium) 'document-fonts)
             :test #'string=)
    (let ((font (pdf:get-font font)))
      (pdf:set-font font size))))
