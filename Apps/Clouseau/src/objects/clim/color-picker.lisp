;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; `radial-gradient-pattern'

(defclass radial-gradient-pattern (clime:pattern)
  ((%radius :initarg :radius
            :reader  radius)
   (%color1 :initarg :color1
            :reader  color1)
   (%color2 :initarg :color2
            :reader  color2)))

(defmethod pattern-width ((pattern radial-gradient-pattern))
  (* 2 (radius pattern)))

(defmethod pattern-height ((pattern radial-gradient-pattern))
  (* 2 (radius pattern)))

(defmethod clime::pattern-repeat-mode ((pattern radial-gradient-pattern))
  :solid)

(defmethod clime:design-ink ((design radial-gradient-pattern) x y)
  (let* ((radius   (radius design))
         (x        (- x radius))
         (y        (- y radius))
         (radius^2 (+ (expt x 2) (expt y 2))))
    (if (<= radius^2 (expt radius 2))
        (mcclim-render-internals::let-rgba ((r1 g1 b1 a1) (climi::%rgba-value (color1 design)))
          (mcclim-render-internals::let-rgba ((r2 g2 b2 a2) (climi::%rgba-value (color2 design)))
            (let* ((s (/ (sqrt radius^2) radius))
                   (f (truncate (* 255 s))))
              (logior (ash (mcclim-render-internals::%byte-blend-value r1 r2 (- 255 f) f) 24)
                      (ash (mcclim-render-internals::%byte-blend-value g1 g2 (- 255 f) f) 16)
                      (ash (mcclim-render-internals::%byte-blend-value b1 b2 (- 255 f) f) 08)
                      (ash (mcclim-render-internals::%byte-blend-value a1 a2 (- 255 f) f) 00)))))
        0)))

;;; `function-pattern-mixin'

(defgeneric compute-pattern-function (pattern))

(defclass function-pattern-mixin ()
  ((%function :type     function
              :accessor pattern-function)))

(defmethod shared-initialize :after ((instance   function-pattern-mixin)
                                     (slot-names t)
                                     &key)
  (setf (pattern-function instance) (compute-pattern-function instance)))

(defmethod clime:design-ink ((design function-pattern-mixin) x y)
  (declare (optimize speed))
  (funcall (the function (pattern-function design)) x y))

;;; `hue-circle-pattern'

(defclass hue-circle-pattern (clime:pattern
                              function-pattern-mixin)
  ((%inner-radius :initarg  :inner-radius
                  :reader   inner-radius)
   (%outer-radius :initarg  :outer-radius
                  :reader   outer-radius)
   (%intensity    :initarg  :intensity
                  :reader   intensity
                  :initform 1.0)
   (%saturation   :initarg  :saturation
                  :reader   saturation
                  :initform 1.0)))

(defmethod compute-pattern-function ((instance hue-circle-pattern))
  (let* ((inner-radius   (inner-radius instance))
         (inner-radius^2 (expt inner-radius 2))
         (outer-radius   (outer-radius instance))
         (outer-radius^2 (expt outer-radius 2))
         (itensity       (intensity instance))
         (saturation     (saturation instance)))
    (lambda (x y)
      (let* ((x        (- x outer-radius))
             (y        (- y outer-radius))
             (radius^2 (+ (expt x 2) (expt y 2))))
        (if (<= inner-radius^2 radius^2 outer-radius^2)
            (let* ((angle (atan y x))
                   (angle (if (minusp angle)
                              (+ (* 2 pi) angle)
                              angle))
                   (hue   (clamp (/ angle (* 2 pi)) 0.0f0 1.0f0)))
              (locally (declare (inline climi::ihs-to-rgb))
                (multiple-value-bind (red green blue)
                    (climi::ihs-to-rgb itensity hue saturation)
                  (declare (type (real 0 1) #+TODO (double-float 0.0d0 1.0d0) red green blue))
                  (logior (ash (truncate (* 255 red))   24)
                          (ash (truncate (* 255 green)) 16)
                          (ash (truncate (* 255 blue))  8)
                          255))))
            0)))))

(defmethod pattern-width ((pattern hue-circle-pattern))
  (* 2 (outer-radius pattern)))

(defmethod pattern-height ((pattern hue-circle-pattern))
  (* 2 (outer-radius pattern)))

(defmethod clime::pattern-repeat-mode ((pattern hue-circle-pattern))
  :solid)

;;; `hue-saturation-pattern'

(defclass hue-saturation-pattern (clime:pattern
                                  function-pattern-mixin)
  ((%width     :initarg :width
               :reader  pattern-width)
   (%height    :initarg :height
               :reader  pattern-height)
   (%intensity :initarg :intensity
               :reader  intensity)))

(defmethod compute-pattern-function ((instance function-pattern-mixin))
  (let ((i             (float (intensity instance) 1.0d0))
        (width-factor  (float (/ (pattern-width instance)) 1.0d0))
        (height-factor (float (/ (pattern-height instance)) 1.0d0)))
    (lambda (x y)
      (declare (type (real 0) x y)
               (optimize speed))
      (let ((h (float (* x width-factor)  1.0d0))
            (s (float (* y height-factor) 1.0d0)))
        (locally (declare (inline climi::ihs-to-rgb))
          (multiple-value-bind (r g b) (climi::ihs-to-rgb i h s)
            (declare (type (real 0 1) #+TODO (double-float 0.0d0 1.0d0) r g b))
            (logior (ash (truncate (* 255 r)) 24)
                    (ash (truncate (* 255 g)) 16)
                    (ash (truncate (* 255 b))  8)
                    255)))))))

(defmethod clime::pattern-repeat-mode ((pattern hue-saturation-pattern))
  :solid)

;;; `intensity-pattern'

(defclass intensity-pattern (clime:pattern)
  ((%width  :initarg :width
            :reader  pattern-width)
   (%height :initarg :height
            :reader  pattern-height)))

(defmethod clime:design-ink ((design intensity-pattern) x y)
  (declare (optimize speed))
  (let* ((i (float (/ y (pattern-height design)) 1.0d0))
         (v (truncate (* 255 i))))
    (declare (type (double-float 0.0d0 1.0d0) i))
    (logior (ash v 8) (ash v 16) (ash v 24) 255)))

;;;

(defun draw-color-chooser (stream width height color)
  (multiple-value-bind (red green blue) (color-rgb color)
    (multiple-value-bind (intensity hue saturation) (color-ihs color)
      (draw-rectangle* stream 0 0 width height
                       :ink (make-instance 'hue-saturation-pattern
                                           :width     width
                                           :height    height
                                           :intensity intensity))
      (draw-circle* stream (* hue width) (* saturation height) 4
                    :filled nil :ink +flipping-ink+ :line-thickness 2)

      (draw-rectangle* stream (+ width 8) 0 (+ width 8 32) height
                       :ink (make-instance 'intensity-pattern
                                           :width  (+ width 8 32)
                                           :height height))
      (draw-circle* stream (+ width 8 16) (* intensity height) 4
                    :filled nil :ink +flipping-ink+ :line-thickness 2)

      (let ((x (+ width 8 32 8))
            (y 0))
        (let ((design (make-instance 'hue-circle-pattern
                                     :inner-radius (* width 1/2 3/4)
                                     :outer-radius (* width 1/2)
                                     :intensity    intensity
                                     :saturation   saturation)))
          (draw-rectangle* stream x y (+ x width) (+ y height) :ink design)
          (let* ((outer-radius (outer-radius design))
                 (radius       (/ (+ (inner-radius design) outer-radius) 2))
                 (radiant      (* 2 pi hue))
                 (cx           (+ x outer-radius (* radius (cos radiant))))
                 (cy           (+ y outer-radius (* radius (sin radiant)))))
            (draw-circle* stream cx cy 4
                          :filled nil :ink +flipping-ink+ :line-thickness 2))))

      (draw-rectangle* stream 0 (+ height 8) (+ width 8 32) (+ height 8 32)
                       :filled t :ink color)

      (let ((x (+ width 8 32 8))
            (y (+ 200 8)))
        ; (medium-clear-area (sheet-medium stream) x y (+ x 120) (+ y 200))
        ; (setf (stream-cursor-position stream) (values x y))
        #+requires-stream (formatting-table (stream)
          (formatting-row (stream)
            (formatting-cell (stream) (write-string "Red" stream))
            (formatting-cell (stream) (format stream "~,2F" red)))
          (formatting-row (stream)
            (formatting-cell (stream) (write-string "Green" stream))
            (formatting-cell (stream) (format stream "~,2F" green)))
          (formatting-row (stream)
            (formatting-cell (stream) (write-string "Blue" stream))
            (formatting-cell (stream) (format stream "~,2F" blue)))
          (formatting-row (stream)
            (formatting-cell (stream) (write-string "Hue" stream))
            (formatting-cell (stream) (format stream "~,2F" hue)))
          (formatting-row (stream)
            (formatting-cell (stream) (write-string "Saturation" stream))
            (formatting-cell (stream) (format stream "~,2F" saturation)))
          (formatting-row (stream)
            (formatting-cell (stream) (write-string "Intensity" stream))
            (formatting-cell (stream) (format stream "~,2F" intensity))))))))

;;; `color-chooser[-pane]'

(defclass color-chooser (value-gadget)
  ())

(defclass color-chooser-pane (color-chooser)
  ())

(defmethod compose-space ((pane color-chooser-pane) &key width height)
  (declare (ignore width height))
  (let ((width  (+ 200 200))
        (height (+ 200 32)))
    (make-space-requirement :min-width  width  :max-width  width
                            :min-height height :max-height height)))

(defmethod handle-repaint ((sheet color-chooser-pane) (region t))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region sheet)
    (let ((width  (floor (- x2 x1) 3))
          (height (floor (- (- y2 y1) 32))))
      (draw-color-chooser sheet width height (gadget-value sheet)))))

;;; `color-chooser-button[-pane]'
