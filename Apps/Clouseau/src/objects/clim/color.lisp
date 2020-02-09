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

;;; Utilities

(defun draw-color-sample (stream opaque color &key width height)
  (let ((width/2 (/ width 2)))
    (draw-rectangle* stream 0       0 width      height      :ink *checkers-design*)
    (draw-rectangle* stream 1       1 width/2    (1- height) :ink opaque)
    (draw-rectangle* stream width/2 1 (1- width) (1- height) :ink color)))

(defmethod draw-design-sample ((stream t) (design color)
                               &rest args &key width height)
  (declare (ignore width height))
  (let ((opaque (multiple-value-call #'make-rgb-color (color-rgb design))))
    (apply #'draw-color-sample stream design opaque args)))

(defmethod draw-design-sample ((stream t) (design climi::uniform-compositum)
                               &rest args &key width height)
  (declare (ignore width height))
  (apply #'draw-color-sample stream design design args))

(defmethod draw-design-sample ((stream t) (design climi::opacity)
                               &rest args &key width height)
  (declare (ignore width height))
  (let ((color (compose-in +red+ design)))
    (apply #'draw-color-sample stream +transparent-ink+ color args)))

(defmethod draw-design-sample ((stream t) (design (eql +transparent-ink+))
                               &rest args &key width height)
  (declare (ignore width height))
  (apply #'draw-color-sample stream design design args))

(defun number-color (number)
  (typecase number
    ((unsigned-byte 32)
     (compose-in
      (make-rgb-color (/ (ldb (byte 8  0) number) 255.0f0)
                      (/ (ldb (byte 8  8) number) 255.0f0)
                      (/ (ldb (byte 8 16) number) 255.0f0))
      (make-opacity (/ (ldb (byte 8 24) number) 255.0f0))))))

(defmethod draw-design-sample ((stream t) (design design) &key width height)
  (draw-rectangle* stream 0 0 width height :ink design))

(defmethod draw-design-sample :around ((stream t) (design t)
                                       &key (width 80)
                                            (height (nth-value 1 (text-size stream "M"))))
  (call-next-method stream design :width width :height height))

;;; Object states

(defclass inspected-color (inspected-instance)
  ())

(defmethod object-state-class ((object clim:color) (place t))
  'inspected-color)

(defmethod object-state-class ((object clim:opacity) (place t))
  'inspected-color)

;;; Object inspection methods

(defclass my-design (clim:design)
  ())

(defmethod bounding-rectangle ((thing climi::transformed-design))
  (transform-region (climi::transformed-design-transformation thing)
                    (bounding-rectangle (climi::transformed-design-design thing))))

(defmethod bounding-rectangle* ((thing climi::transformed-design))
  (bounding-rectangle* (bounding-rectangle thing)))

(defmethod bounding-rectangle* ((thing my-design))
  (values 0 0 100 100))

(defmethod bounding-rectangle ((thing my-design))
  (make-rectangle* 0 0 100 100))

(defmethod inspect-object-using-state ((object design)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (unless (typep object 'bounding-rectangle)
    (with-room-for-graphics (stream)
      (draw-design-sample stream object)))

  (call-next-method))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-color)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-room-for-graphics (stream)
    (draw-design-sample stream object))

  (call-next-method))

;;; Commands

(define-command (com-adjust-color :command-table inspector-command-table)
    ((object inspected-color
             :gesture (:select
                       :tester   ((object)
                                  (let ((place (place object)))
                                    (and (supportsp place 'setf)
                                         (valuep place))))
                       :priority -1)))
  (let ((stream (inspector-state)))
    (with-output-recording-options (stream :record nil :draw t)
      (multiple-value-bind (x y) (pointer-position (port-pointer (port stream)))
        (progn                         ; with-translation (stream x y)
          (let ((width      200)
                (height     200)
                (color)

                (intensity  1) ; TODO initialize correctly
                (hue        1)
                (saturation 1)

                (red)
                (green)
                (blue))
            (block nil
              (tracking-pointer (stream)
                (:pointer-motion (x y)
                  (cond ((<= x width)
                         (setf hue        (clamp (/ x width)  0 1)
                               saturation (clamp (/ y height) 0 1)))
                        ((<= (+ width 8) x (+ width 8 32))
                         (setf intensity  (clamp (/ y height) 0 1)))
                        ((<= (+ width 8 32 8) x)
                         (let* ((angle (atan (- y 100) (- x (+ width 8 32 8 100))))
                                (hue*  (/ (if (minusp angle)
                                              (+ (* 2 pi) angle)
                                              angle)
                                          (* 2 pi))))
                           (setf hue (clamp hue* 0 1)))))
                  (setf color (make-ihs-color intensity hue saturation)
                        (values red green blue) (color-rgb color))

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
                                                 :saturation   saturation)
                                  #+no (make-instance 'radial-gradient-pattern
                                                 :radius 100
                                                 :color1 +gray60+
                                                 :color2 +gray84+)))
                      (draw-rectangle* stream x y (+ x width) (+ y height) :ink design)
                      (let* ((outer-radius (outer-radius design))
                             (radius       (/ (+ (inner-radius design) outer-radius) 2))
                             (radiant      (* 2 pi hue))
                             (cx           (+ x outer-radius    (* radius (cos radiant))))
                             (cy           (+ y outer-radius (* radius (sin radiant)))))
                        (draw-circle* stream cx cy 4
                                      :filled nil :ink +flipping-ink+ :line-thickness 2))))

                  (draw-rectangle* stream 0 (+ height 8) (+ width 8 32) (+ height 8 32)
                                   :filled t :ink color)

                  (let ((x (+ width 8 32 8))
                        (y (+ 200 8)))
                    (medium-clear-area (sheet-medium stream) x y (+ x 120) (+ y 200))
                    (setf (stream-cursor-position stream) (values x y))
                    (formatting-table (stream)
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
                        (formatting-cell (stream) (format stream "~,2F" intensity)))))

                  (force-output stream))
                (:pointer-button-press ()
                  (setf (value (place object)) color)
                  (return))
                #+later (:key-press ()
                                    (return))))))))))
