;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2001 by 
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)

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

(in-package :clim-demo)

(defparameter *postscript-test-file* #p"ps-test.ps")

(defun make-random-color ()
  (make-rgb-color (/ (random 255) 255)
                  (/ (random 255) 255)
                  (/ (random 255) 255)))

(defun draw-rosette (stream x y radius n &rest drawing-options)
  (loop with alpha = (/ (* 2 pi) n)
        and radius = (/ radius 2)
        for i below n
        do (apply #'draw-circle* stream
                  (+ (* radius (cos (* alpha i))) x)
                  (+ (* radius (sin (* alpha i))) y)
                  radius
                  :filled nil
                  drawing-options)))

(defun postscript-test ()
  (format t ";; Creating ~S.~%" *postscript-test-file*)
  (with-open-file (file-stream *postscript-test-file* :direction :output)
    (with-output-to-postscript-stream
        (stream file-stream
                :header-comments '(:title "PostScript Medium Test Output"))

        (loop repeat 200
           do (draw-line* stream (random 600) (random 900)
                          (random 600) (random 900)
                          :ink (make-random-color)))
        (new-page stream)

        (loop repeat 1000
           do (draw-point* stream (random 600) (random 900)
                           :ink (make-random-color)
                           :line-thickness (random 50)))
        (new-page stream)

        (formatting-table (stream :x-spacing 50
                                  :y-spacing 20)
          (formatting-row (stream)
            (formatting-cell (stream))
            (formatting-cell (stream :align-x :center
                                     :align-y :bottom
                                     :min-height 100)
              (draw-text* stream "(Test Page)" 170 30
                          :text-style (make-text-style :fix :bold :huge))))
          (loop for i from 1 to 15
             do (formatting-row (stream)
                  (formatting-cell (stream :align-x :right
                                           :align-y :center
                                           :min-width 100)
                    (draw-point* stream 0 0 :line-thickness i))
                  (formatting-cell (stream :align-x :center
                                           :align-y :center)
                    (draw-line* stream 0 0 200 0
                                :line-thickness i
                                :line-dashes (list (* i 2) (round i 2))))
                  (formatting-cell (stream :align-x :right
                                           :align-y :center)
                    (draw-text* stream (format nil "~D" i) 0 0
                                :text-style (make-text-style
                                             :sans-serif :bold :huge))))))
        (new-page stream)

        (with-translation (stream 540 75)
          (with-scaling (stream 3)
            (with-rotation (stream (/ pi 2))
              (clim:draw-rectangle* stream 10 10 200 150 :filled nil
                                    :line-thickness 2)
              (clim:draw-line* stream 200 10 10 150)
              (clim:draw-point* stream 180 25)
              (clim:draw-circle* stream 100 75 40 :filled nil)
              (clim:draw-ellipse* stream 160 110 30 0 0 10 :filled nil)
              (clim:draw-ellipse* stream 160 110 10 0 0 30)
              (clim:draw-polygon* stream '(20 20 50 80 40 20) :filled nil)
              (clim:draw-polygon* stream '(30 90 40 110 20 110)))))
        (new-page stream)

        (draw-rosette stream 300 300 200 18
                      :ink +steel-blue+ :line-thickness 2)

        (new-page stream)

        (with-text-style (stream '(:serif nil :huge))
          (draw-text* stream "Text alignment test" 170 20
                      :text-family :sans-serif
                      :text-face :bold)
          (with-scaling (stream 50)
            (loop for align-y in '(:bottom :center :top)
               and y from 1
               do (loop for align-x in '(:right :center :left)
                     and x from 1
                     do (draw-text* stream (format nil "~A~A"
                                                   (elt (symbol-name align-x) 0)
                                                   (elt (symbol-name align-y) 0))
                                    x y
                                    :align-x align-x
                                    :align-y align-y)
                       (draw-point* stream x y
                                    :ink +red+
                                    :line-thickness 3
                                    :line-unit :point))))
          (draw-text* stream "Top: pQ" 50 200
                      :align-y :top)
          (draw-text* stream "Bottom: pQ" 170 200
                      :align-y :bottom)
          (draw-text* stream "Center: pQ" 290 200
                      :align-y :center)
          (draw-text* stream "Baseline: pQ" 410 200
                      :align-y :baseline)
          (draw-line* stream 50 200 535 200
                      :ink +red+))

        (new-page stream)

        (formatting-table (stream)
          (flet ((draw (angle line-joint-shape)
                   (let ((record
                          (with-output-to-output-record (stream)
                            (draw-polygon* stream (list 20 0 100 0 50 (* 50 (tan angle)))
                                           :closed nil
                                           :filled nil
                                           :line-thickness 40
                                           :line-joint-shape line-joint-shape
                                           :line-cap-shape :round)
                            (draw-polygon* stream (list 20 0 100 0 50 (* 50 (tan angle)))
                                           :closed nil
                                           :filled nil
                                           :line-thickness 0.01
                                           :ink +green+))))
                     (multiple-value-call #'draw-rectangle*
                       stream (bounding-rectangle* record)
                       :filled nil
                       :ink +red+ :line-thickness 0.01)
                     (stream-add-output-record stream record)
                     (replay record stream))))
            (loop with dag = 2
               with da = (* pi (/ dag 180))
               for i from -10 to 10
               for a = (* i da)
               unless (= i 0)
               do (formatting-row (stream)
                    (formatting-cell (stream) (print (* i dag) stream))
                    (formatting-cell (stream) (draw a :miter))
                    (formatting-cell (stream) (draw a :bevel))
                    (formatting-cell (stream) (draw a :round)))))))))
