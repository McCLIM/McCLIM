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

(in-package :CLIM-DEMO)

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
      (with-output-recording-options (stream :record nil)

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

        (draw-text* stream "(Test Page)" 170 30
                    :text-style (make-text-style :fix :bold :huge))
        (loop for a = 70 then (incf a 50)
           for i from 1 to 15
           do (progn
                (draw-point* stream 100 a :line-thickness i)
                (draw-line* stream 150 a 350 a :line-thickness i
                            :line-dashes (list (* i 2) (round i 2)))
                (draw-text* stream (format nil "~D" i) 400 a
                            :text-style (make-text-style
                                         :sans-serif :bold :huge))))
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

        ))))
