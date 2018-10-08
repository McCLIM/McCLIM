
(defpackage #:ellipse-tests
  (:use #:clim-lisp #:clim #:test-runner #:mcclim-ellipse))

(in-package #:ellipse-tests)

(defparameter *tests* (make-hash-table :test 'equal))
(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))

(setf test-runner:*test-output-directory* "/tmp/ellipse-tests/")

(ensure-directories-exist test-runner:*test-output-directory*)

(defun test-draw-ellipse* (sheet
		           center-x center-y
		           radius-1-dx radius-1-dy radius-2-dx radius-2-dy
		           &rest args
                           &key start-angle
                                end-angle
                                (draw-ellipse-parameters t)
                                (ellipse-parameter-color-1 +red+)
                                (ellipse-parameter-color-2 +black+)
                                &allow-other-keys)
  (declare (ignore start-angle end-angle))
  (apply #'draw-ellipse* sheet center-x  center-y
	 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
         args)
  (when draw-ellipse-parameters
    (multiple-value-bind (a b theta)
        (reparameterize-ellipse radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
      (draw-line* sheet center-x center-y
                  (+ center-x (* a (cos theta)))
                  (+ center-y (* a (sin theta)))
                  :ink ellipse-parameter-color-2)
      (draw-text* sheet "a"
                  (+ center-x (* (+ a 15) (cos theta)))
                  (+ center-y (* (+ a 15) (sin theta))))

      (draw-line* sheet center-x center-y
                  (+ center-x (* b (cos (+ theta (/ pi 2)))))
                  (+ center-y (* b (sin (+ theta (/ pi 2)))))
                  :ink ellipse-parameter-color-2)
      (draw-text* sheet "b"
                  (+ center-x (* (+ b 15) (cos (+ theta (/ pi 2)))))
                  (+ center-y (* (+ b 15) (sin (+ theta (/ pi 2))))))
      ;; radius 1
      (draw-line* sheet center-x center-y
                  (+ center-x radius-1-dx) (+ center-y radius-1-dy)
                  :ink ellipse-parameter-color-1)
      (draw-text* sheet "r1"
                  (+ center-x (+ radius-1-dx 5)) (+ center-y (+ radius-1-dy 5)))

      ;; radius 2
      (draw-line* sheet center-x center-y
                  (+ center-x radius-2-dx) (+ center-y radius-2-dy)
                  :ink ellipse-parameter-color-1)
      (draw-text* sheet "r2"
                  (+ center-x radius-2-dx) (+ center-y radius-2-dy))

      ;; draw parameters for reference
      (draw-text* sheet
                  (format nil "center-x: ~,3F, center-y: ~,3F"
                          center-x center-y)
                  10 30)
      (draw-text* sheet
                  (format nil "r1dx: ~,3F, r1dy: ~,3F, r2dx: ~,3F, r2dy: ~,3F"
                          radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
                  10 50)
      (draw-text* sheet
                  (format nil "a: ~,3F,  b: ~,3F, theta: ~,3F (rad), ~,3F (deg)"
                          a b theta (* 180 (/ theta pi)))
                  10 70))))

(define-test *tests* "01) Circle" (stream)
    ""
  (draw-circle* stream *center-x* *center-y* 150 :ink +orange+))

(define-test *tests* "02) Circle Unfilled" (stream)
    ""
  (draw-circle* stream *center-x* *center-y* 150 :ink +orange+ :filled nil))

(define-test *tests* "03) Circle Unfilled Thick" (stream)
    ""
  (draw-circle* stream *center-x* *center-y* 150 :ink +orange+ :filled nil
                :line-thickness 5))

(define-test *tests* "04) Circle Wedge" (stream)
    ""
  (draw-circle* stream *center-x* *center-y* 150 :ink +orange+
                :start-angle 0 :end-angle (/ pi 4)))

(define-test *tests* "05) Circle Arc" (stream)
    ""
  (draw-circle* stream *center-x* *center-y* 150 :ink +orange+ :filled nil
                :start-angle 0 :end-angle (/ pi 4)))

(define-test *tests* "06) Ellipse Arc 1" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 150 -90 60 25 :filled nil
                 :line-thickness 6
                 :start-angle 0 :end-angle pi))

(define-test *tests* "07) Ellipse Arc 2" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 150 -90 60 25 :filled nil
                 :line-thickness 6
                 :start-angle 0 :end-angle (* 6 (/ pi 4))))

(define-test *tests* "08) Ellipse 1" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 150 -90 60 25 :filled nil
                 :line-thickness 6))

(define-test *tests* "09) Filled Ellipse 1" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 150 -90 60 25 :filled t
                 :line-thickness 6))

(define-test *tests* "10) Misc Ellipse 1" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* -150 150 0 30 :ink +orange+))

(define-test *tests* "11) Misc Ellipse 2" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* -150 150 0 30 :ink +orange+
                 :start-angle 0 :end-angle (/ pi 2)))

(define-test *tests* "12) Misc Ellipse 3" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* -150 150 0 30 :ink +orange+
                 :start-angle 0 :end-angle pi))

(define-test *tests* "13) Off-axis Ellipse 1" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 150 100 0 60
                      :ink +dark-green+ :filled nil))

(define-test *tests* "13) Off-axis Ellipse 2" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 150 -100 0 60
                      :ink +dark-green+ :filled nil))

(define-test *tests* "14) Off-axis Ellipse 1" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 100 99 60 60
                      :ink +dark-green+ :filled nil))

(define-test *tests* "14) Off-axis Ellipse 2" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 100 -99 60 60
                      :ink +dark-green+ :filled nil))

(define-test *tests* "14) Off-axis Ellipse 3 (0-90 deg)" (stream)
    ""
  (test-draw-ellipse* stream *center-x* *center-y* 100 -99 60 60
                      :ink +dark-green+ :filled nil :start-angle (/ pi 4) :end-angle (/ pi 2)))


(defun image-viewer (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'image-viewer)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Image viewer")
        (run))))

(defun ellipse-tests (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'test-runner :tests *tests*)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Ellipse Tests")
        (run))))
