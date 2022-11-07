;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause.
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A testing application for nested clipping and presentations. This
;;; application features two types of clips:
;;;
;;; 1) A single clip for the whole pane that does not move
;;; 2) Multiple small clips for each cell in the table that inherently move 
;;;
;;; In each cell we present four cicles that are clipped so only one quarter
;;; of each is visible. A dummy command with a "select" translator on the
;;; circle is defined to test that only the clipped part of the record is
;;; sensitive to the pointer.
;;;
;;; For clarity below each circle we draw grey circle to show the full extent
;;; of the figure.

(defpackage #:clim-demo.nested-clipping
  (:use #:clim-lisp #:clim)
  (:export #:run #:nested-clipping))
(in-package #:clim-demo.nested-clipping)

(define-presentation-type circle ())
(define-presentation-method presentation-refined-position-test
    ((type circle) record x y)
  (with-bounding-rectangle* (:center-x cx :center-y cy) record
    (region-contains-position-p (make-ellipse* cx cy 45 0 0 45) x y)))

(defparameter *outer-clip* (make-polygon* '(400 0 800 400 400 800 0 400)))
(defparameter *inner-clip* (make-rectangle* 50 50 150 150))

(defun present-circles ()
  (flet ((present-circle (x y)
           (draw-circle* t x y 45 :ink +grey+)
           (with-drawing-options (t :clipping-region *inner-clip*)
             (with-output-as-presentation (t t 'circle)
               (draw-circle* t x y 45 :ink +red+)))))
    (present-circle 50 50)
    (present-circle 150 50)
    (present-circle 150 150)
    (present-circle 50 150)))

(defun display (frame stream)
  (declare (ignore frame stream))
  (draw-design *standard-output* *outer-clip* :ink +light-grey+)
  (with-drawing-options (t :clipping-region *outer-clip*)
    (formatting-table (t :x-spacing 10 :y-spacing 10)
      (formatting-row ()
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles)))
      (formatting-row ()
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles)))
      (formatting-row ()
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles)))
      (formatting-row ()
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))
        (formatting-cell () (present-circles))))))

(define-application-frame nested-clipping ()
  ()
  (:pane :application :display-function 'display
   :scroll-bars nil :borders nil
   :width 800 :height 800))

(define-nested-clipping-command com-circle
    ((obj 'circle :gesture :select))
  (declare (ignore obj)))

