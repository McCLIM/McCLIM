
(in-package #:clim-pdf)

(defparameter *paper-sizes*
  '((:letter 612 . 792)
    (:legal 612 . 1008)
    (:a0 2380 . 3368)
    (:a1 1684 . 2380)
    (:a2 1190 . 1684)
    (:a3 842 . 1190)
    (:a4 595 . 842)
    (:a5 421 . 595)
    (:a6 297 . 421)
    (:a7 210 . 297)
    (:a8 148 . 210)
    (:a9 105 . 148)
    (:a10 74 . 105)
    (:b0 2836 . 4008)
    (:b1 2004 . 2836)
    (:b2 1418 . 2004)
    (:b3 1002 . 1418)
    (:b4 709 . 1002)
    (:b5 501 . 709)
    (:11x17 792 . 1224)))

(defun paper-size (name)
  (let ((size (cdr (assoc name *paper-sizes*))))
    (unless size
      (error "Unknown paper size: ~S." name))
    (values (car size) (cdr size))))

(defun make-pdf-transformation (paper-size-name orientation)
  (multiple-value-bind (width height) (paper-size paper-size-name)
    (case orientation
      (:portrait
       (make-3-point-transformation*
        0 0  0 height  width 0
        0 height  0 0  width height))
      (:landscape
       (make-3-point-transformation*
        0 0  0 width  height 0
        width height  0 height  width 0))
      (t (error "Unknown orientation")))))
