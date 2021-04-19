;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)
;;; 
;;; ---------------------------------------------------------------------------

(in-package #:clim-postscript)

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

(defun paper-region (paper-size-name orientation)
  (if (eq :eps paper-size-name) +everywhere+
      (multiple-value-bind (width height) (paper-size paper-size-name)
        (when (eq orientation :landscape)
          (rotatef width height))
        (make-rectangle* 0 0 width height))))

(defun make-postscript-transformation (page output scale-to-fit)
  (with-bounding-rectangle* (nil nil right bottom) page
    (let ((drawing-region (make-rectangle* 0 0 right bottom)))
      (cond
        (scale-to-fit
         (let ((scale (min (/ (bounding-rectangle-width drawing-region)
                              (bounding-rectangle-width output))
                           (/ (bounding-rectangle-height drawing-region)
                              (bounding-rectangle-height output)))))
           (make-scaling-transformation* scale scale)))
        (t +identity-transformation+)))))
