(in-package #:clim-pdf)

(defparameter *paper-sizes*
  '((:letter . ( 612 .  792))
    (:legal  . ( 612 . 1008))
    (:a0     . (2380 . 3368))
    (:a1     . (1684 . 2380))
    (:a2     . (1190 . 1684))
    (:a3     . ( 842 . 1190))
    (:a4     . ( 595 .  842))
    (:a5     . ( 421 .  595))
    (:a6     . ( 297 .  421))
    (:a7     . ( 210 .  297))
    (:a8     . ( 148 .  210))
    (:a9     . ( 105 .  148))
    (:a10    . (  74 .  105))
    (:b0     . (2836 . 4008))
    (:b1     . (2004 . 2836))
    (:b2     . (1418 . 2004))
    (:b3     . (1002 . 1418))
    (:b4     . ( 709 . 1002))
    (:b5     . ( 501 .  709))
    (:11x17  . ( 792 . 1224))))

(defun paper-size (name)
  (alexandria:if-let ((size (cdr (assoc name *paper-sizes*))))
    (values (car size) (cdr size))
    (error "Unknown paper size: ~S." name)))

(defun paper-region (paper-size-name)
  (multiple-value-bind (width height) (paper-size paper-size-name)
    (make-rectangle* 0 0 width height)))

(defparameter *pdf-top-margin* 0)
(defparameter *pdf-left-margin* 0)
(defparameter *pdf-bottom-margin* 0)
(defparameter *pdf-right-margin* 0)

(defun make-pdf-transformation (region orientation)
  (with-bounding-rectangle* (left top right bottom) region
    (flet ((make-it (&rest image-coordinates)
             (apply #'make-3-point-transformation*
                    left top left bottom right top
                    image-coordinates)))
      (case orientation
        (:portrait
         (make-it *pdf-left-margin* (- bottom *pdf-top-margin*)
                  *pdf-left-margin* *pdf-bottom-margin*
                  (- right *pdf-right-margin*) (- bottom *pdf-top-margin*)))

        (:landscape
         (make-it *pdf-left-margin* (- right *pdf-top-margin*)
                  (- bottom *pdf-top-margin*) (- right *pdf-right-margin*)
                  *pdf-left-margin* *pdf-bottom-margin*))
        (t (error "Unknown orientation"))))))
