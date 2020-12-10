;;; ---------------------------------------------------------------------------
;;;     Title: Themes
;;;   Created: 2020-06-26 15:00
;;;    Author: Daniel Kochmański <daniel@turtleware.eu>
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Theming is a frequently requested feature. For now we'll just move
;;; things related to theming to a single file so it is easier to come
;;; up with the unified abstraction later on.
;;;

(in-package #:clim-internals)

;;; Default Color Scheme Options


;;; Motif-ish
;;;
;;; (defparameter *3d-dark-color*   (make-gray-color .45))
;;; (defparameter *3d-normal-color* (make-gray-color .75))
;;; (defparameter *3d-light-color*  (make-gray-color .92))
;;; (defparameter *3d-inner-color*  (make-gray-color .65))

;;; Gtk-ish

(defparameter *3d-dark-color*   (make-gray-color .59))
(defparameter *3d-normal-color* (make-gray-color .84))
(defparameter *3d-light-color*  (make-gray-color 1.0))
(defparameter *3d-inner-color*  (make-gray-color .75))

;;;
;;; gadget's colors
;;;

;;; GADGET-COLOR-MIXIN is currently kind of dangling, we should reuse
;;; it for effective-gadget-foreground et al.


(defclass gadget-color-mixin ()
  ((normal :type color
           :initform +gray80+
           :initarg :normal
           :accessor gadget-normal-color)
   (highlighted :type color
                :initform +gray85+
                :initarg :highlighted
                :accessor gadget-highlighted-color)
   (pushed-and-highlighted :type color
                           :initform +gray75+
                           :initarg :pushed-and-highlighted
                           :accessor gadget-pushed-and-highlighted-color)
   (current-color :type color
                  :accessor gadget-current-color))
  (:documentation "This class define the gadgets colors."))

(defmethod initialize-instance :after ((gadget gadget-color-mixin) &rest args)
  (declare (ignore args))
  (setf (slot-value gadget 'current-color) (gadget-normal-color gadget)))

(defmethod (setf gadget-current-color) :after (color (gadget gadget-color-mixin))
  (declare (ignore color))
  (dispatch-repaint gadget (sheet-region gadget)))

;;; Common colors:

(defgeneric gadget-highlight-background (gadget)
  (:method ((gadget gadget))
    (compose-over (compose-in #|+paleturquoise+|# +white+ (make-opacity .5))
                  (pane-background gadget))))

(defgeneric effective-gadget-foreground (gadget)
  (:method  ((gadget gadget))
    (if (gadget-active-p gadget)
        +foreground-ink+
        (compose-over (compose-in (pane-foreground gadget)
                                  (make-opacity .5))
                      (pane-background gadget)))))

(defgeneric effective-gadget-background (gadget)
  (:method ((gadget gadget))
    (if (and (gadget-active-p gadget)
             (gadget-armed-p gadget))
        (gadget-highlight-background gadget)
        (pane-background gadget))))

(defgeneric effective-gadget-input-area-color (gadget)
  (:method  ((gadget gadget))
    (if (gadget-active-p gadget)
        +lemonchiffon+
        (compose-over (compose-in +lemonchiffon+ (make-opacity .5))
                      (pane-background gadget)))))

;;;
;;; 3D-BORDER-MIXIN Class
;;;

;;; 3D-BORDER-MIXIN class can be used to add a 3D-ish border to
;;; panes. There are three new options:
;;;
;;;  :border-width       The width of the border
;;;  :border-style       The border's style one of :inset, :outset, :groove, :ridge, :solid,
;;;                      :double, :dotted, :dashed
;;;                      [:dotted and :dashed are not yet implemented]
;;;
;;;  :border-color       The border's color
;;;                      [Not implemented yet]
;;;
;;; [These options are modelled after CSS].
;;;
;;; When using 3D-BORDER-MIXIN, one should query the pane's inner
;;; region, where drawing should take place, by PANE-INNER-REGION.
;;;
;;; --GB

(defparameter *3d-border-thickness* 2)

(defclass 3D-border-mixin ()
  ((border-width :initarg :border-width :initform 2)
   (border-style :initarg :border-style :initform :outset)
   (border-color :initarg :border-color :initform "???")))

(defgeneric pane-inner-region (pane)
  (:method  ((pane 3D-border-mixin))
    (with-slots (border-width) pane
      (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
        (make-rectangle* (+ x1 border-width) (+ y1 border-width)
                         (- x2 border-width) (- y2 border-width))))))

(defmethod handle-repaint :after ((pane 3D-border-mixin) region)
  (declare (ignore region))
  (with-slots (border-width border-style) pane
    (draw-bordered-polygon pane (polygon-points (bounding-rectangle (sheet-region pane)))
                           :border-width border-width
                           :style border-style)))

;;; 3D-ish Look

;; DRAW-BORDERED-POLYGON medium point-seq &key border-width style
;;
;; -GB
(defgeneric compose-label-space (gadget &key wider higher))
(defgeneric draw-label* (pane x1 y1 x2 y2 &key ink))

(labels ((line-hnf (x1 y1 x2 y2)
           (values (- y2 y1) (- x1 x2) (- (* x1 y2) (* y1 x2))))

         (line-line-intersection (x1 y1 x2 y2 x3 y3 x4 y4)
           (multiple-value-bind (a1 b1 c1) (line-hnf x1 y1 x2 y2)
             (multiple-value-bind (a2 b2 c2) (line-hnf x3 y3 x4 y4)
               (let ((d (- (* a1 b2) (* b1 a2))))
                 (cond ((< (abs d) 1e-6)
                        nil)
                       (t
                        (values (/ (- (* b2 c1) (* b1 c2)) d)
                                (/ (- (* a1 c2) (* a2 c1)) d))))))))

         (polygon-orientation (point-seq)
           "Determines the polygon's orientation.
            Returns:  +1 = counter-clock-wise
                      -1 = clock-wise

            The polygon should be clean from duplicate points or co-linear points.
            If the polygon self intersects, the orientation may not be defined, this
            function does not try to detect this situation and happily returns some
            value."
           ;;
           (let ((n (length point-seq)))
             (let* ((min-i 0)
                    (min-val (point-x (elt point-seq min-i))))
               ;;
               (loop for i from 1 below n do
                 (when (< (point-x (elt point-seq i)) min-val)
                   (setf min-val (point-x (elt point-seq i))
                         min-i i)))
               ;;
               (let ((p0 (elt point-seq (mod (+ min-i -1) n)))
                     (p1 (elt point-seq (mod (+ min-i 0) n)))
                     (p2 (elt point-seq (mod (+ min-i +1) n))))
                 (signum (- (* (- (point-x p2) (point-x p0)) (- (point-y p1) (point-y p0)))
                            (* (- (point-x p1) (point-x p0)) (- (point-y p2) (point-y p0)))))))))

         (clean-polygon (point-seq)
           "Cleans a polygon from duplicate points and co-linear points. Furthermore
            tries to bring it into counter-clock-wise orientation."
           ;; first step: remove duplicates
           (setf point-seq
                 (let ((n (length point-seq)))
                   (loop for i from 0 below n
                         for p0 = (elt point-seq (mod (+ i -1) n))
                         for p1 = (elt point-seq (mod (+ i 0) n))
                         unless (and (< (abs (- (point-x p0) (point-x p1))) 10e-8)
                                     (< (abs (- (point-y p0) (point-y p1))) 10e-8))
                           collect p1)))
           ;; second step: remove colinear points
           (setf point-seq
                 (let ((n (length point-seq)))
                   (loop for i from 0 below n
                         for p0 = (elt point-seq (mod (+ i -1) n))
                         for p1 = (elt point-seq (mod (+ i 0) n))
                         for p2 = (elt point-seq (mod (+ i +1) n))
                         unless (< (abs (- (* (- (point-x p1) (point-x p0)) (- (point-y p2) (point-y p0)))
                                           (* (- (point-x p2) (point-x p0)) (- (point-y p1) (point-y p0)))))
                                   10e-8)
                           collect p1)))
           ;; third step: care for the orientation
           (if (and (not (null point-seq))
                    (minusp (polygon-orientation point-seq)))
               (reverse point-seq)
               point-seq) ))

  (defun shrink-polygon (point-seq width)
    (let ((point-seq (clean-polygon point-seq)))
      (let ((n (length point-seq)))
        (values
         point-seq
         (loop for i from 0 below n
               for p0 = (elt point-seq (mod (+ i -1) n))
               for p1 = (elt point-seq (mod (+ i  0) n))
               for p2 = (elt point-seq (mod (+ i +1) n))
               collect
               (let* ((dx1 (- (point-x p1) (point-x p0)))
                      (dy1 (- (point-y p1) (point-y p0)))
                      (dx2 (- (point-x p2) (point-x p1)))
                      (dy2 (- (point-y p2) (point-y p1)))
                      ;;
                      (m1  (/ width (sqrt (+ (* dx1 dx1) (* dy1 dy1)))))
                      (m2  (/ width (sqrt (+ (* dx2 dx2) (* dy2 dy2)))))
                      ;;
                      (q0  (make-point (+ (point-x p0) (* m1 dy1))
                                       (- (point-y p0) (* m1 dx1))))
                      (q1  (make-point (+ (point-x p1) (* m1 dy1))
                                       (- (point-y p1) (* m1 dx1))))
                      (q2  (make-point (+ (point-x p1) (* m2 dy2))
                                       (- (point-y p1) (* m2 dx2))))
                      (q3  (make-point (+ (point-x p2) (* m2 dy2))
                                       (- (point-y p2) (* m2 dx2)))) )
                 ;;
                 (multiple-value-bind (x y)
                     (multiple-value-call #'line-line-intersection
                       (point-position q0) (point-position q1)
                       (point-position q2) (point-position q3))
                   (if x
                       (make-point x y)
                       (make-point 0 0)))))))))

  (defun draw-bordered-polygon (medium point-seq
                                &key (border-width 2)
                                  (style        :inset))
    (labels ((draw-pieces (outer-points inner-points dark light)
               (let ((n (length outer-points)))
                 (dotimes (i n)
                   (let* ((p1 (elt outer-points (mod (+ i  0) n)))
                          (p2 (elt outer-points (mod (+ i +1) n)))
                          (q1 (elt inner-points (mod (+ i  0) n)))
                          (q2 (elt inner-points (mod (+ i +1) n)))
                          (p1* (transform-region +identity-transformation+  p1))
                          (p2* (transform-region +identity-transformation+  p2))
                          (a (mod (atan (- (point-y p2*) (point-y p1*))
                                        (- (point-x p2*) (point-x p1*)))
                                  (* 2 pi))))
                     (draw-polygon medium (list p1 q1 q2 p2)
                                   :ink
                                   (if (<= (* 1/4 pi) a (* 5/4 pi))
                                       dark light)))))))
      (let ((light  *3d-light-color*)
            (dark   *3d-dark-color*))
        ;;
        (ecase style
          (:solid
           (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
             +black+ +black+))
          (:inset
           (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
             dark light))
          (:outset
           (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
             light dark))
          ;;
          ;; Mickey Mouse is the trademark of the Walt Disney Company.
          ;;
          (:mickey-mouse-outset
           (multiple-value-bind (outer-points inner-points)
               (shrink-polygon point-seq border-width)
             (declare (ignore outer-points))
             (multiple-value-bind (outer-points middle-points)
                 (shrink-polygon point-seq (/ border-width 2))
               (draw-pieces outer-points middle-points +white+ +black+)
               (draw-pieces middle-points inner-points light dark))))
          (:mickey-mouse-inset
           (multiple-value-bind (outer-points inner-points)
               (shrink-polygon point-seq border-width)
             (declare (ignore outer-points))
             (multiple-value-bind (outer-points middle-points)
                 (shrink-polygon point-seq (/ border-width 2))
               (draw-pieces outer-points middle-points dark light)
               (draw-pieces middle-points inner-points +black+ +white+))))
          ;;
          (:ridge
           (multiple-value-bind (outer-points inner-points)
               (shrink-polygon point-seq border-width)
             (declare (ignore outer-points))
             (multiple-value-bind (outer-points middle-points)
                 (shrink-polygon point-seq (/ border-width 2))
               (draw-pieces outer-points middle-points light dark)
               (draw-pieces middle-points inner-points dark light))))
          (:groove
           (multiple-value-bind (outer-points inner-points)
               (shrink-polygon point-seq border-width)
             (declare (ignore outer-points))
             (multiple-value-bind (outer-points middle-points)
                 (shrink-polygon point-seq (/ border-width 2))
               (draw-pieces outer-points middle-points dark light)
               (draw-pieces middle-points inner-points light dark))))
          (:double
           (multiple-value-bind (outer-points inner-points)
               (shrink-polygon point-seq border-width)
             (declare (ignore outer-points))
             (multiple-value-bind (outer-points imiddle-points)
                 (shrink-polygon point-seq (* 2/3 border-width))
               (declare (ignore outer-points))
               (multiple-value-bind (outer-points omiddle-points)
                   (shrink-polygon point-seq (* 1/3 border-width))
                 (draw-pieces outer-points omiddle-points +black+ +black+)
                 (draw-pieces imiddle-points inner-points +black+ +black+))))))))) 

  (defun draw-bordered-rectangle* (medium x1 y1 x2 y2 &rest options)
    (apply #'draw-bordered-polygon
           medium
           (polygon-points (make-rectangle* x1 y1 x2 y2))
           options))

  (defun draw-engraved-label* (pane x1 y1 x2 y2)
    (draw-label* pane (1+ x1) (1+ y1) (1+ x2) (1+ y2) :ink *3d-light-color*)
    (draw-label* pane x1 y1 x2 y2 :ink *3d-dark-color*)))
