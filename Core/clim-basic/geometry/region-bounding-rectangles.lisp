;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2003 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 1998-2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2005 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2016 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2017-2019 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Methods for computing bounding rectangles or various region classes.
;;;

(in-package #:climi)

;;; FIXME is this right? nowhere-region is unbound. -- jd 2019-09-30
(defmethod bounding-rectangle* ((x nowhere-region))
  (values 0 0 0 0))

;;; Lazy evaluation of a bounding rectangle.
(defmethod slot-unbound (class (region cached-polygon-bbox-mixin) (slot-name (eql 'bbox)))
  (setf (slot-value region 'bbox)
        (loop for point in (polygon-points region)
              for (x y) = (multiple-value-list (point-position point))
              minimizing x into x1 maximizing x into x2
              minimizing y into y1 maximizing y into y2
              finally (return (make-instance 'standard-bounding-rectangle
                                              :x1 x1 :y1 y1 :x2 x2 :y2 y2)))))

(defmethod bounding-rectangle* ((region cached-polygon-bbox-mixin))
  (with-standard-rectangle* (x1 y1 x2 y2) (bounding-rectangle region)
    (values x1 y1 x2 y2)))

(defun ellipse-bounding-rectangle (el)
  ;; Return bounding rectangle of ellipse centered at (0, 0) with
  ;; radii h and v rotated by the angle phi.
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((sin (sin phi))
           (cos (cos phi))
           (ax (+ (expt (* v sin) 2)
                  (expt (* h cos) 2)))
           (ay (+ (expt (* v cos) 2)
                  (expt (* h sin) 2)))
           (numerator-x (- (* ax h h v v)))
           (numerator-y (- (* ay h h v v)))
           (denominator-common (expt (* cos
                                        sin
                                        (- (* v v) (* h h)))
                                     2))
           (x (sqrt (/ numerator-x
                       (- denominator-common
                          (* ax (+ (expt (* v cos) 2)
                                   (expt (* h sin) 2)))))))
           (y (sqrt (/ numerator-y
                       (- denominator-common
                          (* ay (+ (expt (* v sin) 2)
                                   (expt (* h cos) 2))))))))
      (values (- cx x) (- cy y) (+ cx x) (+ cy y)))))

(defmethod bounding-rectangle* ((region elliptical-thing))
  (with-slots (tr start-angle end-angle) region
    (multiple-value-bind (cx cy) (ellipse-center-point* region)
      (if (every #'zerop (multiple-value-list (ellipse-radii region)))
          (values cx cy cx cy)
          (ellipse-bounding-rectangle region)))))


(defmethod bounding-rectangle* ((a standard-line))
  (with-slots (x1 y1 x2 y2) a
    (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))))

(defmethod bounding-rectangle* ((region standard-rectangle))
  (with-standard-rectangle* (x1 y1 x2 y2) region
    (values x1 y1 x2 y2)))

;;; STANDARD-RECTANGLE-SET: has a slot BOUNDING-RECTANGLE for caching
(defmethod bounding-rectangle* ((region standard-rectangle-set))
  (with-slots (bands bounding-rectangle) region
    (values-list (or bounding-rectangle
                     (setf bounding-rectangle
                       (let (bx1 by1 bx2 by2)
                         (map-over-bands-rectangles (lambda (x1 y1 x2 y2)
                                                      (setf bx1 (min (or bx1 x1) x1)
                                                            bx2 (max (or bx2 x2) x2)
                                                            by1 (min (or by1 y1) y1)
                                                            by2 (max (or by2 y2) y2)))
                                                    bands)
                         (list bx1 by1 bx2 by2)))))))

(defmethod bounding-rectangle* ((region standard-point))
  (with-slots (x y) region
    (values x y x y)))

(defmethod bounding-rectangle* ((region standard-region-union))
  (let (bx1 by1 bx2 by2)
    (map-over-region-set-regions (lambda (r)
                                   (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* r)
                                     (setf bx1 (min (or bx1 x1) x1)
                                           bx2 (max (or bx2 x2) x2)
                                           by1 (min (or by1 y1) y1)
                                           by2 (max (or by2 y2) y2))))
                                 region)
    (values bx1 by1 bx2 by2)))

(defmethod bounding-rectangle* ((region standard-region-difference))
  (with-slots (a b) region
    (cond ((eq a +everywhere+)
           (bounding-rectangle* b))
          (t
           (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* a)
             (multiple-value-bind (u1 v1 u2 v2) (bounding-rectangle* b)
               (values (min x1 u1) (min y1 v1)
                       (max x2 u2) (min y2 v2))))))))

(defmethod bounding-rectangle* ((region standard-region-intersection))
  ;; kill+yank alert
  (let (bx1 by1 bx2 by2)
    (map-over-region-set-regions (lambda (r)
                                   (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* r)
                                     (setf bx1 (min (or bx1 x1) x1)
                                           bx2 (max (or bx2 x2) x2)
                                           by1 (min (or by1 y1) y1)
                                           by2 (max (or by2 y2) y2))))
                                 region)
    (values bx1 by1 bx2 by2)))
