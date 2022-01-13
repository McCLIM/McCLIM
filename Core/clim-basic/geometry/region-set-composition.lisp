;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 1998-2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2019-2022 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Region algebra method for the region set classes.
;;;

(in-package #:climi)

;;; utilities

(defun rectangle-set->polygon-union (rs)
  (let ((res nil))
    (map-over-region-set-regions (lambda (r) (push r res)) rs)
    (make-instance 'standard-region-union :regions res)))

;;; REGION-UNION

;;; STANDARD-REGION-UNION
(defmethod region-union ((a standard-region-union) (b standard-region-union))
  (assert (not (eq b +nowhere+)))
  (assert (not (eq a +nowhere+)))
  (make-instance 'standard-region-union
                 :regions (append (standard-region-set-regions a)
                                  (standard-region-set-regions b))))

(defmethod region-union ((a standard-region-union) (b bounding-rectangle))
  (make-instance 'standard-region-union
                 :regions (cons b (standard-region-set-regions a))))

(defmethod region-union ((b bounding-rectangle) (a standard-region-union))
  (make-instance 'standard-region-union
                 :regions (cons b (standard-region-set-regions a))))

;;; STANDARD-RECTANGLE-SET
(defmethod region-union ((a standard-rectangle-set) (b path)) a)
(defmethod region-union ((b path) (a standard-rectangle-set)) a)
(defmethod region-union ((a standard-rectangle-set) (b point)) a)
(defmethod region-union ((b point) (a standard-rectangle-set)) a)

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-union (standard-rectangle-set-bands xs)
                (standard-rectangle-set-bands ys))))

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-union xs (rectangle->standard-rectangle-set ys)))

(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-union (rectangle->standard-rectangle-set xs) ys))

(defmethod region-union ((a standard-rectangle-set) (b polygon))
  (region-union (rectangle-set->polygon-union a) b))

(defmethod region-union ((a polygon) (b standard-rectangle-set))
  (region-union a (rectangle-set->polygon-union b)))

;;; STANDARD-REGION-INTERSECTION
;;; STANDARD-REGION-DIFFERENCE
(defmethod region-union ((a standard-region-difference) (b bounding-rectangle))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union ((a bounding-rectangle) (b standard-region-difference))
  (make-instance 'standard-region-union :regions (list a b)))

;;; REGION-INTERSECTION

;;; STANDARD-REGION-UNION
(defmethod region-intersection ((a standard-region-union) (b bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r) (setf res (region-union res (region-intersection r b)))) a)
    res))

(defmethod region-intersection ((a bounding-rectangle) (b standard-region-union))
  (region-intersection b a))

;;; STANDARD-RECTANGLE-SET
(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-intersection (standard-rectangle-set-bands xs)
                       (standard-rectangle-set-bands ys))))

(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-intersection xs (rectangle->standard-rectangle-set ys)))

(defmethod region-intersection ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-intersection (rectangle->standard-rectangle-set xs) ys))

(defmethod region-intersection ((a standard-rectangle-set) (b bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r)
       (setf res (region-union res (region-intersection r b))))
     a)
    res))

(defmethod region-intersection ((a bounding-rectangle) (b standard-rectangle-set))
  (region-intersection b a))

;;; standard-region-intersection
(defmethod region-intersection ((a standard-region-intersection) (b standard-region-intersection))
  (make-instance 'standard-region-intersection
                 :regions (remove-duplicates
                           (append (standard-region-set-regions a)
                                   (standard-region-set-regions b))
                           :test #'region-equal)))

(defmethod region-intersection ((a bounding-rectangle) (b standard-region-intersection))
  (collect (results)
    (loop for region in (standard-region-set-regions b)
          when (or (region-equal a +nowhere+)
                   (region-equal a region))
            do (return-from region-intersection a)
          do (let ((intersection (region-intersection a region)))
               (typecase intersection
                 (nowhere-region
                  (return-from region-intersection +nowhere+))
                 (standard-region-intersection
                  (results region))
                 (otherwise
                  (results intersection)))))
    (make-instance 'standard-region-intersection :regions (results))))

(defmethod region-intersection ((a standard-region-intersection) (b bounding-rectangle))
  (region-intersection b a))

;;; STANDARD-REGION-DIFFERENCE
(defmethod region-intersection
    ((bbox bounding-rectangle) (rdif standard-region-difference))
  (make-instance 'standard-region-intersection :regions (list bbox rdif)))

(defmethod region-intersection
    ((rdif standard-region-difference) (bbox bounding-rectangle))
  (region-intersection bbox (region-complement rdif)))

;;; REGION-DIFFERENCE

;;; STANDARD-REGION-UNION
(defmethod region-difference ((x bounding-rectangle) (y standard-region-union))
  ;; A \ (B1 u B2) = (A\B1 ^ A\B2)
  (let ((res +everywhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-intersection res (region-difference x a))))
     y)
    res))

(defmethod region-difference ((x standard-region-union) (y bounding-rectangle))
  ;; (A u B) \ C = A\C u B\C
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

;;; STANDARD-RECTANGLE-SET
(defmethod region-difference ((x bounding-rectangle) (y standard-rectangle-set))
  (let ((res +everywhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-intersection res (region-difference x a))))
     y)
    res))

(defmethod region-difference ((x standard-rectangle-set) (y bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-difference (standard-rectangle-set-bands xs)
                     (standard-rectangle-set-bands ys))))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-difference xs (rectangle->standard-rectangle-set ys)))

(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-difference (rectangle->standard-rectangle-set xs) ys))

;;; STANDARD-REGION-INTERSECTION
(defmethod region-difference ((x bounding-rectangle) (y standard-region-intersection))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (b)
       (setf res (region-union res (region-difference x b))))
     y)
    res))

;;; STANDARD-REGION-DIFFERENCE
(defmethod region-difference ((x bounding-rectangle) (y standard-region-difference))
  (region-intersection x (region-complement y)))

(defmethod region-difference ((x standard-region-difference) (y bounding-rectangle))
  ;; (A\B)\C = A \ (B u C), but A = everywhere (because we've made it so).
  (make-instance 'standard-region-difference
                 :complement (region-union (region-complement x) y)))
