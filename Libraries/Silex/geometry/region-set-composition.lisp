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

(in-package #:silex)

;;; REGION-UNION

;;; STANDARD-REGION-UNION
(defmethod region-union ((a standard-region-union) (b standard-region-union))
  (reduce #'region-union (region-set-regions b) :initial-value a))

(define-commutative-method region-union
    ((a standard-region-union) (b bounding-rectangle))
  (let* ((mergedp nil)
         (regions (loop
                    for r in (region-set-regions a)
                    collect (let ((region (region-union r b)))
                              (if (typep region 'standard-region-union)
                                  (setf region r)
                                  (setf mergedp t))
                              region))))
    (unless mergedp
      (push b regions))
    (make-instance 'standard-region-union :regions regions)))

;;; STANDARD-RECTANGLE-SET
(define-commutative-method region-union ((a standard-rectangle-set) (b path)) a)
(define-commutative-method region-union ((a standard-rectangle-set) (b point)) a)

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-union (standard-rectangle-set-bands xs)
                (standard-rectangle-set-bands ys))))

(define-commutative-method region-union
    ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-union xs (rectangle->standard-rectangle-set ys)))

(define-commutative-method region-union ((a standard-rectangle-set) (b polygon))
  (region-union (rectangle-set->polygon-union a) b))

;;; REGION-INTERSECTION

;;; STANDARD-REGION-UNION
(define-commutative-method region-intersection
    ((a standard-region-union) (b bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r)
       (setf res (region-union res (region-intersection r b))))
     a)
    res))

;;; STANDARD-RECTANGLE-SET
(defmethod region-intersection
    ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-intersection (standard-rectangle-set-bands xs)
                       (standard-rectangle-set-bands ys))))

(define-commutative-method region-intersection
    ((xs standard-rectangle-set) (ys standard-rectangle))
  (make-standard-rectangle-set
   (bands-intersection (standard-rectangle-set-bands xs)
                       (standard-rectangle-set-bands
                        (rectangle->standard-rectangle-set ys)))))

(define-commutative-method region-intersection
    ((a standard-rectangle-set) (b bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r)
       (setf res (region-union res (region-intersection r b))))
     a)
    res))

;;; standard-region-intersection
(defmethod region-intersection
    ((a standard-region-intersection) (b standard-region-intersection))
  (reduce #'region-intersection (region-set-regions b) :initial-value a))

(define-commutative-method region-intersection
    ((a standard-region-intersection) (b bounding-rectangle))
  (when (region-equal b +nowhere+)
    (return-from region-intersection +nowhere+))
  (let* ((mergedp nil)
         (regions (loop
                    for r in (standard-region-set-regions a)
                    when (region-equal b r)
                      do (return-from region-intersection b)
                    collect (let ((region (region-intersection r a)))
                              (when (region-equal region +nowhere+)
                                (return-from region-intersection +nowhere+))
                              (if (typep region 'standard-region-intersection)
                                  (setf region r)
                                  (setf mergedp t))
                              region))))
    (unless mergedp
      (push b regions))
    (make-instance 'standard-region-intersection :regions regions)))

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

(defmethod region-difference ((x standard-region-intersection) (y bounding-rectangle))
  (let ((cy (make-instance 'standard-region-complement :complement y)))
    (make-instance 'standard-region-intersection :regions (list* cy (region-set-regions x)))))
