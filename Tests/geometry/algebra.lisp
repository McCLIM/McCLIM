(cl:in-package #:clim-tests)

;;; IMPLEMENTME Dimensionality rule
;;; IMPLEMENTME The principle of duality

(def-suite* :mcclim.algebra
  :in :mcclim)

(defvar +basic-types+
  '(standard-point standard-line standard-rectangle))

(defvar +polygon-types+
  (append +basic-types+ '(standard-polyline standard-polygon)))

(defvar +ellipse-types+
  (append +basic-types+ '(standard-elliptical-arc standard-ellipse)))

(defvar +bezigon-types+
  (append +basic-types+ '(standard-polybezier standard-bezigon)))

(defvar +composable-types+
  (append +basic-types+ +polygon-types+))

(defvar +standard-types+
  (append +basic-types+ +polygon-types+ +ellipse-types+))

(defvar +extended-types+
  (append +standard-types+ +bezigon-types+))

(defun generate-ratio (&key bound)
  (let ((gen (gen-float :bound bound)))
    (lambda () (+ 150 (round (rationalize (funcall gen)))))))

(defun generate-point (&key (bound1 100.0) (bound2 100.0))
  (let ((gen1 (generate-ratio :bound bound1))
        (gen2 (generate-ratio :bound bound2)))
    (lambda ()
      (make-point (funcall gen1) (funcall gen2)))))

(defun generate-point-sequence (length &key unique)
  (assert (plusp length))
  (let ((gen (generate-point)))
    (lambda ()
      (let ((results '())
            (length length))
        (loop for p = (funcall gen)
              unless (and unique
                          (member p results :test #'region-equal))
                do (push p results)
                   (decf length)
              until (zerop length)
              finally (return results))))))

(defun generate-line ()
  (let ((gen (generate-point-sequence 2 :unique t)))
    (lambda ()
      (apply #'make-line (funcall gen)))))

(defun generate-polyline (length &key closed)
  (assert (> length 2))
  (let ((gen (generate-point-sequence length :unique t)))
    (lambda ()
      (make-polyline (funcall gen) :closed closed))))

(defun generate-polybezier (length)
  (assert (= (mod length 3) 1))
  (let ((gen (generate-point-sequence length :unique t)))
    (lambda ()
      (make-polybezier (funcall gen)))))

;;; FIXME this should be more random.
(defun generate-elliptical-arc ()
  (let ((gen-center (generate-point)))
    (lambda ()
      (make-elliptical-arc (funcall gen-center) 4.4 0 0 13.12))))

(defun generate-rectangle ()
  (let ((gen (generate-point-sequence 2 :unique t)))
    (lambda ()
      (loop for points = (funcall gen)
            for rect = (apply #'make-rectangle points)
            until (rectanglep rect)
            finally (return rect)))))

(defun generate-polygon (length)
  (assert (> length 2))
  (let ((gen (generate-point-sequence length)))
    (lambda ()
      (loop for points = (funcall gen)
            for poly = (make-polygon points)
            until (polygonp poly)
            finally (return poly)))))

(defun generate-bezigon (length)
  (let ((gen (generate-point-sequence length :unique t)))
    (lambda ()
      (make-bezigon (funcall gen)))))

;;; FIXME this should be more random.
(defun generate-ellipse ()
  (let ((gen-center (generate-point)))
    (lambda ()
      (make-ellipse (funcall gen-center) 4.4 0 0 13.12))))

;;; FIXME add region sets and region complements to the mix.
;;; FIXME dimensionality rule breaks some fundamental laws.
;;; FIXME adding elliptical things to the mix leads to failures.
(defun generate-region (&optional (types '(;;standard-point
                                           ;;standard-polyline standard-line
                                           standard-polygon standard-rectangle)))
  (let ((gen-type (apply #'gen-one-element types))
        (gen-len (gen-integer :min 3 :max 7))
        (gen-bezier-len (gen-one-element 4 7 11 14 17)))
    (lambda ()
      (funcall
       (ecase (funcall gen-type)
         (standard-point (generate-point))
         (standard-line (generate-line))
         (standard-polyline (generate-polyline (funcall gen-len)))
         (standard-elliptical-arc (generate-elliptical-arc))
         (standard-polybezier (generate-polybezier (funcall gen-bezier-len)))
         (standard-rectangle (generate-rectangle))
         (standard-polygon (generate-polygon (funcall gen-len)))
         (standard-ellipse (generate-ellipse))
         (standard-bezigon (generate-bezigon (funcall gen-len))))))))


;;; Basic properties

(test algebra.fundamental.identity
  (for-all ((r (generate-region)))
    (is (region-equal r r))
    (is (region-equal r (region-union r +nowhere+)))
    (is (region-equal r (region-intersection r +everywhere+)))))

(test algebra.fundamental.complement
  (for-all ((r (generate-region)))
    (is (region-equal +everywhere+ (region-union r (region-complement r))))
    (is (region-equal +nowhere+    (region-intersection r (region-complement r))))))

(test algebra.fundamental.commutativity
  (for-all* ((r1 (generate-region))
             (r2 (generate-region)))
    (is (region-equal (region-union r1 r2)
                      (region-union r2 r1)))
    (is (region-equal (region-intersection r1 r2)
                      (region-intersection r2 r1)))))

(test algebra.fundamental.associativity
  (for-all* ((r1 (generate-region))
             (r2 (generate-region))
             (r3 (generate-region)))
    (is (region-equal (region-union (region-union r1 r2) r3)
                      (region-union r1 (region-union r2 r3))))
    (is (region-equal (region-intersection (region-intersection r1 r2) r3)
                      (region-intersection r1 (region-intersection r2 r3))))))

(test algebra.fundamental.distributivity
  (for-all* ((r1 (generate-region))
             (r2 (generate-region))
             (r3 (generate-region)))
    (is (region-equal (region-union r1 (region-intersection r2 r3))
                      (region-intersection (region-union r1 r2)
                                           (region-union r1 r3))))
    (is (region-equal (region-intersection r1 (region-union r2 r3))
                      (region-union (region-intersection r1 r2)
                                    (region-intersection r1 r3))))))

(test algebra.extra/union-intersection.idempotent
  (for-all ((r (generate-region)))
    (is (region-equal r (region-union r r)))
    (is (region-equal r (region-intersection r r)))))

(test algebra.extra/union-intersection.domination
  (for-all ((r (generate-region)))
    (is (region-equal +everywhere+ (region-union r +everywhere+)))
    (is (region-equal +nowhere+ (region-intersection r +nowhere+)))))

(test algebra.extra/union-intersection.absorption
  (for-all* ((r1 (generate-region))
             (r2 (generate-region)))
    (is (region-equal r1 (region-union r1 (region-intersection r1 r2))))
    (is (region-equal r1 (region-intersection r1 (region-union r1 r2))))))

(test algebra.extra/complement.de-morgan
  (for-all* ((r1 (generate-region))
             (r2 (generate-region)))
    (is (region-equal (region-complement (region-union r1 r2))
                      (region-intersection (region-complement r1)
                                           (region-complement r2))))
    (is (region-equal (region-complement (region-intersection r1 r2))
                      (region-union (region-complement r1)
                                    (region-complement r2))))))


(test algebra.extra/complement.involution
  (for-all ((r (generate-region)))
    (is (region-equal r (region-complement (region-complement r))))))

(test algebra.extra/complement.universal-and-empty
  (is (region-equal +nowhere+ (clime:region-complement +everywhere+)))
  (is (region-equal +everywhere+ (clime:region-complement +nowhere+))))

;;; FIXME the chance of randomly generating r1 and r2 is low. Use a separate
;;; generator.
(test algebra.extra/uniqueness-of-complements
  (pass)
  #+ (or)
  (for-all* ((r1 (generate-region))
             (r2 (generate-region)))
    (when (and (region-equal +everywhere+ (region-union r1 r2))
               (region-equal +nowhere+ (region-intersection r1 r2)))
      (is (region-equal r2 (region-complement r1))))))

(test algebra.inclusion.reflexivity
  (for-all ((r (generate-region)))
    (is (region-contains-region-p r r))))

;;; FIXME the chance of randomly generating r1 and r2 is low. Use a separate
;;; generator.
(test algebra.inclusion.assymetry
  (pass)
  #+ (or)
  (for-all* ((r1 (generate-region))
             (r2 (generate-region)))
    (when (and (region-contains-region-p r2 r1)
               (region-contains-region-p r1 r2))
      (is (region-equal r1 r2)))))

;;; FIXME the chance of randomly generating r1 and r2 is low. Use a separate
;;; generator.
(test algebra.inclusion.transitivity
  (pass)
  #+ (or)
  (for-all* ((r1 (generate-region))
             (r2 (generate-region))
             (r3 (generate-region)))
    (when (and (region-contains-region-p r2 r1)
               (region-contains-region-p r3 r2))
      (is (region-contains-region-p r1 r3)))))

(test algebra.inclusion.existance-least-greatest
  (for-all ((r (generate-region)))
    (is (and (region-contains-region-p r +nowhere+)
             (region-contains-region-p +everywhere+ r)))))

(test algebra.inclusion.existance-joins
  (for-all* ((r1 (generate-region))
             (r2 (generate-region))
             (r3 (generate-region)))
    (is (region-contains-region-p (region-union r1 r2) r1 ))
    (when (and (and (region-contains-region-p r3 r1)
                    (region-contains-region-p r3 r2)))
      (is (region-contains-region-p r3 (region-union r1 r2))))))

(test algebra.inclusion.existance-meets
  (for-all* ((r1 (generate-region))
             (r2 (generate-region))
             (r3 (generate-region)))
    (is (region-contains-region-p r1 (region-intersection r1 r2)))
    (when (and (region-contains-region-p r1 r3)
               (region-contains-region-p r2 r3))
      (is (region-contains-region-p (region-union r1 r2) r3)))))
