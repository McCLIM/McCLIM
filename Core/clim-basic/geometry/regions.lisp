;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2002 Gilbert Baumann <gbaumann@common-lisp.net>
;;;  (c) copyright 2001 Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 Julien Boninfan
;;;  (c) copyright 2002-2004 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2002 Alexey Dejneka
;;;  (c) copyright 2004-2009 Andy Hefner <ahefner@common-lisp.net>
;;;  (c) copyright 2006-2008 Christophe Rhodes <crhodes@common-lisp.net>
;;;  (c) copyright 2014-2016 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2017 Peter <craven@gmx.net>
;;;  (c) copyright 2017-2019 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2017,2018 Cyrus Harmon <cyrus@bobobeach.com>
;;;  (c) copyright 2018,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Class and protocol implementations of the geometry module.
;;;
;;; TODO
;;;
;;; - ellipses: The intersection of two ellipses is there, but
;;;   handling the start/end angle is not implemented.
;;;
;;; - provide better (faster) implementations for REGION-EQUAL,
;;;   REGION-CONTAINS-REGION-P, and REGION-INTERSECTS-REGION-P.
;;;
;;; - Compute a union/intersection/difference of an union of polygon
;;;   vs another polygon or union of polygons directly via POLYGON-OP.
;;;
;;; - STANDARD-REGION-UNION should either become a subclass
;;;   'STANDARD-DISJUNCT-REGION-UNION' or a flag. Some set operations
;;;   could take advantage out the information, if the subregions of
;;;   an union are disjunct.
;;;
;;; - provide sensible PRINT-OBJECT methods.
;;;
;;; - while you are are at it; provide a reasonable fast vertical scan
;;;   routine.  polygons should make use of the sweep line algorithm.
;;;
;;; - MAKE-POLY{LINE,GON} should canonise its arguments; no edges of
;;;   length 0 and no co-linear vertexes. Maybe: canonise rectangles?
;;;   Also a polygon of less than three vertexes is to be considered
;;;   empty aka +nowhere+.
;;;

(in-package #:clim-internals)

;;; Design <-> Region Equivalences
;;;
;;; As Gilbert points in his notes, transparent ink is in every
;;; respect interchangable with the nowhere region, and likewise
;;; foreground ink is interchangable with the everywhere region.
;;; By defining the following mixins and adding them to the
;;; appropriate ink/region class pairs, we can reduce the number
;;; of methods necessary (in design.lisp).

(defclass everywhere-mixin () ())
(defclass nowhere-mixin    () ())

(defclass nowhere-region    (nowhere-mixin region)    ())
(defclass everywhere-region (everywhere-mixin region) ())

(defconstant +everywhere+ (make-instance 'everywhere-region))
(defconstant +nowhere+    (make-instance 'nowhere-region))

;;; Unbounded regions have very general (and very mundane!)
;;; methods. They are all defined here.
;;;
;;; Other region protocol methods should specialize at least on
;;; bounded-rectangle to prevent superseding methods for unbounded
;;; regions. For instance the following would supersede
;;; implementation for an unbounded region:
;;;
;;;    (defmethod region-difference ((a rectangle) (b region))
;;;      (make-instance 'standard-region-difference a b))
;;;
(macrolet
    ((def-method (name e-vs-e e-vs-n e-vs-r
                       n-vs-e n-vs-n n-vs-r
                       r-vs-e r-vs-n)
       (let ((bodies (list e-vs-e e-vs-n e-vs-r
                           n-vs-e n-vs-n n-vs-r
                           r-vs-e r-vs-n)))
         (collect (methods)
           (dolist (a '(everywhere-region nowhere-region region))
             (dolist (b '(everywhere-region nowhere-region region))
               (methods
                `(defmethod ,name ((a ,a) (b ,b))
                   (declare (ignorable a b))
                   ,(pop bodies)))))
           `(progn ,@(butlast (methods)))))))
  (def-method region-intersects-region-p t nil t   nil nil nil t   nil)
  (def-method region-contains-region-p   t t   t   nil nil nil nil t)
  (def-method region-equal               t nil nil nil t   nil nil nil)
  (def-method region-union               a a   a   b   b   b   b   a)
  (def-method region-intersection        b b   b   a   a   a   a   b)
  ;; We don't support unbounded regions which are not +everywhere+ or
  ;; +nowhere+ (that would complicate the geometry module). If we
  ;; decide otherwise don't use standard-region-difference because it
  ;; is a subclass of a bounded-rectangle so it can't represent
  ;; unbounded region.  -- jd 2019-09-10
  (def-method region-difference
    +nowhere+ a (error "Unsupported unbounded region operation.")
    a a a
    +nowhere+ a))

(defmethod region-contains-position-p ((region everywhere-region) x y)
  (declare (ignore region x y))
  t)

(defmethod region-contains-position-p ((region nowhere-region) x y)
  (declare (ignore region x y))
  nil)

(defmethod map-over-polygon-coordinates (fn (region nowhere-region))
  (declare (ignore fn region))
  nil)

(defmethod map-over-polygon-segments (fn (region nowhere-region))
  (declare (ignore fn region))
  nil)

(defmethod polygon-points ((region nowhere-region))
  (declare (ignore region))
  nil)

;;; FIXME is this right? nowhere-region is unbound. -- jd 2019-09-30
(defmethod bounding-rectangle* ((x nowhere-region))
  (values 0 0 0 0))

(defmethod transform-region (tr (region everywhere-region))
  (declare (ignore tr))
  +everywhere+)

(defmethod transform-region (tr (region nowhere-region))
  (declare (ignore tr))
  +nowhere+)

;;; This class is mixed to avoid repetetive computing of bounding boxes in some
;;; classes.
(defclass cached-bbox-mixin ()
  ((bbox :initform nil :accessor bbox)))

(defmethod bounding-rectangle* ((region cached-bbox-mixin))
  (if-let ((bbox (bbox region)))
    (bounding-rectangle* bbox)
    (multiple-value-bind (x1 y1 x2 y2) (call-next-method)
      (setf (bbox region) (make-bounding-rectangle x1 y1 x2 y2))
      (values x1 y1 x2 y2))))

;;; Region sets

(defmethod region-set-regions ((region region) &key normalize)
  (declare (ignorable normalize))
  (list region))

(defmethod map-over-region-set-regions
    (fun (region region-set) &key normalize)
  (mapc fun (region-set-regions region :normalize normalize)))

(defmethod map-over-region-set-regions (fun (region region) &key normalize)
  (declare (ignorable normalize))
  (funcall fun region))



(defclass standard-region-union (cached-bbox-mixin region-set)
  ((regions :initarg :regions :reader standard-region-set-regions)))

(defmethod region-set-regions ((region standard-region-union) &key normalize)
  (declare (ignorable normalize))
  (standard-region-set-regions region))

(defmethod map-over-region-set-regions
    (fun (region standard-region-union) &key normalize)
  (declare (ignorable normalize))
  (mapc fun (standard-region-set-regions region)))

(defmethod region-contains-position-p ((region standard-region-union) x y)
  (some (lambda (r) (region-contains-position-p r x y))
        (standard-region-set-regions region)))

(defmethod bounding-rectangle* ((region standard-region-union))
  (let (bx1 by1 bx2 by2)
    (map-over-region-set-regions
     (lambda (r)
       (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* r)
         (setf bx1 (min (or bx1 x1) x1)
               bx2 (max (or bx2 x2) x2)
               by1 (min (or by1 y1) y1)
               by2 (max (or by2 y2) y2))))
     region)
    (values bx1 by1 bx2 by2)))

(defmethod transform-region (tr (region standard-region-union))
  (with-slots (regions) region
    (make-instance 'standard-region-union
                   :regions (mapcar (lambda (r) (transform-region tr r)) regions))))



(defclass standard-region-intersection (cached-bbox-mixin region-set)
  ((regions :initarg :regions :reader standard-region-set-regions)))

(defmethod region-set-regions ((region standard-region-intersection) &key normalize)
  (declare (ignorable normalize))
  (standard-region-set-regions region))

(defmethod map-over-region-set-regions
    (fun (region standard-region-intersection) &key normalize)
  (declare (ignorable normalize))
  (mapc fun (standard-region-set-regions region)))

(defmethod region-contains-position-p ((region standard-region-intersection) x y)
  (every (lambda (r) (region-contains-position-p r x y))
         (standard-region-set-regions region)))

(defmethod bounding-rectangle* ((region standard-region-intersection))
  (let (bx1 by1 bx2 by2)
    (map-over-region-set-regions
     (lambda (r)
       (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* r)
         (setf bx1 (max (or bx1 x1) x1)
               bx2 (min (or bx2 x2) x2)
               by1 (max (or by1 y1) y1)
               by2 (min (or by2 y2) y2))))
     region)
    (values bx1 by1 bx2 by2)))

(defmethod transform-region (tr (region standard-region-intersection))
  (with-slots (regions) region
    (make-instance 'standard-region-intersection
                   :regions (mapcar (lambda (r) (transform-region tr r)) regions))))



(defclass standard-region-difference (cached-bbox-mixin region-set)
  ((a :initarg :a :reader standard-region-difference-a)
   (b :initarg :b :reader standard-region-difference-b)))

(defmethod region-set-regions ((region standard-region-difference) &key normalize)
  (declare (ignorable normalize))
  (list (standard-region-difference-a region)
        (standard-region-difference-b region)))

(defmethod map-over-region-set-regions
    (fun (region standard-region-difference) &key normalize)
  (declare (ignorable normalize))
  (funcall fun (standard-region-difference-a region))
  (funcall fun (standard-region-difference-b region)))

(defmethod region-contains-position-p ((region standard-region-difference) x y)
  (let ((region-a (standard-region-difference-a region))
        (region-b (standard-region-difference-b region)))
    (and (region-contains-position-p region-a x y)
         (not (region-contains-position-p region-b x y)))))

(defmethod bounding-rectangle* ((region standard-region-difference))
  ;; We construct a STANDARD-REGION-DIFFERENCE when we can't represent the
  ;; result using unitary regions (i.e for a difference between ellipses).
  ;; While we could sometimes compute a more precise bounding rectangle (i.e
  ;; by converting regions to bezigons first), it is probably not worth the
  ;; effort. Returning the bournding rectangle of the first region guarantees
  ;; that the result region "fits", although a bbox may be noticeably bigger.
  (bounding-rectangle* (slot-value region 'a)))

(defmethod transform-region (tr (region standard-region-difference))
  (with-slots (a b) region
    (make-instance 'standard-region-difference :a (transform-region tr a)
                                               :b (transform-region tr b))))



(defclass standard-rectangle-set (cached-bbox-mixin region-set)
  ((bands
    ;; Represents the set of rectangles. This is list like:
    ;;
    ;;  ((<y_1> . <x_band_1>)
    ;;   (<y_2> . <x_band_2>)
    ;;   :
    ;;   (<y_n>))
    ;;
    ;; <x_band_i> := (x_i_1 u_i_1  x_i_2 u_i_2 ... x_i_m u_i_m)
    ;;
    ;; Now a point (x,y) is member of the rectangle set, if there is an
    ;; i, such that y member of [y_i, y_(i+1)] and x member of x_band_i.
    ;;
    ;; An x is member of an band i, if there is an j, such that x
    ;; member [x_i_j, u_i_j].
    ;;
    ;; That is <x_band_i> describes the possible x-coordinates in the
    ;; y-range [y_i, y_(i+1)].
    ;;
    :initarg :bands
    :reader  standard-rectangle-set-bands)))

(defun make-standard-rectangle-set (bands)
  (cond ((null bands) +nowhere+)
        ((and (= (length bands) 2)
              (null (cdr (second bands)))
              (= (length (cdr (first bands))) 2))
         (make-rectangle* (first (cdar bands)) (caar bands)
                          (second (cdar bands)) (caadr bands)))
        ((= (length (first bands)) 1)
         (make-standard-rectangle-set (rest bands)))
        (t
         (make-instance 'standard-rectangle-set :bands bands))))

(defmethod region-set-regions ((region standard-rectangle-set) &key normalize)
  (let ((res nil))
    (map-over-region-set-regions
     (lambda (r) (push r res))
     region :normalize normalize)
    res))

(defmethod map-over-region-set-regions
    (fun (region standard-rectangle-set) &key normalize)
  (with-slots (bands) region
    (cond ((or (null normalize) (eql normalize :x-banding))
           (map-over-bands-rectangles
            (lambda (x1 y1 x2 y2)
              (funcall fun (make-rectangle* x1 y1 x2 y2)))
            bands))
          ((eql normalize :y-banding)
           (map-over-bands-rectangles
            (lambda (y1 x1 y2 x2)
              (funcall fun (make-rectangle* x1 y1 x2 y2)))
            (xy-bands->yx-bands bands)))
          (t
           (error "Bad ~S argument to ~S: ~S"
                  :normalize 'map-over-region-set-regions normalize)))))

(defmethod region-contains-position-p ((region standard-rectangle-set) x y)
  (block nil
    (map-over-bands (lambda (y1 y2 isum)
                      (when (<= y1 y y2)
                        (when (isum-member x isum)
                          (return t)))
                      (when (< y y2)
                        (return nil)))
                    (standard-rectangle-set-bands region))
    nil))

(defmethod bounding-rectangle* ((region standard-rectangle-set))
  (with-slots (bands) region
    (let (bx1 by1 bx2 by2)
      (map-over-bands-rectangles (lambda (x1 y1 x2 y2)
                                   (setf bx1 (min (or bx1 x1) x1)
                                         bx2 (max (or bx2 x2) x2)
                                         by1 (min (or by1 y1) y1)
                                         by2 (max (or by2 y2) y2)))
                                 bands)
      (values bx1 by1 bx2 by2))))

(defmethod transform-region (tr (region standard-rectangle-set))
  (cond ((scaling-transformation-p tr)
         (multiple-value-bind (mxx mxy myx myy tx ty)
             (get-transformation tr)
           (declare (ignore mxy myx))
           (let ((rev-x-p (< mxx 0))
                 (rev-y-p (< myy 0)))
             (flet ((correct (bands)
                      (loop for ((y . nil) (nil . xs)) on (nreverse bands)
                            collect `(,y . ,xs))))
               (make-standard-rectangle-set
                (loop for band in (standard-rectangle-set-bands region)
                      for new-band = (loop for x in (cdr band)
                                           collect (+ (* mxx x) tx) into new-xs
                                           finally (return (cons (+ (* myy (car band)) ty)
                                                                 (if rev-x-p
                                                                     (nreverse new-xs)
                                                                     new-xs))))
                      collect new-band into new-bands
                      finally (return (if rev-y-p
                                          (correct new-bands)
                                          new-bands))))))))
        (t
         ;; We have insufficient knowledge about the transformation,
         ;; so we have to take the union of all transformed rectangles.
         ;; Maybe there is a faster way to do this.
         (let ((res +nowhere+))
           (map-over-region-set-regions
            (lambda (rect)
              (setf res (region-union res (transform-region tr rect))))
            region)
           res))))
