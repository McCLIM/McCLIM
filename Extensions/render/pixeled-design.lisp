(in-package :mcclim-render-internals)

(deftype pixeled-design-fn () '(function (fixnum fixnum) (values octet octet octet octet)))

(defparameter *pixeled-foreground-design* +black+)
(defparameter *pixeled-background-design* +white+)

;;;
;;; Pixeled Design
;;;
(defclass pixeled-design ()
  ((region :initarg :region :initform +everywhere+ :type region
           :accessor pixeled-design-region)))

(defgeneric pixeled-rgba-fn (design))
(defgeneric pixeled-rgba-unsafe-fn (design))

(defmethod pixeled-rgba-fn :around (design)
  (with-slots (region)
      design
    (if (region-equal region +everywhere+)
        (pixeled-rgba-unsafe-fn design)
        (call-next-method))))

;;;
;;; Uniform Design
;;;
(defclass pixeled-uniform-design (pixeled-design)
  ((red :initarg :red :type octet :initform 0
        :accessor pixeled-uniform-design-red)
   (green :initarg :green :type octet :initform 0
          :accessor pixeled-uniform-design-green)
   (blue :initarg :blue :type octet :initform 0
         :accessor pixeled-uniform-design-blue)
   (alpha :initarg :alpha :type octet :initform 0
          :accessor pixeled-uniform-design-alpha)))

(defun make-pixeled-uniform-design (&key (red 0) (green 0) (blue 0) (alpha 255))
  (make-instance 'pixeled-uniform-design :red red :green green :blue blue :alpha alpha))

(defmethod pixeled-rgba-fn ((design pixeled-uniform-design))
  (with-slots (red green blue alpha region)
      design
    (lambda (x y)
      (if (clim:region-contains-position-p region x y)
          (values red green blue alpha)
          (values 0 0 0 0)))))

(defmethod pixeled-rgba-unsafe-fn ((design pixeled-uniform-design))
  (with-slots (red green blue alpha region)
      design
    (lambda (x y)
      (declare (ignore x y))
      (values red green blue alpha))))

;;;
;;; Functiona Design
;;;
(defclass pixeled-functional-design (pixeled-design)
  ((color-fn :initarg :color-fn :type pixeled-design-fn)))

(defun make-pixeled-functional-design (&key color-fn (region +everywhere+))
  (make-instance 'pixeled-functional-design :color-fn color-fn :region region))

(defmethod pixeled-rgba-fn ((design pixeled-functional-design))
  (with-slots (color-fn region)
      design
    (declare (type pixeled-design-fn color-fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (clim:region-contains-position-p region x y)
          (funcall color-fn x y)
          (values 0 0 0 0)))))

(defmethod pixeled-rgba-unsafe-fn ((design pixeled-functional-design))
  (with-slots (color-fn region)
      design
    (declare (type pixeled-design-fn color-fn))
    color-fn))

;;;
;;; Flippend Design
;;;
(defclass pixeled-flipping-design (pixeled-functional-design)
  ())

(defun make-pixeled-flipping-design (&key color-fn (region +everywhere+))
  (make-instance 'pixeled-flipping-design :color-fn color-fn :region region))

;;;
;;; Image Design
;;;
(defclass pixeled-image-design (pixeled-design)
  ((image :initarg :image :initform nil
          :accessor pixeled-image-design-image)
   (dx :initarg :dx :initform 0 :type fixnum
       :accessor pixeled-image-design-dx)
   (dy :initarg :dy :initform 0 :type fixnum
       :accessor pixeled-image-design-dy)))

(defun make-pixeled-image-design (&key (image nil))
  (make-instance 'pixeled-image-design
                 :image image
                 :region (make-rectangle* 0 0 (1- (image-width image)) (1- (image-height image)))))

(defmethod  pixeled-rgba-fn ((design pixeled-image-design))
  (with-slots (image dx dy region)
      design
    (image-rgba-get-fn image :dx dx :dy dy :region region)))

(defmethod  pixeled-rgba-unsafe-fn ((design pixeled-image-design))
  (with-slots (image dx dy region)
      design
    (image-rgba-get-fn image :dx dx :dy dy :region nil)))

;;;
;;; Make a pixeled design
;;;
(defgeneric %make-pixeled-design (design))

(defgeneric make-pixeled-design (design &key foreground background)
  (:method (design &key foreground background)
    (let ((*pixeled-foreground-design* (or foreground *pixeled-foreground-design*))
          (*pixeled-background-design* (or background *pixeled-background-design*)))
      (%make-pixeled-design design))))

(defmethod %make-pixeled-design (ink)
  (error "unknow how to make an rgba design of the ~A" ink))

(defmethod %make-pixeled-design ((ink named-color))
  (multiple-value-bind (red green blue) (color-rgb ink)
    (make-pixeled-uniform-design
     :red (color-value->octet red)
     :green (color-value->octet green)
     :blue (color-value->octet blue)
     :alpha 255)))

(defmethod %make-pixeled-design ((ink (eql +foreground-ink+)))
  (%make-pixeled-design *pixeled-foreground-design*))

(defmethod %make-pixeled-design ((ink (eql +background-ink+)))
  (%make-pixeled-design *pixeled-background-design*))

(defun make-flipping-fn (design1 design2)
  (let ((d1 (pixeled-rgba-fn (%make-pixeled-design design1)))
	(d2 (pixeled-rgba-fn (%make-pixeled-design design2))))
    (declare (type pixeled-design-fn d1 d2))
    (make-pixeled-flipping-design
     :color-fn (lambda (x y)
		 (multiple-value-bind (r.d1 g.d1 b.d1 a.d1)
		     (funcall d1 x y)
		   (declare (ignore a.d1))
		   (multiple-value-bind (r.d2 g.d2 b.d2 a.d2)
		       (funcall d2 x y)
		     (declare (ignore a.d2))
		     (values
		      (logxor r.d1 r.d2)
		      (logxor g.d1 g.d2)
		      (logxor b.d1 b.d2)
		      255)))))))

(defmethod %make-pixeled-design ((ink (eql +flipping-ink+)))
  (make-flipping-fn *pixeled-background-design* *pixeled-foreground-design*))

(defmethod %make-pixeled-design ((ink standard-flipping-ink))
  (with-slots (climi::design1 climi::design2) ink
    (make-flipping-fn climi::design1 climi::design2)))

(defmethod %make-pixeled-design ((ink %transparent-ink))
  (make-pixeled-uniform-design
   :red 0
   :green 0
   :blue 0
   :alpha 0))

(defmethod %make-pixeled-design ((ink standard-opacity))
  (make-pixeled-uniform-design
   :red 255
   :green 255
   :blue 255
   :alpha (color-value->octet (opacity-value ink))))

(defmethod %make-pixeled-design ((ink indexed-pattern))
  (let* ((width (clim:pattern-width ink))
         (height (clim:pattern-height ink))
         (designs (map 'vector #'(lambda (ink)
                                   (let ((pdesign (%make-pixeled-design ink)))
                                     (if (region-contains-region-p
                                          (pixeled-design-region pdesign)
                                          (make-rectangle* 0 0 (1- width) (1- height)))
                                         (pixeled-rgba-unsafe-fn pdesign)
                                         (pixeled-rgba-fn pdesign))))
                       (climi::pattern-designs ink))))
    (declare (type (simple-array pixeled-design-fn (*)) designs))
    (make-pixeled-functional-design
     :color-fn (lambda (x y)
		 (funcall (elt designs (aref (climi::pattern-array ink) y x)) x y))
     :region (make-rectangle* 0 0
                              (1- (clim:pattern-width ink))
                              (1- (clim:pattern-height ink))))))

(defmethod %make-pixeled-design ((ink rectangular-tile))
  (let* ((design (%make-pixeled-design (rectangular-tile-design ink)))
         (width (pattern-width ink))
         (height (pattern-height ink))
         (design-fn (if (region-contains-region-p
                         (pixeled-design-region design)
                         (make-rectangle* 0 0 (1- width) (1- height)))
                        (pixeled-rgba-unsafe-fn design)
                        (pixeled-rgba-fn design))))
    (make-pixeled-functional-design
     :color-fn (lambda (x y)
                 (declare (type fixnum x y width height)
                          (type pixeled-design-fn design-fn))
                 (funcall design-fn (mod x width) (mod y height))))))

(defgeneric %transform-design (design transformation)
  (:method (design transformation)
    (let ((design-fn (pixeled-rgba-fn design)))
      (declare (type pixeled-design-fn design-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
                   (declare (type fixnum x y))
                   (with-transformed-position (transformation x y)
                     (funcall design-fn (round x) (round y)))))))
  (:method ((design pixeled-image-design) (transformation climi::standard-translation))
    (with-slots (dx dy region)
        design
      (multiple-value-bind (x0 y0)
          (transform-position transformation dx dy)
        (with-bounding-rectangle* (x1 y1 x2 y2)
            (transform-region (invert-transformation transformation) region)
          (setf dx (round x0))
          (setf dy (round y0))
          (setf region (make-rectangle* (round x1) (round y1) (round x2) (round y2))))))
    design)
  (:method ((design pixeled-uniform-design) transformation)
    design))

(defmethod %make-pixeled-design ((ink transformed-design))
  (let ((design (%make-pixeled-design (transformed-design-design ink)))
        (transformation (invert-transformation (transformed-design-transformation ink))))
    (%transform-design design transformation)))

(defgeneric compose-in-rgba-design (ink mask)
  (:method ((ink pixeled-design) (mask pixeled-design))
    (let ((mask-fn (pixeled-rgba-fn mask))
	  (ink-fn (pixeled-rgba-fn ink)))
      (declare (type pixeled-design-fn ink-fn mask-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
		   (multiple-value-bind (r1 g1 b1 a1)
		       (funcall ink-fn x y)
		     (multiple-value-bind (r2 g2 b2 a2)
			 (funcall mask-fn x y)
		       (declare (ignore r2 g2 b2))
		       (values r1 g1 b1 (octet-mult a1 a2)))))
       :region (region-intersection (pixeled-design-region ink)
                                    (pixeled-design-region mask)))))
  (:method ((ink pixeled-uniform-design) (mask pixeled-uniform-design))
    (make-pixeled-uniform-design
     :red (pixeled-uniform-design-red ink)
     :green (pixeled-uniform-design-green ink)
     :blue (pixeled-uniform-design-blue ink)
     :alpha (octet-mult (pixeled-uniform-design-alpha ink)
                        (pixeled-uniform-design-alpha mask)))))

(defgeneric compose-out-rgba-design (ink mask)
  (:method ((ink pixeled-design) (mask pixeled-design))
    (let ((mask-fn (pixeled-rgba-fn mask))
	  (ink-fn (pixeled-rgba-fn ink)))
      (declare (type pixeled-design-fn ink-fn mask-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
		   (multiple-value-bind (r1 g1 b1 a1)
		       (funcall ink-fn x y)
		     (multiple-value-bind (r2 g2 b2 a2)
			 (funcall mask-fn x y)
		       (declare (ignore r2 g2 b2))
		       (values r1 g1 b1 (octet-mult a1 (- 255 a2))))))
       :region (pixeled-design-region ink))))
  (:method ((ink pixeled-uniform-design) (mask pixeled-uniform-design))
    (make-pixeled-uniform-design
     :red (pixeled-uniform-design-red ink)
     :green (pixeled-uniform-design-green ink)
     :blue (pixeled-uniform-design-blue ink)
     :alpha (octet-mult (pixeled-uniform-design-alpha ink)
                        (- 255 (pixeled-uniform-design-alpha mask))))))

(defgeneric compose-over-rgba-design (fore back)
  (:method ((fore pixeled-design) (back pixeled-design))
    (let ((fore-fn (pixeled-rgba-fn fore))
	  (back-fn (pixeled-rgba-fn back)))
      (declare (type pixeled-design-fn fore-fn back-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
                   (multiple-value-bind (r1 g1 b1 a1)
		       (funcall fore-fn x y)
		     (multiple-value-bind (r2 g2 b2 a2)
			 (funcall back-fn x y)
		         (multiple-value-bind (red green blue alpha)
			     (octet-blend-function
			      r2 g2 b2 a2 r1 g1 b1 a1)
			   (values red green blue alpha))))))))
  (:method ((fore pixeled-uniform-design) (back pixeled-uniform-design))
    (multiple-value-bind (red green blue alpha)
        (octet-blend-function
         (pixeled-uniform-design-red fore)
         (pixeled-uniform-design-green fore)
         (pixeled-uniform-design-blue fore)
         (pixeled-uniform-design-alpha fore)
         (pixeled-uniform-design-red back)
         (pixeled-uniform-design-green back)
         (pixeled-uniform-design-blue back)
         (pixeled-uniform-design-alpha back))
      (make-pixeled-uniform-design
       :red red
       :green green
       :blue blue
       :alpha alpha))))

(defmethod %make-pixeled-design ((ink in-compositum))
  (let ((c-ink (make-pixeled-design (compositum-ink ink)))
	(c-mask (make-pixeled-design (compositum-mask ink))))
    (compose-in-rgba-design c-ink c-mask)))

(defmethod %make-pixeled-design ((ink out-compositum))
  (let ((c-ink (make-pixeled-design (compositum-ink ink)))
	(c-mask (make-pixeled-design (compositum-mask ink))))
    (compose-out-rgba-design c-ink c-mask)))

(defmethod %make-pixeled-design ((ink over-compositum))
  (let ((c-fore (make-pixeled-design (compositum-foreground ink)))
	(c-back (make-pixeled-design (compositum-background ink))))
    (compose-over-rgba-design c-fore c-back)))

(defmethod %make-pixeled-design ((ink image-design))
  (let* ((img (slot-value ink 'image)))
    (make-pixeled-image-design :image img)))

;;;
;;; design fix
;;;

(defmethod clim:transform-region (transformation (design named-color))
  design)

(defmethod clim:transform-region (transformation (design standard-flipping-ink))
  design)
