(in-package #:mcclim-render-internals)

(deftype octet ()
  '(unsigned-byte 8))

(deftype color-value ()
  '(real 0 1))

;;; Pixels

(deftype argb-pixel ()
  '(unsigned-byte 32))

;;; Pixel arrays
;;;
;;; Pixel arrays are two-dimensional array whose dimensions are at
;;; most 2^30-1 (which should be enough for all kinds of images and
;;; rendering buffers while also fitting into FIXNUMs most of the
;;; time). Elements are either 32-bit ARGB pixels for color and
;;; opacity information or octets for stencil information.

(cl:defconstant +image-dimension-limit+ (ash 1 30))

(deftype image-dimension ()
  `(integer 0 ,+image-dimension-limit+))

(deftype image-index ()
  `(integer 0 (,+image-dimension-limit+)))

(deftype image-index-displacement ()
  `(integer ,(- (floor +image-dimension-limit+ 2))
            (,(floor +image-dimension-limit+ 2))))

(deftype stencil-array ()
  `(simple-array octet 2))

(deftype argb-pixel-array ()
  `(simple-array argb-pixel 2))

(declaim (inline make-argb-pixel-array))
(defun make-argb-pixel-array (width height &key (initial-element 0))
  (make-array (list height width) :element-type 'argb-pixel
                                  :initial-element initial-element))
