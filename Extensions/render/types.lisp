(cl:in-package #:mcclim-render-internals)

(deftype octet ()
  '(unsigned-byte 8))

(deftype color-value ()
  '(real 0 1))

;;; Pixels

(deftype argb-pixel ()
  '(unsigned-byte 32))

;;; Pixel arrays

(cl:defconstant +image-dimension-limit+ (ash 1 32))

(deftype image-index ()
  `(integer 0 (,+image-dimension-limit+)))

(deftype stencil-array ()
  `(simple-array (unsigned-byte 8) 2))

(deftype argb-pixel-array ()
  `(simple-array argb-pixel 2))

(declaim (inline make-argb-pixel-array))
(defun make-argb-pixel-array (width height &key (initial-element 0))
  (make-array (list height width) :element-type 'argb-pixel
                                  :initial-element initial-element))
