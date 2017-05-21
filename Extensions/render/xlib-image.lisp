(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; opticl image
;;;

(deftype xlib-image-data () '(simple-array (unsigned-byte 32) (* *)))

(defclass xlib-image (image)
  ((data :type (or null xlib-image-data))))

(defun make-xlib-image (width height)
  (let ((data (make-array (list height width)
			  :element-type '(unsigned-byte 32)
			  :initial-element #x00FFFFFF)))
    (make-instance 'xlib-image
		   :width width
		   :height height
		   :data data)))

;;;
;;; get/set pixels
;;;

(declaim (inline xlib-image-data-get-pixel)
	 (ftype (function (xlib-image-data fixnum fixnum)
			  (values octet octet octet octet))
		xlib-image-data-get-pixel))
(defun xlib-image-data-get-pixel (data x y)
  (let ((p (aref data y  x)))
    (let ((r (ldb (byte 8 0) p))
	  (g (ldb (byte 8 8) p))
	  (b (ldb (byte 8 16) p))
	  (a (ldb (byte 8 24) p)))
      (values r g b a))))

(declaim (inline xlib-image-data-set-pixel)
	 (ftype (function (xlib-image-data fixnum fixnum
					   octet octet octet octet)
			  t)
		xlib-image-data-set-pixel))
(defun xlib-image-data-set-pixel (data x y red green blue alpha)
  (setf (aref data y x)
	(dpb blue (byte 8 0)
	     (dpb green (byte 8 8)
		  (dpb red (byte 8 16)
		       (dpb alpha (byte 8 24) 0))))))

;;;
;;; copy image
;;;

(defmethod copy-image ((image xlib-image)
		       (src-image opticl-image)
		       &key (x 0) (y 0)
			 (width (climi::image-width image))
			 (height (climi::image-height image))
			 (src-dx 0)
			 (src-dy 0))
  (declare (type fixnum x y width height src-dx src-dy))
  (def-copy-image
      xlib-image-data xlib-image-data-set-pixel
    opticl-image-data opticl-image-data-get-pixel))

(defmethod copy-image ((image xlib-image)
		       (src-image rgba-image)
		       &key (x 0) (y 0)
			 (width (climi::image-width image))
			 (height (climi::image-height image))
			 (src-dx 0)
			 (src-dy 0))
  (declare (type fixnum x y width height src-dx src-dy))
  (def-copy-image
      xlib-image-data xlib-image-data-set-pixel
    rgba-image-data rgba-image-data-get-pixel))
