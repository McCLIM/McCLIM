(in-package :mcclim-render)

(use-package :clim-clx)

;;; RGB-IMAGE support, from Closure

(defmethod medium-draw-image*
    ((medium sheet-with-medium-mixin) design x y)
  (medium-draw-image* (sheet-medium medium) design x y))

(defmethod mcclim-render::medium-draw-image*
    ((medium clim-clx::clx-medium) (image mcclim-render::drawable-image) x y)
  (let* ((da (clim-clx::sheet-xmirror (medium-sheet medium)))
	 (width (mcclim-render::image-width image))
	 (height (mcclim-render::image-height image)))
    (let ((pixmap (compute-rgb-image-pixmap da image)))
      (multiple-value-bind (x y)
	  (transform-position
	   (sheet-device-transformation (medium-sheet medium))
	   x y)
	(setf x (round x))
	(setf y (round y))
	(let ((gcontext (xlib:create-gcontext :drawable da)))
          (xlib:copy-area pixmap gcontext 0 0 width height
                          da x y)))
      (xlib:free-pixmap pixmap))))

(defun compute-rgb-image-pixmap (drawable image)
  (let* ((width (mcclim-image:image-width image))
         (height (mcclim-image:image-height image))
         (depth (xlib:drawable-depth drawable))
         (im (image-to-ximage-for-drawable drawable image)))
    (setf width (max width 1))
    (setf height (max height 1))
    (let* ((pixmap (xlib:create-pixmap :drawable drawable
				       :width width
				       :height height
				       :depth depth))
	   (gc (xlib:create-gcontext :drawable pixmap)))
      (unless (or (>= width 2048) (>= height 2048)) ;### CLX bug
	(xlib:put-image pixmap gc im
			:src-x 0 :src-y 0
			:x 0 :y 0
			:width width :height height))
      (xlib:free-gcontext gc)
      pixmap)))

(defun image-to-ximage-for-drawable (drawable image)
  (image-to-ximage image
		   (xlib:drawable-depth drawable)
		   (pixel-translator (xlib:window-colormap drawable))))

(defun image-to-ximage (image depth translator)
  (let* ((width (mcclim-image:image-width image))
         (height (mcclim-image:image-height image))
	 ;; FIXME: this (and the :BITS-PER-PIXEL, below) is a hack on
	 ;; top of a hack.  At some point in the past, XFree86 and/or
	 ;; X.org decided that they would no longer support pixmaps
	 ;; with 24 bpp, which seems to be what most AIMAGEs want to
	 ;; be.  For now, force everything to a 32-bit pixmap.
         (xdata (make-array (list height width) :element-type '(unsigned-byte 32)))
         (ximage (xlib:create-image :width  width
                                    :height height
                                    :depth  depth
				    :bits-per-pixel 32
                                    :data   xdata)))
    (mcclim-render::map-rgb-color image #'(lambda (x y r g b)
                             (setf (aref xdata y x)
                                   (funcall translator
                                            x y
                                            r g b))))
    ximage))

(defun mask->byte (mask)
  (let ((h (integer-length mask)))
    (let ((l (integer-length (logxor mask (1- (ash 1 h))))))
      (byte (- h l) l))))

;; fixme!  This is not just incomplete, but also incorrect: The original
;; true color code knew how to deal with non-linear RGB value
;; allocation.

(defvar *translator-cache-lock* (clim-sys:make-lock "translator cache lock"))
(defvar *translator-cache* (make-hash-table :test #'equal))

(defun pixel-translator (colormap)
  (unless (eq (xlib:visual-info-class (xlib:colormap-visual-info colormap))
	      :true-color)
    (error "sorry, cannot draw rgb image for non-true-color drawable yet"))
  (let* ((info (xlib:colormap-visual-info colormap))
	 (rbyte (mask->byte (xlib:visual-info-red-mask info)))
	 (gbyte (mask->byte (xlib:visual-info-green-mask info)))
	 (bbyte (mask->byte (xlib:visual-info-blue-mask info)))
         (key (list rbyte gbyte bbyte)))
    (clim-sys:with-lock-held (*translator-cache-lock*)
      (or (gethash key *translator-cache*)
          ;; COMPILE instead of a closure, because out-of-line byte specifiers
          ;; are universally slow. Getting them inline like this is *much*
          ;; faster.
          (setf (gethash key *translator-cache*)
                (compile nil
                         `(lambda (x y r g b)
                            (declare (ignore x y))
                            (dpb (the (unsigned-byte 8) r)
                                 ',rbyte
                                 (dpb (the (unsigned-byte 8) g)
                                      ',gbyte
                                      (dpb (the (unsigned-byte 8) b)
                                           ',bbyte
                                           0))))))))))
