(in-package :clim-clx)

;;; RGB-IMAGE support, from Closure

(defun transform-pointset (transformation &rest points)
  (loop
    for (x y) on points by #'cddr
    append (multiple-value-list (transform-position transformation x y))))

(defmethod mcclim-image::medium-draw-image-design*
    ((medium clx-medium) (design mcclim-image:rgb-image-design) x y transformation)
  (let* ((da (sheet-xmirror (medium-sheet medium)))
	 (image (slot-value design 'mcclim-image::image))
	 (width (mcclim-image:image-width image))
	 (height (mcclim-image:image-height image))
         (native-transform (sheet-native-transformation (medium-sheet medium)))
         (merged-transform (clim:compose-transformations native-transform transformation)))
    (destructuring-bind (&optional pixmap mask)
	(slot-value design 'mcclim-image::medium-data)
      (unless pixmap
	(setf pixmap (compute-rgb-image-pixmap da image))
	(when (mcclim-image:image-alpha-p image)
	  (setf mask (compute-rgb-image-mask da image)))
	(setf (slot-value design 'mcclim-image::medium-data) (list pixmap mask)))
      (let ((transformed-points (transform-pointset merged-transform
                                                    x y
                                                    (+ x width) y
                                                    (+ x width) (+ y height)
                                                    x (+ y height))))
        (let ((dest (clim-clx::create-dest-picture da))
              (src (clim-clx::create-dest-picture pixmap)))
	  (cond
	    (mask
	     (setf (xlib:picture-clip-mask dest) mask)
             (setf (xlib:picture-clip-x-origin dest) (truncate (+ (first transformed-points) 0.5)))
             (setf (xlib:picture-clip-y-origin dest) (truncate (+ (second transformed-points) 0.5))))
	    (t
             (setf (xlib:picture-clip-mask dest) :none)))
          (multiple-value-bind (rxx rxy ryx ryy)
              (climi::get-transformation transformation)
            (apply #'xlib:render-set-picture-transform src
                   (mapcar (lambda (v)
                             (logand (truncate (* v #x10000)) #xFFFFFFFF))
                           (let ((det (- (* rxx ryy) (* rxy ryx))))
                             (list (/ ryy det) (- (/ rxy det)) 0
                                   (- (/ ryx det)) (/ rxx det) 0
                                   0 0 1)))))
          (xlib:render-triangle-fan dest :over src 0 0
                                    (find-alpha-mask-format (xlib:drawable-display da))
                                    transformed-points))))))

(defmethod mcclim-image:medium-free-image-design
    ((medium clx-medium) (design mcclim-image:rgb-image-design))
  (destructuring-bind (&optional pixmap mask)
      (slot-value design 'mcclim-image::medium-data)
    (when pixmap
      (xlib:free-pixmap pixmap)
      (when mask
	(xlib:free-pixmap mask))
      (setf (slot-value design 'mcclim-image::medium-data) nil))))

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
	   (gc     (xlib:create-gcontext :drawable pixmap)))
      (unless (or (>= width 2048) (>= height 2048)) ;### CLX bug
	(xlib:put-image pixmap gc im
			:src-x 0 :src-y 0
			:x 0 :y 0
			:width width :height height))
      (xlib:free-gcontext gc)
      pixmap)))

(defun compute-rgb-image-mask (drawable image)
  (let* ((width (mcclim-image:image-width image))
         (height (mcclim-image:image-height image))
         (bitmap (xlib:create-pixmap :drawable drawable
                                     :width width
                                     :height height
                                     :depth 1))
         (gc (xlib:create-gcontext :drawable bitmap
				   :foreground 1
				   :background 0))
         (idata (mcclim-image::image-data image))
         (xdata (make-array (list height width)
			    :element-type '(unsigned-byte 1)))
         (im (xlib:create-image :width width
                                :height height
                                :depth 1
                                :data xdata)) )
    (dotimes (y width)
      (dotimes (x height)
        (if (> (aref idata x y) #x80000000)
            (setf (aref xdata x y) 0)
	    (setf (aref xdata x y) 1))))
    (unless (or (>= width 2048) (>= height 2048)) ;### CLX breaks here
      (xlib:put-image bitmap gc im :src-x 0 :src-y 0
		      :x 0 :y 0 :width width :height height
		      :bitmap-p nil))
    (xlib:free-gcontext gc)
    bitmap))

(defun image-to-ximage-for-drawable (drawable image)
  (image-to-ximage image
		   (xlib:drawable-depth drawable) 
		   (pixel-translator (xlib:window-colormap drawable))))

(defun image-to-ximage (image depth translator)
  (let* ((width (mcclim-image:image-width image))
         (height (mcclim-image:image-height image))
         (idata (mcclim-image::image-data image))
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
    (declare (type (simple-array (unsigned-byte 32) (* *)) idata))
    (loop for x fixnum from 0 below width do
	  (loop for y fixnum from 0 below height do
		(setf (aref xdata y x)
		      (funcall translator
			       x y
			       (ldb (byte 24 0) (aref idata y x))))))
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
                         `(lambda (x y sample)
                            (declare (ignore x y))
                            (dpb (the (unsigned-byte 8) (ldb (byte 8 0) sample))
                                 ',rbyte
                                 (dpb (the (unsigned-byte 8) (ldb (byte 8 8) sample))
                                      ',gbyte
                                      (dpb (the (unsigned-byte 8) (ldb (byte 8 16) sample))
                                           ',bbyte
                                           0))))))))))
