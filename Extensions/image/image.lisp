(in-package :mcclim-image)

#+ (or) ;; pixmap caching is implemented ehre
(defun draw-image-design (medium design x y)
  (let* ((da (sheet-xmirror (medium-sheet medium)))
	 (image (slot-value design 'mcclim-image::image))
	 (width (mcclim-image:image-width image))
	 (height (mcclim-image:image-height image)))
    (destructuring-bind (&optional pixmap mask)
	(slot-value design 'mcclim-image::medium-data)
      (unless pixmap
	(setf pixmap (compute-rgb-image-pixmap da image))
	(when (mcclim-image:image-alpha-p image)
	  (setf mask (compute-rgb-image-mask da image)))
	(setf (slot-value design 'mcclim-image::medium-data) (list pixmap mask)))
      (multiple-value-bind (x y)
	  (transform-position
	   (sheet-device-transformation (medium-sheet medium))
	   x y)
	(setf x (round x))
	(setf y (round y))
	(let ((gcontext (xlib:create-gcontext :drawable da)))
	  (cond
	    (mask
             (xlib:with-gcontext (gcontext
                                  :clip-mask mask
                                  :clip-x x
                                  :clip-y y)
               (xlib:copy-area pixmap gcontext 0 0 width height
                               da x y)))
	    (t
             (xlib:copy-area pixmap gcontext 0 0 width height
                             da x y))))))))

#+ (or) ;; free was only used here in fact
(defmethod medium-draw-image-design* :before (current-medium design x y)
  (with-slots (medium medium-data) design
    (unless (eq medium current-medium)
      (when medium
	(medium-free-image-design medium design))
      (setf medium current-medium)
      (setf medium-data nil))))

#+ (or) ;; freeing up the cache
(defun free-image-design (design)
  (destructuring-bind (&optional pixmap mask)
      (slot-value design 'mcclim-image::medium-data)
    (when pixmap
      (xlib:free-pixmap pixmap)
      (when mask
	(xlib:free-pixmap mask))
      (setf (slot-value design 'mcclim-image::medium-data) nil))))

#+transformed-version
(progn
  (defun transform-pointset (transformation &rest points)
    (loop
       for (x y) on points by #'cddr
       append (multiple-value-list (transform-position transformation x y))))

  (defmethod mcclim-image::medium-draw-image-design*
      ((medium clx-medium) (design mcclim-image:rgb-image-design) transformation)
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
        (let ((transformed-points (transform-pointset merged-transform 0 0 width 0 width height 0 height)))
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
                                      transformed-points)))))))
