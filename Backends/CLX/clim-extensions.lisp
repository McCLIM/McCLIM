;;   (c) copyright 2002 by
;;;           Joachim Pouderoux (pixel@pixeledena.com)
;;;  (c) copyright 2001 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(eval-when (:compile-toplevel :load-toplevel)
  ; I don't think that this should be necessarily here,
  ; but while it is, it needs to ensure that the use-package'ing
  ; happens before the rest of the file is read.

(in-package :clim-clx)
  (use-package :IMAGE)
(use-package :clim-extensions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; image drawing

(defun draw-image (sheet image
                         &rest args
                         &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (with-medium-options (sheet args)
    (medium-draw-image* medium image)))

(clim-internals::def-graphic-op draw-image (image))

(defun compute-pixel-value-truecolor-image-24 (pixel colormap)
  (declare (ignore colormap)
           (type (unsigned-byte 24) pixel))
  pixel)

(defun compute-pixel-value-truecolor-image (pixel colormap)
  (declare (type xlib::colormap colormap)
	   (type (unsigned-byte 24) pixel))
  (let* ((visual-info (xlib::colormap-visual-info colormap))
	 (red-mask (xlib:visual-info-red-mask visual-info))
	 (red-mask-gap (- (integer-length red-mask) 8))
	 (green-mask (xlib:visual-info-green-mask visual-info))
	 (green-mask-gap (- (integer-length green-mask) 8))
	 (blue-mask (xlib:visual-info-blue-mask visual-info))
	 (blue-mask-gap (- (integer-length blue-mask) 8)))
    (declare (type fixnum red-mask red-mask-gap
		   green-mask green-mask-gap
		   blue-mask blue-mask-gap))
    (+ (logand red-mask
	       (ash (red-component pixel) red-mask-gap))
       (logand green-mask
	       (ash (green-component pixel) green-mask-gap))
       (logand blue-mask
	       (ash (blue-component pixel) blue-mask-gap)))))

(defun compute-pixel-value-256-gray-level-image-24 (pixel colormap)
  (declare (ignore colormap)
	   (type (unsigned-byte 8) pixel))
  (+ (ash pixel 16) (ash pixel 8) pixel))

(defun compute-pixel-value-256-gray-level-image (pixel colormap)
  (declare (type xlib::colormap colormap)
	   (type (unsigned-byte 8) pixel))
  (let* ((visual-info (xlib::colormap-visual-info colormap))
	 (red-mask (xlib:visual-info-red-mask visual-info))
	 (red-mask-gap (- (integer-length red-mask) 8))
	 (green-mask (xlib:visual-info-green-mask visual-info))
	 (green-mask-gap (- (integer-length green-mask) 8))
	 (blue-mask (xlib:visual-info-blue-mask visual-info))
	 (blue-mask-gap (- (integer-length blue-mask) 8)))
    (declare (type fixnum red-mask red-mask-gap
		   green-mask green-mask-gap
		   blue-mask blue-mask-gap))
    (+ (logand red-mask (ash pixel red-mask-gap))
       (logand green-mask (ash pixel green-mask-gap))
       (logand blue-mask (ash pixel blue-mask-gap)))))

; We only handle case where screen type (i.e. visual-class) is :truecolor
(defun choose-computing-pixel (image depth)
  (declare (type image image)
	   (type (unsigned-byte 16) depth))
  (if (= depth 24)
      (symbol-function (intern (format nil "COMPUTE-PIXEL-VALUE-~a-~a" (type-of image) depth) :clim-clx))
      (symbol-function (intern (format nil "COMPUTE-PIXEL-VALUE-~a" (type-of image)) :clim-clx))))

(defmacro medium-draw-translation-image (medium image transformation clipping-region)
  `(multiple-value-bind (mxx mxy myx myy tx ty) (get-transformation ,transformation)
     (declare (ignore mxx mxy myx myy)
	      (type coordinate tx ty))
     (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* ,clipping-region)
       (declare (type coordinate x1 y1 x2 y2))
       (medium-draw-linear-image ,medium ,image
				 (make-bounding-rectangle (- x1 tx) (- y1 ty)
							  (- x2 tx) (- y2 ty))
				 :print-x (round tx) :print-y (round ty)))))

(defun medium-draw-linear-image (medium image clipping-region &key (print-x 0) (print-y 0))
  (declare (optimize (speed 3)))
  (let* ((r-image-width (1- (image-width image)))
	 (r-image-height (1- (image-height image))))
    (declare (type fixnum r-image-width r-image-height))
    (with-bounding-rectangle* (x-min y-min x-max y-max) clipping-region
      (declare (type coordinate x-min y-min x-max y-max))
      (when (or (<= 0 x-min r-image-width)
		(<= x-min 0 x-max))
	(with-CLX-graphics (medium)
	  (let* ((colormap (xlib:screen-default-colormap (clx-port-screen port)))
		 (depth (xlib:drawable-depth mirror))
                 ;;pdm: see below
                 (pixmap-format (find depth
                                      (xlib:display-pixmap-formats
                                       (clx-port-display port))
                                      :key #'xlib:pixmap-format-depth))
                 (bits-per-pixel (and pixmap-format
                                      (xlib:pixmap-format-bits-per-pixel
                                       pixmap-format)))
		 (computing-pixel-function (choose-computing-pixel image depth))
		 (pixels (image-pixels image))
		 (x-min* (round (max x-min 0)))
		 (y-min* (round (max y-min 0)))
		 (x-max* (round (min x-max r-image-width)))
		 (y-max* (round (min y-max r-image-height)))
		 (width (1+ (max 0 (- x-max* x-min*))))
		 (height (1+ (max 0 (- y-max* y-min*))))
		 (start-x (+ print-x x-min*))
		 (start-y (+ print-y y-min*))
                 ;;pdm: the bits-per-pixel value needn't be the same as the
                 ;; depth.  If they don't match, XLIB signals an error.  So we
                 ;; have to get the right bits-per-pixel value corresponding to
                 ;; the given depth from XLIB and use it here.
		 (data (make-array `(,height ,width) :element-type `(unsigned-byte ,bits-per-pixel)))
		 (server-image (xlib:create-image :data data :depth depth :bits-per-pixel bits-per-pixel)))
	    (declare (type xlib::colormap colormap)
		     (type (unsigned-byte 16) depth)
		     (type fixnum width height start-x start-y x-min* y-min* x-max* y-max*)
		     (type xlib::image server-image))
            ;;pdm: If this is not called here, the clipping is wrong.  I don't
            ;; know what I'm doing here, it just works...
            (setf (xlib:gcontext-clip-mask gc :unsorted)
                  (list start-x start-y width height))
	    (loop for i of-type fixnum from y-min* to y-max*
		  do (loop for j of-type fixnum from x-min* to x-max*
			   do (setf (aref data (- i y-min*) (- j x-min*))
				    (funcall computing-pixel-function (aref pixels i j) colormap))))
	    (xlib:put-image mirror gc server-image
			    :width width
			    :height height
			    :x start-x
			    :y start-y)
	    (xlib::display-force-output (clx-port-display (port medium)))))))))
  

(defmethod medium-draw-image* ((medium clx-medium) (image rgb-image))
  (declare (optimize (speed 3)))
  (let ((transformation (medium-transformation medium))
	(clipping-region (medium-clipping-region medium)))
    (declare ;; This was kind of wrong, never assume some particular class! --GB
             ;; (type standard-transformation transformation)
             (type region clipping-region))
    (cond ; casual cases : identity or translation transformations
     ((transformation-equal transformation +identity-transformation+)
      (medium-draw-linear-image medium image clipping-region))
     ((translation-transformation-p transformation)
      (medium-draw-translation-image medium image transformation clipping-region))
          ; other cases
     (t
      (let* ((image-width (image-width image))
	     (image-height (image-height image))
	     (intersection (region-intersection
			    clipping-region
			    (transform-region transformation
						     (make-bounding-rectangle 0 0
										     image-width
										     image-height)))))
	(declare (type fixnum image-width image-height)
		 (type region intersection))
	(unless (region-equal intersection +nowhere+)
	  (with-CLX-graphics (medium)
	    (with-bounding-rectangle* (x-min y-min x-max y-max) intersection
	      (declare (type coordinate x-min y-min x-max y-max))
	      (let* ((colormap (xlib:screen-default-colormap (clx-port-screen port)))
		     (depth (xlib:drawable-depth mirror))
                     ;;pdm: The same bits-per-pixel vs. depth issue as above.
                     (pixmap-format (find depth
                                          (xlib:display-pixmap-formats
                                           (clx-port-display port))
                                          :key #'xlib:pixmap-format-depth))
                     (bits-per-pixel (and pixmap-format
                                          (xlib:pixmap-format-bits-per-pixel
                                           pixmap-format)))
		     (computing-pixel-function (choose-computing-pixel image depth))
		     (background-pixel (X-pixel (port medium) (medium-background medium)))
		     (inverse-transformation (invert-transformation transformation))
		     (image-region (make-bounding-rectangle 0 0 (1- image-width) (1- image-height)))
		     (pixels (image-pixels image))
		     (flat-pixels (make-array (* image-width image-height) :element-type `(unsigned-byte ,depth)
					      :displaced-to pixels))
		     (data-width (1+ (ceiling (- x-max x-min))))
		     (data (make-array `(1 ,data-width) :element-type `(unsigned-byte ,bits-per-pixel)))
		     (flat-data (make-array data-width :element-type `(unsigned-byte ,bits-per-pixel)
					    :displaced-to data))
		     (r-image-width (1- image-width))
		     (r-image-height (1- image-height))
		     (server-image (xlib:create-image :data data :depth depth :bits-per-pixel bits-per-pixel)))
		(declare (type xlib::colormap colormap)
			 (type (unsigned-byte 16) depth)
			 ;; (type standard-transformation inverse-transformation)
			 (type standard-rectangle image-region)
			 (type fixnum data-width)
			 (type xlib::image server-image))

		; optimisation on server
		(let ((x-min* (floor x-min))
		      (y-min* (floor y-min)) ;why +1 ?
		      (x-max* (floor x-max)) ;idem
		      (y-max* (floor y-max)))
		  (setf (xlib:gcontext-clip-mask gc) (list x-min* y-min* x-max* y-max*))
		  (xlib:clear-area mirror :x x-min* :y y-min*
				   :width (- x-max* x-min*) :height (- y-max* y-min*)
				   :exposures-p nil))
			   
		(loop for i of-type coordinate from y-min to y-max
		      do (multiple-value-bind (tx1 ty1) (transform-position inverse-transformation
									    x-min i)
			   (declare (type coordinate tx1 ty1))
			   (multiple-value-bind (tx2 ty2) (transform-position inverse-transformation
									      x-max i)
			     (declare (type coordinate tx2 ty2))
			     (let ((pos 0))
			       (declare (type fixnum pos))
                               ;;pdm: region-contains-position-p that used to
                               ;; be called below makes the whole drawing about
                               ;; three times slower.  So we have to perform an
                               ;; expanded check to speed the things that are
                               ;; enough slow already.
                               (flet ((image-contains-position-p (x y)
                                        (and (<= 0 x) (< x image-width)
                                             (<= 0 y) (< y image-height))))
			       (cond ((coordinate= tx1 tx2) ; horizontal case
				      (when (<= 0 tx1 r-image-width)
					(let ((dy (abs (round (- ty2 ty1))))
					      (incy (if (< ty1 ty2) 1 -1))
					      (x (floor tx1))
					      (y (floor ty1)))
					  (declare (type fixnum dy incy x y))
					  (loop for j of-type fixnum from 0 to dy
						do (setf (aref flat-data pos)
							 (if (image-contains-position-p x y)
							     (funcall computing-pixel-function
								      (aref pixels y x)
								      colormap)
							     background-pixel))
						(incf pos)
						(incf y incy)))))
				
				     ((coordinate= ty1 ty2) ; vertical case
				      (when (<= 0 ty1 r-image-height)
					(let* ((dx (abs (ceiling (- x-max x-min))))
					       (incx (/ (- tx2 tx1) dx))
					       (x (floor tx1))
					       (y (floor ty1)))
					  (declare (type fixnum dx x y)) 
					  (loop for j of-type fixnum from 0 to dx
						do (setf (aref flat-data pos)
							 (if (image-contains-position-p x y)
							     (funcall computing-pixel-function
								      (aref pixels y x)
								      colormap)
							     background-pixel))
						(incf pos)
						(setf x (floor (incf tx1 incx))))))) 

				     (t ; other case
				      (let ((line (region-intersection (make-line* tx1 ty1 tx2 ty2)
                                                                       image-region)))
					(declare (type region line))
					(unless (region-equal line +nowhere+)
                                          (multiple-value-bind (x1 y1) (line-start-point* line)
                                            (multiple-value-bind (x2 y2) (line-end-point* line)
                                              (declare (type coordinate x1 y1 x2 y2))

                                              (incf pos (round (abs (- y1 ty1))))
                                              (fill flat-data background-pixel :start 0 :end pos)

					; Bresenham
					      
                                              (let* ((dx (round (- x2 x1)))
                                                     (dy (round (- y2 y1)))
                                                     (x1* (round x1))
                                                     (x2* (round x2))
                                                     (stepx (if (< dx 0) -1 1))
                                                     (stepy (if (< dy 0) (- image-width) image-width))
                                                     (y1* (* image-width (round y1)))
                                                     (y2* (* image-width (round y2))))
                                                (declare (type fixnum dx dy stepx stepy x1* y1* x2* y2*))
                                                (setf dx (ash (if (< dx 0) (- dx) dx) -1)
                                                      dy (ash (if (< dy 0) (- dy) dy) -1))
						  
                                                (setf (aref flat-data pos) (aref flat-pixels (+ x1* y1*)))
                                                (incf pos)
                                                (if (> dx dy)
                                                    (let ((fraction (- dy (ash dx 1))))
                                                      (declare (type fixnum fraction))
                                                      (loop while (/= x1* x2*)
                                                        do (when (>= fraction 0)
                                                             (incf y1* stepy)
                                                             (decf fraction dx))
                                                        (incf x1* stepx)
                                                        (incf fraction dy)
                                                        (setf (aref flat-data pos)
                                                              (aref flat-pixels (+ x1* y1*)))
                                                        (incf pos)))
						      
                                                    (let ((fraction (- dx (ash dy 1))))
                                                      (declare (type fixnum fraction))
                                                      (loop while (/= y1* y2*)
                                                        do (when (>= fraction 0)
                                                             (incf x1* stepx)
                                                             (decf fraction dy))
                                                        (incf y1* stepy)
                                                        (incf fraction dx)
                                                        (setf (aref flat-data pos)
                                                              (aref flat-pixels (+ x1* y1*)))
                                                        (incf pos))))))))))))

			       (xlib:put-image mirror gc server-image
					       :width pos
					       :height 1
					       :x (floor x-min)
					       :y (floor i))))))))
		(xlib::display-force-output (clx-port-display (port medium))))))))))
						     
				 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; image pane

(defclass image-gadget (basic-gadget) ())

(defclass image-pane (image-gadget)
  ((image :type image :initform nil :initarg :image :reader image)))

(defmethod realize-mirror ((port clx-port) (pane image-pane))
  (realize-mirror-aux port pane :backing-store :always))

;; [Julien] As drawing-image mostly doesn't work, except drawaing everything (sigh !..)
;;          the region provided by graphical server is ignored, and the entire pane 
;;          region is redrawn. Bugs must be fixed before doing the right thing.
;;pdm: No, don't ignore the region, otherwise the pane becomes completely
;; unusable, spending all the time redrawing, redrawing, redrawing, ...
;; There's no reason to ignore the region, everything works fine with it
;; AFAIK.
(defmethod handle-repaint ((pane image-pane) region)
  (with-slots (image) pane
    (when image
      (draw-image pane image :clipping-region region))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; image as label
#||
(defmethod compose-space-aux ((pane labelled-gadget) (label image))
  (let ((width (image-width label))
	(height (image-height label)))
    (make-space-requirement :width width :height height
				   :min-width width :min-height height
				   :max-width width :max-height height)))

(defmethod draw-label ((pane labelled-gadget) (label image) x y)
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    (declare (ignore x1 x2)
	     (type coordinate x2 y2))
    (let* ((image-width (image-width label))
	   (image-height (image-height label))
	   (tx (- x (ecase (gadget-label-align-x pane)
		      (:left 0)
		      (:center (round image-width 2))
		      (:right image-width))))
	   (ty (ecase (gadget-label-align-y pane)
		 (:top y1)
		 (:center (- (round (- y2 y1) 2) (round image-height 2)))
		 (:baseline y)
		 (:bottom (- y2 image-height)))))
      (draw-image pane label
		  :clipping-region (sheet-region pane)
		  :transformation (make-translation-transformation tx ty)))))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; retrieve image

(defun zimage-to-rgb (zimage)
  (unless (eql (xlib:image-depth zimage) 24)
    (error "sorry, only true color images supported in zimage-to-rgb"))
  (let* ((data (xlib:image-z-pixarray zimage))
	 (w (xlib:image-width zimage))
	 (h (xlib:image-height zimage))
	 (rbyte (mask->byte (xlib:image-red-mask zimage)))
	 (gbyte (mask->byte (xlib:image-green-mask zimage)))
	 (bbyte (mask->byte (xlib:image-blue-mask zimage)))
	 (result (make-array (list h w)
			     :element-type '(unsigned-byte 32))))
    (dotimes (y h)
      (dotimes (x w)
	(setf (aref result y x)
	      (let ((pixel (aref data y x)))
		(dpb (the (unsigned-byte 8) (ldb rbyte pixel))
		     (byte 8 0)
		     (dpb (the (unsigned-byte 8) (ldb gbyte pixel))
			  (byte 8 8)
			  (dpb (the (unsigned-byte 8) (ldb bbyte pixel))
			       (byte 8 16)
			       0)))))))
    result))

(defmethod climi::sheet-rgb-data ((port clx-port) sheet &key x y width height)
  (let ((window (port-lookup-mirror port sheet)))
    (values
     (zimage-to-rgb
      (xlib:get-image window
		      :format :z-pixmap
		      :x (or x 0)
		      :y (or y 0)
		      :width (or width (xlib:drawable-width window))
		      :height (or height (xlib:drawable-height window))))
     nil)))
