;;; -*- Mode: Lisp; -*-

;;;  (c) copyright 2005 by Gilbert Baumann <gilbert@base-engineering.com>
;;;  (c) copyright 2006 David Lichteblau (david@lichteblau.com)

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :clim-gtkairo)

;;; Locking rule for this file: Dokumented entry points in the CLIM
;;; package use WITH-GTK, internal functions can rely on that.

(defclass cairo-medium (gtkairo-medium)
  ((cr :initform nil :initarg :cr :accessor cr)
   (flipping-original-cr :initform nil :accessor flipping-original-cr)
   (flipping-pixmap :initform nil :accessor flipping-pixmap)
   (flipping-region :accessor flipping-region)
   (surface :initarg :surface :accessor surface)
   (last-seen-sheet :accessor last-seen-sheet)
   (last-seen-region :accessor last-seen-region)))

(defmethod initialize-instance :after
    ((instance cairo-medium) &key cr)
  (unless cr
    (setf (last-seen-sheet instance) nil)))

(defparameter *antialiasingp* t)

(defmethod invoke-with-medium (fn (medium cairo-medium))
  (when (or (cr medium)
	    (climi::port-lookup-mirror (port medium) (medium-sheet medium)))
    (with-gtk ()
      (multiple-value-prog1
	  (funcall fn)
	(when (flipping-original-cr medium)
	  (apply-flipping-ink medium))))))

(defun sheet-changed-behind-our-back-p (medium)
  (and (slot-boundp medium 'last-seen-sheet)
       (or (not (eq (last-seen-sheet medium) (medium-sheet medium)))
	   (not (region-equal (last-seen-region medium)
			      (sheet-region (medium-sheet medium)))))))

(defmethod metrik-medium-for ((medium cairo-medium))
  (cairo-metrik-medium (port medium)))

(defun set-antialias (cr)
  (cairo_set_antialias cr
		       (if *antialiasingp*
			   :CAIRO_ANTIALIAS_DEFAULT
			   :CAIRO_ANTIALIAS_NONE)))

(defun sync-sheet (medium)
  (when (medium-sheet medium)		;ignore the metrik-medium
    (setf (gethash medium (dirty-mediums (port medium))) t))
  (when (or (null (cr medium))
	    (sheet-changed-behind-our-back-p medium))
    (with-medium (medium)
      (let* ((mirror (medium-mirror medium))
	     (drawable (mirror-drawable mirror)))
	(setf (cr medium) (gdk_cairo_create drawable))
	(dispose-flipping-pixmap medium)
	(pushnew medium (mirror-mediums mirror))
	(set-antialias (cr medium)))
      (setf (last-seen-sheet medium) (medium-sheet medium))
      (setf (last-seen-region medium) (sheet-region (medium-sheet medium))))))

(defun dispose-flipping-pixmap (medium)
  (when (flipping-pixmap medium)
    (gdk_drawable_unref (flipping-pixmap medium))
    (setf (flipping-pixmap medium) nil)))

;;;; ------------------------------------------------------------------------
;;;; Drawing Options
;;;;

(defun sync-transformation (medium &optional extra-transformation)
  (with-slots (cr) medium
    (cffi:with-foreign-object (matrix 'cairo_matrix_t)
      (let ((tr
	     (if (medium-sheet medium)
		 (sheet-native-transformation (medium-sheet medium))
		 clim:+identity-transformation+)))
	(when extra-transformation
	  (setf tr (compose-transformations extra-transformation tr)))
	(multiple-value-bind (mxx mxy myx myy tx ty)
	    (climi::get-transformation tr)
	  ;; Make sure not to hand transformations to cairo that it won't
	  ;; like, since debugging gets ugly once a cairo context goes
	  ;; into an error state:
	  (invert-transformation tr)
	  (cairo_matrix_init matrix
			     (df mxx) (df mxy) (df myx) (df myy)
			     (df tx) (df ty))
	  (cairo_set_matrix cr matrix))))))

(defmacro with-cairo-matrix ((matrix transformation) &body body)
  `(cffi:with-foreign-object (,matrix 'cairo_matrix_t)
     (multiple-value-bind (mxx mxy myx myy tx ty)
	 (climi::get-transformation ,transformation)
       (cairo_matrix_init ,matrix
			  (df mxx) (df mxy) (df myx) (df myy)
			  (df tx) (df ty))
       (locally ,@body))))

;;; ink

(defmethod sync-ink :before (medium new-value)
  (with-slots (cr) medium
    (cairo_set_operator cr :over)))

(defmethod sync-ink (medium (new-value (eql clim:+foreground-ink+)))
  (sync-ink medium (clim:medium-foreground medium))) ;### circles?

(defmethod sync-ink (medium (new-value (eql clim:+background-ink+)))
  (sync-ink medium (clim:medium-background medium))) ;### circles?

(defmethod sync-ink (medium (new-value clim:opacity))
  (with-slots (cr) medium
    (cond ((= 0 (opacity-value new-value))
           (cairo_set_source_rgba cr 0d0 0d0 0d0 0d0))
          ((= 1 (opacity-value new-value))
           (sync-ink medium (clim:medium-foreground medium)))
          (t
           (sync-ink medium (clim:compose-in (clim:medium-foreground medium)
					     new-value))))))

(defmethod sync-ink (medium (new-value climi::uniform-compositum))
  (with-slots (cr) medium
    (with-slots ((ink climi::ink) (mask climi::mask)) new-value
      (multiple-value-bind (red green blue) (clim:color-rgb ink)
        (cairo_set_source_rgba cr
			       (df red)
			       (df green)
			       (df blue)
			       (df (clim:opacity-value mask)))))))

(defmethod sync-ink (medium (new-value clim:color))
  (with-slots (cr) medium
    (multiple-value-bind (red green blue) (clim:color-rgb new-value)
      (cairo_set_source_rgba cr (df red) (df green) (df blue) (df 1.0d0)))))

(defvar *pattern-hash*
  (make-hash-table))

(defun pattern-cairo-pattern (medium pattern)
  (or (gethash pattern *pattern-hash*)
      (setf (gethash pattern *pattern-hash*)
            (let ((s (make-cairo-surface medium
					 (pattern-width pattern)
					 (pattern-height pattern))))
              (draw-design s pattern)
              (cairo_pattern_create_for_surface (slot-value s 'surface))))))

(defmethod sync-ink (medium (pattern climi::indexed-pattern))
  (with-slots (cr) medium
    (let ((s (make-cairo-surface medium
				 (pattern-width pattern)
				 (pattern-height pattern))))
      (draw-design s pattern)
      (let ((p (cairo_pattern_create_for_surface (slot-value s 'surface))))
        (cairo_set_source cr p)
        p))))

(defmethod sync-ink (medium (pattern climi::indexed-pattern))
  (with-slots (cr) medium
    (let ((p (pattern-cairo-pattern medium pattern)))
      (cairo_set_source cr p)
      p)))

(defmethod sync-ink (medium (design clim-internals::transformed-design))
  (with-slots ((design climi::design) (transformation climi::transformation))
      design
    ;; ### hmm
    (let ((p (sync-ink medium design)))
      (with-cairo-matrix (matrix (invert-transformation transformation))
        (cairo_pattern_set_matrix p matrix))
      p)))

(defun apply-flipping-ink (medium)
  (let ((from-surface (cairo_get_target (cr medium)))
	(from-drawable (flipping-pixmap medium))
	(to-surface (cairo_get_target (flipping-original-cr medium)))
	(to-drawable (medium-gdkdrawable medium)))
    (cairo_surface_flush from-surface)
    (cairo_surface_flush to-surface)
    (let ((gc (gdk_gc_new to-drawable))
	  (region (flipping-region medium)))
      (gdk_gc_set_function gc :GDK_XOR)
      (gdk_draw_drawable to-drawable gc from-drawable
			 (floor (bounding-rectangle-min-x region))
			 (floor (bounding-rectangle-min-y region))
			 (floor (bounding-rectangle-min-x region))
			 (floor (bounding-rectangle-min-y region))
			 (ceiling (bounding-rectangle-max-x region))
			 (ceiling (bounding-rectangle-max-y region))) 
      (gdk_gc_unref gc))
    (cairo_surface_mark_dirty to-surface))
  (cairo_destroy (cr medium))
  (setf (cr medium) (flipping-original-cr medium))
  (setf (flipping-original-cr medium) nil))

(defmethod sync-ink (medium (design climi::standard-flipping-ink))
  (setf (flipping-original-cr medium) (cr medium))
  (let* ((mirror (medium-mirror medium))
	 (drawable (mirror-drawable mirror)))
    (let* ((region (climi::sheet-mirror-region (medium-sheet medium)))
	   (width (floor (bounding-rectangle-max-x region)))
	   (height (floor (bounding-rectangle-max-y region)))
	   (pixmap
	    (or (flipping-pixmap medium)
		(setf (flipping-pixmap medium)
		      (gdk_pixmap_new drawable width height -1)))))
      (setf (cr medium) (gdk_cairo_create pixmap))
      (set-antialias (cr medium))
      (setf (flipping-region medium) region)
      (cairo_paint (cr medium))
      (sync-transformation medium)
      (sync-ink medium +white+))))

(defmethod sync-ink (medium new-value)
  (warn "SYNC-INK lost ~S." new-value))

;;; clipping region

(defun sync-clipping-region (medium region)
  (with-slots (cr) medium
    (cairo_reset_clip cr)
    (unless (eq region +everywhere+)
      (unless (eq region +nowhere+)
	(loop for (x y w h) in (clipping-region->rect-seq region) do
	      (cairo_rectangle cr (df x) (df y) (df w) (df h))))
      (cairo_clip cr))
    (cairo_new_path cr)))

;;; line-style

(defun sync-line-style (medium line-style)
  (with-slots (cr) medium
    (cairo_set_line_cap cr
                        (case (line-style-cap-shape line-style)
                          (:butt :butt)
                          (:square :square)
                          (:round :round)
                          (:no-end-point :round))) ;###
    (cond ((null (line-style-dashes line-style))
           (cairo_set_dash cr (cffi:null-pointer) 0 0d0)) ;hmm
          ((eq t (line-style-dashes line-style))
           (let ((d 10))
             (cairo-set-dash* cr
                              (case (line-style-unit line-style)
                                ((:point :normal)
                                 (map 'vector (lambda (x)
                                                (untransform-size
						 (medium-transformation
						  medium) x))
                                      (list d)))
                                (:coordinate
                                 (list d))))))
          (t
           ;; line-style-unit!
           (cairo-set-dash* cr
                            (case (line-style-unit line-style)
                              ((:point :normal)
                               (map 'vector (lambda (x)
                                              (untransform-size
					       (medium-transformation medium)
					       x))
                                    (line-style-dashes line-style)))
                              (:coordinate
                               (line-style-dashes line-style))))))
    (cairo_set_line_join cr
                         (case (line-style-joint-shape line-style)
                           (:miter :miter)
                           (:bevel :bevel)
                           (:round :round)
                           (:none :round))) ;###
    (cairo_set_line_width cr
                          (max 1.0d0
                               (df
                                (case (line-style-unit line-style)
                                  ((:point :normal)
                                   (untransform-size
				    (medium-transformation medium)
				    (line-style-thickness line-style)))
                                  (:coordinate
                                   (line-style-thickness line-style)))))) ))

(defun cairo-set-dash* (cr dashes)
  (let ((ndash (length dashes)))
    (cffi:with-foreign-object (adashes :double ndash)
      (loop
	  for i below ndash do
	    (setf (cffi:mem-aref adashes :double i) (df (elt dashes i))))
      (cairo_set_dash cr adashes ndash 0d0))))

(defun sync-drawing-options (medium)
  (sync-transformation medium)
  (sync-ink medium (medium-ink medium))
  (sync-clipping-region medium (medium-clipping-region medium))
  (sync-line-style medium (medium-line-style medium)))

;;;; ------------------------------------------------------------------------
;;;;  Drawing Operations
;;;;

(defmethod medium-draw-point* ((medium cairo-medium) x y)
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (with-slots (cr) medium
      (cairo_set_line_cap cr :round)
      (setf x (df x))
      (setf y (df y))
      (cairo_move_to cr x y)
      (cairo_line_to cr (+ x 0.5) (+ y 0.5))
      (cairo_stroke cr))))

(defmethod medium-draw-points* ((medium cairo-medium) coord-seq)
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (with-slots (cr) medium
      (cairo_set_line_cap cr :round)
      (loop for i below (length coord-seq) by 2 do
	    (let ((x (df (elt coord-seq (+ i 0))))
		  (y (df (elt coord-seq (+ i 1)))))
	      (cairo_move_to cr x y)
	      (cairo_line_to cr (+ x 0.5) (+ y 0.5))
	      (cairo_stroke cr))))))

(defmethod medium-draw-line* ((medium cairo-medium) x1 y1 x2 y2)
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (with-slots (cr) medium
      (cairo_move_to cr (df x1) (df y1))
      (cairo_line_to cr (df x2) (df y2))
      (cairo_stroke cr))))

(defmethod medium-draw-lines* ((medium cairo-medium) position-seq)
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (with-slots (cr) medium
      (loop for i below (length position-seq) by 4 do
	    (cairo_move_to cr
			   (df (elt position-seq (+ i 0)))
			   (df (elt position-seq (+ i 1))))
	    (cairo_line_to cr
			   (df (elt position-seq (+ i 2)))
			   (df (elt position-seq (+ i 3)))))
      (cairo_stroke cr))))

(defmethod medium-draw-polygon*
    ((medium cairo-medium) coord-seq closed filled)
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (unless filled
      (sync-line-style medium (medium-line-style medium)))
    (with-slots (cr) medium
      (cairo_move_to cr (df (elt coord-seq 0)) (df (elt coord-seq 1)))
      (loop for i from 2 below (length coord-seq) by 2 do
	    (cairo_line_to cr
			   (df (elt coord-seq i))
			   (df (elt coord-seq (+ i 1)))))
      (when closed
	(cairo_line_to cr (df (elt coord-seq 0)) (df (elt coord-seq 1))))
      (if filled
	  (cairo_fill cr)
	  (cairo_stroke cr)))))

(defmethod medium-draw-rectangle* ((medium cairo-medium) x1 y1 x2 y2 filled)
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (unless filled
      (sync-line-style medium (medium-line-style medium)))
    (when (flipping-original-cr medium)
      (setf (flipping-region medium)
	    (transform-region
	     (if (medium-sheet medium)
		 (sheet-native-transformation (medium-sheet medium))
		 clim:+identity-transformation+)
	     (make-rectangle* x1 y1 x2 y2))))
    (with-slots (cr) medium
      (setf x1 (df x1))
      (setf y1 (df y1))
      (setf x2 (df x2))
      (setf y2 (df y2))
      (when (< x2 x1) (rotatef x1 x2))
      (when (< y2 y1) (rotatef y1 y2))
      (cairo_rectangle cr x1 y1 (- x2 x1) (- y2 y1))
      (if filled
	  (cairo_fill cr)
	  (cairo_stroke cr)))))

(defmethod medium-draw-rectangles*
    ((medium cairo-medium) position-seq filled)
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (unless filled
      (sync-line-style medium (medium-line-style medium)))
    (with-slots (cr) medium
      (loop for i below (length position-seq) by 4 do
	    (let ((x1 (df (elt position-seq (+ i 0))))
		  (y1 (df (elt position-seq (+ i 1))))
		  (x2 (df (elt position-seq (+ i 2))))
		  (y2 (df (elt position-seq (+ i 3)))))
	      (when (< x2 x1) (rotatef x1 x2))
	      (when (< y2 y1) (rotatef y1 y2))
	      (cairo_rectangle cr x1 y1 (- x2 x1) (- y2 y1))
	      (if filled
		  (cairo_fill cr)
		  (cairo_stroke cr)))))))

(defmethod medium-draw-ellipse*
    ((medium cairo-medium) cx cy rx1 ry1 rx2 ry2 start end filled)
  ;; This one is tricky. Cairo doesn't know ellipses, it only knows
  ;; circles. But then it is fully capable to draw circles under affine
  ;; transformations only that the line style is transformed too. So
  ;; what we do: We setup an [additional] transformation to from our
  ;; ellipse to a circle and setup line style properly transformed. ---
  ;; This is not entirely correct in case of shearing or odd scaling
  ;; transformations.
  ;;
  ;; Also: What is done to patterns?
  ;;
  ;; Anyhow, let's hack along.
  ;;
  ;; Quick test if this is a circle:
  (with-medium (medium)
    (cond
      ((= (+ (expt rx1 2) (expt ry1 2))
	  (+ (expt rx2 2) (expt ry2 2)))
	(let ((radius (sqrt (+ (expt rx1 2) (expt ry1 2)))))
	  (sync-sheet medium)
	  (sync-transformation medium)
	  (sync-ink medium (medium-ink medium))
	  (sync-clipping-region medium (medium-clipping-region medium))
	  (sync-line-style medium (medium-line-style medium))
	  (with-slots (cr) medium
	    (cairo_new_path cr)
	    (cairo_arc cr (df cx) (df cy) (df radius) (df start) (df end))
	    ;; Incredible cool: Cairo doesn't respect line dashes while
	    ;; drawing arcs. Quite useful feature actually.
	    (if filled
		(cairo_fill cr)
		(cairo_stroke cr)))))
      ;; general case
      (t
	(let ((tr (make-3-point-transformation* 0 0 1 0 0 1
						cx cy
						(+ cx rx1) (+ cy ry1)
						(+ cx rx2) (+ cy ry2))))
	  (sync-sheet medium)
	  ;; hmm, something is wrong here.
	  (sync-transformation medium tr)
	  (sync-ink medium (medium-ink medium))
	  (sync-clipping-region medium (medium-clipping-region medium))
	  (sync-line-style medium (medium-line-style medium))
	  (with-slots (cr) medium
	    (cairo_new_path cr)
	    (cairo_arc cr 0d0 0d0 1d0 (df start) (df end))
	    (cairo_fill cr)
	    (cairo_set_source_rgba cr 0.0d0 0.0d0 1.0d0 1.0d0)
	    (loop for a from 0 below (* 2 pi) by .1 do
		  (cairo_new_path cr)
		  (cairo_rectangle cr (df (sin a)) (df (cos a)) .05d0 .05d0)
		  (cairo_fill cr))))))))

(defmethod invoke-with-pango-layout
    (fn (medium cairo-medium) &key text-style text)
  (let ((layout (pango_cairo_create_layout (slot-value medium 'cr))))
    (unwind-protect
	(progn
	  (configure-pango-layout layout :text-style text-style :text text)
	  (funcall fn layout))
      (g_object_unref layout))))

(defmethod medium-draw-text*
    ((medium cairo-medium) text x y start end
     align-x align-y toward-x toward-y transform-glyphs)
  (with-medium (medium)
    (sync-sheet medium)
    (with-slots (cr) medium
      (sync-transformation medium)
      (sync-ink medium (medium-ink medium))
      (sync-clipping-region medium (medium-clipping-region medium))
      (setf end (or end (length text)))
      (unless (eql start end)
	(with-pango-layout (layout medium
				   :text-style medium
				   :text (subseq text start end))
	  (let ((y2
		 (nth-value 1 (pango-layout-line-get-pixel-extents layout 0))))
	    (cairo_move_to cr (df x) (df (+ y y2))))
	  (pango_cairo_show_layout cr layout))))))

;; Stolen from the CLX backend.
(defmethod climi::medium-draw-image-design*
    ((medium cairo-medium) (design climi::rgb-image-design) x y)
  (destructuring-bind (&optional surface buffer mask)
      (slot-value design 'climi::medium-data)
    (unless surface
      (let* ((image (slot-value design 'climi::image)))
        (setf (values surface buffer) (image-to-cairosurface image))
        (when (climi::image-alpha-p image)
          (error "~@<Drawing of images with alpha component is not supported.~:@>"))
        (setf (slot-value design 'climi::medium-data) (list surface buffer mask))))
    (when mask
      (error "~@<A mask in your image design.~:@>"))
    (with-medium (medium)
      (multiple-value-bind (x y)
          (transform-position
           (sheet-device-transformation (medium-sheet medium))
           x y)
        (setf x (float x 0d0))
        (setf y (float y 0d0))
        (with-slots (cr) medium
          (cairo_set_source_surface cr surface x y)
          (cond
            #+ (or)
            (mask
             (xlib:with-gcontext (gcontext 
                                  :clip-mask mask
                                  :clip-x x
                                  :clip-y y)
               (xlib:copy-area pixmap gcontext 0 0 width height
                               da x y)))
            (t
             (cairo_paint cr))))))))

(defmethod climi::medium-free-image-design
    ((medium cairo-medium) (design climi::rgb-image-design))
  (destructuring-bind (&optional surface buffer mask)
      (slot-value design 'climi::medium-data)
    (when surface
      #+ (or)
      ;; This one bites, no idea why.
      (cairo_destroy surface)
      (cffi:foreign-free buffer)
      (setf (slot-value design 'climi::medium-data) nil))))

;; Was: CLX/compute-rgb-image-mask
#+ (or)
(defun compute-rgb-image-mask (drawable image)
  (let* ((width (climi::image-width image))
         (height (climi::image-height image))
         (bitmap (xlib:create-pixmap :drawable drawable
                                     :width width 
                                     :height height
                                     :depth 1))
         (gc (xlib:create-gcontext :drawable bitmap
				   :foreground 1
				   :background 0))
         (idata (climi::image-data image))
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

;; Was: CLX/image-to-ximage
(defun image-to-cairosurface (image)
  (let* ((width (climi::image-width image))
         (height (climi::image-height image))
         (idata (climi::image-data image))
         (stride (cairo_format_stride_for_width :rgb24 width))
         (cairodata (cffi:foreign-alloc :uint8 :count (* stride height))))
    (declare (type (simple-array (unsigned-byte 32) (* *)) idata))
    (loop :for row-offset :from 0 :by stride
       :for y :from 0 :below height
       :do (loop :for offset :from row-offset :by 4
              :for x :from 0 :below width
              :do (let ((px (aref idata y x)))
                    (setf (cffi:mem-ref cairodata :uint32 offset)
                          (dpb (ldb (byte 8 0) px) (byte 8 16)
                               (dpb (ldb (byte 8 8) px) (byte 8 8)
                                    (dpb (ldb (byte 8 16) px) (byte 8 0)
                                         0)))))))
    (values (cairo_image_surface_create_for_data cairodata :rgb24 width height stride)
            cairodata)))

(defmethod medium-finish-output ((medium cairo-medium))
  (with-medium (medium)
    (when (cr medium)
      (cairo_surface_flush (cairo_get_target (cr medium)))))
  (medium-force-output medium))

(defmethod medium-force-output ((medium cairo-medium))
  (remhash medium (dirty-mediums (port medium)))
  (with-medium (medium)
    (when (cr medium)
      (cairo_surface_flush (cairo_get_target (cr medium)))
      (invalidate-mirror (medium-mirror medium) (medium-sheet medium)))))

(defmethod medium-beep ((medium cairo-medium))
  ;; fixme: visual beep?
  )

;;;; ------------------------------------------------------------------------
;;;;  General Designs
;;;;

(defun make-cairo-surface (compatible-medium width height
			   &optional (format :CAIRO_CONTENT_COLOR_ALPHA))
  (let* ((s (cairo_surface_create_similar
	     (cairo_get_target (cr compatible-medium))
             format width height))
         (c (cairo_create s)))
    (set-antialias c)
    (make-instance 'cairo-medium :cr c :surface s)))

(defmacro with-pattern ((m1 mp) &body body)
  (let ((p (gensym "P.")))
    `(let ((,p (cairo_pattern_create_for_surface (slot-value ,mp 'surface))))
       (unwind-protect
           (progn
             (cairo_set_source (slot-value ,m1 'cr) ,p)
             (locally ,@body))
	 (cairo_pattern_destroy ,p)))))

;;;; draw design

(defmethod draw-design
    ((medium cairo-medium)
     (pattern clim-internals::indexed-pattern)
     &key &allow-other-keys)
  (with-medium (medium)
    (with-slots ((designs climi::designs) (array climi::array)) pattern
      (loop for y below (array-dimension array 0) do
	    (loop for x below (array-dimension array 1) do
		  (draw-rectangle* medium
				   x y
				   (+ x 1) (+ y 1)
				   :ink (elt designs (aref array y x))))))))

(defmethod draw-design
    ((medium cairo-medium) (pattern clim-internals::stencil)
     &key &allow-other-keys)
  (with-medium (medium)
    (with-slots ((array climi::array)) pattern
      (loop for y below (array-dimension array 0) do
	    (loop for x below (array-dimension array 1) do
		  (draw-rectangle* medium
				   x y
				   (+ x 1) (+ y 1)
				   :ink (make-opacity (aref array y x))))))))

(defmethod draw-design
    ((medium cairo-medium) (design clim-internals::transformed-design)
     &key &allow-other-keys)
  (with-medium (medium)
    (with-slots ((design climi::design) (transformation climi::transformation))
	design
      (with-drawing-options (medium :transformation transformation)
	(draw-design medium design)))))

(defmethod draw-design
    ((medium cairo-medium) (design clim-internals::rectangular-tile)
     &key &allow-other-keys)
  (with-medium (medium)
    (with-slots ((design climi::design)
		 (width climi::width)
		 (height climi::height))
	design
      ;; ###
      (loop for x below 600 by width do
	    (loop for y below 600 by height do
		  ;; ###
		  (draw-design medium
			       (transform-region
				(make-translation-transformation x y)
				design)))))))

(defmethod draw-design
    ((medium cairo-medium) (design clim:opacity) &key &allow-other-keys)
  (with-medium (medium)
    (draw-design medium (compose-in (clim:medium-foreground medium) design))))

(defmethod draw-design
    ((medium cairo-medium) (design climi::uniform-compositum)
     &key &allow-other-keys)
  (with-medium (medium)
    (draw-rectangle* medium 0 0 600 600 :ink design)))

(defmethod draw-design
    ((medium cairo-medium) (design clim:color) &key &allow-other-keys)
  (with-medium (medium)
    (draw-rectangle* medium 0 0 600 600 :ink design)))

(defun destroy-surface-medium (medium)
  (destroy-medium medium)
  (when (surface medium)
    (cairo_surface_destroy (surface medium))))

(defmethod destroy-medium ((medium cairo-medium))
  (when (cr medium)
    (cairo_destroy (cr medium))
    (setf (cr medium) nil)
    (dispose-flipping-pixmap medium)))

(defmethod draw-design
    ((medium cairo-medium) (design clim-internals::in-compositum)
     &key &allow-other-keys)
  (with-medium (medium)
    (with-slots ((ink climi::ink) (mask climi::mask)) design
      (let ((mink (make-cairo-surface medium 600 600))
	    (mmask (make-cairo-surface medium 600 600 :a8))) 
	(draw-design mink ink)
	(draw-design mmask mask)
	(with-pattern (mink mmask)
	  (cairo_set_operator (slot-value mink 'cr) :in-reverse)
	  (cairo_rectangle (slot-value mink 'cr) 0d0 0d0 600d0 600d0)
	  (cairo_fill (slot-value mink 'cr)))
	(with-pattern (medium mink)
	  (sync-transformation medium) ;###
	  (cairo_rectangle (slot-value medium 'cr) 0d0 0d0 600d0 600d0)
	  (cairo_fill (slot-value medium 'cr)))
	;;
	(destroy-surface-medium mink)
	(destroy-surface-medium mmask)))))

(defmethod draw-design
    ((medium cairo-medium) (design clim-internals::out-compositum)
     &key &allow-other-keys)
  (with-medium (medium)
    (with-slots ((ink climi::ink) (mask climi::mask)) design
      (let ((mink (make-cairo-surface medium 600 600))
	    (mmask (make-cairo-surface medium 600 600 :a8))) 
	(draw-design mink ink)
	(draw-design mmask mask)
	(with-pattern (mink mmask)
	  (cairo_set_operator (slot-value mink 'cr) :out-reverse)
	  (cairo_rectangle (slot-value mink 'cr) 0d0 0d0 600d0 600d0)
	  (cairo_fill (slot-value mink 'cr)))
	(with-pattern (medium mink)
	  (sync-transformation medium) ;###
	  (cairo_rectangle (slot-value medium 'cr) 0d0 0d0 600d0 600d0)
	  (cairo_fill (slot-value medium 'cr)))
	;;
	(destroy-surface-medium mink)
	(destroy-surface-medium mmask)))))

(defmethod draw-design ((medium cairo-medium)
			(design clim-internals::over-compositum)
			&key &allow-other-keys)
  (with-medium (medium)
    (with-slots ((foreground climi::foreground) (background climi::background))
	design
      (draw-design medium background)
      (draw-design medium foreground))))


;;;; Bezier support

(defun %draw-bezier-area (medium area)
  (with-slots (cr) medium
    (let ((segments (climi::segments area)))
      (let ((p0 (slot-value (car segments) 'climi::p0)))
        (cairo_move_to cr (df (point-x p0)) (df (point-y p0))))
      (dolist (segment segments)
        (with-slots (climi::p1 climi::p2 climi::p3) segment
          (cairo_curve_to cr
                          (df (point-x climi::p1)) (df (point-y climi::p1))
                          (df (point-x climi::p2)) (df (point-y climi::p2))
                          (df (point-x climi::p3)) (df (point-y climi::p3)))))
      (cairo_fill cr))))

(defmethod climi::medium-draw-bezier-design*
    ((medium cairo-medium) (design climi::bezier-area))
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (%draw-bezier-area medium design)))

(defmethod climi::medium-draw-bezier-design*
    ((medium cairo-medium) (design climi::bezier-union))
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (let ((tr (climi::transformation design)))
      (dolist (area (climi::areas design))
        (%draw-bezier-area medium (transform-region tr area))))))

(defmethod climi::medium-draw-bezier-design*
    ((medium cairo-medium) (design climi::bezier-difference))
  (with-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (dolist (area (climi::positive-areas design))
      (%draw-bezier-area medium area)))
  (with-drawing-options (medium :ink +background-ink+)
    (with-medium (medium)
      (sync-sheet medium)
      (sync-transformation medium)
      (sync-ink medium (medium-ink medium))
      (sync-clipping-region medium (medium-clipping-region medium))
      (sync-line-style medium (medium-line-style medium))
      (dolist (area (climi::negative-areas design))
        (%draw-bezier-area medium area)))))
