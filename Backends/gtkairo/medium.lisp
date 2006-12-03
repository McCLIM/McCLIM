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

(defun df (x) (coerce x 'double-float))

(defclass gtkairo-medium (climi::basic-medium clim:medium)
  ((port :initarg :port :accessor port)
   (cr :initform nil :initarg :cr :accessor cr)
   (flipping-original-cr :initform nil :accessor flipping-original-cr)
   (flipping-pixmap :initform nil :accessor flipping-pixmap)
   (flipping-region :accessor flipping-region)
   (surface :initarg :surface :accessor surface)
   (last-seen-sheet :accessor last-seen-sheet)
   (last-seen-region :accessor last-seen-region)))

(defmethod initialize-instance :after
    ((instance gtkairo-medium) &key cr)
  (unless cr
    (setf (last-seen-sheet instance) nil)))

(defclass metrik-medium (gtkairo-medium)
  ())

(defparameter *antialiasingp* t)

(defun gtkwidget-gdkwindow (widget)
  (cffi:foreign-slot-value widget 'gtkwidget 'gdkwindow))

(defun medium-mirror (medium)
  (or (climi::port-lookup-mirror (port medium) (medium-sheet medium))
      (error "oops, drawing operation on unmirrored sheet ~A" medium)))

(defmacro with-cairo-medium ((medium) &body body)
  `(invoke-with-cairo-medium (lambda () ,@body) ,medium))

(defun invoke-with-cairo-medium (fn medium)
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
    (with-cairo-medium (medium)
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
;;;;  8.3 Output Protocol
;;;;

(defmethod engraft-medium :after ((medium gtkairo-medium) port sheet)
  )

(defmethod degraft-medium :after ((medium gtkairo-medium) port sheet)
  )

(defmethod make-medium ((port gtkairo-port) sheet)
  (make-instance 'gtkairo-medium :port port :sheet sheet))

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

;; copy&paste from medium.lisp|CLX:
;; this seems to work, but find out why all of these +nowhere+s are coming from
;; and kill them at the source...
(defun clipping-region->rect-seq (clipping-region)
  (loop
     for region in (nreverse (mapcan
			      (lambda (v) (unless (eq v +nowhere+) (list v)))
			      (region-set-regions clipping-region
						  :normalize :y-banding)))
     as rectangle = (bounding-rectangle region)
     for clip-x = (round-coordinate (rectangle-min-x rectangle))
     for clip-y = (round-coordinate (rectangle-min-y rectangle))
     collect (list clip-x
		    clip-y
		    (- (round-coordinate (rectangle-max-x rectangle)) clip-x)
		    (- (round-coordinate (rectangle-max-y rectangle)) clip-y))))

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

(defun untransform-size (transformation size)
  (multiple-value-bind (dx dy) (untransform-distance transformation size 0)
    (sqrt (+ (expt dx 2) (expt dy 2)))))

(defun transform-size (transformation size)
  (multiple-value-bind (dx dy) (transform-distance transformation size 0)
    (sqrt (+ (expt dx 2) (expt dy 2)))))

;;; text-style

(defun assert-font-status (cr str)
  (let ((status (cairo_font_face_status (cairo_get_font_face cr))))
    (unless (eq status :success)
      (error "status ~A after call to ~A" status str))))

(defun sync-text-style (medium text-style transform-glyphs-p)
  (with-slots (cr) medium
    (multiple-value-bind (family face size)
        (text-style-components
	 (merge-text-styles text-style *default-text-style*))
      (setf size
            (df
             (case size
               (:normal 12)
               (:tiny 6)
               (:small 10)
               (:very-small 8)
               (:large 14)
               (:very-large 16)
               (:huge 24)
               (otherwise size))))
      ;;
      (when (listp face)
	;; Ein Pfusch ist das!
	(setf face (intern (format nil "~A-~A"
				   (symbol-name (first face))
				   (symbol-name (second face)))
			   :keyword)))
      (cairo_select_font_face
       cr
       (ecase family
	 ((:fix :fixed) "mono")
	 (:serif "serif")
	 (:sans-serif "sansserif"))
       (ecase face
	 ((:roman :bold) :normal)
	 ((:italic :bold-italic :italic-bold) :italic)
	 ((:oblique :bold-oblique :oblique-bold) :oblique))
       (ecase face
	 ((:roman :italic :oblique) :normal)
	 ((:bold :bold-italic :italic-bold :bold-oblique
		 :oblique-bold)
	   :bold)))
      (assert-font-status cr "cairo_select_font_face")
      ;;
      (cond (transform-glyphs-p
	     (cairo_set_font_size cr (df size)))
            (t
             (cairo_set_font_size cr (df size)) ;###
	     ;; das habe ich halb auskommentiert vorgefunden, daher erstmal
	     ;; ganz raus:
	     ;; FIXME: Und Vorsicht, wir rufen das hier auf dem metrik-medium
	     ;; auf, falls die transformation doch eine Rolle spielen sollte,
	     ;; muessten wir sie dann natuerlich vorher mit dem eigentlichen
	     ;; Medium abgleichen.
;;;             (with-cairo-matrix (matrix (medium-transformation medium))
;;;               (multiple-value-bind (mxx mxy myx myy tx ty)
;;;                   (climi::get-transformation )
;;;                 (cairo_matrix_invert matrix)
;;;		 (cairo_transform_font cr matrix)
;;;		 ))
	      ))
      (assert-font-status cr "cairo_set_font_size"))))

(defun sync-drawing-options (medium)
  (sync-transformation medium)
  (sync-ink medium (medium-ink medium))
  (sync-clipping-region medium (medium-clipping-region medium))
  (sync-line-style medium (medium-line-style medium))
  ;;(sync-text-style medium (medium-text-style medium))
  )

;;;; ------------------------------------------------------------------------
;;;;  Drawing Operations
;;;;

(defmethod medium-draw-point* ((medium gtkairo-medium) x y)
  (with-cairo-medium (medium)
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

(defmethod medium-draw-points* ((medium gtkairo-medium) coord-seq)
  (with-cairo-medium (medium)
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

(defmethod medium-draw-line* ((medium gtkairo-medium) x1 y1 x2 y2)
  (with-cairo-medium (medium)
    (sync-sheet medium)
    (sync-transformation medium)
    (sync-ink medium (medium-ink medium))
    (sync-clipping-region medium (medium-clipping-region medium))
    (sync-line-style medium (medium-line-style medium))
    (with-slots (cr) medium
      (cairo_move_to cr (df x1) (df y1))
      (cairo_line_to cr (df x2) (df y2))
      (cairo_stroke cr))))

(defmethod medium-draw-lines* ((medium gtkairo-medium) position-seq)
  (with-cairo-medium (medium)
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
    ((medium gtkairo-medium) coord-seq closed filled)
  (with-cairo-medium (medium)
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

(defmethod medium-draw-rectangle* ((medium gtkairo-medium) x1 y1 x2 y2 filled)
  (with-cairo-medium (medium)
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
    ((medium gtkairo-medium) position-seq filled)
  (with-cairo-medium (medium)
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
    ((medium gtkairo-medium) cx cy rx1 ry1 rx2 ry2 start end filled)
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
  (with-cairo-medium (medium)
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

(defmethod medium-draw-text*
    ((medium gtkairo-medium) text x y start end
     align-x align-y toward-x toward-y transform-glyphs)
  (with-cairo-medium (medium)
    (sync-sheet medium)
    (with-slots (cr) medium
      (sync-transformation medium)
      (sync-ink medium (medium-ink medium))
      (sync-clipping-region medium (medium-clipping-region medium))
      (sync-text-style medium
		       (merge-text-styles (medium-text-style medium)
					  (medium-default-text-style medium))
		       transform-glyphs)
      (cairo_move_to cr (df x) (df y))
      (setf end (or end (length text)))
      (unless (eql start end)		;empty string breaks cairo/windows
	(cairo_show_text cr (subseq text start end))))))

(defmethod medium-finish-output ((medium gtkairo-medium))
  (with-cairo-medium (medium)
    (when (cr medium)
      (cairo_surface_flush (cairo_get_target (cr medium)))))
  (medium-force-output medium))

(defmethod medium-force-output ((medium gtkairo-medium))
  (remhash medium (dirty-mediums (port medium)))
  (with-cairo-medium (medium)
    (when (cr medium)
      (cairo_surface_flush (cairo_get_target (cr medium)))
      (invalidate-mirror (medium-mirror medium) (medium-sheet medium)))))

(defmethod invalidate-mirror ((mirror drawable-mirror) sheet)
  (declare (ignore sheet)))

(defmethod invalidate-mirror ((mirror widget-mirror) sheet)
  (let* ((drawable (mirror-drawable mirror))
	 (real-drawable (mirror-real-drawable mirror)))
    (unless (cffi:pointer-eq drawable real-drawable)
      (let* ((region (climi::sheet-mirror-region sheet))
	     (width (floor (bounding-rectangle-max-x region)))
	     (height (floor (bounding-rectangle-max-y region))))
	(cffi:with-foreign-object (r 'gdkrectangle)
	  (setf (cffi:foreign-slot-value r 'gdkrectangle 'width) width)
	  (setf (cffi:foreign-slot-value r 'gdkrectangle 'height) height)
	  (gdk_window_invalidate_rect real-drawable r 0))))))

(defmethod medium-beep ((medium gtkairo-medium))
  ;; fixme: visual beep?
  )


;;;; ------------------------------------------------------------------------
;;;;  Text Styles
;;;;

;;; Diverse dieser Funktionen werden auf Mediums aufgerufen, deren Sheet
;;; noch keinen Mirror hat, und muessen tatsaechlich schon die richtige
;;; Antwort liefern.  Daher leite ich einfach generell all diese
;;; Anfragen auf ein zuvor angelegtes Medium fuer das Root-Fenster um.

(defmacro slot (o c s)
  `(cffi:foreign-slot-value ,o ,c ,s))

(defun cairo-text-extents (cr str res)
  (cond
    #+(or win32 mswindows windows)	;empty string breaks cairo/windows
    ((string= str "")
      (setf str " ")
      (cairo_text_extents cr str res)
      (cffi:with-foreign-slots
	  ((width x_advance x_bearing) res cairo_text_extents)
	(setf width 0.0d0)
	(setf x_advance 0.0d0)
	(setf x_bearing 0.0d0)))
    (t
      (cairo_text_extents cr str res))))


;;; TEXT-STYLE-ASCENT

;; FIXME: Cairo documentation states that these numbers, AIUI, are not
;; exact measurements but rather values tweaked by the font designer for
;; better visual effect.
;;
;; What this seems to mean in practise is that, say, ASCENT is nearly
;; identical to text_extent.height in the tests I tried.
;;
;; So which one does CLIM want?  What are these function actually being
;; used for?
;;
;;   --DFL

(let ((hash (make-hash-table)))
  (defmethod text-style-ascent :around (text-style (medium gtkairo-medium))
    (or (gethash text-style hash)
        (setf (gethash text-style hash) (call-next-method)))))

(defmethod text-style-ascent (text-style (medium gtkairo-medium))
  (text-style-ascent text-style (metrik-medium (port medium))))

(defmethod text-style-ascent (text-style (medium metrik-medium))
  (with-cairo-medium (medium)
    (ceiling
     (with-slots (cr) medium
       (sync-sheet medium)
       (cairo_identity_matrix cr)
       (sync-text-style medium text-style t)
       (cffi:with-foreign-object (res 'cairo_font_extents)
	 (cairo_font_extents cr res)
	 (slot res 'cairo_font_extents 'ascent))))))


;;; TEXT-STYLE-DESCENT

(let ((hash (make-hash-table)))
  (defmethod text-style-descent :around (text-style (medium gtkairo-medium))
    (or (gethash text-style hash)
        (setf (gethash text-style hash) (call-next-method)))))

(defmethod text-style-descent (text-style (medium gtkairo-medium))
  (text-style-descent text-style (metrik-medium (port medium))))

(defmethod text-style-descent (text-style (medium metrik-medium))
  (with-cairo-medium (medium)
    (ceiling
     (with-slots (cr) medium
       (sync-sheet medium)
       (cairo_identity_matrix cr)
       (sync-text-style medium text-style t)
       (cffi:with-foreign-object (res 'cairo_font_extents)
	 (cairo_font_extents cr res)
	 (slot res 'cairo_font_extents 'descent))))))


;;; TEXT-STYLE-HEIGHT

(let ((hash (make-hash-table)))
  (defmethod text-style-height :around (text-style (medium gtkairo-medium))
    (or (gethash text-style hash)
        (setf (gethash text-style hash) (call-next-method)))))

(defmethod text-style-height (text-style (medium gtkairo-medium))
  (text-style-height text-style (metrik-medium (port medium))))

(defmethod text-style-height (text-style (medium metrik-medium))
;;;  (with-cairo-medium (medium)
;;;    (ceiling
;;;     (with-slots (cr) medium
;;;       (sync-sheet medium)
;;;       (cairo_identity_matrix cr)
;;;       (sync-text-style medium text-style t)
;;;       (cffi:with-foreign-object (res 'cairo_font_extents)
;;;	 (cairo_font_extents cr res)
;;;	 ;; ### let's hope that cairo respects
;;;	 ;; height = ascent + descent.
;;;	 ;;
;;;	 ;; No, it expressly doesn't.  Cairo documentation states that
;;;	 ;; height includes additional space that is meant to give more
;;;	 ;; aesthetic line spacing than ascent+descent would.  Is that a
;;;	 ;; problem for us? --DFL
;;;	 (slot res 'cairo_font_extents 'height)))))
  ;; OK, so it _does_ matter (see bug 15).
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))


;;; TEXT-STYLE-WIDTH

(let ((hash (make-hash-table)))
  (defmethod text-style-width :around (text-style (medium gtkairo-medium))
    (or (gethash text-style hash)
        (setf (gethash text-style hash) (call-next-method)))))

(defmethod text-style-width (text-style (medium gtkairo-medium))
  (text-style-width text-style (metrik-medium (port medium))))

(defmethod text-style-width (text-style (medium metrik-medium))
  (with-cairo-medium (medium)
    (ceiling
     (with-slots (cr) medium
       (sync-sheet medium)
       (cairo_identity_matrix cr)
       (sync-text-style medium text-style t)
       ;; This didn't work well for Climacs. --DFL
;;;       (cffi:with-foreign-object (res 'cairo_text_extents)
;;;         (cairo_text_extents cr "m" res)
;;;         (slot res 'cairo_text_extents 'width))
       (cffi:with-foreign-object (res 'cairo_font_extents)
	 (cairo_font_extents cr res)
	 (slot res 'cairo_font_extents 'max_x_advance))))))


;;; TEXT-STYLE-FIXED-WIDTH-P

(let ((hash (make-hash-table)))
  (defmethod text-style-fixed-width-p
      :around
      (text-style (medium gtkairo-medium))
    (or (gethash text-style hash)
        (setf (gethash text-style hash) (call-next-method)))))

(defmethod text-style-fixed-width-p (text-style (medium gtkairo-medium))
  (text-style-fixed-width-p text-style (metrik-medium (port medium))))

(defmethod text-style-fixed-width-p (text-style (medium metrik-medium))
  (with-cairo-medium (medium)
    (with-slots (cr) medium
      (sync-sheet medium)
      (cairo_identity_matrix cr)
      (sync-text-style medium text-style t)
      (cffi:with-foreign-object (res 'cairo_text_extents)
	(let (i m)
	  (cairo-text-extents cr "i" res)
	  (setf i (slot res 'cairo_text_extents 'width))
	  (cairo-text-extents cr "m" res)
	  (setf m (slot res 'cairo_text_extents 'width))
	  (= i m))))))

(defmethod text-size
    ((medium gtkairo-medium) string &key text-style (start 0) end)
  (with-gtk ()
    (when (characterp string) (setf string (string string)))
    (setf text-style (or text-style (medium-text-style medium)))
    (setf text-style
	  (merge-text-styles text-style (medium-default-text-style medium)))
    (text-size (metrik-medium (port medium))
	       string
	       :text-style text-style
	       :start start
	       :end (or end (length string)))))

(defmethod climi::text-bounding-rectangle*
    ((medium gtkairo-medium) string &key text-style (start 0) end)
  (with-gtk ()
    (when (characterp string) (setf string (string string)))
    (setf text-style (or text-style (medium-text-style medium)))
    (setf text-style
	  (merge-text-styles text-style (medium-default-text-style medium)))
    (climi::text-bounding-rectangle* (metrik-medium (port medium))
				     string
				     :text-style text-style
				     :start start
				     :end (or end (length string)))))

;; FIXME: TEXT-SIZE [and presumably TEXT-BOUNDING-RECTANGLE*, too] are
;; supposed to take newlines into account.  The CLX backend code was
;; written to support that but does not -- T-B-R errors out and T-S
;; doesn't return what WRITE-STRING on the sheet actually does.  So
;; let's not steal code from CLIM-CLX when it's broken.  Doesn't
;; actually look like anyone has been depending on this after all.
;; -- DFL

(defmethod text-size
    ((medium metrik-medium) string &key text-style (start 0) end)
  (with-cairo-medium (medium)
    ;; -> width height final-x final-y baseline
    (when (characterp string) (setf string (string string)))
    (setf text-style (or text-style (make-text-style nil nil nil)))
    (setf text-style
	  (merge-text-styles text-style (medium-default-text-style medium)))
    (with-slots (cr) medium
      (cairo_identity_matrix cr)
      (sync-text-style medium text-style t)
      (cffi:with-foreign-object (res 'cairo_text_extents)
	(cairo-text-extents cr
			    (subseq string start (or end (length string)))
			    res)
	(cffi:with-foreign-slots
	    ((x_advance height y_bearing) res cairo_text_extents)
	  (values
	   ;; use x_advance instead of width, since CLIM wants to trailing
	   ;; spaces to be taken into account.
	   (ceiling x_advance)
	   (ceiling height)
	   ;; Sames values again here: The CLIM spec states that these
	   ;; values differ only for multi-line text.  And y_advance is 0
	   ;; for european text, which is not what we want. --DFL
	   (ceiling x_advance)
	   (ceiling height)
	   ;; This used to be TEXT-STYLE-ASCENT, but see comment there.
	   (abs (ceiling y_bearing))))))))

(defmethod climi::text-bounding-rectangle*
    ((medium metrik-medium) string &key text-style (start 0) end)
  (with-cairo-medium (medium)
    ;; -> left ascent right descent
    (when (characterp string) (setf string (string string)))
    (setf text-style (or text-style (make-text-style nil nil nil)))
    (setf text-style
	  (merge-text-styles text-style (medium-default-text-style medium)))
    (with-slots (cr) medium
      (cairo_identity_matrix cr)
      (sync-text-style medium text-style t)
      (cffi:with-foreign-object (res 'cairo_text_extents)
	(cairo-text-extents cr
			    (subseq string start (or end (length string)))
			    res)
	;; This used to be a straight call to TEXT-SIZE.  Looking at
	;; what CLIM-CLX does, this looks better to me, but I'm not sure
	;; whether it's 100% right:
	;;   --DFL
	(cffi:with-foreign-slots
	    ((width height x_advance y_advance x_bearing y_bearing)
	     res cairo_text_extents)
	  (values (floor x_bearing)
		  (floor y_bearing)
		  (ceiling (+ width (max 0 x_bearing)))
		  (ceiling (+ height y_bearing))))))))

;;;; ------------------------------------------------------------------------
;;;;  General Designs
;;;;

(defun make-cairo-surface
    (compatible-medium width height &optional (format :argb32))
  (let* ((s (cairo_surface_create_similar
	     (cairo_get_target (cr compatible-medium))
             format width height))
         (c (cairo_create s)))
    (set-antialias c)
    (make-instance 'gtkairo-medium :cr c :surface s)))

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
    ((medium gtkairo-medium)
     (pattern clim-internals::indexed-pattern)
     &key &allow-other-keys)
  (with-cairo-medium (medium)
    (with-slots ((designs climi::designs) (array climi::array)) pattern
      (loop for y below (array-dimension array 0) do
	    (loop for x below (array-dimension array 1) do
		  (draw-rectangle* medium
				   x y
				   (+ x 1) (+ y 1)
				   :ink (elt designs (aref array y x))))))))

(defmethod draw-design
    ((medium gtkairo-medium) (pattern clim-internals::stencil)
     &key &allow-other-keys)
  (with-cairo-medium (medium)
    (with-slots ((array climi::array)) pattern
      (loop for y below (array-dimension array 0) do
	    (loop for x below (array-dimension array 1) do
		  (draw-rectangle* medium
				   x y
				   (+ x 1) (+ y 1)
				   :ink (make-opacity (aref array y x))))))))

(defmethod draw-design
    ((medium gtkairo-medium) (design clim-internals::transformed-design)
     &key &allow-other-keys)
  (with-cairo-medium (medium)
    (with-slots ((design climi::design) (transformation climi::transformation))
	design
      (with-drawing-options (medium :transformation transformation)
	(draw-design medium design)))))

(defmethod draw-design
    ((medium gtkairo-medium) (design clim-internals::rectangular-tile)
     &key &allow-other-keys)
  (with-cairo-medium (medium)
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
    ((medium gtkairo-medium) (design clim:opacity) &key &allow-other-keys)
  (with-cairo-medium (medium)
    (draw-design medium (compose-in (clim:medium-foreground medium) design))))

(defmethod draw-design
    ((medium gtkairo-medium) (design climi::uniform-compositum)
     &key &allow-other-keys)
  (with-cairo-medium (medium)
    (draw-rectangle* medium 0 0 600 600 :ink design)))

(defmethod draw-design
    ((medium gtkairo-medium) (design clim:color) &key &allow-other-keys)
  (with-cairo-medium (medium)
    (draw-rectangle* medium 0 0 600 600 :ink design)))

;; FIXME: this is some kind of special-purpose function for mediums
;; created by MAKE-CAIRO-SURFACE.  Normal mediums are handled by
;; DESTROY-MEDIUMS.
(defun destroy-cairo-medium (medium)
  (cairo_destroy (cr medium))
  (setf (cr medium) :destroyed)
  (dispose-flipping-pixmap medium)
  (when (surface medium)
    (cairo_surface_destroy (surface medium))))

(defmethod draw-design
    ((medium gtkairo-medium) (design clim-internals::in-compositum)
     &key &allow-other-keys)
  (with-cairo-medium (medium)
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
	(destroy-cairo-medium mink)
	(destroy-cairo-medium mmask)))))

(defmethod draw-design
    ((medium gtkairo-medium) (design clim-internals::out-compositum)
     &key &allow-other-keys)
  (with-cairo-medium (medium)
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
	(destroy-cairo-medium mink)
	(destroy-cairo-medium mmask)))))

(defmethod draw-design ((medium gtkairo-medium)
			(design clim-internals::over-compositum)
			&key &allow-other-keys)
  (with-cairo-medium (medium)
    (with-slots ((foreground climi::foreground) (background climi::background))
	design
      (draw-design medium background)
      (draw-design medium foreground))))


;;;; ------------------------------------------------------------------------
;;;;  Hmm
;;;;

(defmethod medium-current-text-style ((medium gtkairo-medium))
  (merge-text-styles (medium-text-style medium)
		     (medium-default-text-style medium)))

(defmethod medium-merged-text-style ((medium gtkairo-medium))
  (merge-text-styles (medium-text-style medium)
		     (medium-default-text-style medium)))

;;;; ------------------------------------------------------------------------
