;;; -*- Mode: Lisp; -*-

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

(defun rc (x) (floor (+ x .5)))
(defun rd (d) (floor (+ (abs d) .5)))

(defclass gdk-medium (gtkairo-medium)
    ((gc :initform nil :initarg :gc)))

(defmethod destroy-medium ((medium gdk-medium))
  (with-slots (gc) medium
    (when gc
      (gdk_gc_unref gc)
      (setf gc nil))))

(defmethod metrik-medium-for ((medium gdk-medium))
  (gdk-metrik-medium (port medium)))

(defgeneric gc (medium ink))

(defmethod gc :before (medium ink)
  (declare (ignore ink))
  (with-slots (gc) medium
    (unless gc
      (setf gc (gdk_gc_new (dr medium)))
      (update-clipping-region medium)
      (update-line-style medium)))
  (when (medium-sheet medium)		;ignore the metrik-medium
    (setf (gethash medium (dirty-mediums (port medium))) t)))

(defmethod gc (medium (ink color))
  (with-slots (gc) medium
    (with-gdkcolor (c (or ink (medium-ink medium)))
      (gdk_gc_set_rgb_fg_color gc c))
    (with-gdkcolor (c (medium-background medium))
      (gdk_gc_set_rgb_bg_color gc c))
    (gdk_gc_set_function gc :GDK_COPY)
    gc))

(defmethod gc (medium (ink (eql t)))
  (gc medium (medium-ink medium)))

(defmethod gc (medium (ink (eql +foreground-ink+)))
  (gc medium (medium-foreground medium)))

(defmethod gc (medium (ink (eql +background-ink+)))
  (gc medium (medium-background medium)))

(defmethod gc (medium (ink (eql +flipping-ink+)))
  (with-slots (gc) medium
    (with-gdkcolor (c (medium-foreground medium))
      (with-gdkcolor (d (medium-background medium))
	(gdk_colormap_alloc_color (gdk_colormap_get_system) c 0 1)
	(gdk_colormap_alloc_color (gdk_colormap_get_system) d 0 1)
	(setf (cffi:foreign-slot-value c 'gdkcolor 'pixel)
	      (logxor (cffi:foreign-slot-value c 'gdkcolor 'pixel)
		      (cffi:foreign-slot-value d 'gdkcolor 'pixel))))
      (gdk_gc_set_foreground gc c)
      (gdk_gc_set_background gc c))
    (gdk_gc_set_function gc :GDK_XOR)
    gc))

(defmethod (setf medium-clipping-region) :after (region (medium gdk-medium))
  (declare (ignore region))
  (with-slots (gc) medium
    (when gc
      (update-clipping-region medium))))

(defun update-clipping-region (medium)
  (with-slots (gc) medium
    (let ((region (gdk_region_new)))
      (let ((clim-region (climi::medium-device-region medium)))
	(unless (region-equal clim-region +nowhere+)
	  (loop for (x y w h) in (clipping-region->rect-seq clim-region) do
		(cffi:with-foreign-object (r 'gdkrectangle)
		  (setf (cffi:foreign-slot-value r 'gdkrectangle 'x) x)
		  (setf (cffi:foreign-slot-value r 'gdkrectangle 'y) y)
		  (setf (cffi:foreign-slot-value r 'gdkrectangle 'width) w)
		  (setf (cffi:foreign-slot-value r 'gdkrectangle 'height) h)
		  (gdk_region_union_with_rect region r
					      ))))
	#+(or)
	(when (region-equal clim-region +everywhere+)
	  (gdk_region_union region
			    (gdk_drawable_get_clip_region (dr medium))))) 
      (gdk_gc_set_clip_region gc region)
      (gdk_region_destroy region))))

(defmethod (setf medium-line-style) :after (line-style (medium gdk-medium))
  (declare (ignore line-style))
  (with-slots (gc) medium
    (when gc
      (update-line-style medium))))

(defun update-line-style (medium)
  (with-slots (gc) medium
    (let ((line-style (medium-line-style medium)))
      (gdk_gc_set_line_attributes gc
				  (round (line-style-thickness line-style))
				  (if (line-style-dashes line-style)
				      :GDK_LINE_ON_OFF_DASH
				      :GDK_LINE_SOLID)
				  (ecase (line-style-cap-shape line-style)
				    (:no-end-point :GDK_CAP_NOT_LAST)
				    (:butt :GDK_CAP_BUTT)
				    (:round :GDK_CAP_ROUND)
				    (:square :GDK_CAP_PROJECTING))
				  (ecase (line-style-joint-shape line-style)
				    (:miter :GDK_JOIN_MITER)
				    (:bevel :GDK_JOIN_BEVEL)
				    (:round :GDK_JOIN_ROUND)
				    (:none
				      ;; ??
				      :GDK_JOIN_ROUND)))
      (let ((dashes (coerce (line-style-dashes line-style) 'vector)))
	(case (line-style-dashes line-style)
	  ((t) (setf dashes (vector 10 10)))
	  ((nil) (setf dashes (vector 1))))
	(when (member (line-style-unit line-style) '(:point :normal))
	  (setf dashes (map 'vector (lambda (x)
				      (untransform-size
				       (medium-transformation medium)
				       x))
			    dashes)))
	(cffi:with-foreign-object (adashes :int8 (length dashes))
	  (loop
	      for i from 0
	      for x across dashes
	      do
		(setf (cffi:mem-aref adashes :int8 i) (rc x)))
	  (gdk_gc_set_dashes gc 0 adashes (length dashes)))))))

(defmethod invoke-with-medium (fn (medium gdk-medium))
  (when (climi::port-lookup-mirror (port medium) (medium-sheet medium))
    (with-gtk ()
      (funcall fn))))

(defmethod invoke-with-medium (fn (medium gdk-metrik-medium))
  (with-gtk ()
    (funcall fn)))

(defun dr (medium)
  (mirror-drawable (medium-mirror medium)))

(defmethod medium-draw-point* ((medium gdk-medium) x y)
  (with-medium (medium)
    (let ((tr (sheet-native-transformation (medium-sheet medium))))
      (climi::with-transformed-position (tr x y)
	(gdk_draw_point (dr medium) (gc medium t) (rc x) (rc y))))))

(defmethod medium-draw-points* ((medium gdk-medium) coord-seq)
  (with-medium (medium)
    (let ((tr (sheet-native-transformation (medium-sheet medium)))
	  (dr (dr medium))
	  (gc (gc medium t)))
      (loop for (x y) on (coerce coord-seq 'list) by #'cddr do
	    (climi::with-transformed-position (tr x y)
	      (gdk_draw_point dr gc (rc x) (rc y)))))))

(defmethod medium-draw-line* ((medium gdk-medium) x1 y1 x2 y2)
  (with-medium (medium)
    (let ((tr (sheet-native-transformation (medium-sheet medium))))
      (climi::with-transformed-position (tr x1 y1)
	(climi::with-transformed-position (tr x2 y2)
	  (gdk_draw_line (dr medium)
			 (gc medium t)
			 (rc x1)
			 (rc y1)
			 (rc x2)
			 (rc y2)))))))

(defmethod medium-draw-lines* ((medium gdk-medium) position-seq)
  (with-medium (medium)
    (let ((tr (sheet-native-transformation (medium-sheet medium)))
	  (dr (dr medium))
	  (gc (gc medium t)))
      (loop for (x1 y1 x2 y2) on (coerce position-seq 'list) by #'cddddr do
	    (climi::with-transformed-position (tr x1 y1)
	      (climi::with-transformed-position (tr x2 y2)
		(gdk_draw_line dr
			       gc
			       (rc x1)
			       (rc y1)
			       (rc x2)
			       (rc y2))))))))

(defun typed-pointer-+ (pointer type i)
  (cffi:inc-pointer pointer (* i (cffi:foreign-type-size type))))

(defmethod medium-draw-polygon*
    ((medium gdk-medium) xys closed filled)
  (climi::with-transformed-positions
      ((sheet-native-transformation (medium-sheet medium)) xys)
    (let ((n (truncate (length xys) 2))
	  (fixup (and closed (not filled)))
	  (dr (dr medium))
	  (gc (gc medium t)))
      (when fixup
	(incf n))
      (cffi:with-foreign-object (points 'gdkpoint n)
	(loop
	    for i from 0
	    for p = (typed-pointer-+ points 'gdkpoint i)
	    for (x y) on (coerce xys 'list) by #'cddr
	    do
	      (setf (cffi:foreign-slot-value p 'gdkpoint 'x) (rc x))
	      (setf (cffi:foreign-slot-value p 'gdkpoint 'y) (rc y)))
	(when fixup
	  (let ((q (typed-pointer-+ points 'gdkpoint (1- n))))
	    (setf (cffi:foreign-slot-value q 'gdkpoint 'x)
		  (cffi:foreign-slot-value points 'gdkpoint 'x))
	    (setf (cffi:foreign-slot-value q 'gdkpoint 'x)
		  (cffi:foreign-slot-value points 'gdkpoint 'y))))
	(if closed
	    (gdk_draw_polygon dr gc (if filled 1 0) points n)
	    (gdk_draw_lines dr gc points n))))))

(defmethod medium-draw-rectangle* ((medium gdk-medium) x1 y1 x2 y2 filled)
  (with-medium (medium)
    (let ((tr (sheet-native-transformation (medium-sheet medium))))
      (climi::with-transformed-position (tr x1 y1)
	(climi::with-transformed-position (tr x2 y2)
	  (gdk_draw_rectangle (dr medium)
			      (gc medium t)
			      (if filled 1 0)
			      (rc (min x1 x2))
			      (rc (min y1 y2))
			      (rd (- x2 x1))
			      (rd (- y2 y1))))))))

(defmethod medium-draw-rectangles*
    ((medium gdk-medium) position-seq filled)
  (with-medium (medium)
    (let ((tr (sheet-native-transformation (medium-sheet medium)))
	  (dr (dr medium))
	  (gc (gc medium t)))
      (loop for i below (length position-seq) by 4 do
	    (let ((x1 (df (elt position-seq (+ i 0))))
		  (y1 (df (elt position-seq (+ i 1))))
		  (x2 (df (elt position-seq (+ i 2))))
		  (y2 (df (elt position-seq (+ i 3)))))
	      (climi::with-transformed-position (tr x1 y1)
		(climi::with-transformed-position (tr x2 y2)
		  (gdk_draw_rectangle dr
				      gc
				      (if filled 1 0)
				      (rc (min x1 x2))
				      (rc (min y1 y2))
				      (rd (- x2 x1))
				      (rd (- y2 y1))))))))))

;; taken from clim-clx
(defmethod medium-draw-ellipse*
    ((medium gdk-medium)
     center-x center-y
     radius-1-dx radius-1-dy
     radius-2-dx radius-2-dy
     start-angle end-angle
     filled)
  (unless (or (= radius-2-dx radius-1-dy 0) (= radius-1-dx radius-2-dy 0))
    (error "MEDIUM-DRAW-ELLIPSE* not for non axis-aligned ellipses."))
  (with-medium (medium)
    (climi::with-transformed-position
	((sheet-native-transformation (medium-sheet medium))
	 center-x center-y)
      (let* ((start-angle (* start-angle #.(/ 11520 pi)))
	     (end-angle (* end-angle #.(/ 11520 pi)))
	     (radius-dx (abs (+ radius-1-dx radius-2-dx)))
	     (radius-dy (abs (+ radius-1-dy radius-2-dy)))
	     (min-x (round-coordinate (- center-x radius-dx)))
	     (min-y (round-coordinate (- center-y radius-dy)))
	     (max-x (round-coordinate (+ center-x radius-dx)))
	     (max-y (round-coordinate (+ center-y radius-dy))))
	(gdk_draw_arc (dr medium)
		      (gc medium t)
		      (if filled 1 0)
		      min-x
		      min-y
		      (- max-x min-x)
		      (- max-y min-y)
		      (round start-angle)
		      (round end-angle))))))

(defmethod invoke-with-pango-layout
    (fn (medium gdk-medium) &key text-style text)
  (with-pango-context (context nil)
    (let ((layout (pango_layout_new context)))
      (unwind-protect
	  (progn
	    (configure-pango-layout layout :text-style text-style :text text)
	    (funcall fn layout))
	(g_object_unref layout)))))

(defmethod medium-draw-text*
    ((medium gdk-medium) text x y start end
     align-x align-y toward-x toward-y transform-glyphs)
  (with-medium (medium)
    (setf end (or end (length text)))
    (unless (eql start end)
      (let ((tr (sheet-native-transformation (medium-sheet medium))))
	(climi::with-transformed-position (tr x y)
	  (with-pango-layout
	      (layout medium
		      :text-style medium
		      :text (subseq text start end))
	    (let ((y2
		   (nth-value 1
			      (pango-layout-line-get-pixel-extents layout 0))))
	      (gdk_draw_layout (dr medium)
			       (gc medium t)
			       (rc x)
			       (rc (+ y y2))
			       layout))))))))

(defmethod medium-finish-output ((medium gdk-medium))
  (medium-force-output medium))

(defmethod medium-force-output ((medium gdk-medium))
  (remhash medium (dirty-mediums (port medium)))
  (with-medium (medium)
    (when (slot-value medium 'gc)
      (invalidate-mirror (medium-mirror medium) (medium-sheet medium)))))

(defmethod medium-beep ((medium gdk-medium))
  ;; fixme: visual beep?
  )
