;;; -*- Mode: Lisp; Package: CLIM-CLX -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

(in-package :clim-clx)

(defgeneric X-pixel (port color))

(defmethod X-pixel ((port clx-basic-port) color)
  (let ((table (slot-value port 'color-table)))
    (or (gethash color table)
	(setf (gethash color table)
	      (multiple-value-bind (r g b) (color-rgb color)
		(xlib:alloc-color (xlib:screen-default-colormap
                                   (clx-port-screen port))
				  (xlib:make-color :red r :green g :blue b)))))))

;;; Needed changes:

;; The gc slot in clx-medium must be either thread local, or
;; [preferred] we should have a unified drawing options -> gcontext
;; cache.
;; --GB

;;; CLX-MEDIUM class

(defclass clx-medium (basic-medium)
  ((gc :initform nil)
   (picture :initform nil)
   (last-medium-device-region :initform nil
			      :accessor last-medium-device-region)
   (clipping-region-tmp :initform (vector 0 0 0 0)
     :documentation "This object is reused to avoid consing in the
 most common case when configuring the clipping region.")
   (clipping-pixmap-cache
    :initform (cons +everywhere+ nil)
    :documentation "This object stores cons of the last non-rectangular clipping
region and its clipping pixmap. This is looked up for optimization with region-equal."
    :accessor %clipping-pixmap-cache)
   (buffer :initform nil :accessor medium-buffer)))

#+CLX-EXT-RENDER
(defun clx-medium-picture (clx-medium)
  (with-slots (picture) clx-medium
    (or picture
        (setf picture
              (xlib:render-create-picture (port-lookup-mirror (port clx-medium) (medium-sheet clx-medium)))))))


;;; secondary methods for changing text styles and line styles

(defmethod (setf medium-text-style) :before (text-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-text-style (medium-text-style medium)))
	(unless (eq text-style old-text-style)
	  (setf (xlib:gcontext-font gc)
		(text-style-to-X-font (port medium) (medium-text-style medium))))))))

;;; Translate from CLIM styles to CLX styles.
(defconstant +cap-shape-map+ '((:butt . :butt)
			       (:square . :projecting)
			       (:round . :round)
			       (:no-end-point . :not-last)))

(defun translate-cap-shape (clim-shape)
  (let ((clx-shape (cdr (assoc clim-shape +cap-shape-map+))))
    (if clx-shape
	clx-shape
	(progn
	  (warn "Unknown cap style ~S, using :round" clim-shape)
	  :round))))

(defmethod (setf medium-line-style) :before (line-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-line-style (medium-line-style medium)))
	(unless (eql (line-style-thickness line-style)
		     (line-style-thickness old-line-style))
	  ;; this is kind of false, since the :unit should be taken
	  ;; into account -RS 2001-08-24
	  (setf (xlib:gcontext-line-width gc)
		(round (line-style-thickness line-style))))
	(unless (eq (line-style-cap-shape line-style)
		    (line-style-cap-shape old-line-style))
	  (setf (xlib:gcontext-cap-style gc)
		(translate-cap-shape (line-style-cap-shape line-style))))
	(unless (eq (line-style-joint-shape line-style)
		    (line-style-joint-shape old-line-style))
	  (setf (xlib:gcontext-join-style gc)
		(line-style-joint-shape line-style)))
	;; we could do better here by comparing elements of the vector
	;; -RS 2001-08-24
	(unless (eq (line-style-dashes line-style)
		    (line-style-dashes old-line-style))
	  (setf (xlib:gcontext-line-style gc)
		(if (line-style-dashes line-style) :dash :solid)
		(xlib:gcontext-dashes gc)
		(case (line-style-dashes line-style)
		  ((t nil) 3)
		  (otherwise (line-style-dashes line-style)))))))))

(defun %set-gc-clipping-region (medium gc)
  (declare (type clx-medium medium))
  (let ((clipping-region (medium-device-region medium))
        (tmp (slot-value medium 'clipping-region-tmp)))
    (cond
      ((region-equal clipping-region +nowhere+)
       (setf (xlib:gcontext-clip-mask gc) #()))
      ((typep clipping-region 'standard-rectangle)
       (multiple-value-bind (x1 y1 width height)
           (region->clipping-values clipping-region)
         (setf (aref tmp 0) x1
               (aref tmp 1) y1
               (aref tmp 2) width
               (aref tmp 3) height
               (xlib:gcontext-clip-mask gc :yx-banded) tmp)))
      ((typep clipping-region 'climi::standard-rectangle-set)
       (alexandria:when-let ((rect-seq (clipping-region->rect-seq clipping-region)))
         ;; What McCLIM is generating is not :yx-banded in the same
         ;; sense as CLX requires it. Use :unsorted until we fix it.
         #+ (or) (setf (xlib:gcontext-clip-mask gc :yx-banded) rect-seq)
         #- (or) (setf (xlib:gcontext-clip-mask gc :unsorted) rect-seq)))
      ((typep clipping-region 'standard-ellipse)
       (let ((last-clip (%clipping-pixmap-cache medium)))
         (if (region-equal clipping-region (car last-clip))
             (setf (xlib:gcontext-clip-mask gc :yx-banded) (cdr last-clip))
             (multiple-value-bind (x1 y1 width height)
                 (region->clipping-values (bounding-rectangle clipping-region))
               (let* ((drawable (sheet-xmirror (medium-sheet medium)))
                      (mask (xlib:create-pixmap :drawable drawable
                                                :depth 1
                                                :width (+ x1 width)
                                                :height (+ y1 height)))
                      (mask-gc (xlib:create-gcontext :drawable mask :foreground 1)))
                 (setf (xlib:gcontext-foreground mask-gc) 0)
                 (xlib:draw-rectangle mask mask-gc 0 0 (+ x1 width) (+ y1 height) t)
                 (setf (xlib:gcontext-foreground mask-gc) 1)
                 (flet ((%draw-lines (scan-line)
                          (map-over-region-set-regions
                           (lambda (reg)
                             (when (linep reg)
                               (multiple-value-bind (lx1 ly1) (line-start-point* reg)
                                 (multiple-value-bind (lx2 ly2) (line-end-point* reg)
                                   (xlib:draw-line mask mask-gc
                                                   (round-coordinate lx1)
                                                   (round-coordinate ly1)
                                                   (round-coordinate lx2)
                                                   (round-coordinate ly2))))))
                           scan-line)))
                   (if (<= width height)
                       (loop for x from x1 to (+ x1 width) do
                            (%draw-lines (region-intersection
                                          clipping-region
                                          (make-line* x y1 x (+ y1 height)))))
                       (loop for y from y1 to (+ y1 height) do
                            (%draw-lines (region-intersection
                                          clipping-region
                                          (make-line* x1 y (+ x1 width) y))))))
                 (setf (xlib:gcontext-clip-mask gc :yx-banded) mask))))))
      (t
       (let ((last-clip (%clipping-pixmap-cache medium)))
         (if (region-equal clipping-region (car last-clip))
             (setf (xlib:gcontext-clip-mask gc :yx-banded) (cdr last-clip))
             (multiple-value-bind (x1 y1 width height)
                 (region->clipping-values (bounding-rectangle clipping-region))
               (let* ((drawable (sheet-xmirror (medium-sheet medium)))
                      (mask (xlib:create-pixmap :drawable drawable
                                                :depth 1
                                                :width (+ x1 width)
                                                :height (+ y1 height)))
                      (mask-gc (xlib:create-gcontext :drawable mask :foreground 1)))
                 (setf (xlib:gcontext-foreground mask-gc) 0)
                 (xlib:draw-rectangle mask mask-gc 0 0 (+ x1 width) (+ y1 height) t)
                 (setf (xlib:gcontext-foreground mask-gc) 1)
                 (loop for x from x1 to (+ x1 width) do
                      (loop for y from y1 to (+ y1 height) do
                           (when (region-contains-position-p clipping-region x y)
                             (xlib:draw-point mask mask-gc x y))))
                 (setf (xlib:gcontext-clip-mask gc :yx-banded) mask)))))))))


(defgeneric medium-gcontext (medium ink))

(defmethod medium-gcontext :before ((medium clx-medium) ink)
  (let* ((port (port medium))
	 (mirror (port-lookup-mirror port (medium-sheet medium))))
    (with-slots (gc) medium
      (unless gc
        (setq gc (xlib:create-gcontext :drawable mirror))
        (and gc
             (setf (xlib:gcontext-fill-style gc) :solid))))))

(defmethod medium-gcontext ((medium clx-medium) (ink color))
  (declare (optimize (debug 3)))
  (let* ((port (port medium))
	 (mirror (port-lookup-mirror port (medium-sheet medium)))
	 (line-style (medium-line-style medium)))
    (with-slots (gc last-medium-device-region) medium
      (unless gc
	(setq gc (xlib:create-gcontext :drawable mirror))
	;; this is kind of false, since the :unit should be taken
	;; into account -RS 2001-08-24
	(setf (xlib:gcontext-line-width gc) (line-style-thickness line-style)
	      (xlib:gcontext-cap-style gc) (translate-cap-shape
					    (line-style-cap-shape line-style))
	      (xlib:gcontext-join-style gc) (line-style-joint-shape line-style))
	(let ((dashes (line-style-dashes line-style)))
	  (unless (null dashes)
	    (setf (xlib:gcontext-line-style gc) :dash
		  (xlib:gcontext-dashes gc) (if (eq dashes t) 3
						dashes)))))
      (setf (xlib:gcontext-function gc) boole-1)
      (setf (xlib:gcontext-foreground gc) (X-pixel port ink)
	    (xlib:gcontext-background gc) (X-pixel port (medium-background medium)))
      (let ((fn (text-style-to-X-font port (medium-text-style medium))))
        (when (typep fn 'xlib:font)
          (setf (xlib:gcontext-font gc) fn)))
      (unless (eq last-medium-device-region (medium-device-region medium))
        (setf last-medium-device-region (medium-device-region medium))
        (%set-gc-clipping-region medium gc))
      gc)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +transparent-ink+)))
  (let ((drawable (port-lookup-mirror (port medium) (medium-sheet medium))))
    (with-slots (gc) medium
      (or gc (setf gc (xlib:create-gcontext :drawable drawable))))))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +foreground-ink+)))
  (medium-gcontext medium (medium-foreground medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +background-ink+)))
  (medium-gcontext medium (medium-background medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +flipping-ink+)))
  (let* ((gc (medium-gcontext medium (medium-background medium)))
	 (port (port medium))
	 (flipper (logxor (X-pixel port (medium-foreground medium))
			  (X-pixel port (medium-background medium)))))
    ;; Now, (logxor flipper foreground) => background
    ;; (logxor flipper background) => foreground
    (setf (xlib:gcontext-function gc) boole-xor)
    (setf (xlib:gcontext-foreground gc) flipper)
    (setf (xlib:gcontext-background gc) flipper)
    gc))

;;; From Tagore Smith <tagore@tagoresmith.com>

(defmethod medium-gcontext ((medium clx-medium)
			    (ink climi::standard-flipping-ink))
  (let* ((gc (medium-gcontext medium (medium-background medium)))
	 (port (port medium))
	 (color1 (slot-value ink 'climi::design1))
	 (color2 (slot-value ink 'climi::design2))
	 (flipper (logxor (X-pixel port color1)
			  (X-pixel port color2))))
    (setf (xlib:gcontext-function gc) boole-xor)
    (setf (xlib:gcontext-foreground gc) flipper)
    (setf (xlib:gcontext-background gc) flipper)
    gc))

(defgeneric design-gcontext (medium ink))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::indexed-pattern))
  (multiple-value-bind (mx my)
      ;; For unmirrored sheet we need to apply the native transformation.
      ;; May be it is the wrong place to do it.
      (transform-position (sheet-native-transformation (medium-sheet medium)) 0 0)
    (let ((gc-x (round-coordinate mx))
	  (gc-y (round-coordinate my))
	  (gc (design-gcontext medium ink)))
      (setf (xlib:gcontext-ts-x gc) gc-x
	    (xlib:gcontext-ts-y gc) gc-y
	    (xlib:gcontext-clip-x gc) gc-x
	    (xlib:gcontext-clip-y gc) gc-y)
      gc)))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::rectangular-tile))
  (multiple-value-bind (mx my)
      ;; For unmirrored sheet we need to apply the native transformation.
      ;; May be it is the wrong place to do it.
      (transform-position (sheet-native-transformation (medium-sheet medium)) 0 0)
    (let ((gc-x (round-coordinate mx))
	  (gc-y (round-coordinate my))
	  (gc (design-gcontext medium ink)))
      (setf (xlib:gcontext-ts-x gc) gc-x
	    (xlib:gcontext-ts-y gc) gc-y
	    (xlib:gcontext-clip-x gc) gc-x
	    (xlib:gcontext-clip-y gc) gc-y)
      gc)))

;;;;

(defmethod design-gcontext :around ((medium clx-medium) (ink climi::indexed-pattern))
  (let ((design-cache (slot-value (port medium) 'design-cache)))
    (let ((cached (gethash ink design-cache)))
      (or cached
          (setf (gethash ink design-cache)
                (call-next-method))))))

(defun st3 (x y z)
  (values (logand (truncate (* x 255)) 255)
          (logand (truncate (* y 255)) 255)
          (logand (truncate (* z 255)) 255)))

(declaim (ftype (function (sequence)
                  (values (simple-array (unsigned-byte 8) 1)
                          (simple-array (unsigned-byte 8) 1)
                          (simple-array (unsigned-byte 8) 1)
                          (simple-array (unsigned-byte 8) 1)))
                inks-to-rgb))

(defun inks-to-rgb (inks)
  "Returns four values: byte arrays for the red, green, blue, and opacity components [0,255] of a sequence of inks"
  (let ((red-map (make-array (length inks) :element-type '(unsigned-byte 8)
                             :initial-element 255))
        (green-map (make-array (length inks) :element-type '(unsigned-byte 8)
                             :initial-element 0))
        (blue-map (make-array (length inks) :element-type '(unsigned-byte 8)
                             :initial-element 255))
        (opacity-map (make-array (length inks) :element-type '(unsigned-byte 8)
                             :initial-element 255))
        (length (length inks)))
    (loop for index from 0 below length
          as ink = (elt inks index)
          do (flet ((transform (parameter) (logand (truncate (* parameter 255)) 255)))
               (cond
                 ((colorp ink)
                  (multiple-value-bind (r g b) (color-rgb ink)
                    (setf (elt red-map   index) (transform r)
                          (elt green-map index) (transform g)
                          (elt blue-map  index) (transform b)
                          (elt opacity-map index) 255)))
                 ((eq ink +transparent-ink+)
                  (setf (elt opacity-map index) 0)))))
    (values red-map green-map blue-map opacity-map)))

(defun integer-count-bits (integer)
  (loop for i from 0 below (integer-length integer)
        sum (ldb (byte 1 i) integer)))

(defun compute-channel-fields (mask num-bytes)
  (loop with counted-bits = 0
        with output-width = (integer-count-bits mask)
        for index from (1- num-bytes) downto 0
        as submask = (ldb (byte 8 (* 8 index)) mask)
        as submask-bits = (integer-count-bits submask)
        as output-shift-left = (- (integer-length submask) submask-bits)
        as input-position = (+ (- 8 counted-bits submask-bits))
        collect (if (zerop submask)
                    nil
                    (prog1
                        (list output-shift-left submask-bits input-position)
                      (assert (<= output-width 8))
                      (incf counted-bits submask-bits)))))

(defun compute-channel-expressions (channel-mask-specs num-bytes)
  (labels ((single-channel-expressions (mask channel-name)
             (mapcar (lambda (fieldspec)
                       (and fieldspec
                            (destructuring-bind (output-shift-left submask-bits input-position)
                                fieldspec
                              `(ash (ldb (byte ,submask-bits ,input-position) ,channel-name) ,output-shift-left))))
                     (compute-channel-fields mask num-bytes) )))
    (reduce (lambda (left-exprs right-exprs)
              (mapcar (lambda (left-expr right-expr)
                        (if right-expr
                            (cons right-expr left-expr)
                            left-expr))
                      left-exprs
                      right-exprs))
            channel-mask-specs
            :key (lambda (channel-mask-spec)
                   (destructuring-bind (var-name mask) channel-mask-spec
                   (single-channel-expressions mask var-name)))
            :initial-value (map 'list #'identity (make-array num-bytes :initial-element nil)))))

(defun generate-pixel-assignments (array-var index-var channel-mask-specs num-bytes byte-order)
  `(setf ,@(mapcan (lambda (byte-exprs byte-index)
                     (and byte-exprs
                          (list `(elt ,array-var (+ ,index-var ,byte-index))
                                (if (= 1 (length byte-exprs))
                                    (first byte-exprs)
                                    `(logior ,@byte-exprs)))))
                   (compute-channel-expressions channel-mask-specs num-bytes)
                   (funcall (ecase byte-order
                              (:lsbfirst #'reverse)
                              (:msbfirst #'identity))
                            (loop for i from 0 below num-bytes collect i)))))

(defun generate-indexed-converter-expr (rgb-masks byte-order num-bytes)
  `(lambda (image-array converted-data mask-data width height inks)
    (declare (optimize (speed 3)
                       (safety 0)
                       (space 0)
                       (debug 0))
     (type xlib:card16 width height)
     (type (simple-array xlib:card8 1) converted-data mask-data))
    (macrolet ((conversion-body ()
                 `(let ((index 0)
                        (mask-index 0)
                        (mask-bitcursor 1))
                   (declare (type (unsigned-byte 9) mask-bitcursor)
                    (type xlib:array-index mask-index index))

                   (multiple-value-bind (red-map green-map blue-map opacity-map) (inks-to-rgb inks)
                     (dotimes (y height)
                       (unless (= 1 mask-bitcursor)
                         (setf mask-bitcursor 1
                               mask-index (1+ mask-index)))
                       (dotimes (x width)
                         (let ((ink-index (aref image-array y x)))
                           (when (< (elt opacity-map ink-index) #x40)  ; FIXME? Arbitrary threshold.
                             (setf (elt mask-data mask-index)
				   (logxor (elt mask-data mask-index)
					   mask-bitcursor)))
                           (let ((red   (elt red-map ink-index))
                                 (green (elt green-map ink-index))
                                 (blue  (elt blue-map ink-index)))
                             ,',(generate-pixel-assignments 'converted-data 'index
                                                            (mapcar #'list '(red green blue) rgb-masks)
                                                            num-bytes byte-order))
                           (setf index (+ ,',num-bytes index)
                                 mask-bitcursor (ash mask-bitcursor 1)
                                 mask-index (+ mask-index (ash mask-bitcursor -8))
                                 mask-bitcursor (logand (logior mask-bitcursor
                                                                (ash mask-bitcursor -8))
                                                        #xff)))))))))
      ;; We win big if we produce several specialized versions of this according
      ;; to the type of array holding the color indexes.
    (typecase image-array
      ((simple-array xlib:card8 2)      ; 256-color images
       (locally (declare (type (simple-array xlib:card8 2) image-array)) (conversion-body)))
      ((simple-array fixnum 2)          ; High-color index images (XPM reader produces these..)
       (locally (declare (type (simple-array fixnum 2) image-array)) (conversion-body)))
      (t (conversion-body))))))

(defun convert-indexed->mask (image-array mask-data width height inks)
  (declare (optimize (speed 3)
                     (safety 0)
                     (space 0)
                     (debug 0))
           (type xlib:card16 width height)
           (type (simple-array xlib:card8 1) mask-data))
  (macrolet ((conversion-body ()
              '(let ((mask-index 0)
                     (mask-bitcursor 1))
                 (declare (type (unsigned-byte 9) mask-bitcursor)
                          (type xlib:array-index mask-index))

                 (multiple-value-bind (red-map green-map blue-map opacity-map) (inks-to-rgb inks)
                   (declare (ignore red-map green-map blue-map))

                   (dotimes (y height)
                     (unless (= 1 mask-bitcursor)
                       (setf mask-bitcursor 1
                             mask-index (1+ mask-index)))
                     (dotimes (x width)
                       (let ((ink-index (aref image-array y x)))
                         (when (< (elt opacity-map ink-index) #x40)  ; FIXME? Arbitrary threshold.
                           (setf (elt mask-data mask-index) (logxor (elt mask-data mask-index) mask-bitcursor)))
                         (setf mask-bitcursor (ash mask-bitcursor 1)
                               mask-index (+ mask-index (ash mask-bitcursor -8))
                               mask-bitcursor (logand (logior mask-bitcursor
                                                              (ash mask-bitcursor -8))
                                                      #xff)))))))))
    ;; Again, we win big if we produce several specialized versions of this.
    (typecase image-array
      ((simple-array xlib:card8 2)      ; 256-color images
       (locally (declare (type (simple-array xlib:card8 2) image-array)) (conversion-body)))
      ((simple-array fixnum 2)          ; High-color index images (XPM reader produces these..)
       (locally (declare (type (simple-array fixnum 2) image-array)) (conversion-body)))
      (t (conversion-body)))))

(defparameter *pixel-converter-cache* (make-hash-table :test 'equal))

(defun ensure-indexed-converter (rgb-masks byte-order bytes-per-pixel)
  (let ((key (list rgb-masks byte-order bytes-per-pixel)))
    (symbol-macrolet ((fn (gethash key *pixel-converter-cache*)))
        (or fn (setf fn (compile nil (generate-indexed-converter-expr rgb-masks byte-order bytes-per-pixel)))))))

(defun visual-get-indexed-converter (visual-info byte-order bytes-per-pixel)
  (let ((rgb-masks (list (xlib:visual-info-red-mask visual-info)
                         (xlib:visual-info-green-mask visual-info)
                         (xlib:visual-info-blue-mask visual-info))))
    (ensure-indexed-converter rgb-masks byte-order bytes-per-pixel)))

(defparameter *typical-pixel-formats*
  '(((#xFF0000 #xFF00 #xFF) :LSBFIRST 4)
    ((#xFF0000 #xFF00 #xFF) :MSBFIRST 4))
  "This is a table of the most likely pixel formats. Converters for
these should be compiled in advance. Compiling the indexed->rgba
converter in advance will eliminate the pause observable the first
time an indexed pattern is drawn.")

(dolist (format *typical-pixel-formats*)
  (apply 'ensure-indexed-converter format))

(defun fill-pixmap-indexed (visual-info depth byte-order array pm pm-gc mask mask-gc w h inks)
  (assert (= (array-total-size array) (* w h)))
  (let* ((ceil-w-8 (ceiling w 8))
         (bytes-per-pixel
          (case depth
            ((24 32) 4)
            ((15 16) 2)
            (otherwise nil)))
         (mask-data (make-array (* ceil-w-8 h)
                                :element-type '(unsigned-byte 8)
                                :initial-element #xff))
         (pixel-converter nil))

    (if (and bytes-per-pixel
             (member byte-order '(:lsbfirst :msbfirst))
             (setf pixel-converter (visual-get-indexed-converter
                                    visual-info byte-order bytes-per-pixel)))
        ;; Fast path - Image upload
        (let ((converted-data (make-array (* bytes-per-pixel (array-total-size array)) :element-type 'xlib:card8)))
          ;; Fill the pixel arrays
          (funcall pixel-converter array converted-data mask-data w h inks)

          ;; Create an xlib "image" and copy it to our pixmap.
          ;; I do this because I'm not smart enough to operate xlib:put-raw-image.
          (let ((image (xlib:create-image :bits-per-pixel (* 8 bytes-per-pixel) :depth depth
                                          :width w :height h
                                          :format :z-pixmap
                                          :data converted-data)))
            (xlib:put-image (pixmap-xmirror pm) pm-gc image
                            :x 0 :y 0
                            :width w :height h)))

        ;; Fallback for unsupported visual, plotting pixels
        (progn
          (dotimes (y h)
            (dotimes (x w)
              (let ((ink (elt inks (aref array y x))))
                (unless (eq ink +transparent-ink+)
                  (draw-point* pm x y :ink ink)))))
          (convert-indexed->mask array mask-data w h inks)))

    ;; We can use image upload for the mask in either case.
    (let ((mask-image (xlib:create-image :bits-per-pixel 1 :depth 1
                                         :bit-lsb-first-p t
                                         :byte-lsb-first-p t
                                         :width w :height h
                                         :data mask-data)))
      (xlib:put-image mask mask-gc mask-image
                      :x 0 :y 0
                      :width w :height h))))

(defmethod design-gcontext ((medium clx-medium) (ink climi::indexed-pattern))
  (let* ((array (slot-value ink 'climi::array))
         (inks  (map 'vector
                     (lambda (ink)
                       (cond
                         ((eql ink +foreground-ink+) (medium-foreground medium))
                         ((eql ink +background-ink+) (medium-background medium))
                         ((eql ink +flipping-ink+)
                          (error "Flipping ink within patterns is not supported."))
                         (t ink)))
                     (slot-value ink 'climi::designs)))
         (w     (array-dimension array 1))
         (h     (array-dimension array 0)))
    (assert (not (zerop w)))
    (assert (not (zerop h)))

    ;; Establish color and mask pixmaps
    (let* ((display (clx-port-display (port medium)))
           (screen  (clx-port-screen  (port medium)))
           (drawable (port-lookup-mirror (port medium) (medium-sheet medium)))
           (pm   (allocate-pixmap (first (port-grafts (port medium))) w h))
           (mask (xlib:create-pixmap :drawable drawable
                                     :depth 1
                                     :width w
                                     :height h))
           (pm-gc (xlib:create-gcontext :drawable (pixmap-xmirror pm)))
           (mask-gc (xlib:create-gcontext :drawable mask :foreground 1)))

      (xlib:draw-rectangle mask mask-gc 0 0 w h t)
      (setf (xlib:gcontext-foreground mask-gc) 0)

      (let ((gc (xlib:create-gcontext :drawable drawable)))
        (setf (xlib:gcontext-fill-style gc) :tiled
              (xlib:gcontext-tile gc) (port-lookup-mirror (port pm) pm)
              (xlib:gcontext-clip-x gc) 0
              (xlib:gcontext-clip-y gc) 0
              (xlib:gcontext-ts-x gc) 0
              (xlib:gcontext-ts-y gc) 0
              (xlib:gcontext-clip-mask gc) mask)

        (let ((byte-order (xlib:display-byte-order display))
              ;; Hmm. Pixmaps are not windows, so you can't query their visual.
              ;; We'd like to draw to pixmaps as well as windows, so use the
              ;; depth and visual of the screen root, and hope this works.
              ;(visual-info (xlib:window-visual-info drawable))
              (visual-info (xlib:visual-info display (xlib:screen-root-visual screen)))
              (depth (xlib:screen-root-depth screen))
              (*print-base* 16))
          (fill-pixmap-indexed visual-info depth byte-order array pm pm-gc mask mask-gc w h inks))

        (xlib:free-gcontext mask-gc)
        (xlib:free-gcontext pm-gc)
        gc))))

(defmethod design-gcontext ((medium clx-medium) (ink climi::rectangular-tile))
  (let* ((design (slot-value ink 'climi::design))
         (w      (slot-value ink 'climi::width))
         (h      (slot-value ink 'climi::height)))
    (let ((pm (allocate-pixmap (first (port-grafts (port medium))) w h))) ;dito
      (draw-rectangle* pm 0 0 w h :ink design)
      (let ((gc (xlib:create-gcontext :drawable (port-lookup-mirror (port medium) (medium-sheet medium)))))
        (setf (xlib:gcontext-fill-style gc) :tiled
              (xlib:gcontext-tile gc) (port-lookup-mirror (port pm) pm)
              (xlib:gcontext-clip-x gc) 0
              (xlib:gcontext-clip-y gc) 0
              (xlib:gcontext-ts-x gc) 0
              (xlib:gcontext-ts-y gc) 0)
        gc))))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::transformed-design))
  (let ((transformation (climi::transformed-design-transformation ink))
        (design (climi::transformed-design-design ink)))
    (unless (translation-transformation-p transformation)
      (error "Sorry, not yet implemented."))
    ;; Bah!
    (typecase design
      ((or climi::indexed-pattern climi::rectangular-tile)
       (multiple-value-bind (tx ty)
	   (transform-position transformation 0 0)
	 (let ((gc-x (round-coordinate tx))
	       (gc-y (round-coordinate ty))
	       (gc (clim-clx::medium-gcontext medium design)))
	   (setf (xlib:gcontext-ts-x gc) (+ gc-x (xlib:gcontext-ts-x gc))
		 (xlib:gcontext-ts-y gc) (+ gc-y (xlib:gcontext-ts-y gc))
		 (xlib:gcontext-clip-x gc) (+ gc-x (xlib:gcontext-clip-x gc))
		 (xlib:gcontext-clip-y gc) (+ gc-y (xlib:gcontext-clip-y gc)))
	   gc)))
      (t
       (error "You lost, we not yet implemented transforming an ~S."
              (type-of ink))))))

;;;;

(defun region->clipping-values (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    (let ((clip-x (round-coordinate min-x))
          (clip-y (round-coordinate min-y)))
      (values clip-x
              clip-y
              (- (round-coordinate max-x) clip-x)
              (- (round-coordinate max-y) clip-y)))))

;;; This seems to work, but find out why all of these +nowhere+s are
;;; coming from and kill them at the source...
#-nil
(defun clipping-region->rect-seq (clipping-region)
  (typecase clipping-region
    (area (multiple-value-list (region->clipping-values clipping-region)))
    (t (loop
          for region in (nreverse (mapcan
                                   (lambda (v) (unless (eq v +nowhere+) (list v)))
                                   (region-set-regions clipping-region
                                                       :normalize :y-banding)))
          nconcing (multiple-value-list (region->clipping-values region))))))

(defmacro with-clx-graphics ((&optional (mirror 'mirror)
                                        (line-style 'line-style)
                                        (ink 'ink)
                                        (gcontext 'gc))
                                        medium &body body)
  (let ((medium-var (gensym)))
    `(let* ((,medium-var ,medium)
            (,mirror (sheet-xmirror (medium-sheet ,medium-var))))
       (when ,mirror
         (let* ((,line-style (medium-line-style ,medium-var))
                (,ink (medium-ink ,medium-var))
                (,gcontext (medium-gcontext ,medium-var ,ink)))
           (declare (ignorable ,line-style ,gcontext))
           (unless (eql ,ink +transparent-ink+)
             ,@body))))))


;;; Pixmaps
;;; width and height arguments should be integers, but we'll leave the calls
;;; to round in for now.

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (let* ((from-sheet (medium-sheet from-drawable))
	 (from-transformation (sheet-native-transformation from-sheet))
	 (to-sheet (medium-sheet to-drawable))
	 (to-transformation (sheet-native-transformation to-sheet)))
    (with-transformed-position (from-transformation from-x from-y)
      (with-transformed-position (to-transformation to-x to-y)
	(multiple-value-bind (width height)
	    (transform-distance (medium-transformation from-drawable)
				width height)
	  (xlib:copy-area (sheet-xmirror (medium-sheet from-drawable))
			  ;; why using the context of from-drawable?
			  (medium-gcontext from-drawable +background-ink+)
			  (round-coordinate from-x) (round-coordinate from-y)
			  (round width) (round height)
			  (or (medium-buffer to-drawable)
			      (sheet-xmirror (medium-sheet to-drawable)))
			  (round-coordinate to-x) (round-coordinate to-y)))))))

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (let* ((from-sheet (medium-sheet from-drawable))
	 (from-transformation (sheet-native-transformation from-sheet)))
    (with-transformed-position (from-transformation from-x from-y)
      (climi::with-pixmap-medium (to-medium to-drawable)
	(xlib:copy-area (sheet-xmirror (medium-sheet from-drawable))
			;; we can not use from-drawable
			(medium-gcontext to-medium +background-ink+)
			(round-coordinate from-x) (round-coordinate from-y)
			(round width) (round height)
			(pixmap-xmirror to-drawable)
			(round-coordinate to-x) (round-coordinate to-y))))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
                              to-x to-y)
    (xlib:copy-area (pixmap-xmirror from-drawable)
                    (medium-gcontext to-drawable +background-ink+)
                    (round-coordinate from-x) (round-coordinate from-y)
		    (round width) (round height)
                    (or (medium-buffer to-drawable) (sheet-xmirror (medium-sheet to-drawable)))
                    (round-coordinate to-x) (round-coordinate to-y))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (xlib:copy-area (pixmap-xmirror from-drawable)
                  (medium-gcontext (sheet-medium (slot-value to-drawable 'sheet))
                                   +background-ink+)
                  (round-coordinate from-x) (round-coordinate from-y)
                  (round width) (round height)
                  (pixmap-xmirror to-drawable)
                  (round-coordinate to-x) (round-coordinate to-y)))


;;; Medium-specific Drawing Functions

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-transformed-position ((sheet-native-transformation
                               (medium-sheet medium))
                              x y)
    (with-clx-graphics () medium
      (cond ((< (line-style-thickness line-style) 2)
             (let ((x (round-coordinate x))
                   (y (round-coordinate y)))
               (when (and (typep x '(signed-byte 16))
                          (typep y '(signed-byte 16)))
                 (xlib:draw-point mirror gc x y))))
            (t
             (let* ((radius (/ (line-style-thickness line-style) 2))
		    (min-x (round-coordinate (- x radius)))
		    (min-y (round-coordinate (- y radius)))
		    (max-x (round-coordinate (+ x radius)))
		    (max-y (round-coordinate (+ y radius))))
	       (when (and (typep min-x '(signed-byte 16))
			  (typep min-y '(signed-byte 16)))
                   (xlib:draw-arc mirror gc min-x min-y
				  (- max-x min-x) (- max-y min-y)
				  0 (* 2 pi) t))))))))


(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (with-transformed-positions ((sheet-native-transformation
                                (medium-sheet medium))
                               coord-seq)
    (with-clx-graphics () medium
      (cond ((< (line-style-thickness line-style) 2)
             (do-sequence ((x y) coord-seq)
               (let ((x (round-coordinate x))
                     (y (round-coordinate y)))
                 (when (and (typep x '(signed-byte 16))
                            (typep y '(signed-byte 16)))
                   (xlib:draw-point mirror gc x y)))))
            (t
             (let ((radius (/ (line-style-thickness line-style) 2)))
               (do-sequence ((x y) coord-seq)
                 (let ((min-x (round-coordinate (- x radius)))
		       (min-y (round-coordinate (- y radius)))
		       (max-x (round-coordinate (+ x radius)))
		       (max-y (round-coordinate (+ y radius))))
                   (when (and (typep min-x '(signed-byte 16))
                              (typep min-y '(signed-byte 16)))
                     (xlib:draw-arc mirror gc min-x min-y
				    (- max-x min-x) (- max-y min-y)
				    0 (* 2 pi) t))))))))))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (with-clx-graphics () medium
          (let ((x1 (round-coordinate x1))
                (y1 (round-coordinate y1))
                (x2 (round-coordinate x2))
                (y2 (round-coordinate y2)))
            (cond ((and (<= #x-8000 x1 #x7FFF) (<= #x-8000 y1 #x7FFF)
                        (<= #x-8000 x2 #x7FFF) (<= #x-8000 y2 #x7FFF))
                   (xlib:draw-line mirror gc x1 y1 x2 y2))
                  (t
                   (let ((line (region-intersection (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)
                                                    (make-line* x1 y1 x2 y2))))
                     (when (linep line)
                       (multiple-value-bind (x1 y1) (line-start-point* line)
                         (multiple-value-bind (x2 y2) (line-end-point* line)
                           (xlib:draw-line mirror gc
                                           (min #x7FFF (max #x-8000 (round-coordinate x1)))
                                           (min #x7FFF (max #x-8000 (round-coordinate y1)))
                                           (min #x7FFF (max #x-8000 (round-coordinate x2)))
                                           (min #x7FFF (max #x-8000 (round-coordinate y2))))))))))))))))

;; Invert the transformation and apply it here, as the :around methods on
;; transform-coordinates-mixin will cause it to be applied twice, and we
;; need to undo one of those. The transform-coordinates-mixin stuff needs
;; to be eliminated.
(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (do-sequence ((x1 y1 x2 y2) coord-seq)
        (medium-draw-line* medium x1 y1 x2 y2)))))

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq closed filled)
  ;; TODO:
  ;; . cons less
  ;; . clip
  (assert (evenp (length coord-seq)))
  (with-transformed-positions ((sheet-native-transformation
                                (medium-sheet medium))
                               coord-seq)
    (setq coord-seq (map 'vector #'round-coordinate coord-seq))
    (with-clx-graphics () medium
      (xlib:draw-lines mirror gc
                       (if closed
                           (concatenate 'vector
                                        coord-seq
                                        (vector (elt coord-seq 0)
                                                (elt coord-seq 1)))
                           coord-seq)
                       :fill-p filled))))

(defgeneric medium-draw-rectangle-using-ink*
    (medium ink left top right bottom filled))

(defmethod medium-draw-rectangle* ((medium clx-medium) left top right bottom filled)
  (medium-draw-rectangle-using-ink* medium (medium-ink medium)
                                    left top right bottom filled))

(defmethod medium-draw-rectangle-using-ink* ((medium clx-medium) (ink t) left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
        (with-clx-graphics () medium
          (if (< right left)
              (rotatef left right))
          (if (< bottom top)
              (rotatef top bottom))
          (let ((left   (round-coordinate left))
                (top    (round-coordinate top))
                (right  (round-coordinate right))
                (bottom (round-coordinate bottom)))
            ;; To clip rectangles, we just need to clamp the
	    ;; coordinates
            (xlib:draw-rectangle mirror gc
                                 (max #x-8000 (min #x7FFF left))
                                 (max #x-8000 (min #x7FFF top))
                                 (max 0 (min #xFFFF (- right left)))
                                 (max 0 (min #xFFFF (- bottom top)))
                                 filled)))))))

#+CLX-EXT-RENDER
(defmethod medium-draw-rectangle-using-ink* ((medium clx-medium) (ink climi::uniform-compositum)
                                             x1 y1 x2 y2 filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (multiple-value-bind (r g b) (color-rgb (slot-value ink 'climi::ink))
            (let ((a (opacity-value (slot-value ink 'climi::mask))))
              ;; Hmm, XRender uses pre-multiplied alpha, how useful!
              (setf r (min #xffff (max 0 (round (* #xffff a r))))
                    g (min #xffff (max 0 (round (* #xffff a g))))
                    b (min #xffff (max 0 (round (* #xffff a b))))
                    a (min #xffff (max 0 (round (* #xffff a)))))
              (let ((picture (clx-medium-picture medium)))
		(xlib::%render-change-picture-clip-rectangles picture
							      (clipping-region->rect-seq
							       (last-medium-device-region medium)))
                (xlib:render-fill-rectangle picture :over (list r g b a)
                                            (max #x-8000 (min #x7FFF x1))
                                            (max #x-8000 (min #x7FFF y1))
                                            (max 0 (min #xFFFF (- x2 x1)))
                                            (max 0 (min #xFFFF (- y2 y1))))))))))))


(defmethod medium-draw-rectangles* ((medium clx-medium) position-seq filled)
  (assert (evenp (length position-seq)))
  (with-transformed-positions ((sheet-native-transformation
				(medium-sheet medium))
                               position-seq)
    (with-clx-graphics () medium
      (let ((points (make-array 4 :fill-pointer 0)))
        (do-sequence ((left top right bottom) position-seq)
          (let ((min-x (round-coordinate left))
                (max-x (round-coordinate right))
                (min-y (round-coordinate top))
                (max-y (round-coordinate bottom)))
            (vector-push-extend min-x points)
            (vector-push-extend min-y points)
            (vector-push-extend (- max-x min-x) points)
            (vector-push-extend (- max-y min-y) points)))
        (xlib:draw-rectangles mirror gc points filled)))))

(defun %draw-rotated-ellipse (medium center-x center-y
                              radius-1-dx radius-1-dy
                              radius-2-dx radius-2-dy
                              start-angle end-angle filled)
  (let ((ellipse (make-ellipse* center-x center-y
                                radius-1-dx radius-1-dy
                                radius-2-dx radius-2-dy
                                :start-angle start-angle :end-angle end-angle)))
    (with-clx-graphics () medium
      (multiple-value-bind (x1 y1 width height)
          (region->clipping-values (bounding-rectangle ellipse))
        (labels ((ellipse-border-p (ellipse x-orig y-orig)
                   (with-slots (climi::tr climi::start-angle climi::end-angle) ellipse
                     (multiple-value-bind (x y) (untransform-position climi::tr x-orig y-orig)
                       (and (<= (- 1.0 .05) (+ (* x x) (* y y)) (+ 1.0 .05))
                            (or (null climi::start-angle)
                                (climi::%angle-between-p
                                 (climi::%ellipse-position->angle ellipse x-orig y-orig)
                                 climi::start-angle climi::end-angle))))))
                 (draw-point (x y)
                   (if (< (line-style-thickness line-style) 2)
                       (let ((x (round-coordinate x))
                             (y (round-coordinate y)))
                         (xlib:draw-point mirror gc x y))
                       (let* ((radius (/ (line-style-thickness line-style) 2))
                              (min-x (round-coordinate (- x radius)))
                              (min-y (round-coordinate (- y radius)))
                              (max-x (round-coordinate (+ x radius)))
                              (max-y (round-coordinate (+ y radius))))
                         (xlib:draw-arc mirror gc min-x min-y
                                        (- max-x min-x) (- max-y min-y)
                                        0 (* 2 pi) t))))
                 (maybe-draw-border-points (line)
                   (multiple-value-bind (lx1 ly1) (line-start-point* line)
                     (when (ellipse-border-p ellipse lx1 ly1) (draw-point lx1 ly1)))
                   (multiple-value-bind (lx2 ly2) (line-end-point* line)
                     (when (ellipse-border-p ellipse lx2 ly2) (draw-point lx2 ly2))))
                 (draw-line-1 (line)
                   (multiple-value-bind (lx1 ly1) (line-start-point* line)
                     (multiple-value-bind (lx2 ly2) (line-end-point* line)
                       (xlib:draw-line mirror gc
                                       (round-coordinate lx1)
                                       (round-coordinate ly1)
                                       (round-coordinate lx2)
                                       (round-coordinate ly2)))))
                 (draw-lines (scan-line)
                   (cond
                     ((region-equal scan-line +nowhere+))
                     (filled (map-over-region-set-regions #'draw-line-1 scan-line))
                     (t (map-over-region-set-regions #'maybe-draw-border-points scan-line)))))
          ;; O(n+m) because otherwise we may skip some points (better drawing quality)
          (progn ;if (<= width height)
            (loop for x from x1 to (+ x1 width) do
                 (draw-lines (region-intersection
                              ellipse
                              (make-line* x y1 x (+ y1 height)))))
            (loop for y from y1 to (+ y1 height) do
                 (draw-lines (region-intersection
                              ellipse
                              (make-line* x1 y (+ x1 width) y))))))))))

;;; Round the parameters of the ellipse so that it occupies the expected pixels
(defmethod medium-draw-ellipse* ((medium clx-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (if (or (= radius-2-dx radius-1-dy 0) (= radius-1-dx radius-2-dy 0))
      (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                                  center-x center-y)
        (let* ((arc-angle (- end-angle start-angle))
               (arc-angle (if (< arc-angle 0)
                              (+ (* pi 2) arc-angle)
                              arc-angle)))
          (with-clx-graphics () medium
            (let* ((radius-dx (abs (+ radius-1-dx radius-2-dx)))
                   (radius-dy (abs (+ radius-1-dy radius-2-dy)))
                   (min-x (round-coordinate (- center-x radius-dx)))
                   (min-y (round-coordinate (- center-y radius-dy)))
                   (max-x (round-coordinate (+ center-x radius-dx)))
                   (max-y (round-coordinate (+ center-y radius-dy))))
              (xlib:draw-arc mirror gc
                             min-x min-y (- max-x min-x) (- max-y min-y)
                             (mod start-angle (* 2 pi)) arc-angle
                             filled)))))
      ;; Implementation scans for vertial or horizontal lines to get the
      ;; intersection. That is O(n), which is much better than naive O(n2).
      (%draw-rotated-ellipse medium center-x center-y
                             radius-1-dx radius-1-dy
                             radius-2-dx radius-2-dy
                             start-angle end-angle filled)))

(defmethod medium-draw-circle* ((medium clx-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (with-transformed-position ((sheet-native-transformation (medium-sheet
							    medium))
                              center-x center-y)
    (let* ((arc-angle (- end-angle start-angle))
           (arc-angle (if (< arc-angle 0)
                          (+ (* pi 2) arc-angle)
                          arc-angle))
	   (min-x (round-coordinate (- center-x radius)))
	   (min-y (round-coordinate (- center-y radius)))
	   (max-x (round-coordinate (+ center-x radius)))
	   (max-y (round-coordinate (+ center-y radius))))
      (with-clx-graphics () medium
        (xlib:draw-arc mirror gc
                       min-x min-y
                       (- max-x min-x) (- min-y max-y)
                       start-angle arc-angle
                       filled)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for text styles

(defmethod text-style-ascent (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (font-ascent font)))

(defmethod text-style-descent (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (font-descent font)))

(defmethod text-style-height (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (+ (font-ascent font) (font-descent font))))

(defmethod text-style-character-width (text-style (medium clx-medium) char)
  (font-glyph-width (text-style-to-X-font (port medium) text-style) char))

(defmethod text-style-width (text-style (medium clx-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-style-fixed-width-p (text-style (medium clx-medium))
  (eql (text-style-family text-style) :fix))

(eval-when (:compile-toplevel :execute)
  ;; ASCII / CHAR-CODE compatibility checking
  (unless (equal (mapcar #'char-code '(#\Backspace #\Tab #\Linefeed
                                       #\Page #\Return #\Rubout))
                 '(8 9 10 12 13 127))
    (error "~S not ASCII-compatible for semi-standard characters: ~
           implement a CLX translate function for this implementation."
           'code-char))
  (let ((standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
    (dotimes (i 95)
      (unless (eql (char standard-chars i) (code-char (+ i 32)))
        (error "~S not ASCII-compatible for standard character ~S: ~
                implement a CLX translate function for this implementation."
               'code-char (code-char (+ i 32)))))))

;;; The default CLX translation function is defined to work only for
;;; ASCII characters; quoting from the documentation,
;;;
;;;   The default :translate function handles all characters that
;;;   satisfy graphic-char-p by converting each character into its
;;;   ASCII code.
;;;
;;; We provide our own translation function which is essentially the
;;; same as that of CLX, but with the ASCII restriction relaxed.  This
;;; is by no means a proper solution to the problem of
;;; internationalization, because fonts tend not to have a complete
;;; coverage of the entirety of the Unicode space, even assuming that
;;; the underlying lisp supports it (this is the case at least for SBCL,
;;; CLISP and CCL); instead, the translation function is meant to
;;; handle font sets by requesting the X server change fonts in the
;;; middle of rendering strings.  However, the below stands a chance
;;; of working when using ISO-8859-1-encoded fonts, and will tend to
;;; lose in other cases.
(defun translate (src src-start src-end afont dst dst-start)
  (declare (type sequence src)
	   (type xlib:array-index src-start src-end dst-start)
	   (type (or null xlib:font) afont)
	   (type vector dst))
  ;; FIXME: what if AFONT is null?
  (let ((min-char-index (xlib:font-min-char afont))
	(max-char-index (xlib:font-max-char afont)))
    (if (stringp src)
	(do ((i src-start (xlib::index+ i 1))
	     (j dst-start (xlib::index+ j 1))
	     (char))
	    ((xlib::index>= i src-end)
	     i)
          (declare (type xlib:array-index i j))
          (setq char (char-code (char src i)))
          (if (or (< char min-char-index) (> char max-char-index))
              (progn
                (warn "Character ~S not representable in font ~S"
                      (char src i) afont)
                (return i))
              (setf (aref dst j) char)))
        (do ((i src-start (xlib::index+ i 1))
	     (j dst-start (xlib::index+ j 1))
	     (elt))
	    ((xlib::index>= i src-end)
	     i)
          (declare (type xlib:array-index i j))
          (setq elt (elt src i))
          (when (characterp elt)
            (setq elt (char-code elt)))
          (if (or (not (integerp elt))
                  (< elt min-char-index)
                  (> elt max-char-index))
              (progn
                (warn "Thing ~S not representable in font ~S"
                      (elt src i) afont)
                (return i))
              (setf (aref dst j) elt))))))

(defmethod text-size ((medium clx-medium) string
                      &key text-style (start 0) end)
  (declare (optimize (speed 3)))
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (check-type string string)

  (unless end (setf end (length string)))
  (check-type start (integer 0 #.array-dimension-limit))
  (check-type end (integer 0 #.array-dimension-limit))

  (when (= start end)
    (return-from text-size (values 0 0 0 0 0)))

  (let* ((medium-text-style (medium-merged-text-style medium))
	 (text-style (if text-style
			 (merge-text-styles text-style medium-text-style)
			 medium-text-style))
	 (xfont (text-style-to-X-font (port medium) text-style))
	 (position-newline
          (macrolet ((p (type)
                       `(locally (declare (type ,type string))
                          (position #\newline string :start start :end end))))
            (typecase string
              (simple-base-string (p simple-base-string))
              #+SBCL (sb-kernel::simple-character-string (p sb-kernel::simple-character-string))
              #+SBCL (sb-kernel::character-string (p sb-kernel::character-string))
              (simple-string (p simple-string))
              (string (p string))))))
    (cond ((not (null position-newline))
           (multiple-value-bind (width ascent descent left right
                                       font-ascent font-descent direction
                                       first-not-done)
               (font-text-extents xfont string
                                  :start start :end position-newline
                                  :translate #'translate)
             (declare (ignorable left right
                                 font-ascent font-descent
                                 direction first-not-done))
             (multiple-value-bind (w h x y baseline)
                 (text-size medium string :text-style text-style
                            :start (1+ position-newline) :end end)
               (values (max w width) (+ ascent descent h)
                       x (+ ascent descent y) (+ ascent descent baseline)))))
          (t
           (multiple-value-bind (width ascent descent left right
                                       font-ascent font-descent direction
                                       first-not-done)
               (font-text-extents xfont string
                                  :start start :end end
                                  :translate #'translate)
             (declare (ignorable left right
                                 font-ascent font-descent
                                 direction first-not-done))
             (values width (+ ascent descent) width 0 ascent))))))

(defmethod climi::text-bounding-rectangle*
    ((medium clx-medium) string &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-X-font (port medium) text-style)))
    (cond ((= start end)
           (values 0 0 0 0))
          (t
           (let ((position-newline (position #\newline string :start start :end end)))
             (cond ((not (null position-newline))
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (font-text-extents xfont string
                                           :start start :end position-newline
                                           :translate #'translate)
                      (declare (ignorable width left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (multiple-value-bind (minx miny maxx maxy)
                          (climi::text-bounding-rectangle*
                           medium string :text-style text-style
                           :start (1+ position-newline) :end end)
			(declare (ignore miny))
                        (values (min minx left) (- ascent)
                                (max maxx right) (+ descent maxy)))))
                   (t
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (font-text-extents
                         xfont string :start start :end end :translate #'translate)
                      (declare (ignore width ascent descent)
			       (ignore direction first-not-done))
                      ;; FIXME: Potential style points:
                      ;; * (min 0 left), (max width right)
                      ;; * font-ascent / ascent
                      (values left (- font-ascent) right font-descent)))))))))

(defvar *draw-font-lock* (climi::make-lock "draw-font"))
(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              transformation)
  (declare (ignore toward-x toward-y transform-glyphs))
  (let* ((native-transform (sheet-native-transformation (medium-sheet medium)))
         (merged-transform (clim:compose-transformations native-transform transformation)))
    (with-clx-graphics () medium
      (when (characterp string)
        (setq string (make-string 1 :initial-element string)))
      (if (null end)
          (setq end (length string))
          (setq end (min end (length string))))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
          (text-size medium string :start start :end end)
        (declare (ignore x-cursor y-cursor))
        (unless (and (eq align-x :left) (eq align-y :baseline))
          (setq x (- x (ecase align-x
                         (:left 0)
                         (:center (round text-width 2))
                         (:right text-width))))
          (setq y (ecase align-y
                    (:top (+ y baseline))
                    (:center (+ y baseline (- (floor text-height 2))))
                    (:baseline y)
                    (:bottom (+ y baseline (- text-height)))))))
      (let ((x (round-coordinate x))
            (y (round-coordinate y)))
        (bt:with-lock-held (*draw-font-lock*)
          (font-draw-glyphs
           (text-style-to-X-font (port medium) (medium-text-style medium))
           mirror gc x y string
           #| x (- y baseline) (+ x text-width) (+ y (- text-height baseline )) |#
           :start start :end end
           :translate #'translate :size 16 :transformation merged-transform))))))

(defmethod medium-buffering-output-p ((medium clx-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium clx-medium))
  buffer-p)

(defmethod medium-draw-glyph ((medium clx-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs align-x align-y))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-clx-graphics () medium
      (xlib:draw-glyph mirror gc (round-coordinate x) (round-coordinate y)
		       element
                       :size 16
                       :translate #'translate))))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium clx-medium))
  (xlib:display-finish-output (clx-port-display (port medium))))

(defmethod medium-force-output ((medium clx-medium))
  (xlib:display-force-output (clx-port-display (port medium))))

(defmethod medium-clear-area ((medium clx-medium) left top right bottom)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
	(let ((min-x (round-coordinate (min left right)))
	      (min-y (round-coordinate (min top bottom)))
	      (max-x (round-coordinate (max left right)))
	      (max-y (round-coordinate (max top bottom))))
	  (xlib:draw-rectangle (or (medium-buffer medium)
				   (port-lookup-mirror (port medium)
						       (medium-sheet medium)))
			       (medium-gcontext medium (medium-background medium))
			       (max #x-8000 (min #x7fff min-x))
			       (max #x-8000 (min #x7fff min-y))
			       (max 0 (min #xffff (- max-x min-x)))
			       (max 0 (min #xffff (- max-y min-y)))
			       t))))))

(defmethod medium-beep ((medium clx-medium))
  (xlib:bell (clx-port-display (port medium))))

;;;;

; With-double-buffering is broken, so I remove it for now - BTS
#+nil
(defmethod invoke-with-special-choices (continuation (medium clx-medium))
  (let ((sheet (medium-sheet medium)))
    (with-double-buffering (sheet)
      (funcall continuation (sheet-medium sheet)))))

(defmethod invoke-with-special-choices (continuation (medium clx-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

;;;;

(defmethod medium-miter-limit ((medium clx-medium))
  #.(* pi (/ 11 180)))

(defmethod climi::medium-invoke-with-possible-double-buffering (frame pane (medium clx-medium) continuation)
  (funcall continuation))

;;;  This hack is really ugly. There really should be a better way to
;;;  handle this.
(defmethod (setf medium-text-style) :before (text-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-text-style (medium-text-style medium)))
	(unless (eq text-style old-text-style)
          (let ((fn (text-style-to-X-font (port medium) (medium-text-style medium))))
            (when (typep fn 'xlib:font)
              (setf (xlib:gcontext-font gc)
                    fn))))))))
