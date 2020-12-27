(in-package :clim-gtk)

(defmacro with-medium-mirror ((mirror-sym medium) &body body)
  (check-type mirror-sym symbol)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-copy-sym)
      `(let ((,mirror-copy-sym (climi::port-lookup-mirror (port ,medium) (medium-sheet ,medium))))
         (unless ,mirror-copy-sym
           (break))
         (when ,mirror-copy-sym
           (let ((,mirror-sym ,mirror-copy-sym))
             ,@body))))))

(defun update-mirror-if-needed (mirror)
  (let ((requested-width (gtk-mirror/requested-image-width mirror))
        (requested-height (gtk-mirror/requested-image-height mirror)))
    (when (and requested-width requested-height)
      (cairo:cairo-surface-destroy (gtk-mirror/image mirror))
      (setf (gtk-mirror/image mirror) (cairo:cairo-image-surface-create :argb32 requested-width requested-height))
      (setf (gtk-mirror/requested-image-width mirror) nil)
      (setf (gtk-mirror/requested-image-height mirror) nil))))

(defmacro with-medium-cairo-image ((image-sym medium) &body body)
  (check-type image-sym symbol)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-sym image)
      `(with-medium-mirror (,mirror-sym ,medium)
         (let ((,image (bordeaux-threads:with-lock-held ((gtk-mirror/lock ,mirror-sym))
                         (update-mirror-if-needed ,mirror-sym)
                         (let ((,image (gtk-mirror/image ,mirror-sym)))
                           (cairo:cairo-surface-reference ,image)
                           ,image))))
           (unwind-protect
                (let ((,image-sym ,image))
                  ,@body)
             (cairo:cairo-surface-destroy (gtk-mirror/image ,mirror-sym))))))))

(defun region->clipping-values (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    ;; We don't use here round-coordinate because clipping rectangle
    ;; must cover the whole region. It is especially important when we
    ;; draw arcs (ellipses without filling) which are not drawn if any
    ;; part is outside the clipped area. -- jd 2019-06-17
    (let ((clip-x (floor min-x))
          (clip-y (floor min-y)))
      (values clip-x
              clip-y
              (- (ceiling max-x) clip-x)
              (- (ceiling max-y) clip-y)))))

(defun clipping-region->rect-seq (clipping-region)
  (typecase clipping-region
    (area (multiple-value-list (region->clipping-values clipping-region)))
    (t (loop
         for region in (nreverse (mapcan
                                  (lambda (v) (unless (eq v +nowhere+) (list v)))
                                  (region-set-regions clipping-region :normalize :y-banding)))
         nconcing (multiple-value-list (region->clipping-values region))))))

(defun set-clipping-region (cr medium)
  (cairo:cairo-reset-clip cr)
  (let ((clipping-region (climi::medium-device-region medium)))
    (typecase clipping-region
      (climi::nowhere-region
       (cairo:cairo-rectangle cr 0 0 1 1)
       (cairo:cairo-clip cr))
      (clim:standard-rectangle
       (multiple-value-bind (x1 y1 width height)
           (region->clipping-values clipping-region)
         (cairo:cairo-rectangle cr x1 y1 width height)
         (cairo:cairo-clip cr)))
      (climi::standard-rectangle-set
       (let ((se (clipping-region->rect-seq clipping-region)))
         (loop
           for (x y width height) on se by (lambda (sequence) (nthcdr 4 sequence))
           do (cairo:cairo-rectangle cr x y width height)
           finally (cairo:cairo-clip cr))))
      (t
       (break)))))

(defun apply-transformation-to-context (cr tr)
  (unless (eq tr 'clim:+identity-transformation+)
    (multiple-value-bind (mxx mxy myx myy tx ty)
        (climi::get-transformation tr)
      (cffi:with-foreign-object (matrix '(:struct cairo::cairo-matrix-t))
        (setf (cffi:foreign-slot-value matrix '(:struct cairo::cairo-matrix-t) 'cairo::xx) (coerce mxx 'double-float))
        (setf (cffi:foreign-slot-value matrix '(:struct cairo::cairo-matrix-t) 'cairo::xy) (coerce mxy 'double-float))
        (setf (cffi:foreign-slot-value matrix '(:struct cairo::cairo-matrix-t) 'cairo::yx) (coerce myx 'double-float))
        (setf (cffi:foreign-slot-value matrix '(:struct cairo::cairo-matrix-t) 'cairo::yy) (coerce myy 'double-float))
        (setf (cffi:foreign-slot-value matrix '(:struct cairo::cairo-matrix-t) 'cairo::x0) (coerce tx 'double-float))
        (setf (cffi:foreign-slot-value matrix '(:struct cairo::cairo-matrix-t) 'cairo::y0) (coerce ty 'double-float))
        (cairo:cairo-set-matrix cr matrix)))))

(defgeneric context-apply-ink (cr ink))

(defun apply-colour-from-ink (cr ink)
  (check-type ink clim:color)
  (multiple-value-bind (red green blue alpha)
       (clime::color-rgba ink)
    (cairo:cairo-set-source-rgba cr red green blue alpha)))

(defun resolve-indirect-ink (ink)
  (etypecase ink
    (clim:color ink)
    (clime:indirect-ink (clime:indirect-ink-ink ink))))

(defmethod context-apply-ink (cr (ink clim:color))
  (apply-colour-from-ink cr ink))

(defmethod context-apply-ink (cr (ink climi::standard-flipping-ink))
  (apply-colour-from-ink cr (resolve-indirect-ink (slot-value ink 'climi::design1)))
  (cairo:cairo-set-operator cr :xor))

(defmethod context-apply-ink (cr (ink clime:indirect-ink))
  (context-apply-ink cr (clime:indirect-ink-ink ink)))

(defun update-attrs (cr medium)
  (set-clipping-region cr medium)
  (context-apply-ink cr (medium-ink medium))
  (let ((line-style (medium-line-style medium)))
    (cairo:cairo-set-line-width cr (line-style-thickness line-style))
    (cairo:cairo-set-line-join cr (ecase (line-style-joint-shape line-style)
                                    (:miter :miter)
                                    (:round :round)
                                    (:bevel :bevel)
                                    (:none :miter)))))

(defmacro with-cairo-context ((context-sym medium &key (update-style t)) &body body)
  (check-type context-sym symbol)
  (alexandria:once-only (medium update-style)
    (alexandria:with-gensyms (image-sym context)
      `(with-medium-cairo-image (,image-sym ,medium)
         (let ((,context (cairo:cairo-create ,image-sym)))
           (unwind-protect
                (progn
                  (when ,update-style
                    (update-attrs ,context ,medium))
                  (let ((,context-sym ,context))
                    ,@body))
             (cairo:cairo-destroy ,context)))))))

(defun call-with-cairo-context-measure (medium fn)
  (let ((image (alexandria:if-let ((mirror (climi::port-lookup-mirror (port medium) (medium-sheet medium))))
                 (bordeaux-threads:with-lock-held ((gtk-mirror/lock mirror))
                   (let ((image (gtk-mirror/image mirror)))
                     (cairo:cairo-surface-reference image)
                     image))
                 ;; ELSE: No mirror, use the dedicated image
                 (let ((image (gtk-port/image-fallback (port medium))))
                   (cairo:cairo-surface-reference image)
                   image))))
    (unwind-protect
         (let ((context (cairo:cairo-create image)))
           (unwind-protect
                (funcall fn context)
             (cairo:cairo-destroy context)))
      (cairo:cairo-surface-destroy image))))

(defmacro with-cairo-context-measure ((context-sym medium) &body body)
  (check-type context-sym symbol)
  (alexandria:once-only (medium)
    `(call-with-cairo-context-measure ,medium (lambda (,context-sym) ,@body))))

(defclass gtk-medium (font-rendering-medium-mixin basic-medium)
  ((buffering-output-p :accessor medium-buffering-output-p)))

(defclass gtk-medium-font ()
  ((font-description :initarg :font-description
                     :reader gtk-medium-font/font-description)))

(defun set-current-font (layout font)
  (pango:pango-layout-set-font-description layout (gtk-medium-font/font-description font)))

(defmethod climi::open-font ((port gtk-port) font-designator)
  (break)
  (make-instance 'gtk-medium-font))

(defmethod text-style-mapping ((port gtk-port) (text-style text-style) &optional character-set)
  (declare (ignore character-set))
  (multiple-value-bind (family face size)
      (text-style-components text-style)
    (let ((desc (pango:pango-font-description-new)))
      (cond
        ((stringp family)
         (pango:pango-font-description-set-family desc family))
        ((eq family :fix)
         (pango:pango-font-description-set-family desc "Monospace"))
        ((eq family :sans-serif)
         (pango:pango-font-description-set-family desc "DejaVu Sans"))
        ((eq family :serif)
         (pango:pango-font-description-set-family desc "DejaVu Serif"))
        (t (pango:pango-font-description-set-family desc "DejaVu Sans")))
      (dolist (f (alexandria:ensure-list face))
        (case f
          (:roman
           (pango:pango-font-description-set-style desc :normal))
          (:italic
           (pango:pango-font-description-set-style desc :italic))
          (:bold
           (pango:pango-font-description-set-weight desc :bold))
          ((t)
           nil)))
      (when size
        (let ((size-num (coerce (* (climb:normalize-font-size size) pango:+pango-scale+) 'double-float)))
          (pango:pango-font-description-set-absolute-size desc size-num)))
      (make-instance 'gtk-medium-font :font-description desc))))

(defmethod (setf text-style-mapping) (font-name
                                      (port gtk-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  (error "Can't set mapping"))

(defmethod (setf medium-text-style) :before (text-style (medium gtk-medium))
  (declare (ignore text-style))
  nil)

(defmethod (setf medium-line-style) :before (line-style (medium gtk-medium))
  (declare (ignore line-style))
  nil)

(defmethod (setf medium-clipping-region) :after (region (medium gtk-medium))
  (declare (ignore region))
  nil)

(defmethod medium-copy-area ((from-drawable gtk-medium)
			     from-x from-y width height
                             (to-drawable gtk-medium)
			     to-x to-y)
  (declare (ignore from-x from-y width height to-x to-y))
  nil)

#+nil ; FIXME: PIXMAP class
(progn
  (defmethod medium-copy-area ((from-drawable gtk-medium)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil)

  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable gtk-medium)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil)

  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil))

(defmethod text-style-ascent (text-style (medium gtk-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-descent (text-style (medium gtk-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-height (text-style (medium gtk-medium))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-character-width (text-style (medium gtk-medium) char)
  (declare (ignore text-style char))
  1)

(defmethod text-style-width (text-style (medium gtk-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size ((medium gtk-medium) string &key text-style (start 0) end)
  (setf string (etypecase string
		 (character (string string))
		 (string string)))
  (let ((fixed-string (subseq string (or start 0) (or end (length string)))))
    (with-cairo-context-measure (cr medium)
      (let ((layout (pango:pango-cairo-create-layout cr)))
        (set-current-font layout (clim:text-style-mapping (port medium) (or text-style (clim:medium-text-style medium))))
        (pango:pango-layout-set-text layout fixed-string)
        (multiple-value-bind (ink-rect logical-rect)
            (pango:pango-layout-get-pixel-extents layout)
          (let ((baseline (/ (pango:pango-layout-get-baseline layout) pango:+pango-scale+)))
            (values (pango:pango-rectangle-width logical-rect)
                    (pango:pango-rectangle-height logical-rect)
                    (pango:pango-rectangle-width ink-rect)
                    0
                    baseline))))))
  #+nil
  (let ((fixed-string (subseq string (or start 0) (or end (length string)))))
    (with-cairo-context (cr medium)
      (let ((layout (pango:pango-cairo-create-layout cr)))
        (pango:pango-layout-set-text layout fixed-string)
        (let ((x (multiple-value-list (pango:pango-layout-get-pixel-extents layout))))
          (log:info "x = ~s" x)
          (break)))))
  #+nil
  (let ((width 0)
	(height (text-style-height text-style medium))
	(x (- (or end (length string)) start))
	(y 0)
	(baseline (text-style-ascent text-style medium)))
    (do ((pos (position #\Newline string :start start :end end)
	      (position #\Newline string :start (1+ pos) :end end)))
	((null pos) (values width height x y baseline))
      (let ((start start)
	    (end pos))
	(setf x (- end start))
	(setf y (+ y (text-style-height text-style medium)))
	(setf width (max width x))
	(setf height (+ height (text-style-height text-style medium)))
	(setf baseline (+ baseline (text-style-height text-style medium)))))))

(defmethod climb:text-bounding-rectangle*
    ((medium gtk-medium) string &key text-style (start 0) end align-x align-y direction)
  (declare (ignore align-x align-y direction))
  (multiple-value-bind (width height x y baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore baseline))
    (values x y (+ x width) (+ y height))))

(defmethod medium-draw-text* ((medium gtk-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (let ((merged-transform (sheet-device-transformation (medium-sheet medium))))
    (when (characterp string)
      (setq string (make-string 1 :initial-element string)))
    (let ((fixed-string (subseq string (or start 0) (or end (length string)))))
      (multiple-value-bind (transformed-x transformed-y)
          (transform-position merged-transform x y)
        #+nil (log:info "displaying string at (~s,~s): ~s" transformed-x transformed-y fixed-string)
        (with-cairo-context (cr medium)
          (let ((layout (pango:pango-cairo-create-layout cr)))
            (set-current-font layout (clim:text-style-mapping (port medium) (clim:medium-text-style medium)))
            (unwind-protect
                 (progn
                   (pango:pango-layout-set-text layout fixed-string)
                   (let ((baseline (/ (pango:pango-layout-get-baseline layout) pango:+pango-scale+)))
                     (cairo:cairo-move-to cr transformed-x (- transformed-y baseline)))
                   (pango:pango-cairo-show-layout cr layout))
              (gobject:g-object-unref (gobject:pointer layout)))))))))

#+nil
(defmethod medium-buffering-output-p ((medium gtk-medium))
  t)

#+nil
(defmethod (setf medium-buffering-output-p) (buffer-p (medium gtk-medium))
  buffer-p)

(defmethod medium-finish-output ((medium gtk-medium))
  (with-medium-mirror (mirror medium)
    (let ((widget (gtk-mirror/drawing-area mirror)))
      (in-gtk-thread ()
        (gtk:gtk-widget-queue-draw widget)))))

(defmethod medium-force-output ((medium gtk-medium))
  nil)

(defmethod medium-clear-area ((medium gtk-medium) left top right bottom)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr top left)
      (climi::with-transformed-position (tr right bottom)
        (with-cairo-context (cr medium :update-style nil)
          (cairo:cairo-set-source-rgba cr 1 1 1 1)
          (cairo:cairo-rectangle cr left top right bottom)
          (cairo:cairo-fill cr))))))

(defmethod medium-beep ((medium gtk-medium))
  nil)

(defmethod medium-miter-limit ((medium gtk-medium))
  0)

(defmethod clim:medium-draw-line* ((medium gtk-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
        (with-cairo-context (cr medium)
          (cairo:cairo-move-to cr x1 y1)
          (cairo:cairo-line-to cr x2 y2)
          (cairo:cairo-stroke cr))))))

(defmethod medium-draw-lines* ((medium gtk-medium) coord-seq)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-cairo-context (cr medium)
      (iterate-over-4-blocks (x1 y1 x2 y2 coord-seq)
        (multiple-value-bind (x y)
            (climi::transform-position tr x1 y1)
          (cairo:cairo-move-to cr x y))
        (multiple-value-bind (x y)
            (climi::transform-position tr x2 y2)
          (cairo:cairo-line-to cr x y)))
      (cairo:cairo-stroke cr))))

(defmethod medium-draw-rectangle* ((medium gtk-medium) left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr left top)
      (climi::with-transformed-position (tr right bottom)
        (with-cairo-context (cr medium)
          (when (< right left) (rotatef left right))
          (when (< bottom top) (rotatef top bottom))
          (let ((left   (round-coordinate left))
                (top    (round-coordinate top))
                (right  (round-coordinate right))
                (bottom (round-coordinate bottom)))
            (cairo:cairo-rectangle cr left top (- right left) (- bottom top))
            (if filled
                (cairo:cairo-fill cr)
                (cairo:cairo-stroke cr))))))))

(defmethod medium-draw-rectangles* ((medium gtk-medium) position-seq filled)
  (declare (ignore position-seq filled))
  (log:trace "not implemented")
  (break)
  nil)

(defmethod medium-draw-point* ((medium gtk-medium) x y)
  (declare (ignore x y))
  (break)
  nil)

(defmethod medium-draw-points* ((medium gtk-medium) coord-seq)
  (declare (ignore coord-seq))
  (break)
  nil)

(defmethod medium-draw-polygon* ((medium gtk-medium) coord-seq closed filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-cairo-context (cr medium)
      (iterate-over-seq-pairs (x y coord-seq)
          (climi::with-transformed-position (tr x y)
            (cairo:cairo-move-to cr x y))
        (climi::with-transformed-position (tr x y)
          (cairo:cairo-line-to cr x y)))
      (when closed
        (cairo:cairo-close-path cr))
      (if filled
          (cairo:cairo-fill cr)
          (cairo:cairo-stroke cr)))))

(defmethod medium-draw-ellipse* ((medium gtk-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (declare (ignore center-x center-y
		   radius-1-dx radius-1-dy
		   radius-2-dx radius-2-dy
		   start-angle end-angle filled))
  (break)
  nil)

(defmethod medium-draw-circle* ((medium gtk-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (declare (ignore center-x center-y radius
		   start-angle end-angle filled))
  (break)
  nil)
