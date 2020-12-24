(in-package :clim-gtk)

(defmacro with-medium-mirror ((mirror-sym medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-copy-sym)
      `(let ((,mirror-copy-sym (climi::port-lookup-mirror (port ,medium) (medium-sheet ,medium))))
         (unless ,mirror-copy-sym
           (break))
         (when ,mirror-copy-sym
           (let ((,mirror-sym ,mirror-copy-sym))
             ,@body))))))

(defmacro with-medium-cairo-image ((image-sym medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-sym)
      `(with-medium-mirror (,mirror-sym ,medium)
         (let ((,image-sym (gtk-mirror/image ,mirror-sym)))
           ,@body)))))

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
                                  (region-set-regions clipping-region
                                                      :normalize :y-banding)))
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
       (break) ; Check that this works
       (loop
         for (x y width height) in (clipping-region->rect-seq clipping-region)
         do (cairo:cairo-rectangle cr x y width height)
         finally (cairo:cairo-clip cr)))
      (t
       (break)))))

(defun update-attrs (cr medium)
  (set-clipping-region cr medium)
  (multiple-value-bind (red green blue alpha)
      (clime::color-rgba (medium-ink medium))
    (cairo:cairo-set-source-rgba cr red green blue alpha)
    (let ((line-style (medium-line-style medium)))
      (cairo:cairo-set-line-width cr (line-style-thickness line-style))
      (cairo:cairo-set-line-join cr (ecase (line-style-joint-shape line-style)
                                      (:miter :miter)
                                      (:round :round)
                                      (:bevel :bevel)
                                      (:none :miter))))))

(defmacro with-cairo-context ((context-sym medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (image-sym context)
      `(with-medium-cairo-image (,image-sym ,medium)
         (let ((,context (cairo:cairo-create ,image-sym)))
           (unwind-protect
                (progn
                  (update-attrs ,context ,medium)
                  (let ((,context-sym ,context))
                    ,@body))
             (cairo:cairo-destroy ,context)))))))

(defclass gtk-medium (font-rendering-medium-mixin basic-medium)
  ((buffering-output-p :accessor medium-buffering-output-p)))

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

(defmethod medium-draw-point* ((medium gtk-medium) x y)
  (declare (ignore x y))
  nil)

(defmethod medium-draw-points* ((medium gtk-medium) coord-seq)
  (declare (ignore coord-seq))
  nil)

(defmethod medium-draw-line* ((medium gtk-medium) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2)) 
  nil)

;; FIXME: Invert the transformation and apply it here, as the :around
;; methods on transform-coordinates-mixin will cause it to be applied
;; twice, and we need to undo one of those. The
;; transform-coordinates-mixin stuff needs to be eliminated.
(defmethod medium-draw-lines* ((medium gtk-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (declare (ignore tr))
    nil))

(defmethod medium-draw-polygon* ((medium gtk-medium) coord-seq closed filled)
  (declare (ignore coord-seq closed filled))
  nil)

(defmethod medium-draw-rectangle* ((medium gtk-medium) left top right bottom filled)
  (declare (ignore left top right bottom filled))
  nil)

(defmethod medium-draw-rectangles* ((medium gtk-medium) position-seq filled)
  (declare (ignore position-seq filled))
  nil)

(defmethod medium-draw-ellipse* ((medium gtk-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (declare (ignore center-x center-y
		   radius-1-dx radius-1-dy
		   radius-2-dx radius-2-dy
		   start-angle end-angle filled))
  nil)

(defmethod medium-draw-circle* ((medium gtk-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (declare (ignore center-x center-y radius
		   start-angle end-angle filled))
  nil)

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
  (let ((fixed-string (subseq string (or start 0) (or end (length string))))
        (mirror (climi::port-lookup-mirror (port medium) (medium-sheet medium))))
    (if mirror
        (with-cairo-context (cr medium)
          (let ((layout (pango:pango-cairo-create-layout cr)))
            (pango:pango-layout-set-text layout fixed-string)
            (multiple-value-bind (ink-rect logical-rect)
                (pango:pango-layout-get-pixel-extents layout)
              (values (pango:pango-rectangle-width logical-rect)
                      (pango:pango-rectangle-height logical-rect)
                      (pango:pango-rectangle-width ink-rect)
                      0
                      (- (pango:pango-rectangle-height logical-rect)
                         (pango:pango-rectangle-y logical-rect))))))
        ;; ELSE: No mirror, return default values
        (values 100 50 100 50 30)))
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
  (declare (ignore align-x align-y direction)) ; implement me!
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
      ;; missing stuff
      (multiple-value-bind (transformed-x transformed-y)
          (transform-position merged-transform x y)
        (log:info "Drawing string ~s at (~s,~s)" fixed-string transformed-x transformed-y)
        (with-cairo-context (cr medium)
          (let ((layout (pango:pango-cairo-create-layout cr)))
            (pango:pango-layout-set-text layout fixed-string)
            (cairo:cairo-move-to cr x y)
            (pango:pango-cairo-show-layout cr layout)))))))

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
  (declare (ignore left top right bottom))
  nil)

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
      (multiple-value-bind (x y)
          (climi::transform-position tr (first coord-seq) (second coord-seq))
        (cairo:cairo-move-to cr x y))
      (loop
        for (x y) on (cddr coord-seq) by #'cddr
        do (multiple-value-bind (nx ny)
               (climi::transform-position tr x y)
             (cairo:cairo-line-to cr nx ny)))
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
            (cairo:cairo-stroke cr)))))))

(defmethod medium-draw-rectangles* ((medium gtk-medium) position-seq filled)
  (declare (ignore position-seq filled))
  (log:trace "not implemented")
  (break)
  nil)
