(in-package :clim-gtk)

(defmacro with-medium-mirror ((mirror-sym medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-copy-sym)
      `(let ((,mirror-copy-sym (climi::port-lookup-mirror (port ,medium) (medium-sheet ,medium))))
         (when ,mirror-copy-sym
           (let ((,mirror-sym ,mirror-copy-sym))
             ,@body))))))

(defmacro with-medium-cairo-image ((image-sym medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-sym)
      `(with-medium-mirror (,mirror-sym ,medium)
         (let ((,image-sym (gtk-mirror/image ,mirror-sym)))
           ,@body)))))

(defmacro with-cairo-context ((context-sym medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (image-sym context)
      `(with-medium-cairo-image (,image-sym ,medium)
         (let ((,context (cairo:cairo-create ,image-sym)))
           (unwind-protect
                (let ((,context-sym ,context))
                  ,@body)
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

(defmethod text-size
    ((medium gtk-medium) string &key text-style (start 0) end)
  (setf string (etypecase string
		 (character (string string))
		 (string string)))
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
  (declare (ignore string x y start end align-x align-y toward-x toward-y transform-glyphs))
  nil)

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
          (log:info "Drawing line")
          (cairo:cairo-set-source-rgb cr 0 1 0)
          (cairo:cairo-move-to cr x1 y1)
          (cairo:cairo-line-to cr x2 y2)
          (cairo:cairo-stroke cr))))))

(defmethod medium-draw-lines* ((medium gtk-medium) coord-seq)
  (break))

(defmethod medium-draw-rectangle* ((medium gtk-medium) left top right bottom filled)
  nil)

(defmethod medium-draw-rectangles* ((medium gtk-medium) position-seq filled)
  (declare (ignore position-seq filled))
  (log:trace "not implemented")
  (break)
  nil)
