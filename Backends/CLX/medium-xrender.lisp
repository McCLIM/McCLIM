;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2003 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2018-2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-clx)

(deftype uniform-source ()
  `(or color opacity climi::uniform-compositum))

(defun make-clx-render-color (medium design)
  (declare (ignore medium))
  ;; Hmm, XRender uses pre-multiplied alpha, how useful!
  (multiple-value-bind (r g b a) (clime:color-rgba design)
    (vector (clamp (round (* r a #xffff)) 0 #xffff)
            (clamp (round (* g a #xffff)) 0 #xffff)
            (clamp (round (* b a #xffff)) 0 #xffff)
            (clamp (round (*   a #xffff)) 0 #xffff))))

(defun transform-picture (transformation picture)
  ;; 1. XRender expects a transformation to the target's plane
  ;;      (X SOURCE) -> TARGET
  ;;
  ;; 2. Ink transformation is specified for the source's plane
  ;;      (Y DESIGN) -> SOURCE
  ;;
  ;; 3. Untransformed design has the same plane as the target
  ;;      DESIGN = TARGET
  ;;
  ;; 4. Let's substitute the DESIGN with the TARGET in (2):
  ;;      1: (X SOURCE) -> TARGET
  ;;      2: (Y TARGET) -> SOURCE
  ;;
  ;; C: In other words Y is the inverse transformation of X -- jd 2021-01-22
  (multiple-value-bind (rxx rxy ryx ryy dx dy)
      (climi::get-transformation (invert-transformation transformation))
    (flet ((clx-fixed (value)
             ;; 32 bit value (top 16 integer, bottom 16 fraction)
             (logand (truncate (* value #x10000)) #xFFFFFFFF)))
      (apply #'xlib:render-set-picture-transform picture
             (mapcar #'clx-fixed (list rxx rxy dx ryx ryy dy 0 0 1))))))

;;; FIXME this method is wrong because we transform the pattern's designs
;;; along with the pattern, while "14.2 Patterns and Stencils" says:
;;;
;;;  Applying a coordinate transformation to a pattern does not affect the
;;;  designs that make up the pattern. It only changes the position, size, and
;;;  shape of the cells' holes, allowing different portions of the designs in
;;;  the cells to show through. Consequently, applying make-rectangular-tile
;;;  to a pattern of nonuniform designs can produce a different appearance in
;;;  each tile.
;;;
;;; I think that Doing the "right thing" could be achieved by composing-over
;;; each design onto the source pattern with masks representing appropriate
;;; array cells. -- jd 2021-01-25
(defun collapse-pattern (pattern)
  (loop with width = (pattern-width pattern)
        with height = (pattern-height pattern)
        with array = (make-array (list height width)
                                 :element-type '(unsigned-byte 32)
                                 :initial-element #x00000000)
        for x of-type fixnum below width
        do (loop for y of-type fixnum below height
                 do (multiple-value-bind (r g b a)
                        (clime:color-rgba
                         (clime:design-ink pattern x y))
                      (setf (aref array y x)
                            (logior
                             (ash (truncate (* a   #xff)) 24)
                             (ash (truncate (* r a #xff)) 16)
                             (ash (truncate (* g a #xff))  8)
                             (ash (truncate (* b a #xff))  0)))))
        finally (return array)))

(defun make-clx-render-pixmap (medium pattern)
  (let* ((drawable (clx-drawable medium))
         (idata  (collapse-pattern pattern))
         (height (pattern-height pattern))
         (width  (pattern-width pattern))
         (pixmap (xlib:create-pixmap :drawable drawable
                                     :width width
                                     :height height
                                     :depth 32))
         (gcontext (xlib:create-gcontext :drawable pixmap))
         (ximage   (xlib:create-image :width  width
                                      :height height
                                      :depth  32
                                      :bits-per-pixel 32
                                      :data   idata)))
    (put-image-recursively pixmap gcontext ximage width height 0 0)
    (xlib:free-gcontext gcontext)
    pixmap))

(defun clx-render-color-picture (medium rgba-design)
  (let* ((display (clx-port-display (port medium)))
         (drawable (clx-drawable medium))
         (pixmap (xlib:create-pixmap :drawable drawable
                                     :depth 32
                                     :width 1
                                     :height 1))
         (format (xlib:find-standard-picture-format display :argb32))
         (picture (xlib:render-create-picture pixmap :format format :repeat :on))
         (color (make-clx-render-color medium rgba-design)))
    (xlib:render-fill-rectangle picture :src color 0 0 1 1)
    picture))

(defun clx-render-pixmap-picture (medium pattern repeat)
  (let* ((display (clx-port-display (port medium)))
         (pixmap (make-clx-render-pixmap medium pattern))
         (format (xlib:find-standard-picture-format display :argb32)))
    (xlib:render-create-picture pixmap :format format :repeat repeat)))

(defun clx-render-flipping-picture (medium design)
  (let* ((drawable (clx-drawable medium))
         (port (port medium))
         (display (clx-port-display port))
         (width (xlib:drawable-width drawable))
         (height (xlib:drawable-height drawable))
         (depth (xlib:drawable-depth drawable))
         (pixmap (xlib:create-pixmap :drawable drawable
                                     :width width
                                     :height height
                                     :depth depth))
         (format (xlib:find-standard-picture-format display :rgb24))
         (gcontext (xlib:create-gcontext :drawable pixmap))
         (flipper (logxor (X-pixel port (slot-value design 'climi::design1))
                          (X-pixel port (slot-value design 'climi::design2)))))
    (xlib:copy-area drawable gcontext 0 0 width height pixmap 0 0)
    (xlib:gcontext-fill-style gcontext) :solid
    (setf (xlib:gcontext-function gcontext) boole-xor)
    (setf (xlib:gcontext-foreground gcontext) flipper)
    (setf (xlib:gcontext-background gcontext) flipper)
    (xlib:draw-rectangle pixmap gcontext 0 0 width height t)
    (xlib:free-gcontext gcontext)
    (xlib:render-create-picture pixmap :format format)))

(defclass clx-render-medium (clx-medium
                             climb:multiline-text-medium-mixin
                             climb:font-rendering-medium-mixin)
  ())

(defgeneric clx-render-design (medium design)
  (:method ((medium clx-render-medium) (design color))
    (clx-render-color-picture medium design))
  (:method ((medium clx-render-medium) (design opacity))
    (clx-render-color-picture medium design))
  (:method ((medium clx-render-medium) (design climi::uniform-compositum))
    (clx-render-color-picture medium design))
  (:method ((medium clx-render-medium) (design clime:indirect-ink))
    (clx-render-design medium (clime:indirect-ink-ink design)))
  (:method ((medium clx-render-medium) (design climi::standard-flipping-ink))
    (clx-render-flipping-picture medium design))
  (:method ((medium clx-render-medium) (design clime:transformed-pattern))
    (let* ((edesign (clime:effective-transformed-design design))
           (source-design (clime:transformed-design-design edesign))
           (transformation (clime:transformed-design-transformation edesign))
           (picture (if (typep source-design 'clime:rectangular-tile)
                        (clx-render-pixmap-picture medium source-design :on)
                        (clx-render-pixmap-picture medium source-design :off))))
      (transform-picture transformation picture)
      picture))
  (:method ((medium clx-render-medium) (design clime:rectangular-tile))
    (clx-render-pixmap-picture medium design :on))
  (:method ((medium clx-render-medium) (design clime:pattern))
    (clx-render-pixmap-picture medium design :off))
  (:method ((medium clx-render-medium) (design climi::everywhere-region))
    (clx-render-design medium +foreground-ink+))
  (:method ((medium clx-render-medium) (design climi::nowhere-region))
    (clx-render-design medium +transparent-ink+))
  (:method ((medium clx-render-medium) (design bounding-rectangle))
    (clx-render-design medium +transparent-ink+))
  ;; FIXME We should also handle regions, which are "solid, colorless designs"
  ;; opaque at points in the region and transparent elsewhere. The color is
  ;; taken from the foreground ink (so the ink may have a color and opacity).
  (:method ((medium clx-render-medium) design)
    (warn "Unsupported design: ~s." (class-of design))
    (clx-render-design medium (compose-in +deep-pink+ (make-opacity .4)))))

(defun medium-source-picture (medium)
  (when-let ((drawable (clx-drawable medium)))
    (clx-render-design medium (medium-ink medium))))

(defun medium-target-picture (medium)
  (when-let ((drawable (clx-drawable medium)))
    (clx-drawable-picture drawable)))

(defmacro with-render-context ((source target) medium &body body)
  (with-gensyms (drawable gc pixmap)
    (once-only (medium)
      `(when-let* ((,source (medium-source-picture ,medium))
                   (,target (medium-target-picture ,medium)))
         (let* ((^cleanup nil)
                (,drawable (clx-drawable ,medium))
                (,gc (clx-drawable-gcontext ,drawable)))
           (unwind-protect
                ;; FIXME write an optimized and antialiased equivalent of
                ;; %set-gc-clipping-region for pictures.
                (progn
                  (%set-gc-clipping-region ,medium ,gc)
                  (setf (xlib:picture-clip-mask ,target)
                        (xlib:gcontext-clip-mask ,gc))
                  ,@body)
             (mapc #'funcall ^cleanup)
             (let ((,pixmap (xlib:picture-drawable ,source)))
               (xlib:render-free-picture ,source)
               (xlib:free-pixmap ,pixmap))))))))

(defun medium-fill-rectangle (medium x1 y1 x2 y2)
  ;; If there is no picture that means that sheet does not have a registered
  ;; mirror. Happens with DREI panes during the startup..
  (when-let ((picture (medium-target-picture medium)))
    (let ((gc (clx-drawable-gcontext (clx-drawable medium))))
      (%set-gc-clipping-region medium gc)
      (setf (xlib:picture-clip-mask picture)
            (xlib:gcontext-clip-mask gc)))
    (let ((color (make-clx-render-color medium (medium-ink medium)))
          (tr (climb:medium-native-transformation medium)))
      (with-round-positions (tr x1 y1 x2 y2)
        (xlib:render-fill-rectangle picture :over color
                                    (clamp x1 #x-8000 #x7FFF)
                                    (clamp y1 #x-8000 #x7FFF)
                                    (clamp (- x2 x1) 0 #xffff)
                                    (clamp (- y2 y1) 0 #xffff))))))

(defun medium-fill-rectangle* (medium x1 y1 x2 y2)
  (with-render-context (source target) medium
    (let ((tr (medium-native-transformation medium))
          (x0 0)
          (y0 0))
      (with-round-positions (tr x0 y0 x1 y1 x2 y2)
        (xlib:render-composite :over
                               source nil target
                               (- x1 x0) (- y1 y0)
                               0 0
                               (clamp x1 #x-8000 #x7FFF)
                               (clamp y1 #x-8000 #x7FFF)
                               (clamp (- x2 x1) 0 #xffff)
                               (clamp (- y2 y1) 0 #xffff))))))

;;; FIXME make triangulate-polygon accept the coord-seq so we don't have to cons
;;; a new polygon for drawing operations.
(defun medium-fill-polygon (medium coord-seq)
  (with-render-context (source target) medium
    (let ((tr (medium-native-transformation medium))
          (coord-seq (climi::expand-point-seq
                      (climi::triangulate-polygon
                       (make-polygon* coord-seq))))
          (format (xlib:find-standard-picture-format
                   (clx-port-display (port medium)) :a8)))
      (with-round-coordinates (tr coord-seq)
        (xlib:render-triangles target :over source 0 0 format coord-seq)))))


(defmethod medium-buffering-output-p ((medium clx-render-medium))
  (call-next-method))

(defmethod (setf medium-buffering-output-p) (buffer-p (medium clx-render-medium))
  (call-next-method))

(defmethod medium-finish-output ((medium clx-render-medium))
  (call-next-method))

(defmethod medium-force-output ((medium clx-render-medium))
  (call-next-method))

(defmethod medium-clear-area ((medium clx-render-medium) left top right bottom)
  (call-next-method))

(defmethod medium-beep ((medium clx-render-medium))
  (call-next-method))

(defmethod medium-miter-limit ((medium clx-render-medium))
  (call-next-method))



(defmethod medium-draw-point* ((medium clx-render-medium) x y)
  (call-next-method))

(defmethod medium-draw-points* ((medium clx-render-medium) coord-seq)
  (call-next-method))

;;; XXX: CLX decorates lines for us. When we do it ourself this should be
;;; adjusted. For details see CLIM 2, Part 4, Section 12.4.1. -- jd 2019-01-31

(defmethod medium-draw-line* ((medium clx-render-medium) x1 y1 x2 y2)
  (call-next-method))

(defmethod medium-draw-lines* ((medium clx-render-medium) coord-seq)
  (call-next-method))

(defmethod medium-draw-polygon* ((medium clx-render-medium) coord-seq closed filled)
  (if filled
      (medium-fill-polygon medium coord-seq)
      (call-next-method medium coord-seq closed filled)))

(defmethod medium-draw-rectangle* ((medium clx-render-medium)
                                   left top right bottom filled)
  (if (not filled)
      (call-next-method)
      (if (typep (medium-ink medium) 'uniform-source)
          (medium-fill-rectangle medium left top right bottom)
          (medium-fill-rectangle* medium left top right bottom))))

(defmethod medium-draw-rectangles* ((medium clx-render-medium) position-seq filled)
  (call-next-method medium position-seq filled))

(defmethod medium-draw-ellipse* ((medium clx-render-medium)
                                 center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  (call-next-method))

(defmethod text-size ((medium clx-render-medium) string &key text-style (start 0) end
                      &aux
                        (string (string string))
                        (end (or end (length string))))
  (when (= start end)
    (return-from text-size (values 0 0 0 0 (text-style-ascent text-style medium))))
  (let* ((text-style (merge-text-styles text-style
                                        (medium-merged-text-style medium)))
         (font (text-style-mapping (port medium) text-style))
         (text (subseq (string string) start end))
         (ascent (climb:font-ascent font))
         (line-height (+ ascent (climb:font-descent font)))
         (leading (climb:font-leading font))
         (current-dx 0)
         (maximum-dx 0)
         (current-y 0))
    (dolines (text text)
      (loop
        with origin-x fixnum = 0
        for code across (climb:font-string-glyph-codes font text)
        do (incf origin-x (climb:font-glyph-dx font code))
        finally
           (alexandria:maxf maximum-dx origin-x)
           (setf current-dx origin-x)
           (incf current-y leading)))
    (values maximum-dx (+ current-y line-height (- leading))
            current-dx (- current-y leading)
            ascent)))

(defvar *draw-font-lock* (clim-sys:make-lock "draw-font"))
(defmethod medium-draw-text* ((medium clx-render-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              &aux (end (if (null end)
                                            (length string)
                                            (min end (length string)))))
  (declare (ignore toward-x toward-y))
  (when (alexandria:emptyp string)
    (return-from medium-draw-text*))
  (clim-sys:with-lock-held (*draw-font-lock*)
    (draw-glyphs medium x y string
                 :start start :end end
                 :align-x align-x :align-y align-y
                 :translate #'translate
                 :transformation (sheet-device-transformation
                                  (medium-sheet medium))
                 :transform-glyphs transform-glyphs)))


;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-width values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.
(defun draw-glyphs (medium x y string
                    &key start end
                      align-x align-y
                      translate direction
                      transformation transform-glyphs)
  (declare (optimize (speed 3))
           (ignore translate direction)
           (type #-sbcl (integer 0 #.array-dimension-limit)
                 #+sbcl sb-int:index
                 start end)
           (type string string))
  (with-render-context (source-picture picture) medium
    (let* ((text-style (medium-text-style medium))
           (port (port medium))
           (font (text-style-mapping port text-style))
           (mirror (clx-drawable medium)))

      (when (< (length (the (simple-array (unsigned-byte 32))
                            (clx-truetype-font-%buffer% font)))
               (- end start))
        (setf (clx-truetype-font-%buffer% font)
              (make-array (* 256 (ceiling (- end start) 256))
                          :element-type '(unsigned-byte 32)
                          :adjustable nil :fill-pointer nil)))
      (when (and transform-glyphs
                 (not (translation-transformation-p transformation)))
        (setq string (subseq string start end))
        (ecase align-x
          (:left)
          (:center
           (let ((origin-x (climb:text-size medium string :text-style text-style)))
             (decf x (/ origin-x 2.0))))
          (:right
           (let ((origin-x (climb:text-size medium string :text-style text-style)))
             (decf x origin-x))))
        (ecase align-y
          (:top
           (incf y (climb:font-ascent font)))
          (:baseline)
          (:center
           (let* ((ascent (climb:font-ascent font))
                  (descent (climb:font-descent font))
                  (height (+ ascent descent))
                  (middle (- ascent (/ height 2.0s0))))
             (incf y middle)))
          (:baseline*)
          (:bottom
           (decf y (climb:font-descent font))))
        (return-from draw-glyphs
          (%render-transformed-glyphs
           font string x y align-x align-y transformation mirror
           picture source-picture)))
      (let ((glyph-ids (clx-truetype-font-%buffer% font))
            (glyph-set (ensure-glyph-set port))
            (origin-x 0))
        (loop
          with char = (char string start)
          with i* = 0
          for i from (1+ start) below end
          as next-char = (char string i)
          as next-char-code = (char-code next-char)
          as code = (dpb next-char-code (byte #.(ceiling (log char-code-limit 2))
                                              #.(ceiling (log char-code-limit 2)))
                         (char-code char))
          do
             (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
                   (the (unsigned-byte 32) (mcclim-truetype:font-glyph-id font code)))
             (setf char next-char)
             (incf i*)
             (incf origin-x (climb:font-glyph-dx font code))
          finally
             (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
                   (the (unsigned-byte 32)
                        (mcclim-truetype:font-glyph-id font (char-code char))))
             (incf origin-x (climb:font-glyph-dx font (char-code char))))
        (multiple-value-bind (new-x new-y) (transform-position transformation x y)
          (setq x (ecase align-x
                    (:left
                     (truncate (+ new-x 0.5)))
                    (:center
                     (truncate (+ (- new-x (/ origin-x 2.0)) 0.5)))
                    (:right
                     (truncate (+ (- new-x origin-x) 0.5)))))
          (setq y (ecase align-y
                    (:top
                     (truncate (+ new-y (climb:font-ascent font) 0.5)))
                    (:baseline
                     (truncate (+ new-y 0.5)))
                    (:center
                     (let* ((ascent (climb:font-ascent font))
                            (descent (climb:font-descent font))
                            (height (+ ascent descent))
                            (middle (- ascent (/ height 2.0s0))))
                       (truncate (+ new-y middle 0.5))))
                    (:baseline*
                     (truncate (+ new-y 0.5)))
                    (:bottom
                     (truncate (+ new-y (- (climb:font-descent font)) 0.5)))))
          (when (and (typep x '(signed-byte 16))
                     (typep y '(signed-byte 16)))
            (xlib:render-composite-glyphs picture
                                          glyph-set
                                          source-picture
                                          x y
                                          glyph-ids
                                          :src-x x :src-y y
                                          :end (- end start))))))))

(defun %font-generate-glyph (font code transformation glyph-set)
  (let ((glyph-id (draw-glyph-id (port font)))
        (character (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0)
                                   code)))
        (next-character (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                              #.(ceiling (log char-code-limit 2)))
                                        code))))
    (multiple-value-bind (arr left top width height dx dy udx udy)
        (if (identity-transformation-p transformation)
            (mcclim-truetype:glyph-pixarray
             font character next-character transformation)
            (mcclim-truetype:glyph-pixarray
             font character next-character
             (compose-transformations
              #1=(make-scaling-transformation 1.0 -1.0)
              (compose-transformations transformation #1#))))
      (when (= (array-dimension arr 0) 0)
        (setf arr (make-array (list 1 1)
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (xlib:render-add-glyph glyph-set glyph-id
                             :data arr
;;; We negate LEFT, because we want to start drawing array LEFT pixels after the
;;; pen (pixarray contains only a glyph without its left-side bearing). TOP is
;;; not negated because glyph coordinates are in the first quadrant (while
;;; array's are in fourth).
;;;
;;; I'm leaving this comment because it wasn't obvious to me why we negate LEFT
;;; and not TOP here. -- jd 2018-09-29
                             :x-origin (- left)
                             :y-origin top
                             :x-advance dx
                             :y-advance dy)
      (let ((right (+ left (1- (array-dimension arr 1))))
            (bottom (- top (1- (array-dimension arr 0))))
            (array #|arr|# nil))
        ;; INV udx and udy are not transformed here for the transformed glyph
        ;; rendering (to avoid accumulation of a roundnig error). See
        ;; %RENDER-TRANSFORMED-GLYPHS. ARR is set to NIL because we are not
        ;; interested in keeping opacity array (it is already kept on the X11
        ;; side and we have no need for it on the Lisp side).
        (mcclim-truetype:glyph-info glyph-id array width height
                                    left right top bottom
                                    dx dy udx udy)))))

(defmethod mcclim-truetype:font-generate-glyph
    ((font clx-truetype-font) code
     &optional (transformation +identity-transformation+))
  (%font-generate-glyph font code transformation (ensure-glyph-set (port font))))

;;; Transforming glyphs is very inefficient because we don't cache them.
(defun %render-transformed-glyphs
    (font string x y align-x align-y tr mirror target-picture source-picture
     &aux (end (length string)))
  (declare (ignore align-x align-y))
  (loop
    with glyph-tr = (multiple-value-bind (x0 y0)
                        (transform-position tr 0 0)
                      (compose-transformation-with-translation tr (- x0) (- y0)))
    ;; for rendering one glyph at a time
    with current-x = x
    with current-y = y
    ;; ~
    with glyph-ids = (clx-truetype-font-%buffer% font)
    with glyph-set = (make-glyph-set (xlib:drawable-display mirror))
    with char = (char string 0)
    with i* = 0
    for i from 1 below end
    as next-char = (char string i)
    as next-char-code = (char-code next-char)
    as code = (dpb next-char-code (byte #.(ceiling (log char-code-limit 2))
                                        #.(ceiling (log char-code-limit 2)))
                   (char-code char))
    as glyph-info = (%font-generate-glyph font code glyph-tr glyph-set)
    do
       (setf (aref (the (simple-array (unsigned-byte 32))
                        glyph-ids)
                   i*)
             (the (unsigned-byte 32)
                  (mcclim-truetype:glyph-info-id glyph-info)))
    do ;; rendering one glyph at a time
       (with-round-positions (tr current-x current-y)
         (when (and (typep current-x '(signed-byte 16))
                    (typep current-y '(signed-byte 16)))
           (xlib:render-composite-glyphs target-picture glyph-set source-picture
                                         current-x current-y
                                         glyph-ids
                                         :start i* :end (1+ i*)
                                         :src-x current-x
                                         :src-y current-y)))
       ;; INV advance values are untransformed - see FONT-GENERATE-GLYPH.
       (incf current-x (mcclim-truetype:glyph-info-advance-width* glyph-info))
       (incf current-y (mcclim-truetype:glyph-info-advance-height* glyph-info))
    do
       (setf char next-char)
       (incf i*)
    finally
       (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
             (the (unsigned-byte 32)
                  (mcclim-truetype:glyph-info-id
                   (%font-generate-glyph font (char-code char)
                                         glyph-tr glyph-set))))
    finally
       ;; rendering one glyph at a time (last glyph)
       (with-round-positions (tr current-x current-y)
         (when (and (typep current-x '(signed-byte 16))
                    (typep current-y '(signed-byte 16)))
           (xlib:render-composite-glyphs target-picture glyph-set source-picture
                                         current-x current-y
                                         glyph-ids
                                         :start i* :end (1+ i*)
                                         :src-x current-x
                                         :src-y current-y)))
       (xlib:render-free-glyphs glyph-set (subseq glyph-ids 0 (1+ i*)))
    #+ (or)
    ;; rendering all glyphs at once
    ;; This solution is correct in principle, but advance-width and
    ;; advance-height are victims of rounding errors and they don't
    ;; hold the line for longer text in case of rotations and other
    ;; hairy transformations. That's why we take our time and
    ;; render one glyph at a time. -- jd 2018-10-04
       (with-round-positions (tr x y)
         (when (and (typep x '(signed-byte 16))
                    (typep y '(signed-byte 16)))
           (xlib:render-composite-glyphs target-picture
                                         glyph-set
                                         source-picture
                                         x y
                                         glyph-ids
                                         :start 0 :end end
                                         :src-x x
                                         :src-y y)))
    finally
       (xlib:render-free-glyph-set glyph-set)))
