;;;;  Copyright (c) 2003       Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;;  Copyright (c) 2018-2021  Daniel Kochmański <daniel@turtleware.eu>
;;;;
;;;;    License:  LGPL-2.1-or-later

(in-package #:clim-clx)

(defun make-clx-render-color (r g b a)
  ;; Hmm, XRender uses pre-multiplied alpha, how useful!
  (xlib:render-make-color :red (* r a) :green (* g a) :blue (* b a) :alpha a))

(defclass clx-render-medium (clx-medium
                             climb:multiline-text-medium-mixin
                             climb:font-rendering-medium-mixin)
  ())

(defun medium-target-picture (medium)
  (when-let ((drawable (medium-drawable medium)))
    (clx-drawable-picture drawable)))

(defun medium-source-picture (medium)
  (let ((design (medium-ink medium)))
    (when (clime:indirect-ink-p design)
      (setf design (clime:indirect-ink-ink design)))
    (unless (typep design '(or climi::uniform-compositum color opacity))
      (setf design (compose-in +deep-pink+ (make-opacity .5))))
    (let* ((drawable (medium-drawable medium))
           (pixmap (xlib:create-pixmap
                    :drawable drawable
                    :depth (xlib:drawable-depth drawable)
                    :width 1 :height 1))
           (picture (xlib:render-create-picture
                     pixmap
                     :format (xlib:find-window-picture-format
                              (xlib:drawable-root drawable))
                     :repeat :on)))
      (multiple-value-bind (r g b a) (clime:color-rgba design)
        (let ((color (make-clx-render-color r g b a)))
          (xlib:render-fill-rectangle picture :src color 0 0 1 1)))
      picture)))

(defun medium-fill-rectangle (medium x1 y1 x2 y2)
  (alexandria:when-let ((picture (medium-target-picture medium)))
    ;; If there is no picture that means that sheet does not have a registered
    ;; mirror. Happens with DREI panes during the startup..
    (let ((color (multiple-value-call #'make-clx-render-color
                   (clime:color-rgba (medium-ink medium))))
          (tr (climb:medium-native-transformation medium)))
      (with-transformed-position (tr x1 y1)
        (with-transformed-position (tr x2 y2)
          (let ((x1 (round-coordinate x1))
                (y1 (round-coordinate y1))
                (x2 (round-coordinate x2))
                (y2 (round-coordinate y2)))
            (setf (xlib:picture-clip-mask picture)
                  (clipping-region->rect-seq
                   (or (last-medium-device-region medium)
                       (medium-device-region medium))))
            (xlib:render-fill-rectangle picture :over color
                                        (max #x-8000 (min #x7FFF x1))
                                        (max #x-8000 (min #x7FFF y1))
                                        (max 0 (min #xFFFF (- x2 x1)))
                                        (max 0 (min #xFFFF (- y2 y1))))))))))


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
  (call-next-method medium coord-seq closed filled))

(defmethod medium-draw-rectangle* ((medium clx-render-medium)
                                   left top right bottom filled)
  (call-next-method))

(defmethod medium-draw-rectangles* ((medium clx-render-medium) position-seq filled)
  (call-next-method medium position-seq filled))

(defmethod medium-draw-ellipse* ((medium clx-render-medium)
                                 center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  (call-next-method))

(defmethod medium-draw-circle* ((medium clx-render-medium)
                                center-x center-y radius start-angle end-angle
                                filled)
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
    (return-from clim:medium-draw-text*))
  (with-clx-graphics () medium
    (clim-sys:with-lock-held (*draw-font-lock*)
      (draw-glyphs medium mirror gc x y string
                   :start start :end end
                   :align-x align-x :align-y align-y
                   :translate #'translate
                   :transformation (sheet-device-transformation
                                    (medium-sheet medium))
                   :transform-glyphs transform-glyphs))))


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
      (let ((fixed-width (climb:font-fixed-width font)))
        (when (and (numberp fixed-width) (/= fixed-width dx))
          (setf (slot-value font 'mcclim-truetype::fixed-width) t)
          (warn "Font ~A is fixed width, but the glyph width appears to vary.
Disabling fixed width optimization for this font. ~A vs ~A" font dx fixed-width)))
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

;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-width values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.
(defun draw-glyphs (medium mirror gc x y string
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
  (let* ((text-style (medium-text-style medium))
         (port (port medium))
         (font (text-style-mapping port text-style))
         (picture (medium-target-picture medium))
         (source-picture (medium-source-picture medium))
         (clip-mask (xlib:gcontext-clip-mask gc)))
    ;; Sync the picture-clip-mask with that of the gcontext.
    (unless (eq (xlib:picture-clip-mask picture) clip-mask)
      (setf (xlib:picture-clip-mask picture) clip-mask))

    (when (< (length (the (simple-array (unsigned-byte 32))
                          (clx-truetype-font-%buffer% font)))
             (- end start))
      (setf (clx-truetype-font-%buffer% font)
            (make-array (* 256 (ceiling (- end start) 256))
                        :element-type '(unsigned-byte 32)
                        :adjustable nil :fill-pointer nil)))
    (when (and transform-glyphs
               (not (clim:translation-transformation-p transformation)))
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
      (multiple-value-bind (new-x new-y) (clim:transform-position transformation x y)
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
        (unless (and (typep x '(signed-byte 16))
                     (typep y '(signed-byte 16)))
          (warn "Trying to render string outside the mirror.")
          (return-from draw-glyphs)))
      (xlib:render-composite-glyphs picture
                                    glyph-set
                                    source-picture
                                    x y
                                    glyph-ids
                                    :end (- end start)))))

;;; Transforming glyphs is very inefficient because we don't cache them.
(defun %render-transformed-glyphs
    (font string x y align-x align-y tr mirror picture source-picture
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
       (multiple-value-bind (current-x current-y)
           (transform-position tr current-x current-y)
         (xlib:render-composite-glyphs picture glyph-set source-picture
                                       (truncate (+ current-x 0.5))
                                       (truncate (+ current-y 0.5))
                                       glyph-ids :start i* :end (1+ i*)))
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
       (multiple-value-bind (current-x current-y)
           (transform-position tr current-x current-y)
         (xlib:render-composite-glyphs picture glyph-set source-picture
                                       (truncate (+ current-x 0.5))
                                       (truncate (+ current-y 0.5))
                                       glyph-ids :start i* :end (1+ i*)))
       (xlib:render-free-glyphs glyph-set (subseq glyph-ids 0 (1+ i*)))
    #+ (or)
    ;; rendering all glyphs at once
    ;; This solution is correct in principle, but advance-width and
    ;; advance-height are victims of rounding errors and they don't
    ;; hold the line for longer text in case of rotations and other
    ;; hairy transformations. That's why we take our time and
    ;; render one glyph at a time. -- jd 2018-10-04
       (multiple-value-bind (x y) (clim:transform-position tr x y)
         (xlib:render-composite-glyphs picture
                                       glyph-set
                                       source-picture
                                       (truncate (+ x 0.5))
                                       (truncate (+ y 0.5))
                                       glyph-ids :start 0 :end end))
    finally
       (xlib:render-free-glyph-set glyph-set)))
