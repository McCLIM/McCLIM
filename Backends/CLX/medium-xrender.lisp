;;;;  Copyright (c) 2003       Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;;  Copyright (c) 2018       Daniel Kochmański <daniel@turtleware.eu>
;;;;
;;;;    License:  LGPL-2.1-or-later

(in-package #:clim-clx)

(defclass clx-render-medium (clx-medium climb:multiline-text-medium-mixin)
  ((picture :initform nil)))

(defun clx-render-medium-picture (medium)
  (with-slots (picture) medium
    (alexandria:when-let* ((mirror (port-lookup-mirror (port medium) (medium-sheet medium)))
                           (format (xlib:find-window-picture-format (xlib:drawable-root mirror))))
      (cond
        ((null picture)
         (setf picture (xlib:render-create-picture mirror :format format)))
        ;; We need this comparison to mitigate a rogue mirror swaps with a
        ;; pixmap, i.e in WITH-TEMP-MIRROR%%% for double buffering.
        ((eq mirror (xlib:picture-drawable picture))
         picture)
        (T ;; mirror has been swapped!
         (xlib:render-free-picture picture)
         (setf picture (xlib:render-create-picture mirror :format format)))))))


(defmethod clim:medium-copy-area ((from-drawable clx-render-medium) from-x from-y
                                  width height
                                  (to-drawable clx-render-medium) to-x to-y)
  (call-next-method))

(defmethod clim:medium-copy-area ((from-drawable clx-render-medium) from-x from-y
                                  width height
                                  (to-drawable pixmap) to-x to-y)
  (call-next-method))

(defmethod clim:medium-copy-area ((from-drawable pixmap) from-x from-y
                                  width height
                                  (to-drawable clx-render-medium) to-x to-y)
  (call-next-method))


(defmethod clim:medium-draw-point* ((medium clx-render-medium) x y)
  (call-next-method))

(defmethod clim:medium-draw-points* ((medium clx-render-medium) coord-seq)
  (call-next-method))


(defmethod clim:medium-draw-line* ((medium clx-render-medium) x1 y1 x2 y2)
  (call-next-method))

(defmethod clim:medium-draw-lines* ((medium clx-render-medium) coord-seq)
  (call-next-method))


(defmethod clim:medium-draw-polygon* ((medium clx-render-medium) coord-seq closed filled)
  (call-next-method))


(defun medium-draw-rectangle-xrender (medium x1 y1 x2 y2 filled)
  (declare (ignore filled))
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (multiple-value-bind (r g b a) (clime:color-rgba (medium-ink medium))
            ;; Hmm, XRender uses pre-multiplied alpha, how useful!
            (setf r (min #xffff (max 0 (round (* #xffff a r))))
                  g (min #xffff (max 0 (round (* #xffff a g))))
                  b (min #xffff (max 0 (round (* #xffff a b))))
                  a (min #xffff (max 0 (round (* #xffff a)))))
            ;; If there is no picture that means that sheet does not have a
            ;; registered mirror. Happens with DREI panes during the startup..
            (alexandria:when-let ((picture (clx-render-medium-picture medium)))
              (setf (xlib:picture-clip-mask picture) (clipping-region->rect-seq
                                                      (or (last-medium-device-region medium)
                                                          (medium-device-region medium))))
              (xlib:render-fill-rectangle picture :over (list r g b a)
                                          (max #x-8000 (min #x7FFF x1))
                                          (max #x-8000 (min #x7FFF y1))
                                          (max 0 (min #xFFFF (- x2 x1)))
                                          (max 0 (min #xFFFF (- y2 y1)))))))))))

#+ (or) ;; this causes regressions (i.e in draggable graph)
(defmethod clim:medium-draw-rectangle* ((medium clx-render-medium) left top right bottom filled)
  (unless filled
    (return-from clim:medium-draw-rectangle*
      (call-next-method)))
  (typecase (medium-ink medium)
    ((or climi::uniform-compositum clim:color clim:opacity)
     (medium-draw-rectangle-xrender medium left top right bottom filled))
    (otherwise
     (call-next-method))))

(defmethod clim:medium-draw-rectangles* ((medium clx-render-medium) position-seq filled)
  (call-next-method))


(defmethod clim:medium-draw-ellipse* ((medium clx-render-medium) center-x center-y
                                      radius-1-dx radius-1-dy
                                      radius-2-dx radius-2-dy
                                      start-angle end-angle filled)
  (call-next-method))

(defmethod clime:medium-draw-circle* ((medium clx-render-medium)
                                      center-x center-y radius start-angle end-angle
                                      filled)
  (call-next-method))

(defvar *draw-font-lock* (climi::make-lock "draw-font"))
(defmethod clim:medium-draw-text* ((medium clx-render-medium) string x y
                                   start end
                                   align-x align-y
                                   toward-x toward-y transform-glyphs
                                   &aux (end (if (null end)
                                                 (length string)
                                                 (min end (length string)))))
  ;; Possible optimalzaions:
  ;;
  ;; * with-clx-graphics already creates appropriate pixmap for us (correct one!) and we have
  ;; medium picture in place - there is no need for gcontext-picture (see xrender-fonts)
  ;; * don't use (PICTURE-DRAWABLE (CLX-RENDER-MEDIUM-PICTURE MEDIUM)) - it is slow due to possible
  ;; mirror swaps, use (SHEET-XMIRROR (MEDIUM-SHEET MEDIUM)) instead. It might be a good idea to
  ;; wrap our own (CLX-RENDER-MEDIUM-MIRROR MEDIUM) function.
  (declare (ignore toward-x toward-y))
  (when (alexandria:emptyp string)
    (return-from clim:medium-draw-text*))
  (with-clx-graphics () medium
    (unless (eq align-y :baseline)
      (let* ((font (climb:text-style-to-font (port medium) (medium-text-style medium)))
             (ascent (climb:font-ascent font))
             (descent (climb:font-descent font))
             (text-height (+ ascent descent)))
        (setq y (ecase align-y
                  (:top (+ y ascent))                              ; OK
                  #+ (or) (:baseline y)                            ; OK
                  (:center (+ y ascent (- (/ text-height 2.0s0)))) ; See :around for multiline
                  (:baseline* y)                                   ; See :around for multiline
                  (:bottom (- y descent))))))                      ; See :around for multiline
    (unless (eq align-x :left)
      ;; This is the worst case - we need to compute whole text width what
      ;; requires walking all lines char-by char.
      (let ((text-width (text-size medium string)))
        (setq x (- x (ecase align-x
                       ;;(:left 0)
                       (:center (/ text-width 2.0s0))
                       (:right text-width))))))
    (bt:with-lock-held (*draw-font-lock*)
      (mcclim-font:draw-glyphs medium mirror gc x y string
                               :start start :end end
                               :translate #'translate
                               :transformation (sheet-device-transformation (medium-sheet medium))
                               :transform-glyphs transform-glyphs))))

(defmethod clime:medium-draw-glyph ((medium clx-render-medium) element x y
                                    align-x align-y toward-x toward-y
                                    transform-glyphs)
  (clim:medium-draw-text* medium (string element) x y 0 1
                          align-x align-y toward-x toward-y
                          transform-glyphs))


(defmethod clim:medium-buffering-output-p ((medium clx-render-medium))
  (call-next-method))

(defmethod (setf clim:medium-buffering-output-p) (buffer-p (medium clx-render-medium))
  (call-next-method))

(defmethod clim:medium-finish-output ((medium clx-render-medium))
  (call-next-method))

(defmethod clim:medium-force-output ((medium clx-render-medium))
  (call-next-method))

(defmethod clim:medium-clear-area ((medium clx-render-medium) left top right bottom)
  (call-next-method))

(defmethod clim:medium-beep ((medium clx-render-medium))
  (call-next-method))

(defmethod clime:medium-miter-limit ((medium clx-render-medium))
  (call-next-method))
