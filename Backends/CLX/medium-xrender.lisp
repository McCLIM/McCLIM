;;;;  Copyright (c) 2003       Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;;  Copyright (c) 2018       Daniel Kochmański <daniel@turtleware.eu>
;;;;
;;;;    License:  LGPL-2.1-or-later

(in-package #:clim-clx)

(defclass clx-render-medium (clx-medium)
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


(defmethod climi::text-bounding-rectangle*
    ((medium clx-render-medium) string &key text-style (start 0) end)
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
(defmethod clim:medium-draw-text* ((medium clx-render-medium) string x y
                                   start end
                                   align-x align-y
                                   toward-x toward-y transform-glyphs
                                   &aux (end (if (null end)
                                                 (length string)
                                                 (min end (length string)))))
  ;; Possible optimalzaions:
  ;;
  ;; * if align-x = left, then there is no need to compute text width (big win with kerning)
  ;; * if align-y is not member (:center :last-line-baseline :bottom) then text-height is not needed
  ;; * with-clx-graphics already creates appropriate pixmap for us (correct one!) and we have
  ;; medium picture in place - there is no need for gcontext-picture (see xrender-fonts)
  ;; * don't use (PICTURE-DRAWABLE (CLX-RENDER-MEDIUM-PICTURE MEDIUM)) - it is slow due to possible
  ;; mirror swaps, use (SHEET-XMIRROR (MEDIUM-SHEET MEDIUM)) instead. It might be a good idea to
  ;; wrap our own (CLX-RENDER-MEDIUM-MIRROR MEDIUM) function.

  (declare (ignore toward-x toward-y transform-glyphs))
  (let* ((medium-transform (medium-transformation medium))
         (native-transform (sheet-native-transformation (medium-sheet medium)))
         (merged-transform (clim:compose-transformations native-transform medium-transform)))
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
                         (:center (round text-width 2)) ; worst case
                         (:right text-width))))         ; worst case
          (setq y (ecase align-y
                    (:top (+ y baseline))                              ; OK
                    (:first-line-baseline :baseline y)                 ; OK
                    (:center (+ y baseline (- (floor text-height 2)))) ; change
                    (:last-line-baseline  y)                           ; change
                    (:bottom (+ y baseline (- text-height)))))))       ; change
      (let ((x (round-coordinate x))
            (y (round-coordinate y)))
        (bt:with-lock-held (*draw-font-lock*)
          (font-draw-glyphs
           (text-style-to-X-font (port medium) (medium-text-style medium))
           mirror gc x y string
           #| x (- y baseline) (+ x text-width) (+ y (- text-height baseline )) |#
           :start start :end end
           :translate #'translate :size 16 :transformation merged-transform))))))

(defmethod clime:medium-draw-glyph ((medium clx-medium) element x y
                                    align-x align-y toward-x toward-y
                                    transform-glyphs)
  (call-next-method))


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
