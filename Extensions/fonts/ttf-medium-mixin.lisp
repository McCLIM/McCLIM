(in-package #:mcclim-truetype)

(defclass ttf-medium-mixin ()
  ()
  (:documentation "Mixed in when the medium text-style-mapping returns
a font implementing the protocol defined below."))

(defmethod text-style-ascent (text-style (medium ttf-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (font-ascent font)))

(defmethod text-style-descent (text-style (medium ttf-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (font-descent font)))

(defmethod text-style-leading (text-style (medium ttf-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (font-leading font)))

(defmethod text-style-character-width (text-style (medium ttf-medium-mixin) char)
  (font-character-width (text-style-mapping (port medium) text-style) char))

(defmethod text-bounding-rectangle* ((medium ttf-medium-mixin) string
     &key text-style (start 0) end (align-x :left) (align-y :baseline) (direction :ltr)
     &aux (end (or end (length string))))
  (when (= start end)
    (return-from text-bounding-rectangle* (values 0 0 0 0)))
  (let ((text (string string))
        (font (text-style-mapping (port medium)
                                  (merge-text-styles text-style
                                                     (medium-merged-text-style medium)))))
    (multiple-value-bind (xmin ymin xmax ymax)
        (font-text-extents font text :start start :end end
                                 :align-x align-x :align-y align-y :direction direction)
      (values xmin ymin xmax ymax))))

(defmethod text-size ((medium ttf-medium-mixin) string &key text-style (start 0) end)
  (setf string (string string)
        end (or end (length string)))
  (when (>= start end)
    (return-from text-size
      (values 0 0 0 0 (text-style-ascent text-style medium))))
  (let ((font (text-style-mapping (port medium)
                                  (merge-text-styles
                                   text-style
                                   (medium-merged-text-style medium)))))
    (when (null (position #\newline string))
      (return-from text-size
        (multiple-value-bind (xmin ymin xmax ymax origin-x origin-y)
            (line-bbox font string start end :left)
          (declare (ignore xmin ymin))
          (values xmax ymax
                  origin-x origin-y
                  (font-ascent font)))))
    (let* ((text (subseq (string string) start end))
           (ascent (font-ascent font))
           (line-height (+ ascent (font-descent font)))
           (leading (font-leading font))
           (current-dx 0)
           (maximum-dx 0)
           (current-y 0))
      (climi::dolines (text text)
        (loop
          with origin-x fixnum = 0
          for code across (font-string-glyph-codes font text)
          do (incf origin-x (font-glyph-dx font code))
          finally
             (maxf maximum-dx origin-x)
             (setf current-dx origin-x)
             (incf current-y leading)))
      (values maximum-dx (+ current-y line-height (- leading))
              current-dx (- current-y leading)
              ascent))))

;;; A fallback drawing routine using the function MEDIUM-DRAW-POLYGON*.
(defmethod medium-draw-text* ((medium ttf-medium-mixin) string x y start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (flet ((normalize-arguments ()
           (multiple-value-bind (width height cursor-dx cursor-dy ascent)
               (text-size medium string :start start :end end)
             (declare (ignore cursor-dx cursor-dy))
             (let ((dx (ecase align-x
                         (:left   0)
                         (:center (- (/ width 2)))
                         (:right  (- width))))
                   (dy (ecase align-y
                         ((:baseline :baseline*) 0)
                         (:top       ascent)
                         (:center    (- ascent (/ height 2)))
                         (:bottom    (- ascent height)))))
               (incf x dx)
               (incf y dy)
               (setf end (if (null end)
                             (length string)
                             (min end (length string))))))))
    (normalize-arguments))
  (with-scaling (medium 1 -1 (make-point x y))
    (loop with font = (text-style-mapping (port medium) (medium-text-style medium))
          with loader = (zpb-ttf-font-loader (clime:font-face font))
          with units->pixels = (slot-value font 'units->pixels)
          for current-x = x then (+ current-x (font-glyph-dx font code))
          for current-y = y then (+ current-y (font-glyph-dy font code))
          for code across (font-string-glyph-codes font string :start start :end end)
          for char = (font-glyph-code-char font code)
          for glyf = (zpb-ttf:find-glyph char loader)
          do (climi::collect (polygons)
               (zpb-ttf:do-contours (contour glyf)
                 (climi::collect (result)
                   (labels ((collect-coords (&rest coords)
                              (climi::do-sequence ((x y) coords)
                                (result (+ current-x (* units->pixels x))
                                        (+ current-y (* units->pixels y)))))
                            (process-segment (p0 p1 p2)
                              (multiple-value-bind (x0 y0 x1 y1 x2 y2 x3 y3)
                                  (climi::bezier-segment/quadric-to-cubic (zpb-ttf:x p0) (zpb-ttf:y p0)
                                                                          (zpb-ttf:x p1) (zpb-ttf:y p1)
                                                                          (zpb-ttf:x p2) (zpb-ttf:y p2))
                                (apply #'collect-coords
                                       (climi::polygonalize-bezigon (list x0 y0 x1 y1 x2 y2 x3 y3)))))
                            (process-contour (contour)
                              (zpb-ttf:do-contour-segments (p0 p1 p2) contour
                                (if (null p1)
                                    (collect-coords (zpb-ttf:x p0) (zpb-ttf:y p0)
                                                    (zpb-ttf:x p2) (zpb-ttf:y p2))
                                    (process-segment p0 p1 p2)))))
                     (process-contour contour)
                     (polygons (result)))))
               (map-over-region-set-regions
                (lambda (polygon) (draw-design medium polygon))
                (let ((splits (climi::polygon-op-inner*
                               (loop for coords in (polygons)
                                     for polygon = (make-polygon* coords)
                                     appending (climi::polygon->pg-edges polygon nil))
                               :non-zero)))
                  (climi::pg-splitters->polygons splits)))))))
