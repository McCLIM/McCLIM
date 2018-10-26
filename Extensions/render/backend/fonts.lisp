(in-package :mcclim-render-internals)

;;;
;;; Font utilities.
;;;
(defstruct (render-glyph-info
             (:include glyph-info)
             (:constructor render-glyph-info (id width height left right top bottom advance-width advance-height paths opacity-image)))
  paths
  opacity-image)

(defclass render-truetype-font (mcclim-truetype::cached-truetype-font)
  ())

(defmethod font-generate-glyph ((font render-truetype-font) code &optional tr)
  (declare (ignore tr))
  (multiple-value-bind (paths left top width height dx dy)
      (glyph-paths font code)
    (let ((right (+ left width))
          (bottom (+ top height))
          (opacity-image (font-generate-opacity-image paths width height left top)))
      (render-glyph-info 0 dx dy left right top bottom dx dy paths opacity-image))))

(defun font-glyph-info (font character)
  (with-slots (mcclim-truetype::char->glyph-info) font
    (ensure-gethash character mcclim-truetype::char->glyph-info
                    (font-generate-glyph font (char-code character)))))

(defun font-glyph-paths (font character)
  (render-glyph-info-paths (font-glyph-info font character)))

(defun font-glyph-opacity-image (font character)
  (render-glyph-info-opacity-image (font-glyph-info font character)))

;;;
;;; font to opacity mask image
;;;

(defun font-generate-opacity-image (paths width height dx dy)
  (let* ((image (make-image (1+ (* 1 width))
                            (1+ (* 1 height)))))
    (if (= (* width height) 0)
        nil
        (aa-fill-alpha-paths image nil paths (aa:make-state)
                       (make-translation-transformation
                        (- dx) dy)
                       (make-rectangle* 0 0 (* 1 width) (* 1 height))))
    image))

;;;
;;; text geometry
;;;
(defmethod climb:font-text-extents ((font render-truetype-font) string
                                    &key (start 0) (end (length string)) direction)
  (declare (ignore direction))
  (let ((width
         ;; We could work a little harder and eliminate generic arithmetic
         ;; here. It might shave a few percent off a draw-text benchmark.
         ;; Rather silly to obsess over the array access considering that.
         (macrolet ((compute ()
                      `(loop
                          for i from start below end
                          as char = (aref string i)
                          as code = (char-code char)
                          sum (max (climb:font-glyph-right font code)
                                   (climb:font-glyph-width font code)))))
           (if (climb:font-fixed-width font)
               (* (climb:font-fixed-width font) (- end start))
               (typecase string
                 (simple-string
                  (locally (declare (type simple-string string))
                    (compute)))
                 (string
                  (locally (declare (type string string))
                    (compute)))
                 (t (compute)))))))
    (let* ((ascent (climb:font-ascent font))
           (descent (climb:font-descent font))
           (height (+ ascent descent))
           (linegap (- (climb:font-leading font) height)))
      (values
       ;; bounding box: xmin ymin xmax ymax
       0 (- ascent) width descent
       ;; text properties: left top width height
       0 (- ascent) width height
       ;; line properties: ascent descent linegap
       ascent descent linegap
       ;; cursor motion: cursor-dx cursor-dy
       width 0))))
