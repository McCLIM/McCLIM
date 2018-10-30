(in-package :mcclim-render-internals)

;;;
;;; Font utilities.
;;;
(defstruct (render-glyph-info
             (:include glyph-info)
             (:constructor render-glyph-info (id width height left right top bottom advance-width advance-height opacity-image)))
  opacity-image)

(defclass render-truetype-font (mcclim-truetype::cached-truetype-font)
  ())

(defmethod font-generate-glyph ((font render-truetype-font) code
                                &optional (tr +identity-transformation+))
  (let ((character (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code)))
        (next-character (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                              #.(ceiling (log char-code-limit 2)))
                                        code))))
    (multiple-value-bind (arr left top width height dx dy udx udy)
        (glyph-pixarray font character next-character tr)
      (declare (ignore udx udy))
      (let ((right (+ left (array-dimension arr 1)))
            (bottom (- top (array-dimension arr 0))))
        (render-glyph-info 0 width height left right top bottom dx dy arr)))))

(defun font-glyph-opacity-image (font code)
  (render-glyph-info-opacity-image (font-glyph-info font code)))

;;;
;;; text geometry
;;;
#+ (or)
(defmethod climb:font-string-glyph-codes ((font render-truetype-font) string
                                          &key (start 0) (end (length string)))
  (map 'vector #'char-code (subseq string start end)))
