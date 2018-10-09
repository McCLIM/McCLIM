(in-package #:clim-standard)

;; (defgeneric clim:text-style-to-font (port text-style))
;; (defgeneric clim:make-device-font-text-style (display-device device-font-name))

(defgeneric open-font (port font-designator)
  (:documentation "Opens font for a port"))

(defgeneric climb:font-character-width (font character)
  (:method (font character)
    (+ (climb:font-glyph-left font character)
       (climb:font-glyph-width font character)
       (climb:font-glyph-right font character))))

(defgeneric climb:font-string-width (font string))
(defgeneric climb:font-text-extents (font string &key start end))

(defgeneric climb:font-ascent (font))
(defgeneric climb:font-descent (font))

(defgeneric climb:font-leading  (font)
  (:method (font) (* 1.2 (+ (climb:font-ascent font) (climb:font-descent font))))
  (:documentation "Font leading is a vertical space between baselines of a
consecutive lines."))

(defgeneric climb:font-tracking (font)
  (:method (font) 0)
  (:documentation "Font tracking is an additional horizontal space between
consecutive chracters also known as a letterspacing."))

(defgeneric climb:font-glyph-width (font code))
(defgeneric climb:font-glyph-height (font code))
(defgeneric climb:font-glyph-top (font code))
(defgeneric climb:font-glyph-left (font code))
(defgeneric climb:font-glyph-bottom (font code))
(defgeneric climb:font-glyph-right (font code))
(defgeneric climb:font-glyph-dx (font code))
(defgeneric climb:font-glyph-dy (font code))
