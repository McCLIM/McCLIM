(in-package #:climi)

(defgeneric open-font (port font-designator)
  (:documentation "Opens font for a port"))

(defgeneric climb:font-face (font))
(defgeneric climb:font-size (font))

(defgeneric climb:font-character-width (font character)
  (:method (font character)
    (let* ((codes (climb:font-string-glyph-codes font (string character)))
           (code (char-code character) (alexandria:first-elt codes)))
      (assert (alexandria:length= 1 codes))
      (+ (climb:font-glyph-left font code)
         (climb:font-glyph-width font code)
         (climb:font-glyph-right font code))))
  (:documentation "Returns width of the character. Character may be composed of
many codepoints, but argument must constitute exactly one character."))

(defgeneric climb:font-string-width (font string &key start end)
  (:method (font string &key start end)
    (alexandria:if-let ((character-width (climb:font-fixed-width font))
                        (glyph-sequence (climb:font-string-glyph-codes font string :start start :end end)))
      (* character-width (length glyph-sequence))
      (values (climb:font-text-extents font string :start start :end end))))
  (:documentation "Returns a width of the string."))

(defgeneric climb:font-string-glyph-codes (font string &key start end)
  (:method (font string &key (start 0) (end (length string)))
    (map 'list #'char-code (subseq string start end)))
  (:documentation "Converts string to a sequence of glyph codes. Some characters
are composed of many codepoints – it is not guaranteed that length of the string
and the length of resulting sequence are equal."))

(defgeneric climb:font-glyph-code-char (font code)
  (:method (font code)
    (code-char code)))

(defgeneric climb:font-text-extents (font string &key start end direction)
  (:method (font string &key start end direction)
    (declare (ignore direction))
    (let ((glyph-codes (climb:font-string-glyph-codes font string :start start :end end)))
      (loop
         for code across glyph-codes
         with origin-x fixnum = 0
         with origin-y fixnum = 0
         with newline-counter fixnum = 0
         with line-height = (+ (climb:font-ascent font) (climb:font-descent font))
         with width = 0
         with height = line-height
         with line-gap fixnum = 0
         with xmin fixnum = most-positive-fixnum
         with ymin fixnum = most-positive-fixnum
         with xmax fixnum = most-negative-fixnum
         with ymax fixnum = most-negative-fixnum
         as glyph-left fixnum = (+ (climb:font-glyph-left font code) origin-x)
         as glyph-top fixnum = (- (climb:font-glyph-top font code) origin-y)
         as glyph-right fixnum = (+ (climb:font-glyph-right font code) origin-x -1)
         as glyph-bottom fixnum = (- (climb:font-glyph-bottom font code) origin-y -1)
         do
         ;; Character may have more than one codepoint so in case of
         ;; font-glyph-code-char returning string we use EQL not CHAR=.
           (if (eql (climb:font-glyph-code-char font code) #\newline)
               (progn
                 (incf newline-counter)
                 (setf line-gap (ceiling (- (climb:font-leading font)
                                            (+ (climb:font-ascent font)
                                               (climb:font-descent font)))))
                 (setf origin-x 0)
                 (setf origin-y (ceiling (* newline-counter (climb:font-leading font)))))
               (progn
                 (alexandria:minf xmin glyph-left)
                 (alexandria:minf ymin glyph-bottom)
                 (alexandria:maxf xmax glyph-right)
                 (alexandria:maxf ymax glyph-top)
                 (incf origin-x (climb:font-glyph-dx font code))
                 (incf origin-y (climb:font-glyph-dy font code))))
           (alexandria:maxf height (+ origin-y line-height))
           (alexandria:maxf width origin-x)
         finally
           (return (values
                    ;; text bounding box (x1, y1, x2, y2)
                    xmin (- ymax) xmax (- ymin)
                    ;; text-bounding-rectangle
                    0 #|x0|# (climb:font-ascent font) #|y0|# width height
                    ;; line properties (ascent, descent, line gap)
                    (climb:font-ascent font)
                    (climb:font-descent font)
                    line-gap
                    ;; cursor-dx cursor-dy
                    origin-x origin-y)))))
  (:documentation "Function computes text extents as if it were drawn with a
specified font. It returns two distinct extents: first is an exact pixel-wise
bounding box. The second is a text bounding box with all its bearings. Text may
contain newlines, if it doesn't linegap should be nil. Cursor advance is
returned as the last two values.

Width and height are relative to the position [-top, left]. For right-to-left
direction left will be probably a negative number with the width being close to
its absolute value. All other values are relative to the postion
origin. Coordinate system is in the fourth quadrant (same as sheet coordinates).

Returned values:

xmin ymin xmax ymax
left top width height ascent descent linegap
cursor-dx cursor-dy"))

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

(defgeneric climb:font-fixed-width (font)
  (:method (font) nil)
  (:documentation "Generalized boolean. If the font character width is fixed it
is returned, otherwise returns NIL."))

(defgeneric climb:font-kerning-p (font)
  (:method (font) nil)
  (:documentation "Kerning is a customized advance-width between different pairs
of letters specified in a separate kerning-table."))

(defgeneric climb:font-glyph-width (font code))
(defgeneric climb:font-glyph-height (font code))
(defgeneric climb:font-glyph-top (font code))
(defgeneric climb:font-glyph-left (font code))
(defgeneric climb:font-glyph-bottom (font code))
(defgeneric climb:font-glyph-right (font code))
(defgeneric climb:font-glyph-dx (font code))
(defgeneric climb:font-glyph-dy (font code))


;; clim:make-device-font-text-style

(defgeneric climb:text-style-to-font (port text-style))

;(defgeneric climb:text-style-character-width)

(defgeneric climb:text-bounding-rectangle*
    (medium string &key text-style start end &allow-other-keys)
  (:documentation "Function returns a bounding box of the text for given
text-style, alignment and direction. Direction is derived from the toward-point
argument.

Argument types:
align-x   (member :left :center :right)
align-y   (member :top :baseline :center :baseline* :bottom)
direction (member :ltr :rtl)

Returned values:
xmin ymin xmax ymax."))

(defmethod climb:text-bounding-rectangle* (medium string
                                           &key
                                             text-style
                                             (start 0) end
                                             (align-x :left) (align-y :baseline) (direction :ltr)
                                           &aux (end (or end (length string))))
  (declare (ignore align-x align-y))
  (when (= start end)
    (return-from text-bounding-rectangle* (values 0 0 0 0)))
  (let ((text (string string))
        (font (text-style-to-font (port medium)
                                  (merge-text-styles text-style
                                                     (medium-merged-text-style medium)))))
    (multiple-value-bind (xmin ymin xmax ymax)
        (climb:font-text-extents font text :start start :end end :direction direction)
      (values xmin ymin xmax ymax))))

(defmethod text-size (medium string &key text-style (start 0) end
                      &aux (end (or end (length string))))
  (when (= start end)
    (return-from text-size (values 0 0 0 0)))
  (let ((text (string string))
        (font (text-style-to-font (port medium)
                                  (merge-text-styles text-style
                                                     (medium-merged-text-style medium)))))
    (multiple-value-bind (xmin ymin xmax ymax
                               left top width height
                               ascent descent linegap
                               cursor-dx cursor-dy)
        (climb:font-text-extents font text :start start :end end)
      (declare (ignore xmin ymin xmax ymax left top descent linegap))
      (values width height cursor-dx cursor-dy ascent))))
