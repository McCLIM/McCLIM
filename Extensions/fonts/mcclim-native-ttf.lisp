;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2008 by Andy Hefner <ahefner@gmail.com>
;;;  (c) Copyright 2016 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Glyph rendering via zpb-ttf and cl-vectors.
;;;

(in-package #:mcclim-truetype)

;;; TODO:
;;;  * Implement fixed-font-width-p for zpb-ttf.
;;;  * Implement text direction for font-text-extents

;;; Wish-list:
;;;  * Subpixel antialiasing. It would be straightforward to generate the
;;;    glyphs by tripling the width as passed to cl-vectors and compressing
;;;    triplets of pixels together ourselves. I'm not certain how to draw
;;;    the result through xrender. I've seen hints on Google that there is
;;;    subpixel AA support in xrender, which isn't obvious from CLX or the
;;;    spec. Failing that, we could use a 24bpp mask with component-alpha.
;;;    That might even be how you're supposed to do it. I'm skeptical as to
;;;    whether this would be accelerated for most people.

;;;  * Subpixel positioning. Not hard in principle - render multiple versions
;;;    of each glyph, offset by fractions of a pixel. Horizontal positioning
;;;    is more important than vertical, so 1/4 pixel horizontal resolution
;;;    and 1 pixel vertical resolution should suffice. Given how ugly most
;;;    CLIM apps are, and the lack of WYSIWYG document editors crying out
;;;    for perfect text spacing in small fonts, we don't really need this.


(defvar *zpb-font-lock* (clim-sys:make-lock "zpb-font"))
(defparameter *dpi* 72)

(defclass truetype-font-family (font-family)
  ((all-faces :initform nil
              :accessor all-faces
              :reader font-family-all-faces)))

(defclass truetype-face (font-face)
  ((all-fonts :initform nil :accessor all-fonts)
   (font-loader :initarg :loader :reader zpb-ttf-font-loader)))

(defmethod initialize-instance :after ((face truetype-face) &key &allow-other-keys)
  (let ((family (font-face-family face)))
    (pushnew face (all-faces family))))

(defclass truetype-font ()
  ((face          :initarg :face     :reader font-face)
   (size          :initarg :size     :reader font-size)
   ;; Kerning is a customized advance-width between different pairs of letters
   ;; specified in a separate kerning-table.
   (kerning-p     :initarg :kerning  :reader font-kerning-p)
   ;; Font tracking is an additional horizontal space between consecutive
   ;; chracters also known as a letterspacing.
   (tracking      :initarg :tracking :reader font-tracking)
   ;; Font leading is a vertical space between baselines of a consecutive lines.
   (leading       :initarg :leading  :reader font-leading)
   ;; Generalized boolean. If the font character width is fixed it is returned,
   ;; otherwise returns NIL.
   (fixed-width   :initarg :fixed    :reader font-fixed-width :type (or fixnum null))
   (ascent                           :reader font-ascent)
   (descent                          :reader font-descent)
   (units->pixels                    :reader zpb-ttf-font-units->pixels))
  ;; Parameters TRACKING and LEADING are specified in [em]. Internally we keep
  ;; them in [units].
  (:default-initargs :fixed nil :kerning t :tracking 0.0 :leading 1.2))

(defmethod port ((font truetype-font))
  (font-family-port (font-face-family (font-face font))))

(defmethod initialize-instance :after ((font truetype-font) &key tracking leading &allow-other-keys)
  (with-slots (face size ascent descent font-loader) font
    (let* ((loader (zpb-ttf-font-loader face))
           (em->units (zpb-ttf:units/em loader))
           (units->pixels (/ (* size (/ *dpi* 72)) em->units)))
      (setf ascent  (+ (* units->pixels (zpb-ttf:ascender loader)))
            descent (- (* units->pixels (zpb-ttf:descender loader)))
            (slot-value font 'tracking) (* units->pixels (* em->units tracking))
            (slot-value font 'leading)  (* units->pixels (* em->units leading))
            (slot-value font 'units->pixels) units->pixels))
    (pushnew font (all-fonts face))))

(defmethod zpb-ttf:kerning-offset ((left character) (right character) (font truetype-font))
  (if (null (font-kerning-p font))
      0
      (zpb-ttf:kerning-offset left right (zpb-ttf-font-loader (font-face font)))))

(defmethod font-face-all-sizes ((face truetype-face))
  (sort (mapcar #'font-size (all-fonts face)) #'<))

(defmethod font-face-text-style ((face truetype-face) &optional size)
  (make-text-style (font-family-name (font-face-family face))
                   (font-face-name face)
                   size))

(defmethod print-object ((object truetype-font) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots (size ascent descent units->pixels) object
      (format stream " size=~A ascent=~A descent=~A units->pixels=~A"
              size ascent descent units->pixels))))

;;; Derived from CL-VECTORS library function PATHS-TTF:PATHS-FROM-GLYPH.
(defun paths-from-glyph* (glyph tr)
  "Extract paths from a glyph."
  (flet ((point (p) (multiple-value-call #'net.tuxee.paths:make-point
                      (transform-position tr (zpb-ttf:x p) (zpb-ttf:y p)))))
    (let (result)
      (zpb-ttf:do-contours (contour glyph)
        (let ((path (net.tuxee.paths:create-path :polygon))
              (last-point nil))
          (zpb-ttf:do-contour-segments (a b c) contour
            (let ((pa (point a))
                  (pb (when b (point b)))
                  (pc (point c)))
              (unless last-point
                (net.tuxee.paths:path-reset path pa))
              (net.tuxee.paths:path-extend path
                                           (if b
                                               (net.tuxee.paths:make-bezier-curve (list pb))
                                               (net.tuxee.paths:make-straight-line))
                                           pc)
              (setq last-point pc)))
          (push path result)))
      (setq result (nreverse result))
      result)))

(defun glyph-pixarray (font char next transformation)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array suitable
   as an alpha mask, and dimensions. This function returns seven values: alpha
   mask byte array, x-origin, y-origin (subtracted from position before
   rendering), glyph width and height, horizontal and vertical advances."
  (declare (optimize (debug 3)))
  (clim-sys:with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels size ascent descent) font
      (let* ((font-loader (zpb-ttf-font-loader (font-face font)))
             (glyph (zpb-ttf:find-glyph char font-loader))
             ;; (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
             ;; (right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
             (udx (+ (* units->pixels (zpb-ttf:advance-width glyph))
                     (* units->pixels (zpb-ttf:kerning-offset char next font))
                     (font-tracking font)))
             (udy 0)
             (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                                (zpb-ttf:bounding-box glyph)))
             (min-x (elt bounding-box 0))
             (min-y (elt bounding-box 1))
             (max-x (elt bounding-box 2))
             (max-y (elt bounding-box 3))
             ;; top-side-bearing is mostly useful for vertical
             ;; metrics. left-side-bearing is the same as xmin,
             ;; right-side-bearing may be inferred as well. bottom-side-bearing
             ;; is usually not mentioned in the literature due to its limited
             ;; purpose (I'm not aware of any vertical alphabet which direction
             ;; is bottom-to-top), but we could imagine it has a similar
             ;; relation as the right-side-bearing.
             ;;
             ;;   left-side-bearing = min-x
             ;;   right-side-bearing = advance-width - left-side-bearing - width
             ;;   top-side-bearing = baseline + max-y
             ;;   bottom-side-bearing = advance-height - top-side-bearing - height
             ;;
             ;; all these values may be inferred from other glyph properties so
             ;; we do not return them. -- jd 2018-10-14
             width height left top array)

        (with-bounding-rectangle* (x1 y1 x2 y2)
            (transform-region transformation (make-rectangle* min-x min-y max-x max-y))
          (setq width  (- (ceiling x2) (floor x1)))
          (setq height (- (ceiling y2) (floor y1)))
          (setq left (- (floor x1)))
          (setq top (ceiling y2))
          (setq array (make-array (list height width)
                                  :initial-element 0
                                  :element-type '(unsigned-byte 8))))
        (let* ((glyph-tr (compose-transformations
                          (compose-transformations
                           (make-translation-transformation left top)
                           (make-scaling-transformation units->pixels (- units->pixels)))
                          transformation))
               (paths (paths-from-glyph* glyph glyph-tr))
               (state (aa:make-state)))
          (dolist (path paths)
            (vectors:update-state state path))
          (aa:cells-sweep state
                          (lambda (x y alpha)
                            (when (array-in-bounds-p array y x)
                              (setf alpha (min 255 (abs alpha))
                                    (aref array y x) (climi::clamp
                                                      (floor (+ (* (- 256 alpha) (aref array y x))
                                                                (* alpha 255))
                                                             256)
                                                      0 255))))))
        #+ (or) ;; draw delicate border around each glyph (for testing)
        (progn
          (loop for j from 0 below height do (setf (aref array j 0)
                                                   (logior #x40 (aref array j 0))
                                                   (aref array j (1- width))
                                                   (logior #x40 (aref array j (1- width)))))
          (loop for i from 0 below width do (setf (aref array 0 i)
                                                  (logior #x40 (aref array 0 i))
                                                  (aref array (1- height) i)
                                                  (logior #x40 (aref array (1- height) i)))))
        (multiple-value-bind (dx dy)
            ;; Transformation is supplied in font coordinates for easy
            ;; composition with offset and scaling. advance values should be
            ;; returned in screen coordinates, so we transform it here.
            (transform-distance (compose-transformations
                                 #1=(make-scaling-transformation 1.0 -1.0)
                                 (compose-transformations transformation #1#))
                                udx udy)
          (values array (- left) top width height
                  ;; X uses horizontal/vertical advance between letters. That
                  ;; way glyph sequence may be rendered. This should not be
                  ;; confused with font width/height! -- jd 2018-09-28
                  (round dx)
                  (round dy)
                  ;; Transformed text is rendered glyph by glyph to mitigate
                  ;; accumulation of the rounding error. For that we need values
                  ;; without rounding nor transformation. -- jd 2018-10-04
                  udx
                  udy))))))


(deftype glyph-pixarray () '(simple-array (unsigned-byte 8) (* *)))

(defstruct (glyph-info (:constructor glyph-info (id pixarray width height
                                                 left right top bottom
                                                 advance-width advance-height
                                                 advance-width* advance-height*)))
  (id 0                             :type fixnum)
  (pixarray nil        :read-only t :type (or null glyph-pixarray))
  (width 0             :read-only t)
  (height 0            :read-only t)
  (left 0              :read-only t)
  (right 0             :read-only t)
  (top 0               :read-only t)
  (bottom 0            :read-only t)
  (advance-width 0     :read-only t)
  (advance-height 0    :read-only t)
  ;; untransformed values
  (advance-width* 0s0  :read-only t)
  (advance-height* 0s0 :read-only t))

(defclass cached-truetype-font (truetype-font)
  ((char->glyph-info  :initform (make-hash-table :size 512))))

(defun font-glyph-info (font code)
  (with-slots (char->glyph-info) font
    (ensure-gethash code char->glyph-info
                    (font-generate-glyph (port font) font code))))

(defgeneric font-generate-glyph (port font code &key &allow-other-keys)
  (:documentation "Truetype TTF renderer internal interface.")
  (:method (port (font cached-truetype-font) code &key (transformation +identity-transformation+))
    (declare (ignore port))
    (let ((character (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code)))
          (next-character (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                                #.(ceiling (log char-code-limit 2)))
                                          code)))
          (transformation (let ((scale (make-scaling-transformation 1.0 -1.0)))
                            (compose-transformations
                             scale (compose-transformations transformation scale)))))
      (multiple-value-bind (arr left top width height dx dy udx udy)
          (glyph-pixarray font character next-character transformation)
        (let ((right (+ left (1- (array-dimension arr 1))))
              (bottom (- top (1- (array-dimension arr 0)))))
          (glyph-info code arr width height left right top bottom dx dy udx udy))))))

(defun font-glyph-id (font code)
  (glyph-info-id (font-glyph-info font code)))

(defun font-glyph-width (font code)
  (glyph-info-width (font-glyph-info font code)))

(defun font-glyph-height (font code)
  (glyph-info-height (font-glyph-info font code)))

(defun font-glyph-dx (font code)
  (glyph-info-advance-width (font-glyph-info font code)))

(defun font-glyph-dy (font code)
  (glyph-info-advance-height (font-glyph-info font code)))

(defun font-glyph-left (font code)
  (glyph-info-left (font-glyph-info font code)))

(defun font-glyph-right (font code)
  (glyph-info-right (font-glyph-info font code)))

(defun font-glyph-top (font code)
  (glyph-info-top (font-glyph-info font code)))

(defun font-glyph-bottom (font code)
  (glyph-info-bottom (font-glyph-info font code)))

(defun font-string-glyph-codes (font string &key (start 0) (end (length string)))
  "Converts string to a sequence of glyph codes. Some characters are composed of
many codepoints – it is not guaranteed that length of the string and the length
of resulting sequence are equal."
  (declare (ignore font))
  (alexandria:minf end (length string))
  (when (>= start end)
    (return-from font-string-glyph-codes #()))
  (loop
    with array = (make-array (- end start) :fill-pointer 0)
    as char = (char string start) then next-char
    for i fixnum from (1+ start) below end
    as next-char = (char string i)
    as code = (dpb (char-code next-char)
                   (byte #.(ceiling (log char-code-limit 2))
                         #.(ceiling (log char-code-limit 2)))
                   (char-code char))
    do
       (vector-push code array)
    finally
       (vector-push (char-code char) array)
       (return array)))

(defun font-glyph-code-char (font code)
  (declare (ignore font))
  (values (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code))
          (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                #.(ceiling (log char-code-limit 2)))
                          code))))

(defun font-character-width (font character)
  "Returns width of the character. Character may be composed of many codepoints,
but argument must constitute exactly one character."
  (let* ((codes (font-string-glyph-codes font (string character)))
         (code (alexandria:first-elt codes)))
    (assert (alexandria:length= 1 codes))
    (font-glyph-dx font code)))

(defun font-string-width (font string &key start end)
  "Returns a width of the string."
  (if-let ((character-width (font-fixed-width font))
           (glyph-sequence (font-string-glyph-codes font string :start start :end end)))
    (* character-width (length glyph-sequence))
    (values (font-text-extents font string :start start :end end))))


(defun line-bbox (font glyph-codes align-x)
  (loop
     for code across glyph-codes
     with origin-x fixnum = 0
     with origin-y fixnum = 0
     with xmin = most-positive-fixnum
     with ymin = most-positive-fixnum
     with xmax = most-negative-fixnum
     with ymax = most-negative-fixnum
     as glyph-left fixnum =   (+ origin-x (font-glyph-left font code))
     as glyph-top fixnum =    (+ origin-y (- (font-glyph-top font code)))
     as glyph-right fixnum =  (+ origin-x (font-glyph-right font code))
     as glyph-bottom fixnum = (+ origin-y (- (font-glyph-bottom font code)))
     do
       (alexandria:minf xmin glyph-left)
       (alexandria:minf ymin glyph-top)
       (alexandria:maxf xmax glyph-right)
       (alexandria:maxf ymax glyph-bottom)
       (incf origin-x (font-glyph-dx font code))
       (incf origin-y (font-glyph-dy font code))
     finally
       (case align-x
         (:center
          (let ((width/2 (/ (- xmax xmin) 2)))
            (setf xmin (- width/2))
            (setf xmax (+ width/2))))
         (:right
          (let ((width (- xmax xmin)))
            (setf xmin (- width))
            (setf xmax 0))))
       (return (values xmin ymin xmax ymax origin-x origin-y))))

(defun font-text-extents (font string &key start end align-x align-y direction)
  "Function computes text extents as if it were drawn with a specified font. It
returns two distinct extents: first is an exact pixel-wise bounding box. The
second is a text bounding box with all its bearings. Text may contain newlines,
if it doesn't linegap should be nil. Cursor advance is returned as the last two
values.

Width and height are relative to the position [-top, left]. For right-to-left
direction left will be probably a negative number with the width being close to
its absolute value. All other values are relative to the postion
origin. Coordinate system is in the fourth quadrant (same as sheet coordinates).

Returned values:

xmin ymin xmax ymax
left top width height ascent descent linegap
cursor-dx cursor-dy"
  (declare (ignore direction))
  (when (alexandria:emptyp string)
    (values 0 0 0 0 0 0))
  (let* ((ascent (font-ascent font))
         (descent (font-descent font))
         (line-height (+ ascent descent))
         (xmin most-positive-fixnum)
         (ymin most-positive-fixnum)
         (xmax most-negative-fixnum)
         (ymax most-negative-fixnum)
         (dx 0)
         (dy 0)
         (current-y 0)
         (current-dx 0))
    (climi::dolines (line (subseq string start end))
      (multiple-value-bind (xmin* ymin* xmax* ymax* dx* dy*)
          (if (alexandria:emptyp line)
              (values 0 0 0 0 0 0)
              (line-bbox font (font-string-glyph-codes font line) align-x))
        (case align-y
          (:top
           (let ((height (- ymax* ymin*))
                 (ymin* (- ascent (abs ymin*))))
             (minf ymin (+ current-y ymin*))
             (maxf ymax (+ current-y (+ ymin* height)))))
          (:center
           (let ((height/2 (/ (+ current-y (- ymax* ymin*)) 2)))
             (minf ymin (- height/2))
             (maxf ymax (+ height/2))))
          (:bottom
           (let ((height (- ymax* ymin*))
                 (ymax* (- ymax* descent)))
             (minf ymin (- (- ymax* height) current-y))
             (maxf ymax ymax*)))
          (:baseline*
           (minf ymin (- ymin* current-y))
           (maxf ymax ymax*))
          (otherwise
           (minf ymin (+ current-y ymin*))
           (maxf ymax (+ current-y ymax*))))
        (minf xmin xmin*)
        (maxf xmax xmax*)
        (maxf dx dx*)
        (maxf dy (+ current-y dy*))
        (incf current-y (font-leading font))
        (setf current-dx dx*)))
    (return-from font-text-extents
      (values
       ;; text bounding box
       xmin ymin xmax ymax
       ;; text-bounding-rectangle
       0 #|x0|# (font-ascent font) #|y0|# dx (+ dy line-height)
       ;; line properties (ascent, descent, line gap)
       (font-ascent font)
       (font-descent font)
       (- (font-leading font)
          (+ (font-ascent font)
             (font-descent font)))
       ;; cursor-dx cursor-dy
       current-dx dy))))
