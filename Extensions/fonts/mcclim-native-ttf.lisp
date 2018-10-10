;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Glyph rendering via zpb-ttf and cl-vectors
;;;   Created: 2008-01-26 16:32
;;;    Author: Andy Hefner <ahefner@gmail.com>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2008 by Andy Hefner
;;;  (c) copyright 2016 by Daniel Kochmański
;;;
;;;    See toplevel file 'Copyright' for the copyright details.
;;;

(in-package :mcclim-truetype)

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


(defvar *zpb-font-lock* (climi::make-lock "zpb-font"))
(defparameter *dpi* 72)

(defclass truetype-font-family (clim-extensions:font-family)
  ((all-faces :initform nil
              :accessor all-faces
              :reader clim-extensions:font-family-all-faces)))

(defclass truetype-face (clim-extensions:font-face)
  ((all-fonts :initform nil
              :accessor all-fonts)
   (font-loader :initarg :loader :reader zpb-ttf-font-loader)))

(defmethod initialize-instance :after ((face truetype-face) &key &allow-other-keys)
  (let ((family (clim-extensions:font-face-family face)))
    (pushnew face (all-faces family))))

(defclass truetype-font ()
  ((face          :initarg :face     :reader climb:font-face)
   (size          :initarg :size     :reader climb:font-size)
   (kerning-p     :initarg :kerning  :reader climb:font-kerning-p)
   (tracking      :initarg :tracking :reader climb:font-tracking)
   (leading       :initarg :leading  :reader climb:font-leading)
   (fixed-width   :initarg :fixed    :reader climb:font-fixed-width)
   (ascent                           :reader climb:font-ascent)
   (descent                          :reader climb:font-descent)
   (units->pixels                    :reader zpb-ttf-font-units->pixels))
  ;; Parameters TRACKING and LEADING are specified in [em]. Internally we keep
  ;; them in [units].
  (:default-initargs :fixed nil :kerning t :tracking 0.0 :leading 1.2))

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
  (if (null (climb:font-kerning-p font))
      0
      (zpb-ttf:kerning-offset left right (zpb-ttf-font-loader (climb:font-face font)))))

(defmethod clim-extensions:font-face-all-sizes ((face truetype-face))
  (sort (mapcar #'climb:font-size (all-fonts face)) #'<))

(defmethod clim-extensions:font-face-text-style
    ((face truetype-face) &optional size)
  (make-text-style (clim-extensions:font-family-name
                    (clim-extensions:font-face-family face))
                   (clim-extensions:font-face-name face)
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
  (climi::with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels size ascent descent) font
      (let* ((font-loader (zpb-ttf-font-loader (climb:font-face font)))
             (glyph (zpb-ttf:find-glyph char font-loader))
             ;; (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
             ;; (right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
             (advance-width (+ (* units->pixels (zpb-ttf:advance-width glyph))
                               (* units->pixels (zpb-ttf:kerning-offset char next font))
                               (climb:font-tracking font)))
             (advance-height 0)
             (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                                (zpb-ttf:bounding-box glyph)))
             (min-x (elt bounding-box 0))
             (min-y (elt bounding-box 1))
             (max-x (elt bounding-box 2))
             (max-y (elt bounding-box 3))
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

        (multiple-value-bind (advance-width* advance-height*)
            ;; Transformation is supplied in font coordinates for easy
            ;; composition with offset and scaling. advance values should be
            ;; returned in screen coordinates, so we transform it here.
            (transform-distance (compose-transformations
                                 #1=(make-scaling-transformation 1.0 -1.0)
                                 (compose-transformations transformation #1#))
                                advance-width advance-height)
          (values array (- left) top width height
                  ;; X uses horizontal/vertical advance between letters. That
                  ;; way glyph sequence may be rendered. This should not be
                  ;; confused with font width/height! -- jd 2018-09-28
                  (round advance-width*)
                  (round advance-height*)
                  ;; Transformed text is rendered glyph by glyph to mitigate
                  ;; accumulation of the rounding error. For that we need values
                  ;; without rounding nor transformation. -- jd 2018-10-04
                  advance-width
                  advance-height))))))

(defstruct (glyph-info (:constructor glyph-info (id width height left right top advance-width advance-height)))
  (id 0               :read-only t :type fixnum)
  (width 0            :read-only t)
  (height 0           :read-only t)
  (left 0             :read-only t)
  (right 0            :read-only t)
  (top 0              :read-only t)
  (advance-width 0s0  :read-only t)
  (advance-height 0s0 :read-only t))

;;; Simple custom cache for glyph IDs and widths. Much faster than
;;; using the char->glyph-info hash table directly.

(defun make-gcache ()
  (let ((array (make-array 512 :adjustable nil :fill-pointer nil)))
    (loop for i from 0 below 256 do (setf (aref array i) (1+ i)))
    array))

(declaim (inline gcache-get gcache-set))

(defun gcache-get (cache key-number)
  (declare (optimize (speed 3))
           (type (simple-array t (512))))
  (let ((hash (logand (the fixnum key-number) 512)))    ; hello there.
    (and (= key-number (the fixnum (svref cache hash))) ; general kenobi.
         (svref cache (+ 256 hash)))))

(defun gcache-set (cache key-number value)
  (declare (optimize (speed 3))
           (type (simple-array t (512))))
  (let ((hash (logand (the fixnum key-number) 512)))
    (setf (svref cache hash) key-number
          (svref cache (+ 256 hash)) value)))

(defmacro ensure-font-glyph-id (font cache code)
  `(if (< ,code 512)
       (or (gcache-get ,cache ,code)
           (gcache-set ,cache ,code (font-glyph-id ,font ,code)))
       (font-glyph-id ,font ,code)))

(defmacro ensure-font-glyph-width (font cache code)
  `(if (< ,code 512)
       (or (gcache-get ,cache ,code)
           (gcache-set ,cache ,code (glyph-info-advance-width
                                     (font-glyph-info ,font ,code))))
       (glyph-info-advance-width
        (font-glyph-info ,font ,code))))

(defclass cached-truetype-font (truetype-font)
  ((glyph-id-cache :initform (make-gcache))
   (glyph-width-cache :initform (make-gcache))
   (char->glyph-info  :initform (make-hash-table :size 512))))

(defun font-glyph-info (font code)
  (with-slots (char->glyph-info) font
    (ensure-gethash code char->glyph-info
                    (font-generate-glyph font code))))

(defgeneric font-generate-glyph (font code &optional tr)
  (:documentation "Truetype TTF renderer internal interface. Backend-specific."))

(defun font-glyph-id (font code)
  (glyph-info-id (font-glyph-info font code)))

(defmethod climb:font-glyph-width ((font cached-truetype-font) code)
  (glyph-info-width (font-glyph-info font code)))

(defmethod climb:font-glyph-height ((font cached-truetype-font) code)
  (glyph-info-height (font-glyph-info font code)))

(defmethod climb:font-glyph-dx ((font cached-truetype-font) code)
  (glyph-info-advance-width (font-glyph-info font code)))

(defmethod climb:font-glyph-dy ((font cached-truetype-font) code)
  (glyph-info-advance-height (font-glyph-info font code)))

(defmethod climb:font-glyph-left ((font cached-truetype-font) code)
  (glyph-info-left (font-glyph-info font code)))

(defmethod climb:font-glyph-right ((font cached-truetype-font) code)
  (glyph-info-right (font-glyph-info font code)))

(defmethod climb:font-glyph-top ((font cached-truetype-font) code)
  (glyph-info-top (font-glyph-info font code)))
