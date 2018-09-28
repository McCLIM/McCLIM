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
;;;  * Kerning (we didn't do this with Freetype, either. Oops.)
;;;  * Implement fixed-font-width-p for zpb-ttf.
;;;  * Boxes for missing glyphs.
;;;  * Make certain left/right bearings and text-bounding-rectangle*
;;;    are correct. (I doubt they are..)

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
  ((face          :initarg :face :reader truetype-font-face)
   (size          :initarg :size :reader truetype-font-size)
   (ascent                       :reader truetype-font-ascent)
   (descent                      :reader truetype-font-descent)
   (units->pixels                :reader zpb-ttf-font-units->pixels)))

(defmethod initialize-instance :after ((font truetype-font) &key &allow-other-keys)
  (with-slots (face size ascent descent font-loader units->pixels) font
    (let ((loader (zpb-ttf-font-loader face)))
      (setf units->pixels (/ (* size (/ *dpi* 72))
                             (zpb-ttf:units/em loader))
            ascent (* (zpb-ttf:ascender loader) units->pixels)
            descent (- (* (zpb-ttf:descender loader) units->pixels))))
    (pushnew font (all-fonts face))))

(defmethod clim-extensions:font-face-all-sizes ((face truetype-face))
  (sort (mapcar #'truetype-font-size (all-fonts face)) #'<))

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

(defun glyph-pixarray (font char)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array suitable
   as an alpha mask, and dimensions. This function returns seven values: alpha
   mask byte array, x-origin, y-origin (subtracted from position before
   rendering), glyph width and height, horizontal and vertical advances."
  (declare (optimize (debug 3)))
  (climi::with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels size ascent descent) font
      (let* ((glyph (zpb-ttf:find-glyph char (zpb-ttf-font-loader
                                              (truetype-font-face font))))
             (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
             (right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
             (advance-width (* units->pixels (zpb-ttf:advance-width glyph)))
             (advance-height 0)
             (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                                (zpb-ttf:bounding-box glyph)))
             (min-x (elt bounding-box 0))
             (min-y (elt bounding-box 1))
             (max-x (elt bounding-box 2))
             (max-y (elt bounding-box 3))
             (width  (- (ceiling max-x) (floor min-x)))
             (height (- (ceiling max-y) (floor min-y)))
             (array (make-array (list height width)
                                :initial-element 0
                                :element-type '(unsigned-byte 8)))
             (state (aa:make-state))
             (paths (paths-ttf:paths-from-glyph  glyph                                                 
                                                 :offset (paths:make-point (- (floor min-x))
                                                                           (ceiling max-y))
                                                 :scale-x units->pixels
                                                 :scale-y (- units->pixels))))
        (assert (<= (elt bounding-box 0) (elt bounding-box 2)))
        (assert (<= (elt bounding-box 1) (elt bounding-box 3)))
        ;; Oops. I think the other mcclim-truetype code expects that the rendered glyph
        ;; includes the left and right bearing, as it computes right = width - left.
        ;; Fix that. (Do we even use 'right' anywhere?)
        ;(assert (= left-side-bearing (elt bounding-box 0))) ; Doesn't hold.
        #+NIL
        (assert (= advance-width 
                   (+ left-side-bearing right-side-bearing 
                      (elt bounding-box 2) (- (elt bounding-box 0)))))

        (dolist (path paths)
          (vectors:update-state state path))
        (aa:cells-sweep state
           (lambda (x y alpha)              
             (when (and (<= 0 x (1- width))
                        (<= 0 y (1- height)))
               (setf alpha (min 255 (abs alpha))
                     (aref array y x) (climi::clamp
                                       (floor (+ (* (- 256 alpha) (aref array y x))
                                                 (* alpha 255))
                                              256)
                                       0 255)))))
        (values array
                (floor min-x)             ; left
                (ceiling max-y)           ; top
                (ceiling (- max-y min-y)) ; glyph width
                (ceiling (- max-y min-y)) ; glyph height
                ;; X uses horizontal/vertical advance between letters. That way
                ;; transformed glyph sequence may be rendered. This should not
                ;; be confused with font width/height! -- jd 2018-09-28
                (round advance-width)
                (round advance-height))))))

(defun font-text-width (font string)
  (let ((bb (zpb-ttf:string-bounding-box string
                                         (zpb-ttf-font-loader (truetype-font-face font))
                                         :kerning nil)))
    (* (slot-value font 'units->pixels) (- (zpb-ttf:xmax bb) (zpb-ttf:xmin bb)))))

(defun font-fixed-width-p (truetype-font)
  (declare (ignore truetype-font))
  nil)
