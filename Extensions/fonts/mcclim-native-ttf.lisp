;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Glyph rendering via zpb-ttf and cl-vectors
;;;   Created: 2008-01-26 16:32
;;;    Author: Andy Hefner <ahefner@gmail.com>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2008 by Andy Hefner

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

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

;; So weird..
(defun make-vague-font (filename)
  (let ((val (gethash filename *vague-font-hash*)))
    (or val
        (setf (gethash filename *vague-font-hash*)
              (make-instance 'vague-font
                             :lib t
                             :filename filename)))))

;;; Ignore the 'concrete font' indirection.

#+NIL
(defun make-concrete-font (vague-font size &key (dpi *dpi*))
  (error "Go away."))

#+NIL
(defun set-concrete-font-size (face size dpi)
  (declare (ignore face size dpi)))

(defclass zpb-ttf-face (truetype-face)
  ((font-loader :reader zpb-ttf-font-loader :initarg :loader)
   (units->pixels :reader zpb-ttf-font-units->pixels :initarg :units->pixels)))

(let ((font-loader-cache (make-hash-table :test #'equal))
      (font-cache        (make-hash-table :test #'equal)))
  (defun make-truetype-face (display filename size)
    (unless display (break "no display!"))
    (let* ((loader (or (gethash filename font-loader-cache)
                       (setf (gethash filename font-loader-cache)
                             (zpb-ttf:open-font-loader filename))))
           (units/em (zpb-ttf:units/em loader))
           (pixel-size (* size (/ *dpi* 72)))
           (units->pixels (* pixel-size (/ units/em)))           
           (font (or (gethash (list display loader size) font-cache)
                     (setf (gethash (list display loader size) font-cache)
                           (make-instance 'zpb-ttf-face
                                          :display display
                                          :filename filename
                                          :size size
                                          :units->pixels units->pixels
                                          :loader loader
                                          :ascent  (* (zpb-ttf:ascender loader) units->pixels)
                                          :descent (- (* (zpb-ttf:descender loader) units->pixels)))))))
      font)))

(defmethod print-object ((object zpb-ttf-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots (font-loader filename size ascent descent) object      
      (format stream "~W size=~A ascent=~A descent=~A" 
              (or (zpb-ttf:name-entry-value :full-name font-loader) filename)
              size ascent descent))))

(defun glyph-pixarray (font char)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array
   suitable as an alpha mask, and dimensions. This function returns five
   values: alpha mask byte array, x-origin, y-origin (subtracted from
   position before rendering), horizontal and vertical advances."
  (declare (optimize (debug 3)))
  (with-slots (font-loader units->pixels size ascent descent) font
      (let* ((glyph (zpb-ttf:find-glyph char font-loader))
             (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
             (right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
             (advance-width (* units->pixels (zpb-ttf:advance-width glyph)))
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
                (floor min-x)
                (ceiling max-y)
                (round advance-width)
                ;; Bah! Why does X add the vertical advance when we are rendering horizontally?
                ;; Is this considered a property of the font and glyphs rather than a particular drawing call?
                0 #+NIL (round (+ ascent descent))))))

(defun font-fixed-width-p (zpb-ttf-font)
  (declare (ignore zpb-ttf-font))
  nil)
