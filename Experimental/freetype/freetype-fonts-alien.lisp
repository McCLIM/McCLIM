;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Experimental FreeType support for CMUCL and SBCL
;;;   Created: 2003-05-25 16:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Gilbert Baumann
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import #+(or cmu scl) '(alien:slot alien:make-alien alien:alien alien:deref)
          #+sbcl '(sb-alien:slot sb-alien:make-alien sb-alien:alien sb-alien:deref)))

(declaim (optimize (speed 1) (safety 3) (debug 1) (space 0)))

(defclass freetype-face (truetype-face)
  ((concrete-font :initarg :concrete-font :reader freetype-face-concrete-font)))

(defun make-vague-font (filename)
  (let ((val (gethash filename *vague-font-hash*)))
    (or val
        (setf (gethash filename *vague-font-hash*)
              (make-instance 'vague-font
                             :lib (let ((libf (make-alien freetype:library)))
                                    (declare (type (alien (* freetype:library)) libf))
                                    (freetype:init-free-type libf)
                                    (deref libf))
                             :filename filename)))))

;; A 'concrete' font is an instance of a 'vague' font at a particular text size.

(defparameter *concrete-font-hash* (make-hash-table :test #'equal))

(defun make-concrete-font (vague-font size &key (dpi *dpi*))
  (with-slots (lib filename) vague-font
    (let* ((key (cons lib filename))
           (val (gethash key *concrete-font-hash*)))
      (unless val
        (let ((facef (make-alien freetype:face)))
          (declare (type (alien (* freetype:face)) facef))
          (if (zerop (freetype:new-face lib filename 0 facef))
              (setf val (setf (gethash key *concrete-font-hash*)
                              (deref facef)))
              (error "Freetype error in make-concrete-font"))))
      val)))

;;; One "concrete font" is shared for a given face, regardless of text size.
;;; We call set-concrete-font-size to choose the current size before 
;;; generating glyphs.

(defun set-concrete-font-size (face size dpi)
  (declare (type (alien freetype:face) face))
  (freetype:set-char-size face 0 (round (* size 64)) (round dpi) (round dpi))
  face)


(defun glyph-pixarray (font char)
  (declare (optimize (speed 3))
           (inline freetype:load-glyph freetype:render-glyph))
  (let ((face (the (alien freetype:face) (freetype-face-concrete-font font))))
    (set-concrete-font-size face (truetype-face-size font) *dpi*)
    (freetype:load-glyph face (freetype:get-char-index face (char-code char)) 0)
    (freetype:render-glyph (slot face 'freetype:glyph) 0)
    (symbol-macrolet
          ((glyph (slot face 'freetype:glyph))
           (bm (slot glyph 'freetype:bitmap)))
        (let* ((width  (slot bm 'freetype:width))
               (pitch  (slot bm 'freetype:pitch))
               (height (slot bm 'freetype:rows))
               (buffer (slot bm 'freetype:buffer))
               (res    (make-array (list height width) :element-type '(unsigned-byte 8))))
          (declare (type (simple-array (unsigned-byte 8) (* *)) res))
          (let ((m (* width height)))
            (locally
                (declare (optimize (speed 3) (safety 0)))
              (loop for y*width of-type fixnum below m by width 
                    for y*pitch of-type fixnum from 0 by pitch do
                    (loop for x of-type fixnum below width do
                          (setf (row-major-aref res (+ x y*width))
                                (deref buffer (+ x y*pitch)))))))
          (values
           res
           (slot glyph 'freetype:bitmap-left)
           (slot glyph 'freetype:bitmap-top)
           (/ (slot (slot glyph 'freetype:advance) 'freetype:x) 64)
           (/ (slot (slot glyph 'freetype:advance) 'freetype:y) 64))))))

(defun font-fixed-width-p (freetype-font)
  (zerop (logand (slot (freetype-face-concrete-font freetype-font)
                       'freetype:face-flags) 4))) ; FT_FACE_FLAG_FIXED_WIDTH

(defparameter *font-hash* 
  (make-hash-table :test #'equalp))

(let ((cache (make-hash-table :test #'equal)))
  (defun make-truetype-face (display filename size)
    (or (gethash (list display filename size) cache)
        (setf (gethash (list display filename size) cache)
              (let* ((f.font (or (gethash filename *font-hash*)
                                 (setf (gethash filename *font-hash*)
                                       (make-vague-font filename))))
                     (f (make-concrete-font f.font size)))
                (declare (type (alien freetype:face) f))
                (set-concrete-font-size f size *dpi*)
                (make-instance 'freetype-face
                               :display display
                               :filename filename
                               :size size
                               :concrete-font f
                               :ascent  (/ (slot (slot (slot f 'freetype:size_s) 'freetype:metrics) 'freetype:ascender) 64)
                               :descent (/ (slot (slot (slot f 'freetype:size_s) 'freetype:metrics) 'freetype:descender) -64)))))))