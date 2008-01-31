;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Experimental FreeType support
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

;;; Can't unconditionally use this in the package definition..
#+NIL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cffi))

;;; reset safety up to 3 to try to work out some of my problems.  If
;;; this gets fixed, presumably it should be dropped back down to
;;; 1... [2006/05/24:rpg]
(declaim (optimize (speed 2) (safety 3) (debug 3) (space 3)))

(defclass freetype-face (truetype-face)
  ((concrete-font :initarg :concrete-font :reader freetype-face-concrete-font)))

(defun make-vague-font (filename)
  (let ((val (gethash filename *vague-font-hash*)))
    (or val
        (setf (gethash filename *vague-font-hash*)
              (make-instance 'vague-font
                             :lib
                             ;; I am not at all sure that this is the
                             ;; right translation --- because of the
                             ;; difference between SBCL aliens and
                             ;; CFFI, I'm not sure what the deref was
                             ;; intended to achieve... [2006/05/24:rpg]
                             (let ((libf (cffi:foreign-alloc 'freetype:library)))
                               (freetype:init-free-type libf)
                               (cffi:mem-aref libf 'freetype:library))
                             :filename filename)))))

(defparameter *concrete-font-hash* (make-hash-table :test #'equal))

(defun make-concrete-font (vague-font size &key (dpi *dpi*))
  (with-slots (lib filename) vague-font
    (let* ((key (cons lib filename))
           (val (gethash key *concrete-font-hash*)))
      (unless val
        (let ((facef
               ;; this will allocate a pointer (notionally to a
               ;; face-rec), and return a pointer to it (the pointer).
               (cffi:foreign-alloc 'freetype:face)
                ;;(make-alien freetype:face))
                ))
          ;;(declare (type (alien (* freetype:face)) facef))
          (if (zerop (freetype:new-face lib filename 0 facef))
              (setf val (setf (gethash key *concrete-font-hash*)
                              (cffi:mem-ref facef 'freetype:face)))
              
;;;           (setf val (setf (gethash key *concrete-font-hash*)
;;;                              (deref facef)))
              (error "Freetype error in make-concrete-font"))))
      (let ((face val))
        ;; (declare (type (alien freetype:face) face))
        face))))

                                        ;(declaim (inline make-concrete-font))

(defun set-concrete-font-size (face size dpi)
  ;(declare (type (alien freetype:face) face))
  (freetype:set-char-size face 0 (round (* size 64)) (round dpi) (round dpi))
  face)

(defun glyph-pixarray (font char)
;;;  (declare (optimize (speed 3) (debug 1))
;;;           (inline freetype:load-glyph freetype:render-glyph)
;;;           (type (alien freetype:face) face))
  (let ((face (freetype-face-concrete-font font)))
    (set-concrete-font-size face (truetype-face-size font) *dpi*)
    (freetype:load-glyph face (freetype:get-char-index face (char-code char)) 0)
    (freetype:render-glyph (cffi:foreign-slot-value face 'freetype:face-rec 'freetype:glyph) 0)
    (cffi:with-foreign-slots ((freetype:glyph) face freetype:face-rec)
      (cffi:with-foreign-slots ((freetype:bitmap) freetype:glyph freetype:glyph-slot-rec)
        (cffi:with-foreign-slots ((freetype:width freetype:pitch freetype:rows freetype:buffer) 
                                  freetype:bitmap freetype:bitmap)
          (let ((res
                 (make-array (list freetype:rows freetype:width)
                             :element-type '(unsigned-byte 8))))
;;;    (let* ((width  (slot bm 'freetype:width))
;;;           (pitch  (slot bm 'freetype:pitch))
;;;           (height (slot bm 'freetype:rows))
;;;           (buffer (slot bm 'freetype:buffer))
;;;           (res    (make-array (list height width) :element-type '(unsigned-byte 8))))
            (declare (type (simple-array (unsigned-byte 8) (* *)) res))
            (let ((m (* freetype:width freetype:rows)))
              (locally
                  (declare (optimize (speed 3) (safety 0)))
                (loop for y*width of-type fixnum below m by freetype:width 
                      for y*pitch of-type fixnum from 0 by freetype:pitch do
                      (loop for x of-type fixnum below freetype:width do
                            (setf (row-major-aref res (+ x y*width))
                                  (cffi:mem-aref freetype:buffer :uint8 (+ x y*pitch))
                                  ;; (deref buffer (+ x y*pitch))
                                  )))))
            (cffi:with-foreign-slots 
                ((freetype:bitmap-left
                  freetype:bitmap-top
                  freetype:advance) freetype:glyph freetype:glyph-slot-rec)
              (cffi:with-foreign-slots ((freetype:x freetype:y) freetype:advance freetype:vector)
                (values
                 res
                 freetype:bitmap-left
                 freetype:bitmap-top
                 (/ freetype:x 64)
                 (/ freetype:y 64))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun font-fixed-width-p (freetype-font)
  ;; Fixme! Translate this to CFFI-speak to get the fixed-width optimization:
  ;;(zerop (logand (slot (freetype-face-concrete-font freetype-font)
  ;; 'freetype:face-flags) 4))) ; FT_FACE_FLAG_FIXED_WIDTH
  nil)

(defparameter *font-hash* 
  (make-hash-table :test #'equalp))

(let ((cache (make-hash-table :test #'equal)))
  (defun make-truetype-face (display font size)
    (or (gethash (list display font size) cache)
        (setf (gethash (list display font size) cache)
              (let* ((f.font (or (gethash font *font-hash*)
                                 (setf (gethash font *font-hash*)
                                       (make-vague-font font))))
                     (f (make-concrete-font f.font size)))
                (set-concrete-font-size f size *dpi*)
                (cffi:with-foreign-slots ((freetype:size_s) f freetype:face-rec)
                  (cffi:with-foreign-slots ((freetype:metrics) freetype:size_s freetype:size-rec)
                    (cffi:with-foreign-slots ((freetype:ascender freetype:descender) 
                                              freetype:metrics freetype:size-metrics)
                      (make-instance 'freetype-face
                                     :display display
                                     :filename font
                                     :size size
                                     :concrete-font f
                                     :ascent  (/ freetype:ascender 64)
                                     :descent (/ freetype:descender -64))))))))))

