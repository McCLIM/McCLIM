;;; -*- Mode: Lisp; Package: MCCLIM-IMAGES -*-

;;;  (c) copyright 2008 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

(in-package :mcclim-images)

(define-image-reader "gif" (image-pathname &key)
  (let* ((data-stream (skippy:load-data-stream image-pathname))
         (first-image (aref (skippy:images data-stream) 0))
         (pattern-array (make-array (list (skippy:height first-image)
                                          (skippy:width first-image))))
         (designs (coerce (loop with color-table = (skippy:color-table data-stream)
                             for i below 255
                             collecting (multiple-value-bind (r g b) 
                                            (skippy:color-rgb (skippy:color-table-entry color-table i))
                                          (make-rgb-color (/ r 255) (/ g 255) (/ b 255))))
                          'vector)))
    (dotimes (y (array-dimension pattern-array 0))
      (dotimes (x (array-dimension pattern-array 1))
        (setf (aref pattern-array y x) (skippy:pixel-ref first-image x y))))
    (make-pattern pattern-array designs)))
