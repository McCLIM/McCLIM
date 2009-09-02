;;; -*- Mode: Lisp; Package: MCCLIM-IMAGES -*-

;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)

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

(in-package :clim-internals)

(define-bitmap-file-reader :png (image-pathname)
  (let* ((png-state (png-read:read-png-file image-pathname))
         (data (png-read:image-data png-state))
         (depth (png-read:bit-depth png-state))
         (height (png-read:height png-state))
         (width (png-read:width png-state))
         (array (make-array (list height width) :element-type '(unsigned-byte 32))))
    (unless (member depth '(8 32))
      (error "~@<PNG-encoded images with bit depth ~D are not supported. The only supported bit depths are 8 and 32.~:@>" depth))
    (dotimes (y height)
      (dotimes (x width)
        (case depth
          ((8 32)
           (let ((red (aref data x y 0))
                 (green (aref data x y 1))
                 (blue (aref data x y 2)))
             (setf (aref array y x)
                   (dpb red (byte 8 0)
                        (dpb green (byte 8 8)
                             (dpb blue (byte 8 16)
                                  (dpb (- 255 0) (byte 8 24) 0))))))))))
    array))
