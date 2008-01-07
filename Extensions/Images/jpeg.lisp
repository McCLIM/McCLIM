;;; -*- Mode: Lisp; Package: MCCLIM-IMAGES -*-

;;;  (c) copyright 2008
;;;           Eric Marsden
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

(define-image-reader "jpeg" (pathname)
  (with-open-file (stream pathname :direction :input)
    (multiple-value-bind (rgb height width)
        (jpeg::decode-image stream)
      (let* ((rgb-image-data (make-array (list height width)
					 :element-type '(unsigned-byte 32)))
             (rgb-image (make-instance 'clim-internals::rgb-image
				       :width width :height height :alphap nil
				       :data rgb-image-data)))
        (loop for y from (1- height) downto 0 do
             (loop for x from (1- width) downto 0 do
                  (let ((grey (svref rgb (+ x (* y width)))))
                    (setf (aref rgb-image-data y x)
                          (dpb grey (byte 8 0)
                               (dpb grey (byte 8 8)
                                    (dpb grey (byte 8 16)
                                         (dpb (- 255 0) (byte 8 24) 0))))))))
        (clim-internals::make-rgb-image-design rgb-image)))))

(define-image-reader "jpg" (pathname)
  (load-image-of-format "jpeg" pathname))
