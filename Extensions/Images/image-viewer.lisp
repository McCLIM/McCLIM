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

;;; A simple image viewer gadget. It would be nice if it could modify
;;; its space requirements to fit the image, but MCCLIM-IMAGES does
;;; not provide this functionality.

(in-package :mcclim-images)

(defclass image-viewer (value-gadget)
  ()
  (:documentation "An abstract gadget for displaying images. The
value of the gadget is the image being displayed.")
  (:default-initargs :value nil))

(defmethod (setf gadget-value) :after (new-value (gadget image-viewer)
                                                 &key &allow-other-keys)
  (handle-repaint gadget (or (pane-viewport-region gadget)
                             (sheet-region gadget))))

(defclass image-viewer-pane (image-viewer basic-gadget)
  ()
  (:documentation "A concrete gadget for displaying images. The
value of the gadget is the image being displayed."))

(defmethod handle-repaint ((pane image-viewer-pane) region)
  (declare (ignore region))
  ;; Clear the old image.
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)    
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+))
  (when (gadget-value pane)
    ;; Try to ensure there is room for the new image.
    (change-space-requirements pane
     :height (image-height (gadget-value pane))
     :width (image-width (gadget-value pane)))
    ;; Draw the new one, if there is one.
    (draw-image pane (gadget-value pane))))
