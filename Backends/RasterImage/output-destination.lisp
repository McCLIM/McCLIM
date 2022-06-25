;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------

(in-package #:mcclim-raster-image)

(defclass raster-image-destination (file-destination)
  ((%file-format :initarg :file-format
                 :reader file-format)
   (%width :initarg :width
           :reader width
           :initform :compute)
   (%height :initarg :height
            :reader height
            :initform :compute)))

(defmethod shared-initialize :after ((instance raster-image-destination)
                                     (slot-names t)
                                     &key
                                     (file nil file-p)
                                     (file-format nil file-format-p))
  (declare (ignore file-format))
  (when (and file-p (not file-format-p))
    (setf (slot-value instance '%file-format) (extract-format file))))

(defmethod destination-element-type ((destination raster-image-destination))
  '(unsigned-byte 8))

(defmethod invoke-with-standard-output
    (continuation (destination raster-image-destination))
  (call-next-method (lambda ()
                      (with-output-to-drawing-stream
                          (*standard-output* :raster *standard-output*
                                             :format (file-format destination)
                                             :width (width destination)
                                             :height (height destination))
                        (funcall continuation)))
                    destination))

(register-output-destination-type
 "Raster Image File" 'raster-image-destination)
