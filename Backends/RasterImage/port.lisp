(in-package #:mcclim-raster-image)

;;; Port

(defclass raster-image-port (render-port-mixin basic-port)
  ((width :reader raster-image-port-width)
   (height :reader raster-image-port-height)))

(defmethod find-port-type ((type (eql :raster)))
  (values 'raster-image-port 'identity))

;;; Initialize and Destroy port

(defmethod initialize-instance :after ((port raster-image-port)
                                       &rest initargs
                                       &key server-path)
  (declare (ignore initargs))
  (destructuring-bind (raster-image &key
                                    (num-pages 1)
                                    (width 1000) (height 1000))
      server-path
    (declare (ignore raster-image num-pages))
    (setf (slot-value port 'width) width)
    (setf (slot-value port 'height) height)
    (make-graft port)))

(defun %destroy-all-mirrors (port)
  (maphash (lambda (key val)
             (declare (ignore key))
             (when (sheetp val)
               (destroy-mirror port val)))
           (slot-value port 'mirror->%image)))

(defmethod destroy-port :before ((port raster-image-port))
  (%destroy-all-mirrors port))

;;; Port-Graft methods

(defmethod make-graft ((port raster-image-port) &key (orientation :default)
                                                     (units       :device))
  (let* ((width  (raster-image-port-width port))
         (height (raster-image-port-height port))
         (region (make-bounding-rectangle 0 0 width height)))
    (make-instance 'raster-image-graft :port        port
                                       :region      region
                                       :mirror      nil
                                       :orientation orientation
                                       :units       units
                                       :width       width
                                       :height      height)))

(defmethod graft ((port raster-image-port))
  (first (port-grafts port)))

;;; medium

(defclass raster-image-medium (render-medium-mixin basic-medium)
  ())

(defmethod make-medium ((port raster-image-port) (sheet basic-sheet))
  (make-instance 'raster-image-medium :port port :sheet sheet))

;;; mirror

(defmethod realize-mirror ((port raster-image-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (setf (mirror->%image port mirror) mirror)
    (multiple-value-bind (width height)
        (bounding-rectangle-size sheet)
      (mcclim-render::%create-mirror-image mirror width height))
    mirror))

(defmethod destroy-mirror ((port raster-image-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (setf (mirror->%image port mirror) nil)))

(defmethod port-set-mirror-geometry ((port raster-image-port) sheet region)
  (declare (ignore port sheet))
  (bounding-rectangle* region))
