(in-package :mcclim-raster-image)

;;;
;;; Port
;;;

(defclass raster-image-port (render-port-mixin basic-port)
  ((width :reader raster-image-port-width)
   (height :reader raster-image-port-height)))
   
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
  (maphash #'(lambda (key val)
	       (port-unregister-mirror port key val)
	       (destroy-mirror port key))
	   (slot-value port 'climi::sheet->mirror)))

(defmethod destroy-port :before ((port raster-image-port))
 (%destroy-all-mirrors port))
	    
;;; server path

(defun parse-raster-image-server-path (path)
  path)

;;; Port-Graft methods

(defmethod make-graft ((port raster-image-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'raster-image-graft
			      :port port
			      :mirror nil
			      :orientation orientation
			      :units units
			      :width (raster-image-port-width port)
			      :height (raster-image-port-height port))))
    (climi::%%set-sheet-region (make-bounding-rectangle 0 0
                                                        (raster-image-port-width port)
                                                        (raster-image-port-height port))
                               graft)
    (push graft (port-grafts port))
    graft))

(defmethod graft ((port raster-image-port))
  (first (port-grafts port)))

(defmethod destroy-mirror ((port raster-image-port) sheet)
  (declare (ignore port sheet))
  nil)

;;; medium

(defmethod make-medium ((port raster-image-port) (sheet basic-sheet))
  (make-instance 'raster-image-medium :sheet sheet :port port))


(defmethod port-set-mirror-region ((port raster-image-port) mirror region)
  (declare (ignore port mirror region))
  nil)

(defmethod port-set-mirror-transformation ((port raster-image-port) mirror transformation)
  (declare (ignore port mirror transformation))
  nil)



(defgeneric make-raster-top-level-sheet (port format))

(defmethod make-raster-top-level-sheet ((port raster-image-port) format)
  (declare (ignore format))
  (let ((tlp (make-instance 'raster-image-top-level-pane
			    :enabled-p nil :port port)))
    tlp))
