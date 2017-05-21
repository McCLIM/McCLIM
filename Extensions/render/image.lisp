(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; image base class
;;;

(defclass image ()
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)
   (data :initarg :data
	 :accessor image-data)))

(defgeneric fill-image (image design mask &key x y width height 
					    mask-dx mask-dy))

(defgeneric copy-image (image src-image &key x y width height 
					  src-dx src-dy))

(defgeneric save-image-to-file (image file))
(defgeneric save-image-to-stream (image stream format))

;;;
;;; conversion
;;;

(defgeneric coerce-to-clim-rgb-image (image)
  (:method ((image climi::rgb-image))
    image))

(defgeneric coerce-to-opticl-image (image))
 
;;;
;;; I/O
;;;

(defparameter *image-stream-writer-hash-table* (make-hash-table))
(map nil (lambda (z)
           (destructuring-bind (x y) z
             (setf (gethash x *image-stream-writer-hash-table*) y)))
     '((:tiff opticl:write-tiff-stream)
       (:tif opticl:write-tiff-stream)
       (:jpeg opticl:write-jpeg-stream)
       (:jpg opticl:write-jpeg-stream)
       (:png opticl:write-png-stream)
       (:pbm opticl:write-pbm-stream)
       (:pgm opticl:write-pgm-stream)
       (:ppm opticl:write-ppm-stream)
       (:gif opticl:write-gif-stream)))

(defmethod save-image-to-file (image file)
  (let ((optimg (coerce-to-opticl-image image)))
    (opticl:write-image-file file optimg)))
    
(defmethod save-image-to-stream (image stream format)
  (let ((fn (gethash format *image-stream-writer-hash-table*)))
    (if fn
	(funcall fn stream (coerce-to-opticl-image image))
	(error "Cannot write image stream: ~S" stream))))

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  (floor (+ x .5)))


;;;
;;; copy image
;;;

(defmacro make-copy-image-function (image-get-code image-set-code)
  `(flet ((copy-ff ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from y to max-y do
		     (loop for i from x to max-x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code))))))
	  (copy-bf ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from y to max-y do
		     (loop for i from max-x downto x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code))))))
	  (copy-fb ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from max-y downto y do
		     (loop for i from x to max-x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code))))))
	  (copy-bb ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from max-y downto y do
		     (loop for i from max-x downto x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code)))))))
     (if (eq image src-image)
	 (cond
	   ((and (<= src-dx 0) (<= src-dy 0))
	    (copy-bb))
	   ((and (<= src-dx 0) (> src-dy 0))
	    (copy-bf))
	   ((and (> src-dx 0) (<= src-dy 0))
	    (copy-fb))
	   ((and (> src-dx 0) (> src-dy 0))
	    (copy-ff)))
	 (copy-ff))))

(defmacro def-copy-image (image-data-type image-data-set-pixel src-image-data-type image-data-get-pixel)
  `(progn
     (let ((data-image (image-data image))
	 (src-data-image (image-data src-image)))
     (declare (type ,image-data-type data-image)
	      (type ,src-image-data-type src-data-image))
     (make-copy-image-function
      (,image-data-get-pixel src-data-image (+ src-dx i) (+ src-dy j))
      (,image-data-set-pixel data-image i j red green blue alpha)))
     (make-rectangle* x y (+ x width) (+ y height))))


;;;
;;; fill image
;;;

(defmacro make-fill-image-function (image-get-code image-set-code design-get-code aa-alpha-code)
  `(when (and (> width 0)
	      (> height 0))
     (let ((max-y (+ y height -1))
	   (max-x (+ x width -1)))
       (loop for j from y to max-y do
	    (loop for i from x to max-x do
		 (multiple-value-bind (red green blue alpha)
		     ,design-get-code
		   (let ((aa-alpha ,aa-alpha-code))
		     (if (> (imult aa-alpha alpha) 250)
			 ,image-set-code
			 (multiple-value-bind (r.bg g.bg b.bg a.bg)
			     ,image-get-code
			   (multiple-value-bind (red green blue alpha)	  
			       (octet-blend r.bg g.bg b.bg a.bg red green blue alpha aa-alpha)
			     ,image-set-code))))))))))
		      
