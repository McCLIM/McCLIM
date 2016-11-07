(in-package :mcclim-render)

;;;
;;; Paths
;;;

(defgeneric transformablep (interpolation)
  (:method (interpolation)
    nil)
  (:method ((interpolation paths::bezier))
    t)
  (:method ((interpolation (eql :straight-line)))
    t))

(defun empty-path-p (path)
  (zerop (length (paths::path-knots path))))

(defun make-path (x y)
  (let ((path (paths:create-path :open-polyline)))
    (paths:path-reset path (paths:make-point x y))
    path))

(defun line-to (path x y)
  (paths:path-extend path (paths:make-straight-line)
                     (paths:make-point x y)))

(defun close-path (path )
  (setf (paths::path-type path) :closed-polyline))

(defun stroke-path (path line-style)
  (when (line-style-dashes line-style)
    (setf path (paths:dash-path path
				(ctypecase (line-style-dashes line-style)
				  (simple-array
				   (line-style-dashes line-style))
				  (cons
				   (map 'vector #'(lambda (x) x)
					(line-style-dashes line-style)))
				  (t
				   #(2 1))))))
  (setf path (paths:stroke-path path
				(max 1 (line-style-thickness line-style))
				:joint (funcall #'(lambda (c)
						    (if (eq c :bevel)
							:none
							c))
						  (line-style-joint-shape line-style))
				:caps (funcall #'(lambda (c)
						   (if (eq c :no-end-point)
						       :butt
						       c))
					       (line-style-cap-shape line-style))))
  path)

(defun transform-path (path transformation)
  (labels ((transform-point (point)
	     (let ((x (paths:point-x point))
		   (y (paths:point-y point)))
	       (with-transformed-position (transformation x y)
		 (paths:make-point x y))))
	   (transform-interpolation (interpolation)
	     (ctypecase interpolation
	       (symbol
		interpolation)
	       (paths::bezier
		(let ((control-points (slot-value interpolation
						  'paths::control-points)))
		  (dotimes (i (length control-points) interpolation)
		    (setf (aref control-points i)
			  (transform-point (aref control-points i)))))))))
    (when (empty-path-p path)
      (return-from transform-path path))
    (let ((new-path (paths:create-path (paths::path-type path)))
	  (iterator (paths:path-iterator-segmented path
						   (complement #'transformablep))))
      (loop
	 (multiple-value-bind (interpolation knot endp)
	     (paths:path-iterator-next iterator)
	   (paths:path-extend new-path
			      (transform-interpolation interpolation)
			      (transform-point knot))
	   (when endp
	     (return new-path)))))))

