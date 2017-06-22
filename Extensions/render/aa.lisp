(in-package :mcclim-render)

(declaim (optimize speed))

(defgeneric %make-blend-draw-fn (image clip-region rgba-design))
(defgeneric %make-blend-draw-span-fn (image clip-region ink)) 
(defgeneric %make-xor-draw-fn (image clip-region rgba-design))
(defgeneric %make-xor-draw-span-fn (image clip-region ink))

(defmacro %make-blend-draw-function-macro (data-type image-get-code image-set-code source-code)
  `(lambda (data x y alpha)
     (declare (type ,data-type data)
	      (type fixnum x y)
	      (type fixnum alpha))
     (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
       (setf alpha (min (abs alpha) 255))
       (when (plusp alpha)
	 (multiple-value-bind (r.fg g.fg b.fg a.fg)
	     ,source-code
	   (if (> (octet-mult a.fg alpha) 250)
	       (multiple-value-bind (red green blue alpha)	  
		   (values r.fg g.fg b.fg 255)
		 ,image-set-code
		 (values red green blue alpha))
	       (multiple-value-bind (r.bg g.bg b.bg a.bg)
		   ,image-get-code
		 (multiple-value-bind (red green blue alpha)
		     (octet-blend-function r.bg g.bg b.bg a.bg r.fg g.fg b.fg (octet-mult a.fg alpha))
		   ,image-set-code
		   (values red green blue alpha)))))))))

(defmacro %make-blend-draw-span-function-macro (data-type image-get-code image-set-code source-code)
  `(lambda (data x1 x2 y alpha)
     (declare (type ,data-type data)
	      (type fixnum x1 x2 y)
	      (type fixnum alpha))
     (setf alpha (min (abs alpha) 255))
     (when (plusp alpha)
       (loop for x from x1 below x2 do
	    (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	      (multiple-value-bind (r.fg g.fg b.fg a.fg)
		  ,source-code
		(if (> (octet-mult a.fg alpha) 250)
		    (multiple-value-bind (red green blue alpha)	  
			(values r.fg g.fg b.fg 255)
		      ,image-set-code
		      (values red green blue alpha))
		    (multiple-value-bind (r.bg g.bg b.bg a.bg)
			,image-get-code
		      (multiple-value-bind (red green blue alpha)	  
			  (octet-blend-function r.bg g.bg b.bg a.bg r.fg g.fg b.fg
                                                (octet-mult a.fg alpha))
			,image-set-code
			(values red green blue alpha))))))))))
  
(defmacro %make-xor-draw-function-macro (data-type image-get-code image-set-code source-code)
  `(lambda (data x y alpha)
     (declare (type ,data-type data)
	      (type fixnum x y)
	      (type fixnum alpha))
     (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
       (setf alpha (min (abs alpha) 255))
       (when (plusp alpha)
	 (multiple-value-bind (r.bg g.bg b.bg a.bg)
	     ,image-get-code
	   (multiple-value-bind (r.fg g.fg b.fg a.fg)
	       ,source-code
	     (multiple-value-bind (red green blue alpha)
		 (octet-blend-function r.bg g.bg b.bg a.bg
                                       (color-octet-xor r.bg r.fg) (color-octet-xor g.bg g.fg)
                                       (color-octet-xor b.bg b.fg) (octet-mult a.fg alpha))
	       ,image-set-code
	       (values red green blue alpha))))))))

(defmacro %make-xor-draw-span-function-macro (data-type image-get-code image-set-code source-code)
  `(lambda (data x1 x2 y alpha)
     (declare (type ,data-type data)
	      (type fixnum x1 x2 y)
	      (type fixnum alpha))
     (setf alpha (min (abs alpha) 255))
     (when (plusp alpha)
       (loop for x from x1 below x2 do
	    (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	      (multiple-value-bind (r.bg g.bg b.bg a.bg)
		  ,image-get-code
		(multiple-value-bind (r.fg g.fg b.fg a.fg)
		    ,source-code
		  (multiple-value-bind (red green blue alpha)
		      (octet-blend-function r.bg g.bg b.bg a.bg
                                            (color-octet-xor r.bg r.fg) (color-octet-xor g.bg g.fg)
                                            (color-octet-xor b.bg b.fg) (octet-mult a.fg alpha))
		    ,image-set-code
		    (values red green blue alpha)))))))))

;;;
;;; function used by aa engine
;;;

(declaim (inline render-line-f))
(defun render-line-f (state mxx mxy myx myy tx ty x1 y1 x2 y2)
  (declare (type coordinate mxx mxy myx myy tx ty))
  (let ((x1 (+ (* mxx x1) (* mxy y1) tx))
	(y1 (+ (* myx x1) (* myy y1) ty))
	(x2 (+ (* mxx x2) (* mxy y2) tx))
	(y2 (+ (* myx x2) (* myy y2) ty)))
    (aa::line-f state x1 y1 x2 y2)))

(defun render-update-state (state paths transformation)
  (multiple-value-bind (mxx mxy myx myy tx ty)
      (climi::get-transformation transformation)
    (if (listp paths)
	(dolist (path paths)
	  (%render-update-state state path mxx mxy myx myy tx ty))
	(%render-update-state state paths mxx mxy myx myy tx ty))))


(defun %render-update-state (state paths mxx mxy myx myy tx ty)
  (let ((iterator (vectors::path-iterator-segmented paths)))
    (multiple-value-bind (i1 k1 e1) (vectors::path-iterator-next iterator)
      (declare (ignore i1))
      (when (and k1 (not e1))
	;; at least 2 knots
	(let ((first-knot k1))
	  (loop
	     (multiple-value-bind (i2 k2 e2) (vectors::path-iterator-next iterator)
	       (declare (ignore i2))
	       (render-line-f state mxx mxy myx myy tx ty
			      (vectors::point-x k1) (vectors::point-y k1)
			      (vectors::point-x k2) (vectors::point-y k2))
	       (setf k1 k2)
	       (when e2
		 (return))))
	  (render-line-f state mxx mxy myx myy tx ty
			 (vectors::point-x k1) (vectors::point-y k1)
			 (vectors::point-x first-knot) (vectors::point-y first-knot)))))
    state))

(defun render-scanline-sweep (data scanline function function-span &key start end)
  "Call FUNCTION for each pixel on the polygon covered by
SCANLINE. The pixels are scanned in increasing X. The sweep can
be limited to a range by START (included) or/and END (excluded)."
  (declare (optimize speed (debug 0) (safety 0) (space 2))
	   (type fixnum start end)
	   (type (function (t fixnum fixnum octet) t) function)
	   (type (function (t fixnum fixnum fixnum octet) t) function-span))
  (let ((cover 0)
        (y (aa::scanline-y scanline))
        (cells scanline)
        (last-x 0))
    (declare (type fixnum last-x))
    (when start
      ;; skip initial cells that are before START
      (loop while (and cells (< (aa::cell-x (car cells)) start))
         do (incf cover (aa::cell-cover (car cells)))
         (setf last-x (aa::cell-x (car cells))
               cells (cdr cells))))
    (when cells
      (dolist (cell cells)
        (let ((x (aa::cell-x cell)))
          (when (and last-x (> x (1+ last-x)))
            (let ((alpha (aa::compute-alpha cover 0)))
              (unless (zerop alpha)
                (let ((start-x (if start (max start (1+ last-x)) (1+ last-x)))
                      (end-x (if end (min end x) x)))
                  (if function-span
                      (funcall function-span data start-x end-x y alpha)
                      (loop for ix from start-x below end-x
                         do (funcall function data ix y alpha)))))))
          (when (and end (>= x end))
            (return))
          (incf cover (aa::cell-cover cell))
          (let ((alpha (aa::compute-alpha cover (aa::cell-area cell))))
            (unless (zerop alpha)
              (funcall function data x y alpha)))
          (setf last-x x))))))


(defun render-cells-sweep/rectangle (data state x1 y1 x2 y2 function &optional function-span)
  "Call FUNCTION for each pixel on the polygon described by
previous call to LINE or LINE-F. The pixels are scanned in
increasing Y, then on increasing X. This is limited to the
rectangle region specified with (X1,Y1)-(X2,Y2) (where X2 must be
greater than X1 and Y2 must be greater than Y1, to describe a
non-empty region.)

For optimization purpose, the optional FUNCTION-SPAN, if
provided, is called for a full span of identical alpha pixel. If
not provided, a call is made to FUNCTION for each pixel in the
span."
  (let ((scanlines (aa::freeze-state state)))
    (dolist (scanline scanlines)
      (when (<= y1 (aa::scanline-y scanline) (1- y2))
	(render-scanline-sweep data scanline function function-span :start x1 :end x2))))
  (values))

