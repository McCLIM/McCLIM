(in-package :clim-demo)

(defparameter *drawing-tests* (make-hash-table :test 'equal))

(defparameter *width* 500)
(defparameter *height* 700)
(defparameter *border-width* 5)

(defstruct drawing-test name description drawer)

(defmacro define-drawing-test (name arglist description &body body)
  (check-type name string)
  (check-type description string)
  `(setf (gethash ,name *drawing-tests*)
         (make-drawing-test :name ,name
                         :description ,description
                         :drawer (lambda ,arglist ,@body))))

(define-application-frame drawing-tests ()
  ((recording-p :initform t)
   (signal-condition-p :initform nil)
   (current-selection :initform nil))
  (:panes
   (backend-output :application-pane 
		   :min-width *width*
		   :min-height *height*
                   :display-time nil
                   :display-function #'display-backend-output
		   :end-of-line-action :wrap
		   :end-of-page-action :wrap)
   (render-output :application-pane
		  :min-width *width*
		  :min-height *height*
                  :display-time nil
                  :display-function #'display-render-output
		  :end-of-line-action :wrap
		  :end-of-page-action :wrap)
   (description :application-pane)
   (selector :list-pane
             :mode :exclusive
             :name-key #'drawing-test-name
             :items (sort (loop for x being the hash-values of *drawing-tests*
                                collect x) #'string< :key #'drawing-test-name)
             :value-changed-callback #'%update-selection)
   (recording-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-recording-option)
      (clim:radio-box-current-selection "yes")
      "no"))
   (condition-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-condition-option)
      (clim:radio-box-current-selection "message")
      "break"))
   (print-ps :push-button
	     :label "Print All (/tmp/*.ps)"
	     :activate-callback #'(lambda (x)
                                    (declare (ignore x))
                                    (print-all-postscript-tests)))
   (print-pdf :push-button
              :label "Print All (/tmp/*.pdf)"
              :activate-callback #'(lambda (x)
                                     (declare (ignore x))
                                     (print-all-pdf-tests)))
   (print-png :push-button
	      :label "Print All (/tmp/*.png)"
	      :activate-callback #'(lambda (x)
                                     (declare (ignore x))
                                     (print-all-raster-image-tests :png))))
  (:layouts
   (default
     (spacing (:thickness 3)
       (horizontally ()
	 (vertically ()
	   (spacing (:thickness 3)
	     (clim-extensions:lowering ()
	       (scrolling (:scroll-bar :vertical :height *height*) selector)))
	   (labelling (:label "Recording")
             recording-option)
           (labelling (:label "Condition")
             condition-option)
	   print-ps
           print-pdf
	   print-png)
         (vertically ()
	   (spacing (:thickness 3)
	     (clim-extensions:lowering ()
	       (horizontally ()
		 (labelling (:label "Backend")
		   (scrolling (:width *width* :height *height* :scroll-bar nil)
		     backend-output))
		 (labelling (:label "Render")
		   (scrolling (:width *width* :height *height* :scroll-bar nil)
                     render-output)))))
	   (spacing (:thickness 3)
	     (clim-extensions:lowering ()
	       (scrolling (:scroll-bar :vertical :height 200) description)))))))))

(defun %update-recording-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (recording-p) clim:*application-frame*
    (setf recording-p
	  (string= (clim:gadget-label selected-gadget) "yes"))))

(defun %update-condition-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (signal-condition-p) clim:*application-frame*
    (setf signal-condition-p
	  (string= (clim:gadget-label selected-gadget) "break"))))

(defun %update-selection (pane item)
  (declare (ignore pane))
  (with-slots (current-selection) clim:*application-frame*
    (setf current-selection item))
  (window-clear (get-frame-pane *application-frame* 'description))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'backend-output) :force-p t)
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'render-output) :force-p t))

(defun display-backend-output (frame pane)
  (let ((output (get-frame-pane frame 'backend-output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane *application-frame* 'description)))
      (when item
        (with-text-style (description (make-text-style :sans-serif :roman :normal))
          (write-string (drawing-test-description item) description))
        (if (slot-value *application-frame* 'signal-condition-p)
            (clim:with-drawing-options (output :clipping-region
                                               (clim:make-rectangle* 0 0 *width* *height*))
              (clim:draw-rectangle* output 0 0 *width* *height* :filled t
                                    :ink clim:+grey90+)
              (funcall (drawing-test-drawer item) output))
            (handler-case
                (clim:with-drawing-options (output :clipping-region
                                                   (clim:make-rectangle* 0 0 *width* *height*))
                  (clim:draw-rectangle* output 0 0 *width* *height* :filled t
                                        :ink clim:+grey90+)
                  (funcall (drawing-test-drawer item) output))
              (simple-error (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "Backend:~a~%" condition)))))))))

(defun display-render-output (frame pane)
  (declare (ignore pane))
  (let ((output (get-frame-pane frame 'render-output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane *application-frame* 'description)))
      (when item
        (if (slot-value *application-frame* 'signal-condition-p)
            (with-slots (recording-p) clim:*application-frame*
              (let ((pattern (mcclim-raster-image::with-output-to-image-pattern
                                 (stream :width *width* :height *height* :border-width *border-width*
                                         :recording-p recording-p)
                               (clim:draw-rectangle* stream 0 0 *width* *height* :filled t
                                                     :ink clim:+grey90+)
                               (funcall (drawing-test-drawer item) stream))))
                (draw-pattern* output pattern 0 0)
                (medium-finish-output (sheet-medium output))))
            (handler-case
                (with-slots (recording-p) clim:*application-frame*
                  (let ((pattern (mcclim-raster-image::with-output-to-image-pattern
                                     (stream :width *width* :height *height* :border-width *border-width*
                                             :recording-p recording-p)
                                   (clim:draw-rectangle* stream 0 0 *width* *height* :filled t
                                                         :ink clim:+grey90+)
                                   (funcall (drawing-test-drawer item) stream))))
                    (draw-pattern* output pattern 0 0)
                    (medium-finish-output (sheet-medium output))))
              (simple-error (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "Render:~a~%" condition)))))))))

(defun run-drawing-tests ()
  (run-frame-top-level
   (make-application-frame
    'drawing-tests)))

(defun drawing-test-postscript (test &optional filename)
  (let* ((test (if (stringp test) (gethash test *drawing-tests*) test))
         (test-name (drawing-test-name test))
         (filename (or filename (format nil "/tmp/~a.eps"
                                        test-name))))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (with-output-to-postscript-stream (stream out :device-type :eps)
        #+nil
        (with-text-style (stream (make-text-style :sans-serif :roman :normal))
          (format stream "~&~a: ~a~%" test-name (drawing-test-description test)))
        (funcall (drawing-test-drawer test) stream)))))

(defun print-all-postscript-tests ()
  (loop for test being the hash-values of *drawing-tests* do
       (restart-case (drawing-test-postscript test)
         (:skip ()
           :report (lambda (stream) (format stream "skip ~a" (drawing-test-name test)))))))


(defun drawing-test-pdf (test &optional filename)
  (let* ((test (if (stringp test) (gethash test *drawing-tests*) test))
         (test-name (drawing-test-name test))
         (filename (or filename (format nil "/tmp/~a.pdf"
                                        test-name))))
    (with-open-file (out filename :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (clim-pdf:with-output-to-pdf-stream (stream out)
        (with-text-style (stream (make-text-style :sans-serif :roman :normal))
          (format stream "~&~a: ~a~%" test-name (drawing-test-description test)))
        (funcall (drawing-test-drawer test) stream)))))

(defun print-all-pdf-tests ()
  (loop for test being the hash-values of *drawing-tests* do
       (restart-case (drawing-test-pdf test)
         (:skip ()
           :report (lambda (stream) (format stream "skip ~a" (drawing-test-name test)))))))

(defun drawing-test-raster-image (test format &optional filename)
  (let* ((test (if (stringp test) (gethash test *drawing-tests*) test))
         (test-name (drawing-test-name test))
         (filename (or filename (format nil "/tmp/output-~a.~a"
                                        test-name format))))
    (with-open-file (out filename :element-type '(unsigned-byte 8)
			 :direction :output :if-exists :supersede)
      (mcclim-raster-image:with-output-to-raster-image-stream
	  (stream out format :width *width* :height *height*)
	(clim:draw-rectangle* stream 0 0 *width* *height* :filled t
			      :ink clim:+grey90+)
        (funcall (drawing-test-drawer test) stream)))))

(defun print-all-raster-image-tests (format)
  (time
   (loop for test being the hash-values of *drawing-tests* do
	(restart-case (drawing-test-raster-image test format)
	  (:skip ()
	    :report (lambda (stream) (format stream "skip ~a" (drawing-test-name test))))))))

;;;
;;; utility functions
;;;

(defun make-random-col ()
  (clim:make-rgb-color (/ (random 255) 255)
                  (/ (random 255) 255)
                  (/ (random 255) 255)))

(defun make-random-alpha ()
  (clim:make-rgb-color (/ (random 255) 255)
                  (/ (random 255) 255)
                  (/ (random 255) 255)))

(defun draw-rosette2 (stream x y radius n &rest drawing-options)
  (loop with alpha = (/ (* 2 pi) n)
        and radius = (/ radius 2)
        for i below n
        do (apply #'clim:draw-circle* stream
                  (+ (* radius (cos (* alpha i))) x)
                  (+ (* radius (sin (* alpha i))) y)
                  radius
                  :filled nil
                  drawing-options)))

(defun test-simple-clipping-region (stream draw-fn)
  (let ((cr (make-rectangle* 50 50 (- *width* 50) (- *height* 50))))
    (with-bounding-rectangle* (min-x min-y max-x max-y)
	cr
      (draw-rectangle* stream (- min-x 10) (- min-y 10) (+ max-x 10) (+ max-y 10) :line-thickness 2 :filled t :ink +green+)
      (draw-rectangle* stream min-x min-y max-x max-y :line-thickness 1 :filled nil)
      (with-drawing-options (stream :clipping-region cr)
	(draw-rectangle* stream min-x min-y max-x max-y :filled t :ink +grey60+)
	(loop repeat 30
	   do (funcall draw-fn stream))))))

(defun test-simple-scale-region (stream draw-fn)
    (let ((y 100))
      (dolist (sc '(0.3 0.5 1 1.5))
	(draw-text* stream (format nil "~a " sc) 20 y)
	(draw-rectangle* stream 140 (- y 60) 260 (+ y 60) :ink +grey20+ :filled nil)
	(draw-rectangle* stream 290 (- y 60) 410 (+ y 60) :ink +grey20+ :filled nil)
	(with-scaling (stream sc sc (make-point 200 y))
	  (funcall draw-fn stream 200 y))
	(with-scaling (stream sc sc (make-point 300 (- y 50)))
	  (funcall draw-fn stream 350 y))
	(draw-point* stream 200 y :ink +red+ :line-thickness 5)
	(draw-point* stream 300 (- y 50) :ink +red+ :line-thickness 5)
	(setf y (+ 150 y)))))

(defun test-simple-rotation-region (stream draw-fn)
    (let ((y 100))
      (dolist (sc (list 0 (/ pi 8) (/ pi 6) (/ pi 4)))
	
	(draw-text* stream (format nil "~2$" sc) 20 y)
	(draw-rectangle* stream 140 (- y 60) 260 (+ y 60) :ink +grey20+ :filled nil)
	(draw-rectangle* stream 290 (- y 60) 410 (+ y 60) :ink +grey20+ :filled nil)
	(with-rotation (stream sc (make-point 200 y))
	  (funcall draw-fn stream 200 y))
	(with-rotation (stream sc (make-point 300 (- y 50)))
	  (funcall draw-fn stream 350 y))
	(draw-point* stream 200 y :ink +red+ :line-thickness 5)
	(draw-point* stream 300 (- y 50) :ink +red+ :line-thickness 5)
	(setf y (+ 150 y)))))

;;;
;;; Testing
;;;

;;;
;;; Testing points
;;;

(define-drawing-test "01) Point Basic" (stream)
    ""
  (let ((y 20))
    (dolist (lu '(:normal :point :coordinate))
      (dolist (lt '(1 5 6 7 10))
	(with-drawing-options (stream :line-thickness lt :line-unit lu)
	  (draw-text* stream (format nil "~A ~A" lu lt) 20 y)
	  (let ((x-positions '(150 170 190 210 230 250 270 290)))
	    (let ((position-seq
		   (mapcan #'(lambda (x) (list x y))
			   x-positions)))
	      (draw-points* stream position-seq))
	    (dolist (x-pos x-positions)
	      (draw-point* stream x-pos (+ 10 y)))
	    (let ((point-seq (mapcar #'(lambda (x) (make-point x (+ 20 y)))
				     x-positions)))
	      (draw-points stream point-seq)
	      (dolist (p point-seq)
		(draw-point stream (make-point (point-x p) (+ 10 (point-y p)))))))
	  (setf y (+ 40 y)))))))

(define-drawing-test "01) Point Scaling" (stream)
    ""
  (let ((y 20))
    (dolist (sc '(1 0.5 1.5))
      (dolist (lu '(:normal :point :coordinate))
	(dolist (lt '(5))
	  (with-scaling (stream sc sc (make-point 20 y))
	    (with-drawing-options (stream :line-thickness lt :line-unit lu)
	      (draw-text* stream (format nil "~A ~A ~A" sc lu lt) 20 y)
	      (let ((x-positions '(250 260 270 280 290)))
		(dolist (x-pos x-positions)
		  (draw-point* stream x-pos y)))))
	  (setf y (+ 50 y)))))))

(define-drawing-test "01) Point Transformation" (stream)
    ""
  (let ((y 20))
    (with-drawing-options (stream :line-thickness 1)
      (draw-line* stream 230 (- y 10) 230 470)
      (draw-line* stream 310 (- y 10) 310 470))
    (with-drawing-options (stream :line-thickness 5)
      (dolist (tr '((0 . 0) (-10 . 0) (0 . 10)))
	(dolist (ro '(0 0.2 0.5))
	  (draw-line* stream 230 (- y 10) 310 (- y 10) :line-thickness 1 :ink +grey22+)
	  (draw-text* stream (format nil "~A ~A" tr ro) 20 y)
	  (with-translation (stream (car tr) (cdr tr))
	    (with-rotation (stream ro (make-point 250 y)) 
	      (let ((x-positions '(250 260 270 280 290)))
		(dolist (x-pos x-positions)
		  (draw-point* stream x-pos y)))))
	  (setf y (+ 50 y)))))))

			 
(define-drawing-test "01) Point Clipping" (stream)
    #.(format nil "Points should be drawn only inside the green frame. Anything ~
outside the clipping area should be grey.")
  (test-simple-clipping-region stream
			       #'(lambda (stream)
				   (clim:draw-point* stream (random *width*) (random *height*)
						     :ink (make-random-col)
						     :line-thickness (random 100)))))

;;;
;;;
;;;

(define-drawing-test "02) Line Basic" (stream)
    ""
  (let ((y 20))
    (dolist (lc '(:round :butt :square :no-end-point))
      (dolist (lt '(1 3 5 7))
	(with-drawing-options (stream :line-thickness lt :line-cap-shape lc)
	  (draw-text* stream (format nil "~A ~A" lc lt) 20 y)
	  (let ((x-positions '(150 300)))
	    (let ((position-seq
		   (mapcan #'(lambda (x) (list x y (+ x 100) y))
			   x-positions)))
	      (draw-lines* stream position-seq))
	    (dolist (x-pos x-positions)
	      (draw-line* stream x-pos (+ 10 y) (+ x-pos 100) (+ 10 y)))
	    (let ((point-seq (mapcan #'(lambda (x) (list (make-point x (+ 20 y))
							 (make-point (+ x 100) (+ 20 y))))
				     x-positions)))
	      (draw-lines stream point-seq)
	      (dolist (x-pos x-positions)
		(draw-line stream
			   (make-point x-pos (+ 30 y))
			   (make-point (+ x-pos 100) (+ 30 y))))))
	  (setf y (+ 40 y)))))))

(define-drawing-test "02) Line Dashes" (stream)
    ""
  (let ((y 20))
    (dolist (lc '(:round :butt :square))
      (dolist (ld '( (5 3) (8 8) (5 3 3 5)))
	(dolist (lt '(1 3 5))
	  (with-drawing-options (stream :line-thickness lt :line-dashes ld :line-cap-shape lc)
	    (draw-text* stream (format nil "~A ~A ~A" ld lt lc) 20 y)
	    (draw-line* stream 170 y 450 y))
	  (setf y (+ 20 y)))))))

(define-drawing-test "02) Line Transformation" (stream)
    ""
  (let ((y 20))
    (with-drawing-options (stream :line-thickness 1)
      (draw-line* stream 200 (- y 10) 200 470)
      (draw-line* stream 350 (- y 10) 350 470))
    (with-drawing-options (stream :line-thickness 5)
      (dolist (tr '((0 . 0) (-10 . 0) (0 . 10)))
	(dolist (ro '(0 0.2 0.5))
	  (draw-line* stream 230 (- y 10) 310 (- y 10) :line-thickness 1 :ink +grey22+)
	  (draw-text* stream (format nil "~A ~A" tr ro) 20 y)
	  (with-translation (stream (car tr) (cdr tr))
	    (with-rotation (stream ro (make-point 220 y)) 
	      (draw-line* stream 220 y 330 y)))
	  (setf y (+ 50 y)))))))

(define-drawing-test "02) Line Clipping" (stream)
    #.(format nil "Lines should be drawn only inside the green frame. Anything ~
outside the clipping area should be grey.")
    (test-simple-clipping-region stream
				 #'(lambda (stream)
				     (let ((x (random *width*))
					   (y (random *height*)))
				   (clim:draw-line* stream
						    x y
						    (+ (random 100)  x) (+ (random 100) y)
						    :ink (make-random-col)
						    :line-thickness (random 10))))))
;;;
;;; Polygon
;;;

(define-drawing-test "03) Polygon Cap Shape"  (stream)
  ""
  (flet ((draw (angle line-joint-shape)
	   (let ((v (* 50 (tan angle))))
	     (clim:draw-polygon* stream (list 20 0 100 0 50 v)
				 :closed nil
				 :filled nil
				 :line-thickness 20
				 :line-joint-shape line-joint-shape
				 :line-cap-shape :round)
	     (clim:draw-polygon* stream (list 20 0 100 0 50 v)
				 :closed nil
				 :filled nil
				 :line-thickness 0.01
				 :ink clim:+green+))))
  (loop with dag = 2
     with da = (* pi (/ dag 180))
     for i from -10 to 10
     for a = (* i da)
     unless (= i 0)
     do 
       (with-translation (stream 10 (+ 30 (* 32 (+ 10 i))))
	 (draw a :miter)
	 (with-translation (stream 170 0)
	   (draw a :bevel))
	 (with-translation (stream 320 0)
	   (draw a :round))))))


(define-drawing-test "03) Polygon Clipping" (stream)
    #.(format nil "Polygons should be drawn only inside the green frame. Anything ~
outside the clipping area should be grey.")
    (test-simple-clipping-region stream
				 #'(lambda (stream)
				     (let ((v (mapcan #'(lambda (x)
                                                          (declare (ignore x))
                                                          (list (random *width*) (random *height*)))
						      '(1 2 3))))
				       (clim:draw-polygon* stream v
							   :closed t
							   :filled t
							   :line-thickness (random 10)
							   :ink (make-random-col))))))
(define-drawing-test "03) Polygon Scale" (stream)
    ""
    (test-simple-scale-region stream
			      #'(lambda (stream cx cy)
				  (clim:draw-polygon* stream (list (- cx 50) (- cy 50)
								   (- cx 30) (- cy 40)
								   (- cx 20) (- cy 20)
								   cx cy
								   (+ cx 10) (+ cy 40)
								   (+ cx 50) (+ cy 50))
						      :closed nil
						      :filled nil
						      :line-thickness 4))))


(define-drawing-test "03) Polygon Rotation" (stream)
    ""
    (test-simple-rotation-region stream
				 #'(lambda (stream cx cy)
				     (clim:draw-polygon* stream (list (- cx 50) (- cy 50)
								      (- cx 30) (- cy 40)
								      (- cx 20) (- cy 20)
								      cx cy
								      (+ cx 10) (+ cy 40)
								      (+ cx 50) (+ cy 50))
							 :closed nil
							 :filled nil
							 :line-thickness 4))))

		     

;;;
;;; Rectangle
;;;

(define-drawing-test "04) Rectangle Basic" (stream)
    ""
  (let ((y 20))
    (dolist (lc '(:round :bevel :miter))
      (dolist (lt '(5 9))
	(with-drawing-options (stream :line-thickness lt :line-joint-shape lc)
	  (draw-text* stream (format nil "~A ~A" lc lt) 20 y)
	  (let ((x-positions '(150 300)))
	    (let ((position-seq
		   (mapcan #'(lambda (x) (list x y (+ x 100) (+ 15 y)))
			   x-positions)))
	      (draw-rectangles* stream position-seq :ink +yellow+))
	    
	    (dolist (x-pos x-positions)
	      (draw-rectangle* stream x-pos (+ 25 y) (+ x-pos 100) (+ 40 y)
			       :ink +green+
			       :filled nil))
	    (let ((point-seq (mapcan #'(lambda (x) (list (make-point x (+ 50 y))
							 (make-point (+ x 100) (+ 65 y))))
				     x-positions)))
	      (draw-rectangles stream point-seq :ink +blue+))
	    
	    (dolist (x-pos x-positions)
	      (draw-rectangle stream
			      (make-point x-pos (+ 75 y))
			      (make-point (+ x-pos 100) (+ 90 y))
			      :ink +red+
			      :filled nil))
	    
	    )
	  (setf y (+ 115 y)))))))

(define-drawing-test "04) Rectangle Dashes" (stream)
    ""
    (let ((y 20))
      (dolist (lj '(:round :bevel :miter))
	(dolist (ld '( (5 3) (8 8) (5 3 3 5)))
	  (dolist (lt '(2 7))
	    (with-drawing-options (stream :line-thickness lt :line-joint-shape lj :line-dashes ld)
	      (draw-text* stream (format nil "~A ~A ~A" ld lj lt) 20 y)
	      (draw-rectangle* stream 200 (+ 10 y) 450 (+ 35 y)
                               :filled nil)
	      (setf y (+ 40 y))))))))

(define-drawing-test "04) Rectangle Clipping" (stream)
    #.(format nil "Rectangles should be drawn only inside the green frame. Anything ~
outside the clipping area should be grey.")
    (test-simple-clipping-region stream
				 #'(lambda (stream)
				     (let ((x (random *width*))
					   (y (random *height*)))
				   (clim:draw-rectangle* stream
							 x y
							 (+ (random 100) x) (+ (random 100) y)
							 :ink (make-random-col)
							 :line-thickness (random 10)
							 :filled t)))))

(define-drawing-test "04) Rectangle Scale" (stream)
    ""
    (test-simple-scale-region stream
			      #'(lambda (stream cx cy)
				  (clim:draw-rectangle* stream
							(- cx 50) (- cy 50) (+ cx 50) (+ cy 50)
							:line-thickness 4
							:filled nil))))

(define-drawing-test "04) Rectangle Rotation" (stream)
    ""
    (test-simple-rotation-region stream
			      #'(lambda (stream cx cy)
				  (clim:draw-rectangle* stream
							(- cx 50) (- cy 50) (+ cx 50) (+ cy 50)
							:line-thickness 4
							:filled nil))))


;;;
;;; Ellipse
;;;

(define-drawing-test "05) Ellipse Basic" (stream)
    ""
  (let ((y 20))
    (dolist (lc '(:round :butt :square))
      (dolist (lt '(1 3 7))
	(with-drawing-options (stream :line-thickness lt :line-cap-shape lc)
	  (draw-text* stream (format nil "~A ~A" lc lt) 20 y)
	  (draw-ellipse* stream 150 y 30 0 0 10 :filled nil :start-angle 0 :end-angle (/ (* 6 pi) 4))
	  (draw-ellipse* stream 230 y 30 0 0 10 :filled t :start-angle 0 :end-angle (/ (* 6 pi) 4))
	  (draw-ellipse stream (make-point 310 y) 30 0 0 10 :filled nil :start-angle 0 :end-angle (/ (* 6 pi) 4))
	  (draw-ellipse stream (make-point 390 y) 30 0 0 10 :filled t :start-angle 0 :end-angle (/ (* 6 pi) 4)))
	(setf y (+ 60 y))))))

(define-drawing-test "05) Ellipse Dashes" (stream)
    ""
    (let ((y 20))
      (dolist (ld '( (5 3) (8 8)))
	(dolist (lt '(2 7 10))
	  (with-drawing-options (stream :line-thickness lt :line-dashes ld)
	    (draw-text* stream (format nil "~A ~A" ld lt) 20 y)
	    (draw-ellipse* stream 150 (+ 10 y) 0 30 50 0
                           :filled nil)
            (draw-ellipse* stream 270 (+ 10 y) 0 30 50 0 :start-angle (/ pi 2) :end-angle pi
                           :filled nil)
            (draw-ellipse* stream 390 (+ 10 y) 0 30 50 0 :start-angle (- (/ pi 2)) :end-angle pi
                           :filled nil)
	    (setf y (+ 80 y)))))))

(define-drawing-test "05) Ellipse Clipping" (stream)
    #.(format nil "Ellipses should be drawn only inside the green frame. Anything ~
outside the clipping area should be grey.")
  (test-simple-clipping-region stream
                               #'(lambda (stream)
                                   (draw-ellipse* stream (random *width*) (random *height*)
                                                  (+ 1 (random 50)) 0 0 (+ 1 (random 50))
                                                  :line-thickness (random 5)
                                                  :ink (make-random-col)
                                                  :filled t))))

(define-drawing-test "05) Ellipse Scale" (stream)
    ""
    (test-simple-scale-region stream
			      #'(lambda (stream cx cy)
				  (clim:draw-ellipse* stream
						      cx cy
						      50 0 0 30
						      :line-thickness 4
						      :filled nil))))

(define-drawing-test "05) Ellipse Rotation" (stream)
    ""
    (test-simple-rotation-region stream
				 #'(lambda (stream cx cy)
				     (clim:draw-ellipse* stream
							 cx cy
							 50 0 0 30
							 :line-thickness 4
							 :filled nil))))

(define-drawing-test "05) Ellipse slope line intersection" (stream)
    "Ellipse with rotation and limited angle is surrounded by its bounding
rectangle. Slope lines go through the sheet and the intersections of lines with
the ellipse are drawn in different color and thickness."
  (let* ((cx (/ *width* 2))
         (cy (/ *height* 2))
         (start-angle (/ pi 8))
         (end-angle (+ (* 3 (/ pi 2)) (/ pi 4)))
         (ellipse (clim:make-ellipse* cx cy
                                      100 -200
                                      -100 -50
                                      :start-angle start-angle
                                      :end-angle end-angle))
         (clip-rect (make-rectangle* 0 0 *width* *height*))
         (line-distance 50))
    (with-bounding-rectangle* (min-x min-y max-x max-y) ellipse
      (draw-rectangle* stream min-x min-y max-x max-y
                       :ink +dark-blue+ :line-thickness 2 :filled nil)
      (draw-design stream ellipse :ink +gray+)
      (dotimes (v (round (/ *height* (/ line-distance 2))))
        (let* ((tr (make-translation-transformation
                    0 (* (* v line-distance) (sqrt 2))))
               (line (make-line* 0 (- *height*) *width* 0))
               (line (transform-region tr line))
               (line (region-intersection line clip-rect)))
          (draw-design stream line)
          (draw-design stream (region-intersection ellipse line)
                       :ink +dark-red+ :line-thickness 5)))
      (dotimes (v (round (/ *height* (/ line-distance 2))))
        (let* ((tr (make-translation-transformation
                    0 (* (* v line-distance) (sqrt 2))))
               (line (make-line* 0 0 *width* (- *height*)))
               (line (transform-region tr line))
               (line (region-intersection line clip-rect)))
          (draw-design stream line)
          (draw-design stream (region-intersection ellipse line)
                       :ink +dark-green+ :line-thickness 5))))))

(define-drawing-test "05) Ellipse xy-line intersection" (stream)
    "Ellipse with rotation and limited angle is surrounded by its bounding
rectangle. Horizontal and vertical lines go through the sheet and the
intersections of lines with the ellipse are d1rawn in different color and
thickness."
  (let* ((cx (/ *width* 2))
         (cy (/ *height* 2))
         (start-angle (/ pi 8))
         (end-angle (+ (* 3 (/ pi 2)) (/ pi 4)))
         (ellipse (clim:make-ellipse* cx cy
                                      100 -200
                                      -100 -50
                                      :start-angle start-angle
                                      :end-angle end-angle))
         (clip-rect (make-rectangle* 0 0 *width* *height*))
         (line-distance 50))
    (with-bounding-rectangle* (min-x min-y max-x max-y) ellipse
      (draw-rectangle* stream min-x min-y max-x max-y
                       :ink +dark-blue+ :line-thickness 2 :filled nil)
      (draw-design stream ellipse :ink +gray+)
      (dotimes (v (round (/ *height* (/ line-distance 2))))
        (let* ((tr (make-translation-transformation 0 (* v line-distance)))
               (line (make-line* 0 0 *width* 0))
               (line (transform-region tr line))
               (line (region-intersection line clip-rect)))
          (draw-design stream line)
          (draw-design stream (region-intersection ellipse line)
                       :ink +dark-red+ :line-thickness 5)))
      (dotimes (h (round (/ *width* (/ line-distance 2))))
        (let* ((tr (make-translation-transformation (* h line-distance) 0))
               (line (make-line* 0 0 0 *height*))
               (line (transform-region tr line))
               (line (region-intersection line clip-rect)))
          (draw-design stream line)
          (draw-design stream (region-intersection ellipse line)
                       :ink +dark-green+ :line-thickness 5))))))

(define-drawing-test "05) Ellipse angle transformations" (stream)
    "Uses internal interfaces. We should see 6 ellipses with limited
angle. First four should be filled and should have angle radius drawn. Last two
should not be filled (and no angle radius is drawn). Each ellipse is bound by
its bounding rectangle and surrounded by four points marking where min-x, max-x,
min-y and max-y are (extremum points) are."
  (let* ((sa (/ pi 2))
         (ea (+ pi (* 3 (/ pi 4))))
         (ellipse-1 (clim:make-ellipse* 0 0 +40 -80 -40 -20 :start-angle sa :end-angle ea))
         (ellipse-2 (clim:make-ellipse* 0 0 +40 -80 +40 +20 :start-angle sa :end-angle ea))
         (ellipse-3 (clim:make-ellipse* 0 0 -40 -20 +40 +80 :start-angle sa :end-angle ea))
         (ellipse-4 (clim:make-ellipse* 0 0 +40 +20 -40 +80 :start-angle sa :end-angle ea)))
    (flet ((draw-el (el x y rays-p &rest options)
             (with-translation (stream x y)
               (multiple-value-bind (x-min y-min x-max y-max)
                   (climi::ellipse-bounding-rectangle el)
                 (draw-point* stream x-min 0 :line-thickness 10 :ink +dark-red+)
                 (draw-point* stream x-max 0 :line-thickness 10 :ink +dark-red+)
                 (draw-point* stream 0 y-min :line-thickness 10 :ink +dark-blue+)
                 (draw-point* stream 0 y-max :line-thickness 10 :ink +dark-blue+))
               (draw-design stream (bounding-rectangle el)
                            :line-thickness 3 :ink +dark-goldenrod+ :filled nil)
               (apply #'draw-design stream el options)
               (when rays-p
                 (draw-design stream
                              (multiple-value-call #'make-line*
                                0 0
                                (climi::%ellipse-angle->position el sa))
                              :ink +dark-green+ :line-thickness 3)
                 (draw-design stream
                              (multiple-value-call #'make-line*
                                0 0
                                (climi::%ellipse-angle->position el ea))
                              :ink +dark-red+ :line-thickness 3)))))
      (draw-el ellipse-1 100 100 t :ink +gray+)
      (draw-el ellipse-2 300 100 t :ink +gray+)
      (draw-el ellipse-3 100 300 t :ink +gray+)
      (draw-el ellipse-4 300 300 t :ink +gray+)
      (draw-el ellipse-1 100 500 nil :ink +red+ :filled nil :line-thickness 10)
      (draw-el ellipse-3 300 500 nil :ink +red+ :filled nil :line-thickness 10))))

;;;
;;; Circle
;;;

(define-drawing-test "06) Circle Basic" (stream)
    ""
  (let ((y 30))
    (dolist (lc '(:round :butt :square))
      (dolist (lt '(1 3 7))
	(with-drawing-options (stream :line-thickness lt :line-cap-shape lc)
	  (draw-text* stream (format nil "~A ~A" lc lt) 20 y)
	  (draw-circle* stream 150 y 25 :filled nil :start-angle 0 :end-angle (/ (* 6 pi) 4))
	  (draw-circle* stream 230 y 25 :filled t :start-angle 0 :end-angle (/ (* 6 pi) 4))
	  (draw-circle stream (make-point 310 y) 25 :filled nil :start-angle 0 :end-angle (/ (* 6 pi) 4))
	  (draw-circle stream (make-point 390 y) 25 :filled t :start-angle 0 :end-angle (/ (* 6 pi) 4)))
	(setf y (+ 70 y))))))

(define-drawing-test "06) Circle Dashes" (stream)
    ""
    (let ((y 20))
      (dolist (ld '( (5 3) (8 8)))
	(dolist (lt '(2 7 10))
	  (with-drawing-options (stream :line-thickness lt :line-dashes ld)
	    (draw-text* stream (format nil "~A ~A" ld lt) 20 y)
	    (draw-circle* stream 200 (+ 10 y) 30
                          :filled nil)
            (draw-circle* stream 290 (+ 10 y) 30 :start-angle (/ pi 2) :end-angle pi
                          :filled nil)
            (draw-circle* stream 380 (+ 10 y) 30 :start-angle (- (/ pi 2)) :end-angle pi
                          :filled nil)
	    (setf y (+ 80 y)))))))

(define-drawing-test "06) Circle Clipping" (stream)
    #.(format nil "Circles should be drawn only inside the green frame. Anything ~
outside the clipping area should be grey.")
  (test-simple-clipping-region stream
                               #'(lambda (stream)
                                   (draw-circle* stream (random *width*) (random *height*)
                                                 (+ 1 (random 50))
                                                 :line-thickness (random 5)
                                                 :ink (make-random-col)
                                                 :filled t))))

(define-drawing-test "06) Circle Scale" (stream)
    ""
    (test-simple-scale-region stream
			      #'(lambda (stream cx cy)
				  (clim:draw-circle* stream
						      cx cy
						      50 
						      :line-thickness 4
						      :filled nil))))
;;;
;;; Text 
;;;

(define-drawing-test "07) Text Align" (stream)
  ""
  (clim:with-text-style (stream '(:serif nil :huge))
    (clim:draw-text* stream "Text Align" 170 20
		     :text-family :sans-serif
		     :text-face :bold)
    (loop for align-y in '(:bottom :center :top)
       and y from 1
       do (loop for align-x in '(:right :center :left)
	     and x from 1
	     do
	       (clim:draw-text* stream (format nil "~A~A"
					       (elt (symbol-name align-x) 0)
					       (elt (symbol-name align-y) 0))
				(* 50 x) (* 50 y)
				:align-x align-x
				:align-y align-y)
	       (clim:draw-point* stream (* 50 x) (* 50 y)
				 :ink clim:+red+
				 :line-thickness 5
				 :line-unit :point)))
    (clim:draw-text* stream "Top" 20 220
		     :align-y :top)
    (clim:draw-text* stream "Bottom" 120 220
		     :align-y :bottom)
    (clim:draw-text* stream "Center" 250 220
		     :align-y :center)
    (clim:draw-text* stream "Baseline" 370 220
		     :align-y :baseline)
    (clim:draw-line* stream 20 220 500 220
		     :ink clim:+red+)))

(define-drawing-test "07) Text Underlining" (stream)
    ""
    (with-text-family (stream :sans-serif)
    (format stream "~&We all live in a yellow subroutine.~%")
    (format stream "~&We ")
    (surrounding-output-with-border (stream :shape :underline
					    :line-thickness 2
					    :move-cursor nil)
      (format stream "all live"))
    (format stream " in a yellow subroutine.~%")
    (format stream "~&We ")
    (surrounding-output-with-border (stream :shape :underline
					    :ink +red+
					    :line-dashes t
					    :move-cursor nil)
      (format stream "all live"))
    (format stream " in a yellow subroutine.~%")
    (format stream "~&We all live in a yellow subroutine.~%")
    (format stream "~&We all live in a yellow subroutine.~%")))

(define-drawing-test "07) Text Fonts" (stream)
    ""
  (let ((y 30))
    (dolist (fam '(:fix :serif :sans-serif))
      (dolist (face '(:roman :italic :bold))
	(dolist (size '(:normal :huge))
	  (clim:draw-text* stream (format nil "~A ~A ~A" fam face size)
			   20 y
			   :text-style (clim:make-text-style fam face size))
	  (setf y (+ 30 y)))))))


(define-drawing-test "07) Text Tranformation" (stream)
    ""
  (with-drawing-options (stream :text-style (clim:make-text-style  :sans-serif :italic :huge))
    (clim:draw-text* stream "normal" 20 30)
    (clim:draw-text* stream "scale 0" 20 60)
    (with-scaling (stream 2 2 (make-point 200 60))
      (clim:draw-text* stream "scale 2" 200 60))
    (clim:draw-text* stream "rotation 0" 20 90)
    (with-rotation (stream (/ pi 4) (make-point 200 90))
      (clim:draw-text* stream "rotation 90" 200 90))
    (with-translation (stream 20 30)
      (clim:draw-text* stream "translation 20 20" 200 90))

    (clim:draw-text* stream "toward y" 20 200 :toward-y 30)
    (clim:draw-text* stream "toward x" 200 200 :toward-x 30)))

(define-drawing-test "07) Text Clipping" (stream)
    #.(format nil "Text should be drawn only inside the green frame. Anything ~
outside the clipping area should be grey.")
    (test-simple-clipping-region stream
				 #'(lambda (stream)
				     (draw-text* stream (format nil "~A" (random 10))
						 (random *width*) (random *height*)
						 :text-size (random 100)
						 :ink (make-random-col)))))

(define-drawing-test "07) Text Scale" (stream)
    ""
    (test-simple-scale-region stream
			      #'(lambda (stream cx cy)
				  (clim:draw-text* stream
						   "Ciao"
						   cx cy
						   :align-x :center :align-y :center
						   :text-size 50))))


(define-drawing-test "07) Text Rotation" (stream)
    ""
  (test-simple-rotation-region stream
			       #'(lambda (stream cx cy)
				   (clim:draw-text* stream
						    "Ciao"
						    cx cy
						    :align-x :center :align-y :center
						    :text-size 50))))

(define-drawing-test "07) Text - 1" (stream)
  ""
  (dotimes (i 40)
    (format stream "~&row ~A abcdefghilmnopqrstuvz ABCDEFGHILMNOPRSTUVZ~%" i)))

(define-drawing-test "07) Text - 2" (stream)
  ""
  (dotimes (i 10)
    (format stream "~&row ~A abcdefghilmnopqrstuvz ABCDEFGHILMNOPRSTUVZ 1234567890 <>.;-~%" i)))

(defun draw-vstrecke2 (stream x y1 y2 &rest args &key ink &allow-other-keys)
  (draw-line* stream (- x 10) y1 (+ x 10) y1 :ink ink)
  (draw-line* stream (- x 10) y2 (+ x 10) y2 :ink ink)
  (apply #'draw-arrow* stream x y1 x y2 args))

(defun draw-hstrecke2 (stream y x1 x2 &rest args &key ink &allow-other-keys)
  (draw-line* stream x1 (- y 10) x1 (+ y 10) :ink ink)
  (draw-line* stream x2 (- y 10) x2 (+ y 10) :ink ink)
  (apply #'draw-arrow* stream x1 y x2 y args))

(defun legend-text-style2 ()
  (make-text-style :sans-serif :roman :small))

(defun draw-legend2 (stream &rest entries)
  (let* ((style (legend-text-style))
	 (y 2)
	 (h (nth-value 1 (text-size stream "dummy" :text-style style))))
    (dolist (entry entries)
      (when entry
	(incf y h)
	(let ((y* (+ 0.5 (round (- y (/ h 2))))))
	  (apply #'draw-line* stream 2 y* 35 y* (cdr entry)))
	(draw-text* stream (car entry) 40 y :text-style style)))))

(define-drawing-test "07) Text Size" (stream)
    ""
  (let* ((pane-width (rectangle-width (sheet-region stream)))
	 (pane-height (rectangle-height (sheet-region stream)))
	 (str "Ciao")
         (rectangle :text-size)
	 (style (make-text-style :sans-serif :roman 100))
	 (medium (sheet-medium stream)))
    (multiple-value-bind (width height final-x final-y baseline)
	(text-size stream str :text-style style)
      (let* ((x1 (/ (- pane-width width) 2))
	     (y1 (/ (- pane-height height) 2))
	     (ybase (+ y1 baseline)))
	(draw-text* stream
		    (format nil "fixed-width-p: ~(~A~)"
			    (handler-case
				(text-style-fixed-width-p style medium)
			      (error (c)
				c)))
		    2
		    pane-height
		    :text-style (legend-text-style))
	(draw-legend2 stream
		     (list "Ascent"
			   ;; :line-style (make-line-style :dashes '(1.5))
			   :ink +black+)
		     (list "Descent" :ink +black+)
		     (list "Height"
			   :line-style (make-line-style :thickness 2)
			   :ink +black+)
		     (list "Width (Avg.)" :ink +black+)
		     (list "Baseline" :ink +green+)
		     (when (eq rectangle :text-bounding-rectangle)
		       (list "Bounding rectangle" :ink +purple+))
		     (when (eq rectangle :text-size)
		       (list "Text size (width/height)" :ink +red+))
		     (when (eq rectangle :text-size)
		       (list "Text size (final x/y)" :ink +blue+)))
	(draw-vstrecke2 stream
		      (- x1 20)
		      ybase
		      (- ybase (text-style-ascent style medium))
		      ;; :line-style (make-line-style :dashes '(1.5))
		      :ink +black+)
	(draw-vstrecke stream
		      (- x1 20)
		      ybase
		      (+ ybase (text-style-descent style medium))
		      :ink +black+)
	(draw-vstrecke2 stream
		      (- x1 40)
		      y1
		      (+ y1 (text-style-height style medium))
		      :line-style (make-line-style :thickness 2)
		      :ink +black+)
	(draw-hstrecke2 stream
		       (- y1 20)
		       x1
		       (+ x1 (text-style-width style medium))
		       :ink +black+)
	(draw-line* stream
		    0 ybase
		    pane-width ybase
		    :ink +green+)
	(draw-text* stream str x1 ybase :text-style style)
	;; Gtkairo's DRAW-TEXT* understands multiple lines.
	;; (CLIM-CLX doesn't like multiple lines much.)
	;;
	;; If we use WRITE-STRING instead of DRAW-TEXT, the frontend will
	;; handle the line breaks, but lines 2..n will start at x = 0 rather
	;; than x = x1, confusing our diagram.
;;;	(setf (stream-cursor-position stream) (values x1 y1))
;;;	(with-text-style (stream style)
;;;	  (write-string str stream))
        (ecase rectangle
          ((:text-size)
           (draw-rectangle* stream
                            x1 y1
                            (+ x1 width) (+ y1 height)
                            :ink +red+
                            :filled nil)
	    (draw-line* stream
			0 (+ y1 final-y)
			pane-width (+ y1 final-y)
			:ink +blue+)
	    (draw-line* stream
			(+ x1 final-x) 0
			(+ x1 final-x) pane-height
			:ink +blue+))
          ((:text-bounding-rectangle)
           (multiple-value-bind (left top right bottom)
               (climi::text-bounding-rectangle* medium str :text-style style)
             (draw-rectangle* stream 
                              (+ x1 left) (+ y1 baseline top)
                              (+ x1 right) (+ y1 baseline bottom)
                              :ink +purple+
                              :filled nil))))))))


;;;
;;; Arrow
;;;

(define-drawing-test "08) Arrows" (stream)
    ""
  (let ((scale 1.2)
        (from-head t)
        (to-head t))
  (with-room-for-graphics (stream :first-quadrant nil)
   (with-scaling (stream scale scale)
     (loop for theta from 0.0 below (* 2 pi) by (/ (* 2 pi) 17) do
          (progn (let* ((x2 (* 250 (sin theta)))
                        (y2 (* 250 (cos theta)))
                        (x1 (* 0.2 x2))
                        (y1 (* 0.2 y2)))
                   (draw-arrow* stream x1 y1 x2 y2
                                :line-thickness (1+ (* 8 theta))
                                :head-width (* 5 (1+ theta))
                                :to-head to-head
                                :from-head from-head
                                :head-length (* 10 (1+ theta)) )
                   (draw-point* stream x1 y1 :ink +red+ :line-thickness 5)
                   (draw-point* stream x2 y2 :ink +green+ :line-thickness 5))))))))


;;;
;;; Oval
;;;

(define-drawing-test "09) Ovals" (stream)
    ""
  (let ((scale 0.8))
      (with-room-for-graphics (stream :first-quadrant nil)
	(with-scaling (stream scale scale)
	  (with-translation (stream  0 100)
	    (loop for theta from 0.0 below (* 2 pi) by (/ (* 2 pi) 11) do
		 (progn (let* ((x2 (* 250 (sin theta)))
			       (y2 (* 250 (cos theta)))
			       (x1 (* 0.2 x2))
			       (y1 (* 0.2 y2)))
			  (draw-oval* stream
				      (/ (+ x1 x2) 2)
				      (/ (+ y1 y2) 2)
				      (abs (/ (- x2 x1) 2))
				      (abs (/ (- y2 y1) 2))
				      :line-thickness 3
				      :filled nil)
			  (draw-point* stream (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
				       :ink +blue+ :line-thickness 5)
			  (draw-point* stream x1 y1 :ink +red+ :line-thickness 5)
			  (draw-point* stream x2 y2 :ink +green+ :line-thickness 5)))))))))

;;;
;;; Clipping
;;;

(define-drawing-test "10) Clipping region ellipse" (stream)
    "Non-rectangular clipping region. We should see grey rotated ellipse with
limited angle drawn inside green border. This ellipse is a clipping region. Then
we randomly drawn points on the screen which should be clipped to the grey area."
  (let ((cr (clim:make-ellipse* (/ *width* 2) (/ *height* 2)
                                (- (/ *width* 2) 50) 100
                                0 (- (/ *height* 2) 50)
                                :start-angle (/ pi 8)
                                :end-angle (* 3 (/ pi 2)))))
    (with-bounding-rectangle* (min-x min-y max-x max-y)	cr
      (draw-rectangle* stream (- min-x 10) (- min-y 10) (+ max-x 10) (+ max-y 10)
                       :line-thickness 2 :filled t :ink +green+)
      (draw-rectangle* stream min-x min-y max-x max-y
                       :line-thickness 1 :filled nil)
      (with-drawing-options (stream :clipping-region cr)
        (draw-rectangle* stream min-x min-y max-x max-y :filled t :ink +grey50+)
        (loop repeat 100
           do (clim:draw-point* stream (random *width*) (random *height*)
                                :ink (make-random-col)
                                :line-thickness (random 100)))))))

(define-drawing-test "10) Clipping Region" (stream)
    "Various clipping regions. We should see seven blue bounding
rectangles. Each of them host a clipping area: square, rotated rectangle,
circle, polygon, region intersection, region union and region difference (last
three are based on rectangles). Randomly drawn points should be clipped to these
clipping areas. No point should be drawn outside the blue rectangles."
  (dolist (cr (list
	       (make-rectangle* 20 20 120 120)
	       (clim:transform-region (clim:make-rotation-transformation
					   0.5 (make-point 250 70))
				      (make-rectangle* 200 20 300 120))
	       (clim:make-ellipse* 70 230 0 50 50 0)
	       (clim:make-polygon* (list 200 170 250 180 300 200 300 250 280 280 200 270 210 250))
	       (clim:region-intersection
		(make-rectangle* 20 300 90 370)
		(make-rectangle* 50 330 120 400))
	       (clim:region-union
		(make-rectangle* 170 300 240 370)
		(make-rectangle* 200 330 270 400))
	       (clim:region-difference
		(make-rectangle* 320 300 390 370)
		(make-rectangle* 350 330 420 400))))
    (with-bounding-rectangle* (min-x min-y max-x max-y)
	cr
      (draw-rectangle* stream (- min-x 2) (- min-y 2) (+ max-x 2) (+ max-y 2) :line-thickness 4 :filled nil :ink +blue+)
      ;;(draw-design  stream cr :ink +red+)
      (with-drawing-options (stream :clipping-region cr)
	(draw-rectangle* stream min-x min-y max-x max-y :filled t :ink +grey50+)
	(loop repeat 100
	   do (clim:draw-point* stream (random 500) (random 500)
				:ink (make-random-col)
				:line-thickness (random 100)))))))


;;;
;;; Bordered Output
;;;
(define-drawing-test "11) Bordered Empty Records 1" (stream)                                          
    ""
  (surrounding-output-with-border (stream :shape :rectangle)
    (draw-circle* stream 100 200 40)
    (with-new-output-record (stream)))
  (surrounding-output-with-border (stream :shape :oval)
    (draw-circle* stream 300 200 40)
    (with-new-output-record (stream)))
  (surrounding-output-with-border (stream :shape :rounded)
    (draw-circle* stream 100 350 40)
    (with-new-output-record (stream)))
  (surrounding-output-with-border (stream :shape :ellipse)
    (draw-circle* stream 300 350 40)
    (with-new-output-record (stream)))
(surrounding-output-with-border (stream :shape :drop-shadow)
    (draw-circle* stream 100 500 40)
    (with-new-output-record (stream))))
                                           
(define-drawing-test "11) Bordered Empty Borders" (stream)
    ""
  (with-room-for-graphics (stream :first-quadrant nil)
    (with-text-style (stream (make-text-style :sans-serif :roman :small))
      (loop with outer-radius = 180
            with inner-radius = 27
            with n = 12
            for i from 0 below n do
            (setf (stream-cursor-position stream)
                  (values (* outer-radius (sin (* i 2 pi (/ n))))
                          (* outer-radius (cos (* i 2 pi (/ n))))))
            (surrounding-output-with-border (stream :shape :ellipse
                                                    :circle t
                                                    :min-radius inner-radius
                                                    :shadow +gray88+
                                                    :shadow-offset 7
                                                    :filled t
						    :line-thickness 1
                                                    :background +gray50+
                                                    :outline-ink +gray40+))))))

;;;
;;;  Table
;;;

(define-drawing-test "12) Table - Polygon" (stream)
  ""
  (clim:draw-text* stream "Text test" 170 20
		   :text-family :sans-serif
		   :text-face :bold)
  (clim:formatting-table (stream)
    (flet ((draw (angle line-joint-shape)
	     (let ((record
		    (clim:with-output-to-output-record (stream)
		      (let ((v (* 50 (tan angle))))
			;;(when (< v 0)
			;;  (setf v 0))
			(clim:draw-polygon* stream (list 20 0 100 0 50 v)
					    :closed nil
					    :filled nil
					    :line-thickness 40
					    :line-joint-shape line-joint-shape
					    :line-cap-shape :round)
			(clim:draw-polygon* stream (list 20 0 100 0 50 v)
					    :closed nil
					    :filled nil
					    :line-thickness 0.01
					    :ink clim:+green+)))))
	       (multiple-value-call #'clim:draw-rectangle*
		 stream (clim:bounding-rectangle* record)
		 :filled nil
		 :ink clim:+red+ :line-thickness 0.01)
	       (clim:stream-add-output-record stream record)
	       (clim:replay record stream))))
      (loop with dag = 2
	 with da = (* pi (/ dag 180))
	 for i from -10 to 10
	 for a = (* i da)
	 unless (= i 0)
	 do (clim:formatting-row (stream)
	      (clim:formatting-cell (stream) (print (* i dag) stream))
	      (clim:formatting-cell (stream) (draw a :miter))
	      (clim:formatting-cell (stream) (draw a :bevel))
	      (clim:formatting-cell (stream) (draw a :round)))))))

(define-drawing-test "12) Table dashes" (stream)
  ""
  (clim:formatting-table (stream :x-spacing 50
				 :y-spacing 20)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream))
      (clim:formatting-cell (stream :align-x :center
				    :align-y :bottom
				    :min-height 100)
	(clim:draw-text* stream "(Test Page)" 170 30
			 :text-style (clim:make-text-style :fix :bold :huge))
	))
    (loop for i from 1 to 15
       do (clim:formatting-row (stream)
	    (clim:formatting-cell (stream :align-x :right
					  :align-y :center
					  :min-width 100)
	      (clim:draw-point* stream 0 0 :line-thickness i))
	    (clim:formatting-cell (stream :align-x :center
					  :align-y :center)
	      (clim:draw-line* stream 0 0 200 0
			       :line-thickness i
			       :line-dashes (list (* i 2) (round i 2))))
	    (clim:formatting-cell (stream :align-x :right
					  :align-y :center)
	      (clim:draw-text* stream (format nil "~D" i) 0 0
			       :text-style (clim:make-text-style
			       :sans-serif :bold :huge))
	      )))))


;;;
;;; Ink
;;;

(define-drawing-test "13) Ink Transparent" (stream)
    ""
  (let ((table '((1 1 1 0 1)
                 (1 1 1 0 1)
                 (1 1 1 0 1)
                 (0 0 0 2 0)
                 (1 1 1 0 1)))
        (inks (list +transparent-ink+ +red+ +blue+))
        (records nil))
    ;; Draw some junk to make sure the transparent ink is really transparent,
    ;; and not just matching the background:
    (dotimes (i 200)
      (draw-circle* stream (- (random 500) 100) (- (random 500) 100) (1+ (* 30 (random 1.0) (random 1.0))) :ink +blue+))
    ;; Draw two tables:
    (format-items '(0 2) :stream stream :printer
      (lambda (foo stream)
        ;; Why isn't there an :equalize-row-heights ?
        (surrounding-output-with-border (stream)
          (formatting-table (stream :equalize-column-widths nil)
            (dolist (row table)
              (formatting-row (stream)
                (dolist (cell row)
                  (formatting-cell (stream)
                    (push
                     (with-new-output-record (stream)
                       (draw-rectangle* stream 0 0 25 25
                                        :ink (elt inks (if (eql cell 2)
                                                           foo
                                                           cell))))
                     records)))))))))
    ;; Make sure the bounding rectangles are the same:
    (unless (reduce
             (lambda (a b)
               (and a
                    (> 1 (abs (- (bounding-rectangle-width a)
                                 (bounding-rectangle-width b))))
                    (> 1 (abs (- (bounding-rectangle-height a)
                                 (bounding-rectangle-height b))))
                    b))
             records)
      (format stream "~&The bounding rectangles don't look right..~%"))))


(define-drawing-test "13) Ink Opaque" (stream)
    ""
  (let ((table '((1 1 1 0 1)
                 (1 1 1 0 1)
                 (1 1 1 0 1)
                 (0 0 0 2 0)
                 (1 1 1 0 1)))
        (inks (list (make-opacity 0.5) +red+ +blue+))
        (records nil))
    ;; Draw some junk to make sure the transparent ink is really transparent,
    ;; and not just matching the background:
    (dotimes (i 200)
      (draw-circle* stream (- (random 500) 100) (- (random 500) 100) (1+ (* 30 (random 1.0) (random 1.0))) :ink +blue+))
    ;; Draw two tables:
    (format-items '(0 2) :stream stream :printer
      (lambda (foo stream)
        ;; Why isn't there an :equalize-row-heights ?
        (surrounding-output-with-border (stream)
          (formatting-table (stream :equalize-column-widths nil)
            (dolist (row table)
              (formatting-row (stream)
                (dolist (cell row)
                  (formatting-cell (stream)
                    (push
                     (with-new-output-record (stream)
                       (draw-rectangle* stream 0 0 25 25
                                        :ink (elt inks (if (eql cell 2)
                                                           foo
                                                           cell))))
                     records)))))))))
    ;; Make sure the bounding rectangles are the same:
    (unless (reduce
             (lambda (a b)
               (and a
                    (> 1 (abs (- (bounding-rectangle-width a)
                                 (bounding-rectangle-width b))))
                    (> 1 (abs (- (bounding-rectangle-height a)
                                 (bounding-rectangle-height b))))
                    b))
             records)
      (format stream "~&The bounding rectangles don't look right..~%"))))


(define-drawing-test "13) Ink - compose in" (stream)
    ""
  (let ((y 20))
    (with-drawing-options (stream :line-thickness 5)
      (dolist (ink (list +foreground-ink+ +transparent-ink+
			 (clim:compose-in +red+
					  (clim:make-opacity 0.25))
			 (clim:compose-in clim:+red+
					  (clim:make-opacity 0.50))
			 clim:+green+))
	(draw-text* stream (format nil "~A" ink) 20 (- y 10) :text-size 8)
	(draw-text* stream "Text" 20 (+ 20 y) :ink ink)
	(draw-rectangle* stream 70 y 170 (+ y 20) :ink ink)
	(draw-circle* stream 220 (+ 10 y) 20 :ink ink :filled nil)
	(setf y (+ 70 y))))))

(define-drawing-test "13) Ink - compose out" (stream)
    ""
  (let ((y 20))
    (with-drawing-options (stream :line-thickness 5)
      (dolist (ink (list +foreground-ink+ +transparent-ink+
			 (clim:compose-out +red+
					   (clim:make-opacity 0.25))
			 (clim:compose-out clim:+red+
					   (clim:make-opacity 0.50))
			 clim:+green+))
	(draw-text* stream (format nil "~A" ink) 20 (- y 10) :text-size 8)
	(draw-text* stream "Text" 20 (+ 20 y) :ink ink)
	(draw-rectangle* stream 70 y 170 (+ y 20) :ink ink)
	(draw-circle* stream 220 (+ 10 y) 20 :ink ink :filled nil)
	(setf y (+ 70 y))))))

(define-drawing-test "13) Ink - compose over" (stream)
    ""
  (let ((y 20))
    (with-drawing-options (stream :line-thickness 5)
      (dolist (ink (list +foreground-ink+ +transparent-ink+
			 (compose-over (clim:compose-in +red+
							(clim:make-opacity 0.25))
				       +green+)
			 (compose-over (clim:compose-out clim:+red+
							(clim:make-opacity 0.50))
				      (clim:make-opacity 0.50))
			 (compose-over +red+
				       clim:+green+)))
	(draw-text* stream (format nil "~A" ink) 20 (- y 10) :text-size 8)
	(draw-text* stream "Text" 20 (+ 20 y) :ink ink)
	(draw-rectangle* stream 70 y 170 (+ y 20) :ink ink)
	(draw-circle* stream 220 (+ 10 y) 20 :ink ink :filled nil)
	(setf y (+ 70 y))))))

(define-drawing-test "13) Ink - flipping" (stream)
    ""
  (let ((y 20)
	(flipping +flipping-ink+))
    (draw-rectangle* stream 0 0 400 400 :filled t :ink +background-ink+)
    (with-drawing-options (stream :line-thickness 5)
      (dolist (ink (list flipping flipping ))
	(draw-text* stream (format nil "~A" ink) 20 (- y 10) :text-size 8 :ink ink)
	(draw-text* stream "Text" 20 (+ 40 y) :ink ink)
	(draw-rectangle* stream 70 y 170 (+ y 40) :ink ink)
	(draw-circle* stream 220 (+ 10 y) 20 :ink ink :filled nil)
	(setf y (+ 30 y))))))



(define-drawing-test "13) Ink - flipping 2" (stream)
    ""
  (let ((y 20)
	(flipping (clim:make-flipping-ink +red+ +green+)))
    (draw-rectangle* stream 0 0 400 400 :filled t :ink +red+)
    (with-drawing-options (stream :line-thickness 5)
      (dolist (ink (list flipping flipping))
	(draw-text* stream (format nil "~A" ink) 20 (- y 10) :text-size 8 :ink ink)
	(draw-text* stream "Text" 20 (+ 40 y) :ink ink)
	(draw-rectangle* stream 70 y 170 (+ y 50) :ink ink)
	(draw-circle* stream 220 (+ 10 y) 20 :ink ink :filled nil)
	(setf y (+ 30 y))))))

(define-drawing-test "13) Ink - Pattern" (stream)
    ""
  (let* ((y 20)
	 (array #2A((0 0 0 1 1 0 0 0)
		    (0 0 1 1 1 1 0 0)
		    (0 1 1 1 1 1 1 0)
		    (1 1 1 0 0 1 1 1)
		    (1 1 1 0 0 1 1 1)
		    (0 1 1 1 1 1 1 0)
		    (0 0 1 1 1 1 0 0)
		    (0 0 0 1 1 0 0 0)))
	 (pattern1 (clim:make-pattern array
				      (list clim:+red+ clim:+green+)))
	 (pattern2 (clim:make-pattern array
				      (list clim:+green+ clim:+red+)))
	 (pattern3 (clim:make-pattern array
				      (list clim:+blue+ clim:+yellow+))))
    (with-drawing-options (stream :line-thickness 5)
      (dolist (ink (list (clim:make-rectangular-tile pattern1 8 8)
			 (clim:make-rectangular-tile pattern1 6 6)
			 (clim:make-rectangular-tile pattern2 8 8)
			 (clim:make-rectangular-tile pattern2 6 6)
			 (clim:make-rectangular-tile pattern3 8 8)
			 (clim:make-rectangular-tile pattern3 6 6)))
	(draw-text* stream "Text" 20 (+ 20 y) :ink ink :text-size 30)
	(draw-rectangle* stream 100 y 170 (+ y 20) :ink ink)
	(draw-circle* stream 220 (+ 10 y) 20 :ink ink :filled nil)
	(setf y (+ 70 y))))))

;;;
;;; Pictures
;;;

(defun picture01 (stream)
  (clim:draw-rectangle* stream 10 10 200 150 :filled nil
			:line-thickness 6
			;;:line-joint-shape :none)
			)
  (clim:draw-rectangle* stream 20 20 210 160 :filled nil
			:line-joint-shape :round
			:line-thickness 6)
  (clim:draw-rectangle* stream 30 30 220 170 :filled nil
			:line-joint-shape :bevel
			:line-thickness 6)
  (clim:draw-rectangle* stream 40 40 230 180 :filled nil
			:line-joint-shape :miter
			:line-thickness 6)
  
  (clim:draw-line* stream 200 10 10 150 :line-thickness 5 :line-cap-shape :round)
  (clim:draw-line* stream 220 10 30 150 :line-thickness 5 :line-cap-shape :butt)
  (clim:draw-line* stream 240 10 50 150 :line-thickness 5 :line-cap-shape :square)
  (clim:draw-line* stream 260 10 70 150 :line-thickness 5 :line-cap-shape :no-end-point)
  
  (clim:draw-point* stream 180 25)
  (clim:draw-circle* stream 100 75 40 :filled nil :ink +green+)
  (clim:draw-ellipse* stream 160 110 30 0 0 10 :filled nil :ink +green+)
  (clim:draw-ellipse* stream 160 110 10 0 0 30
		      :filled t
		      :ink +red+)
  (clim:draw-polygon* stream '(20 20 50 80 40 20) :filled nil :ink +blue+)
  (clim:draw-polygon* stream '(30 90 40 110 20 110) :ink +blue+))

(define-drawing-test "14) Picture - 1" (stream)
    ""
    (picture01 stream)
    (clim:with-translation (stream 400 200)
      (clim:with-scaling (stream 1.6)
	(clim:with-rotation (stream (/ pi 2))
	  (picture01 stream)))))

(define-drawing-test "14) Pictures - Rosette" (stream)
  ""
  (draw-rosette2 stream 250 300 180 18
		:ink clim:+steel-blue+ :line-thickness 2))


;;;
;;; Pixmap
;;;

(define-drawing-test "15) Pixmap 1" (stream)
    ""
  (let ((pixmap (with-output-to-pixmap (m stream :width 200 :height 200)
		  (draw-rectangle* m 0 0 200 200 :ink +green+)
		  (draw-rosette2 m 100 100 80 18
				:ink clim:+steel-blue+ :line-thickness 2))))
    (copy-from-pixmap pixmap 0 0 200 200 stream 0 0)
    (copy-from-pixmap pixmap 50 50 100 100 stream 250 50)
    (with-drawing-options (stream :clipping-region (make-rectangle* 70 270 130 330))
      (copy-from-pixmap pixmap 50 50 100 100 stream 50 250))
    (with-drawing-options (stream)
      (copy-from-pixmap pixmap 50 50 100 100 stream 250 250))
    (with-translation (stream -50 0)
      (copy-from-pixmap pixmap 50 50 100 100 stream 250 250))
    (with-translation (stream 0 50)
      (copy-from-pixmap pixmap 50 50 100 100 stream 250 250))))

    
(define-drawing-test "15) Pixmap 2" (stream)
    ""
  (let ((pixmap (with-output-to-pixmap (m stream :width 200 :height 200)
		  (draw-rectangle* m 0 0 200 200 :ink +red+)
		  (picture01 m))))
    (copy-from-pixmap pixmap 0 0 200 200 stream 0 0)
    (draw-rectangle* stream 50 50 150 150 :ink +green+ :filled nil)
    (copy-from-pixmap pixmap 50 50 100 100 stream 0 250)
    (copy-from-pixmap pixmap 50 50 100 100 stream 150 250)))

(define-drawing-test "15) Pixmap 3" (stream)
    ""
  (let ((pixmap (with-output-to-pixmap (m stream :width 200 :height 200)
		  (draw-rectangle* m 0 0 200 200 :ink +red+)
		  (picture01 m))))
    (copy-from-pixmap pixmap 0 0 200 200 stream 0 0)
    (copy-to-pixmap stream 0 0 100 100 pixmap 100 100)
    (copy-from-pixmap pixmap 0 0 200 200 stream 0 200)
    (copy-to-pixmap stream 0 0 50 50 pixmap 100 100)
    (copy-from-pixmap pixmap 0 0 200 200 stream 200 0)))

;;;
;;; RGB design
;;;

(define-drawing-test "RGB Design" (stream)
  ""
  (let ((pattern (mcclim-raster-image:with-output-to-rgb-pattern
		     (stream :width 200 :height 200)
		   (clim:draw-rectangle* stream 0 0 200 200 :filled t
					 :ink clim:+grey90+)
		   (draw-rectangle* stream 0 0 200 200 :ink +yellow+)
		   (picture01 stream))))
    (draw-pattern* stream pattern 0 0)
    (clim:draw-rectangle* stream 50 50 150 150 :filled nil :line-thickness 5 :ink clim:+blue+)
    (with-drawing-options (stream :clipping-region (make-rectangle* 250 50 350 150))
      (draw-pattern* stream pattern 200 0))
    (with-drawing-options (stream :clipping-region (make-rectangle* 250 200 350 300))
      (with-translation (stream 0 50)
	(draw-pattern* stream pattern 200 150)))
    (with-drawing-options (stream :clipping-region (make-rectangle* 250 350 350 450))
      (with-translation (stream 50 0)
	(draw-pattern* stream pattern 200 300)))))

;;;
;;; Bezier curves
;;;

;;; FIXME! The bezier-curve stuff (as opposed to the bezier-area) is
;;; currently broken and the test case is commented out for now.
(define-drawing-test "16) Bezier Area" (stream)
    "Draws a single bezier-area. Currently this is quite slow and needs to be optimized. Also, the shape of the drawn bezier area is not particularly attractive."
  (let* ((r1 (mcclim-bezier:make-bezier-area* '(120 160 35 200 220 280 220 40 180 160 160 180 120 160))))
    (mcclim-bezier:draw-bezier-design* stream r1 :ink +cyan2+)))

(define-drawing-test "16) Bezier Curve" (stream)
    "Draws a single bezier curve. This is currently broken as it should just draw the stroke of the bezier and instead renders the design as a bezier area."
  (let* ((r4 (mcclim-bezier:make-bezier-curve* (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140))))
    (mcclim-bezier:draw-bezier-design* stream r4
                                       :line-thickness 12
                                       :ink +orange+)))

(define-drawing-test "16) Bezier Test 3" (stream)
    "Some more complicated bezier design drawings. We draw two overlapping bezier areas, the difference between these two areas, a bezier curve, and a convolution of a curve and an area."
  (let* ((r1 (mcclim-bezier:make-bezier-area* '(100 100 200 200 300 200 400 100 300 50 200 50 100 100)))
         (r2 (mcclim-bezier:make-bezier-area* '(150 100 200 120 300 150 350 100 300 80 200 80 150 100)))
         (r3 (mcclim-bezier:region-difference r1 r2))
         (r4 (mcclim-bezier:make-bezier-curve* (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140)))
         (r5 (mcclim-bezier:convolve-regions r2 r4)))
    (mcclim-bezier:draw-bezier-design* stream r3)
    (mcclim-bezier:draw-bezier-design* stream r4
                                       :line-thickness 12
                                       :ink +orange+)
    (mcclim-bezier:draw-bezier-design* stream r5)))

(define-drawing-test "16) Bezier Test 4" (stream)
    "Some more complicated bezier design drawings."
  (formatting-table (stream :x-spacing 20
                            :y-spacing 20)
    (formatting-row (stream)
      (formatting-cell (stream :align-x :center
                               :align-y :bottom
                               :min-height 110)
        (draw-text* stream "Bezier Test" 170 30
                    :text-style (make-text-style :fix :bold :normal))))
    (formatting-row (stream)
      (formatting-cell (stream :align-x :left :align-y :center)
        (let ((line-thickness 4))
          (draw-circle* stream 30 30 20
                        :start-angle (/ pi 2)
                        :end-angle (+ (/ pi 2) pi)
                        :filled nil
                        :line-thickness line-thickness)
          (draw-circle* stream 60 30 20
                        :start-angle (+ (/ pi 2) pi)
                        :end-angle (/ pi 2)
                        :filled nil
                        :line-thickness line-thickness)
          (draw-rectangle* stream 0 0 10 10 :ink +green+)
          (draw-rectangle* stream 3 3 13 13 :ink +red+)
          (draw-rectangle* stream 34 44 247 256 :ink +yellow+)
          (draw-polygon* stream '(10 200 50 120 120 200) :ink +blue+)
          (let ((design
                 (mcclim-bezier:make-bezier-area*
                  (list 34 44 34 128 147 44 247 256 34 128 50 50 34 44))))
            (mcclim-bezier:draw-bezier-design* stream design
                                               :line-thickness line-thickness
                                               :ink +black+))
          (let ((design
                 (mcclim-bezier:make-bezier-curve*
                  (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140))))
            (mcclim-bezier:draw-bezier-design* stream design
                                               :line-thickness line-thickness
                                               :ink +royal-blue+))))
      (formatting-cell (stream :align-x :left
                               :align-y :center)
        (let ((line-thickness 4))
          (draw-circle* stream 30 30 20
                        :start-angle (/ pi 2)
                        :end-angle (+ (/ pi 2) pi)
                        :filled nil
                        :line-thickness line-thickness)
          (draw-circle* stream 60 30 20
                        :start-angle (+ (/ pi 2) pi)
                        :end-angle (/ pi 2)
                        :filled nil
                        :line-thickness line-thickness)
          (draw-rectangle* stream 0 0 10 10 :ink +green+)
          (draw-rectangle* stream 3 3 13 13 :ink +red+)
          (draw-rectangle* stream 34 44 247 256 :ink +pink+)
          (draw-polygon* stream '(10 200 50 120 120 200) :ink +blue+)
          (let ((design
                 (mcclim-bezier:make-bezier-area*
                  (list 34 44 34 128 147 44 247 256 34 128 50 50 34 44))))
            (mcclim-bezier:draw-bezier-design* stream design
                                               :line-thickness 8
                                               :ink +sea-green+))
          (let ((design (mcclim-bezier:make-bezier-curve*
                         (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140))))
            (mcclim-bezier:draw-bezier-design* stream design
                                               :line-thickness 16
                                               :ink +orange+)))))))

(define-drawing-test "16) Bezier Test 5" (stream)
    "Some more complicated bezier design drawings."
  (formatting-table (stream :x-spacing 20 :y-spacing 20)
    (formatting-row (stream)
      (formatting-cell (stream :min-height 80)))
    (formatting-row (stream)
      (formatting-cell (stream :align-x :left
                               :align-y :top)
        (loop for i from 0 to 300 by 10
           do
             (draw-line* stream i 0 i 300 :line-thickness 2 :ink +black+)
             (draw-line* stream 0 i 300 i :line-thickness 2 :ink +black+))
        (draw-rectangle* stream 100 100 200 200 :ink +blue+)
        (draw-text* stream "Bogus" 200 200
                    :text-style (make-text-style :sans-serif :roman :normal))
        (draw-rectangle* stream 200 200 210 210)
        (let ((design
               (mcclim-bezier:make-bezier-curve*
                (mapcar (lambda (x) (+ x 10))
                        (list 100 100 20 80 90 110 90 170 90 220 140 210 140 140)))))
          (mcclim-bezier:draw-bezier-design* stream design
                                             :line-thickness 16
                                             :ink +green+))
        (draw-line* stream 110 110 150 150 :line-thickness 2 :ink +green+)
        (let ((design
               (mcclim-bezier:make-bezier-curve*
                (list 100 100 20 80 90 110 90 170 90 220 140 210 140 140))))
          (mcclim-bezier:draw-bezier-design* stream design
                                             :line-thickness 16
                                             :ink +orange+))
        (let* ((coords (mcclim-bezier:relative-to-absolute-coord-seq (list 200 200 0 -50 80 -50 100 -20)))
               (c1 (mcclim-bezier:make-bezier-curve* coords)))
          (mcclim-bezier:draw-bezier-design* stream c1 :line-thickness 5 :ink +blue+)
          (destructuring-bind (arrow-y arrow-x &rest args)
              (reverse coords)
            (declare (ignore args))
            (draw-arrow* stream arrow-x arrow-y arrow-x arrow-y)))
        (let ((design
               (mcclim-bezier:make-bezier-curve*
                (mapcar (lambda (x) (+ x 25))
                        (list 100 100 20 80 90 110 90 170 90 220 140 210 140 140)))))
          (mcclim-bezier:draw-bezier-design* stream design
                                             :line-thickness 16
                                             :ink +pink+))))))

(define-drawing-test "16) Bezier Difference" (stream)
    "Draws a single bezier difference"
  (let* ((r1 (mcclim-bezier:make-bezier-area* '(100 100 200 200 300 200 400 100 300 50 200 50 100 100)))
         (r2 (mcclim-bezier:make-bezier-area* '(150 100 200 120 300 150 350 100 300 80 200 80 150 100)))
         (r3 (mcclim-bezier:region-difference r1 r2)))
    (mcclim-bezier:draw-bezier-design* stream r3)))

