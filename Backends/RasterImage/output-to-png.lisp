(in-package :mcclim-raster-image)

;;;
;;; with-output-to ...
;;;

(defmacro with-output-to-raster-image-stream ((stream-var stream format &rest options)
					      &body body)
  (let ((cont (gensym))
	(exit-fn (gensym))
	(enter-fn (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body)
	    (,exit-fn (sheet stream)
	      (with-output-recording-options (stream :draw t :record nil)
		(stream-replay stream))
	      (medium-finish-output (sheet-medium sheet))
	      (medium-finish-output (sheet-medium stream))
	      (save-sheet-image-to-stream sheet ,stream ,format))
	    (,enter-fn (sheet stream)
	      (declare (ignore sheet))
	      nil))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-raster-image #',cont #',enter-fn #',exit-fn 
					   ,format
					   ,@options))))

(defmacro with-output-to-raster-image-file ((stream-var file &rest options)
					    &body body)
  (flet ((extract-format (file)
	   (intern (string-upcase (pathname-type (pathname file))) (find-package :keyword))))
    (let ((cont (gensym))
	  (exit-fn (gensym))
	  (enter-fn (gensym)))
      `(flet ((,cont (,stream-var)
		,@body)
	      (,exit-fn (sheet stream)
		(with-output-recording-options (stream :draw t :record nil)
		  (stream-replay stream))
		(save-sheet-image-to-file sheet ,file (extract-format ,file)))
	      (,enter-fn (sheet stream)
		(declare (ignore sheet stream))
		nil))
	 (declare (dynamic-extent #',cont))
	 (invoke-with-output-to-raster-image #',cont #',enter-fn #',exit-fn 
					     (extract-format ,file)
					     ,@options)))))

(defmacro with-output-to-rgb-image ((stream-var image &rest options)
				       &body body)
  (let ((cont (gensym))
	(exit-fn (gensym))
	(enter-fn (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body)
	    (,exit-fn (sheet stream)
	      (with-output-recording-options (stream :draw t :record nil)
		(stream-replay stream))
	      (medium-finish-output (sheet-medium sheet))
	      (medium-finish-output (sheet-medium stream))
	      (mcclim-render::image-sheet-image sheet))
	    (,enter-fn (sheet stream)
	      (declare (ignore stream))
	      (when ,image
		(setf (mcclim-render::image-sheet-image sheet) ,image))))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-raster-image #',cont #',enter-fn #',exit-fn 
					   :rgb-image
					   ,@options))))

(defmacro with-output-to-rgb-pattern ((stream-var &rest options)
				      &body body)
  `(make-instance 'clim-internals::rgb-pattern
		  :image (with-output-to-rgb-image
			     (,stream-var nil ,@options)
			   ,@body)))

(defun invoke-with-output-to-raster-image (continuation enter-fn exit-fn format
					   &key (width 1000) (height 1000))
  (let ((port (find-port :server-path (list :raster-image
					    :width width :height height)))
	(result nil))
    (let* ((top-level-sheet (make-raster-top-level-sheet port format))
	   (vbox (make-instance 'vbox-pane :port port)))
      (sheet-adopt-child top-level-sheet vbox)
      (let ((stream (make-raster-image-stream port)))
	(clim:sheet-adopt-child vbox stream)
	(funcall enter-fn top-level-sheet stream)
	(realize-mirror port top-level-sheet)
	(setf (sheet-region top-level-sheet)
	      (clim:make-rectangle* 0 0 width height))
	(with-output-recording-options (stream :record t :draw nil)
	  (funcall continuation stream)
	  (medium-finish-output (sheet-medium stream)))
	(setf result (funcall exit-fn top-level-sheet stream))))
    (destroy-port port)
    result))
