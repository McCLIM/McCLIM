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
              (declare (ignorable stream))
	      (climi::write-bitmap-file (image-mirror-image (sheet-mirror sheet))
                                        ,stream :format ,format))
	    (,enter-fn (sheet stream)
	      (declare (ignore sheet stream))
	      nil))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-raster-image #',cont #',enter-fn #',exit-fn 
					   :rgb-image
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
		(declare (ignore stream))
		(climi::write-bitmap-file (image-mirror-image (sheet-mirror sheet) ,file
                                                              :format (extract-format ,file))))
	      (,enter-fn (sheet stream)
		(declare (ignore sheet stream))
		nil))
	 (declare (dynamic-extent #',cont))
	 (invoke-with-output-to-raster-image #',cont #',enter-fn #',exit-fn
					     :rgb-image
					     ,(extract-format file)
					     ,@options)))))

(defmacro with-output-to-rgba-image ((stream-var image &rest options)
				       &body body)
  (let ((cont (gensym))
	(exit-fn (gensym))
	(enter-fn (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body)
	    (,exit-fn (sheet stream)
	      (declare (ignore stream))
              (image-mirror-image (sheet-mirror sheet)))
	    (,enter-fn (sheet stream)
	      (declare (ignore stream))
	      (when ,image
                (setf (image-mirror-image sheet) ,image))))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-raster-image #',cont #',enter-fn #',exit-fn 
					   :rgb-image
					   :rgb-image
					   ,@options))))

(defmacro with-output-to-rgba-pattern ((stream-var &rest options)
				      &body body)
  `(with-output-to-rgba-image (,stream-var nil ,@options)
     ,@body))

(defmacro with-output-to-image ((stream-var image &rest options)
                                &body body)
  (let ((cont (gensym))
	(exit-fn (gensym))
	(enter-fn (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body)
	    (,exit-fn (sheet stream)
	      (declare (ignore stream))
              (image-mirror-image (sheet-mirror sheet)))
	    (,enter-fn (sheet stream)
	      (declare (ignore stream))
	      (when ,image
                (setf (image-mirror-image sheet) ,image))))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-raster-image #',cont #',enter-fn #',exit-fn
					   :rgb-image
					   :rgb-image
					   ,@options))))

(defmacro with-output-to-image-pattern ((stream-var &rest options)
				      &body body)
  `(with-output-to-image (,stream-var nil ,@options)
     ,@body))

(defun invoke-with-output-to-raster-image (continuation enter-fn exit-fn server format
					   &key (width 1000) (height 1000)
					     (border-width 0) (recording-p t))
  (let ((port (find-port :server-path (list server
					    :width width :height height)))
	(result nil))
    (let* ((top-level-sheet (make-raster-top-level-sheet port format))
	   (vbox (make-instance 'vbox-pane :port port))
	   (border-pane (make-instance 'climi::border-pane :port port :border-width border-width)))
      (sheet-adopt-child top-level-sheet vbox)
      (let ((stream (make-raster-image-stream port)))
	(clim:sheet-adopt-child border-pane stream)
	 (sheet-adopt-child vbox border-pane)
	(funcall enter-fn top-level-sheet stream)
	(realize-mirror port top-level-sheet)
	(setf (sheet-region top-level-sheet)
	      (clim:make-rectangle* 0 0 width height))
        (with-output-recording-options (stream :record recording-p :draw t)
              (funcall continuation stream)
              (medium-finish-output (sheet-medium stream)))
	(setf result (funcall exit-fn top-level-sheet stream))))
    (destroy-port port)
    result))
