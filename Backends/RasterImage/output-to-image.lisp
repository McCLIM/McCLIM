(in-package #:mcclim-raster-image)

;;;
;;; with-output-to ...
;;;

(defmethod invoke-with-output-to-drawing-stream
    (continuation (backend (eql :raster)) destination &rest args)
  (with-port (port backend :width 1 :height 1)
    (apply #'invoke-with-output-to-drawing-stream continuation port destination args)))

(defun extract-format (file)
  (alexandria:make-keyword (string-upcase (pathname-type (pathname file)))))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (backend raster-image-port) (destination t) &rest args
     &key (format (extract-format destination)) &allow-other-keys)
  (with-open-file (stream destination :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
    (apply #'invoke-with-output-to-drawing-stream
           continuation backend stream :format format args)))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (backend raster-image-port) (destination stream)
     &rest args &key (format :png) &allow-other-keys)
  (multiple-value-bind (pattern record)
      (apply #'invoke-with-output-to-drawing-stream
             continuation backend :pattern args)
    (climi::write-bitmap-file pattern destination :format format)
    (values pattern record)))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (port raster-image-port) (destination (eql :pattern))
     &key (width :compute) (height :compute) (format :png)
       (target nil) (recording-p nil)
     &allow-other-keys)
  (let* ((graft (graft port))
         (top-level-sheet (make-raster-top-level-sheet port format))
         (stream (make-raster-image-stream port)))
    (sheet-adopt-child top-level-sheet stream)
    (sheet-adopt-child graft top-level-sheet)
    (when target
      (setf (image-mirror-image top-level-sheet) target)
      (if (eq width :compute)
          (setf width (pattern-width target))
          (assert (= width (pattern-width target))))
      (if (eq height :compute)
          (setf height (pattern-height target))
          (assert (= height (pattern-height target)))))
    (flet ((draw-it ()
             (change-space-requirements stream :width width :height height)
             (funcall continuation stream))
           (record-and-draw-it ()
             (change-space-requirements stream :width width :height height)
             (with-output-recording-options (stream :record t :draw t)
               (funcall continuation stream))
             (setf recording-p (stream-output-history stream)))
           (record-then-draw-it ()
             ;; XXX WIDTH nor HEIGHT are not specified for the stream so we
             ;; use its default region [0 0 100 100] - this impacts margins.
             (with-output-recording-options (stream :record t :draw nil)
               (funcall continuation stream))
             (setf recording-p (stream-output-history stream))
             (when (eq width :compute)
               (setf width (bounding-rectangle-max-x recording-p)))
             (when (eq height :compute)
               (setf height (bounding-rectangle-max-y recording-p)))
             (change-space-requirements stream :width width :height height)
             (stream-replay stream (stream-output-history stream))))
      (cond ((or (eq width :compute) (eq height :compute))
             (record-then-draw-it))
            (recording-p
             (record-and-draw-it))
            (t
             (draw-it)))
      (finish-output stream))
    (let ((image (image-mirror-image top-level-sheet)))
      (sheet-disown-child graft top-level-sheet)
      (values image recording-p))))

;;; These macros are deprecated.
(progn
  (defmacro with-output-to-raster-image-file ((stream-var file &rest options) &body body)
    `(with-output-to-drawing-stream (,stream-var :raster ,file ,@options)
       (warn "~s is deprecated.~%Please use ~s instead."
             'with-output-to-raster-image-file 'with-output-to-drawing-stream)
       ,@body))

  (defmacro with-output-to-raster-image-stream
      ((stream-var stream format &rest options) &body body)
    `(with-output-to-drawing-stream (,stream-var :raster ,stream :format ,format ,@options)
       (warn "~s is deprecated.~%Please use ~s instead."
             'with-output-to-raster-image-stream 'with-output-to-drawing-stream)
       ,@body))

  (defmacro with-output-to-rgba-pattern ((stream-var &rest options) &body body)
    `(with-output-to-drawing-stream (,stream-var :raster :pattern ,@options)
       (warn "~s is deprecated.~%Please use ~s instead."
             'with-output-to-rgba-pattern 'with-output-to-drawing-stream)
       ,@body))

  (defmacro with-output-to-image-pattern ((stream-var &rest options) &body body)
    `(with-output-to-drawing-stream (,stream-var :raster :pattern ,@options)
       (warn "~s is deprecated.~%Please use ~s instead."
             'with-output-to-image-pattern 'with-output-to-drawing-stream)
       ,@body))

  (defmacro with-output-to-image ((stream-var image &rest options) &body body)
    `(with-output-to-drawing-stream (,stream-var :raster :pattern :target ,image ,@options)
       (warn "~s is deprecated.~%Please use ~s instead."
             'with-output-to-image 'with-output-to-drawing-stream)
       ,@body)))
