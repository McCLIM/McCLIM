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
  (let ((pattern (apply #'invoke-with-output-to-drawing-stream
                        continuation backend :pattern args)))
    (climi::write-bitmap-file pattern destination :format format)))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (port raster-image-port) (destination (eql :pattern))
     &key (width :compute) (height :compute) (border-width 0)
          (recording-p t) (format :png) (target nil)
     &allow-other-keys)
  (let* ((graft (graft port))
         (top-level-sheet (make-raster-top-level-sheet port format))
         (vbox (make-instance 'vbox-pane :port port))
         (border-pane (make-instance 'climi::border-pane
                                     :port port
                                     :border-width border-width))
         (stream (make-raster-image-stream port)))
    (sheet-adopt-child border-pane stream)
    (sheet-adopt-child vbox border-pane)
    (sheet-adopt-child top-level-sheet vbox)
    (sheet-adopt-child graft top-level-sheet)
    (unwind-protect
         (progn
           (when target
             (setf (image-mirror-image top-level-sheet) target))
           ;; When WIDTH or HEIGHT is :COMPUTE, render into an output record and
           ;; use its dimensions to change the space requirements of
           ;; STREAM. When WIDTH is :COMPUTE, do this two times: one time to
           ;; determine the required width and a second time to determine the
           ;; resulting height when using the computed width.
           (if (or (eq width :compute) (eq height :compute))
               (flet ((try ()
                        (let ((record (with-output-to-output-record (stream)
                                        (funcall continuation stream))))
                          ;; FIXME Enlarging the space requirements a bit is
                          ;; needed to prevent things from getting clipped.
                          (change-space-requirements
                           stream
                           :width (if (eq width :compute)
                                      (+ (bounding-rectangle-width record) 2)
                                      width)
                           :height (if (eq height :compute)
                                       (+ (bounding-rectangle-height record) 2)
                                       height)))))
                 ;; Ensure STREAM's preferred width is set to something
                 ;; reasonable, then call CONTINUATION and update STREAM's space
                 ;; requirements.
                 (change-space-requirements
                  stream :width (if (eq width :compute) 1000 width))
                 (try)
                 (when (eq width :compute)
                   (try)))
               (change-space-requirements stream :width width :height height))
           (with-output-recording-options (stream :record recording-p :draw t)
             (funcall continuation stream)
             (medium-finish-output (sheet-medium stream)))
           (values (image-mirror-image top-level-sheet)
                   (when recording-p
                     (stream-output-history stream))))
      (sheet-disown-child graft top-level-sheet))))

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
