
(defpackage #:test-runner
  (:use #:clim-lisp #:clim)
  (:export #:test-runner
           #:define-test
           #:*width*
           #:*height*
           #:*test-output-directory*))

(in-package #:test-runner)

(defparameter *width* 500)
(defparameter *height* 700)
(defparameter *border-width* 5)

(defvar *test-output-directory* "/tmp/")

(ensure-directories-exist *test-output-directory*)

(defstruct test name description drawer)

(defmacro define-test (test-hash name arglist description &body body)
  (check-type name string)
  (check-type description string)
  `(setf (gethash ,name ,test-hash)
    (make-test :name ,name
     :description ,description
     :drawer (lambda ,arglist ,@body))))

(define-application-frame test-runner ()
  ((recording-p :initform t)
   (signal-condition-p :initform nil)
   (current-selection :initform nil)
   (tests :initform (make-hash-table :test 'equal) :accessor tests :initarg :tests))
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
             :name-key #'test-name
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
             :label (format nil "Run PostScript Tests (~A)" *test-output-directory*)
             :activate-callback #'(lambda (x)
                                    (declare (ignore x))
                                    (print-all-postscript-tests *application-frame*)))
   (print-pdf :push-button
              :label (format nil "Run PDF Tests (~A)" *test-output-directory*)
              :activate-callback #'(lambda (x)
                                     (declare (ignore x))
                                     (print-all-pdf-tests *application-frame*)))
   (print-png :push-button
              :label (format nil "Run PNG Tests (~A)" *test-output-directory*)
              :activate-callback #'(lambda (x)
                                     (declare (ignore x))
                                     (print-all-raster-image-tests  *application-frame* :png))))
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

(defmethod redisplay-frame-panes :around ((frame test-runner) &key &allow-other-keys)
  (let ((selector (find-pane-named frame 'selector)))
    (setf (climi::list-pane-items selector)
          (sort (loop for x being the hash-values of (tests frame)
                   collect x)
                #'string< :key #'test-name))))

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
  (redisplay-frame-pane *application-frame*
                        (get-frame-pane *application-frame* 'backend-output) :force-p t)
  (redisplay-frame-pane *application-frame*
                        (get-frame-pane *application-frame* 'render-output) :force-p t))

(defun display-backend-output (frame pane)
  (declare (ignore pane))
  (let ((output (get-frame-pane frame 'backend-output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane *application-frame* 'description)))
      (when item
        (with-text-style (description (make-text-style :sans-serif :roman :normal))
          (write-string (test-description item) description))
        (if (slot-value *application-frame* 'signal-condition-p)
            (clim:with-drawing-options (output :clipping-region
                                               (clim:make-rectangle* 0 0 *width* *height*))
              (clim:draw-rectangle* output 0 0 *width* *height* :filled t
                                    :ink clim:+grey90+)
              (funcall (test-drawer item) output))
            (handler-case
                (clim:with-drawing-options (output :clipping-region
                                                   (clim:make-rectangle* 0 0 *width* *height*))
                  (clim:draw-rectangle* output 0 0 *width* *height* :filled t
                                        :ink clim:+grey90+)
                  (funcall (test-drawer item) output))
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
                                 (stream :width *width*
                                         :height *height*
                                         :border-width *border-width*
                                         :recording-p recording-p)
                               (clim:draw-rectangle* stream 0 0 *width* *height*
                                                     :filled t
                                                     :ink clim:+grey90+)
                               (funcall (test-drawer item) stream))))
                (draw-pattern* output pattern 0 0)
                (medium-finish-output (sheet-medium output))))
            (handler-case
                (with-slots (recording-p) clim:*application-frame*
                  (let ((pattern (mcclim-raster-image::with-output-to-image-pattern
                                     (stream :width *width*
                                             :height *height*
                                             :border-width *border-width*
                                             :recording-p recording-p)
                                   (clim:draw-rectangle* stream 0 0 *width* *height*
                                                         :filled t
                                                         :ink clim:+grey90+)
                                   (funcall (test-drawer item) stream))))
                    (draw-pattern* output pattern 0 0)
                    (medium-finish-output (sheet-medium output))))
              (simple-error (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "Render:~a~%" condition)))))))))

(defun run-tests ()
  (run-frame-top-level
   (make-application-frame
    'tests)))

(defun test-postscript (runner test &optional filename)
  (let* ((test (if (stringp test) (gethash test (tests runner)) test))
         (test-name (test-name test))
         (filename (or filename
                       (merge-pathnames
                        (make-pathname :name test-name :type "eps")
                        *test-output-directory*))))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (with-output-to-postscript-stream (stream out :device-type :eps)
        #+nil
        (with-text-style (stream (make-text-style :sans-serif :roman :normal))
          (format stream "~&~a: ~a~%" test-name (test-description test)))
        (funcall (test-drawer test) stream)))))

(defun print-all-postscript-tests (runner)
  (loop for test being the hash-values of (tests runner) do
       (restart-case (test-postscript runner test)
         (:skip ()
           :report (lambda (stream) (format stream "skip ~a" (test-name test)))))))


(defun test-pdf (runner test &optional filename)
  (let* ((test (if (stringp test) (gethash test (tests runner)) test))
         (test-name (test-name test))
         (filename (or filename
                       (merge-pathnames
                        (make-pathname :name test-name :type "pdf")
                        *test-output-directory*))))
    (with-open-file (out filename :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (clim-pdf:with-output-to-pdf-stream (stream out)
        (with-text-style (stream (make-text-style :sans-serif :roman :normal))
          (format stream "~&~a: ~a~%" test-name (test-description test)))
        (funcall (test-drawer test) stream)))))

(defun print-all-pdf-tests (runner)
  (loop for test being the hash-values of (tests runner) do
       (restart-case (test-pdf runner test)
         (:skip ()
           :report (lambda (stream) (format stream "skip ~a" (test-name test)))))))

(defun test-raster-image (runner test format &optional filename)
  (let* ((test (if (stringp test) (gethash test (tests runner)) test))
         (test-name (test-name test))
         (filename (or filename
                       (merge-pathnames
                        (make-pathname :name test-name :type format)
                        *test-output-directory*))))
    (with-open-file (out filename :element-type '(unsigned-byte 8)
                         :direction :output :if-exists :supersede)
      (mcclim-raster-image:with-output-to-raster-image-stream
          (stream out format :width *width* :height *height*)
        (clim:draw-rectangle* stream 0 0 *width* *height* :filled t
                              :ink clim:+grey90+)
        (funcall (test-drawer test) stream)))))

(defun print-all-raster-image-tests (runner format)
  (time
   (loop for test being the hash-values of (tests runner) do
        (restart-case (test-raster-image runner test format)
          (:skip ()
            :report (lambda (stream)
                      (format stream "skip ~a" (test-name test))))))))

