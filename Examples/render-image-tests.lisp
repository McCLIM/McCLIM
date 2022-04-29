;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2017 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2018 Daniel Kochmanski <daniel@turtleware.eu>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-demo)

(defparameter *render-image-tests* (make-hash-table :test 'equal))

(defparameter *render-image-width* 510)
(defparameter *render-image-height* 700)
(defparameter *render-image-border-width* 5)

(defparameter *testing-image-directory*
  (uiop/pathname:merge-pathnames*
   "Examples/images/" (asdf:system-source-directory (asdf:find-system :mcclim))))

(defparameter *testing-image-rgb-file* "RGBXPLORER8.png")
(defparameter *testing-image-bn1-file* "White_Balance_RGB.png")
(defparameter *testing-image-bn2-file* "MicroGrayTest.png")
(defparameter *testing-image-files*
  (list *testing-image-rgb-file*
        *testing-image-bn1-file*
        *testing-image-bn2-file*))

(defstruct render-image-test name description drawer)

(defmacro define-render-image-test (name arglist description &body body)
  (check-type name string)
  (check-type description string)
  `(setf (gethash ,name *render-image-tests*)
         (make-render-image-test :name ,name
                                 :description ,description
                                 :drawer (lambda ,arglist ,@body))))

(define-application-frame render-image-tests ()
  ((signal-condition-p :initform nil)
   (current-selection :initform nil))
  (:menu-bar nil)
  (:panes
   (output :application-pane
           :min-width *render-image-width*
           :min-height *render-image-height*
           :display-time t
           :display-function #'render-image-display-output
           :end-of-line-action :wrap
           :end-of-page-action :wrap)
   (description :application-pane)
   (selector :list-pane
             :mode :exclusive
             :name-key #'render-image-test-name
             :items (sort (alexandria:hash-table-values *render-image-tests*)
                          #'string< :key #'render-image-test-name)
             :value-changed-callback #'render-image-update-selection)
   (condition-option
    (with-radio-box (:orientation :vertical
                     :value-changed-callback 'render-image-update-condition-option)
      (radio-box-current-selection "message")
      "break")))
  (:layouts
   (default
    (spacing (:thickness 3)
      (horizontally ()
        (vertically ()
          (spacing (:thickness 3)
            (clim-extensions:lowering ()
              (scrolling (:scroll-bar :vertical :height *render-image-height*) selector)))
          (labelling (:label "Condition")
            condition-option))
        (vertically ()
          (spacing (:thickness 3)
            (clim-extensions:lowering ()
              (horizontally ()
                (labelling (:label "Stream")
                  (outlining (:thickness *render-image-border-width*)
                    output)))))
          (spacing (:thickness 3)
            (clim-extensions:lowering ()
              (scrolling (:scroll-bar :vertical :height 200) description)))))))))

(defun render-image-update-condition-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (signal-condition-p) *application-frame*
    (setf signal-condition-p
          (string= (gadget-label selected-gadget) "break"))))

(defun render-image-update-selection (pane item)
  (declare (ignore pane))
  (let ((frame *application-frame*))
    (with-slots (current-selection) frame
      (setf current-selection item))
    (window-clear (get-frame-pane frame 'description))
    (redisplay-frame-pane frame (get-frame-pane frame 'output) :force-p t)))

(defun render-image-display-output (frame pane)
  (declare (ignore pane))
  (let ((output (get-frame-pane frame 'output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane frame 'description)))
      (when item
        (with-text-style (description (make-text-style :sans-serif :roman :normal))
          (write-string (render-image-test-description item) description)
          (terpri description))
        (if (slot-value frame 'signal-condition-p)
            (with-drawing-options (output :clipping-region
                                          (make-rectangle* 0 0 *render-image-width* *render-image-height*))
              (draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                                                                     :ink +grey90+)
              (funcall (render-image-test-drawer item) output))
            (handler-case
                (with-drawing-options (output :clipping-region
                                              (make-rectangle* 0 0 *render-image-width* *render-image-height*))
                  (draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                                                                         :ink +grey90+)
                  (funcall (render-image-test-drawer item) output))
              (condition (condition)
                (with-drawing-options (description :ink +red+)
                  (format description "Error:~a~%" condition)))))))))

(defun run-render-image-tests ()
  (run-frame-top-level (make-application-frame 'render-image-tests)))

(defun render-image-test-make-rgba-image (w h color)
  (let* ((image (make-instance 'climi::%rgba-pattern
                               :array (make-array (list h w)
                                                  :element-type '(unsigned-byte 32)
                                                  :initial-element #xFFFFFF00)))
         (pixels (clime:pattern-array image)))
    (dotimes (y h)
      (dotimes (x w)
        (setf (aref pixels y x) (climi::%rgba-value color))))
    image))

(defun render-image-test-01-2d (stream)
  (flet ((draw-rect (color w h)
           (let ((image (render-image-test-make-rgba-image 90 70 color)))
             (clim-render:draw-image* stream image w h))))
    (draw-rect #xffffffff  10  10) ; white
    (draw-rect #xff000000 110  10) ; black
    (draw-rect #xfff00000  10 100) ; red
    (draw-rect #xff00f000 110 100) ; green
    (draw-rect #xff0000ff 210 100) ; blue
    (draw-rect #xff800080  10 200) ; purple (not quite +purple+)
    (draw-rect #xff808000 110 200) ; olive drab (not quite +olive-drab+)
    ))

(defun render-image-test-02 (stream)
  (flet ((draw-rect (color w h)
           (let ((image (render-image-test-make-rgba-image 90 70 color)))
             (clim-render:draw-image* stream image w h))))
    (draw-rect +white+       10  10)
    (draw-rect +black+      110  10)
    (draw-rect +red+         10 100)
    (draw-rect +green+      110 100)
    (draw-rect +blue+       210 100)
    (draw-rect +purple+      10 200)
    (draw-rect +olive-drab+ 110 200)))

(defun render-image-test-03 (stream h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (clim-render:draw-image* stream image 10 h))))

(defun render-image-test-04 (stream)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (clim-render:draw-image* stream image 10 10))))

(defun render-image-test-05 (stream transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (clim-render:draw-image* stream image 0 0 :transformation transformation))))

(defun render-image-test-06 (stream transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
                                clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
                                (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (clim-render:draw-image* stream image 0 0
                               :transformation transformation
                               :clipping-region clipping-region))))

(defun render-image-test-08 (stream h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (draw-design stream image :x 10 :y h))))

(defun render-image-test-09 (stream transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (draw-design stream image :x 0 :y 0 :transformation transformation))))

(defun render-image-test-10 (stream transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
                                clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
                                (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (draw-design stream image :x 0 :y 0
                                :transformation transformation
                                :clipping-region clipping-region))))

(defun render-image-test-16 (stream design cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let* ((image (make-pattern-from-bitmap-file path))
           (array (pattern-array image)))
      (clim-render:fill-image array design cx cy (+ cx cw) (+ cy ch) +everywhere+)
      (clim-render:draw-image* stream image w h))))

(define-render-image-test "2d - 01) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-01-2d stream))

(define-render-image-test "2d - 02) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-02 stream))

(define-render-image-test "2d - 03) read rgb" (stream)
    ""
  (render-image-test-03 stream 10))


(define-render-image-test "2d - 04) read gray" (stream)
    ""
  (render-image-test-04 stream))

(define-render-image-test "2d - 05) translate" (stream)
    ""
  (render-image-test-05 stream (make-translation-transformation 10 10)))

(define-render-image-test "2d - 06) clipping" (stream)
    ""
  (render-image-test-06 stream
                        (make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "2d - 07) with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-03 stream 0)))

(define-render-image-test "2d - 08) design draw" (stream)
    ""
  (render-image-test-08 stream 10))

(define-render-image-test "2d - 09) design translate" (stream)
    ""
  (render-image-test-09 stream (make-translation-transformation 10 10)))

(define-render-image-test "2d - 10) design clipping" (stream)
    ""
  (render-image-test-10 stream
                        (make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "2d - 11) design with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-08 stream 0)))

(define-render-image-test "2d - 16) fill color" (stream)
    ""
  (render-image-test-16 stream
                        +red+
                        100 100 100 150
                        10 10)
  (render-image-test-16 stream
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))

;;;
;;; independent
;;;

(define-render-image-test "zz - 01) output record :draw nil" (stream)
    ""
  (with-output-to-output-record (stream)
    (with-output-recording-options (stream :record t :draw nil)
      (render-image-test-08 stream 10)
      (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t))))

(define-render-image-test "zz - 01) output record moving" (stream)
    ""
  (let ((record
          (with-output-to-output-record (stream)
            (with-output-recording-options (stream :record t :draw t)
              (render-image-test-08 stream 10)
              (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))
    (setf (output-record-position record) (values 10 310))
    (replay record stream)))
