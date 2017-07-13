(in-package :clim-demo)

(defparameter *render-image-tests* (make-hash-table :test 'equal))

(defparameter *render-image-width* 510)
(defparameter *render-image-height* 700)
(defparameter *render-image-border-width* 5)

(defparameter *testing-image-directory* (uiop/pathname:merge-pathnames* "Examples/images/" (asdf:system-source-directory (asdf:find-system :mcclim))))
(defparameter *testing-image-files* '("RGBXPLORER8.png"
                                      "White_Balance_RGB.png"
                                      "MicroGrayTest.png"))

(defparameter *testing-image-rgb-file* "RGBXPLORER8.png")
(defparameter *testing-image-bn1-file* "White_Balance_RGB.png")
(defparameter *testing-image-bn2-file* "MicroGrayTest.png")

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
  (:panes
   (output :application-pane
           :min-width *render-image-width*
           :min-height *render-image-height*
           :display-time nil
           :display-function #'render-image-display-output
           :end-of-line-action :wrap
           :end-of-page-action :wrap)
   (description :application-pane)
   (selector :list-pane
             :mode :exclusive
             :name-key #'render-image-test-name
             :items (sort (loop for x being the hash-values of *render-image-tests*
                                collect x) #'string< :key #'render-image-test-name)
             :value-changed-callback #'render-image-update-selection)
   (condition-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback 'render-image-update-condition-option)
      (clim:radio-box-current-selection "message")
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
                     (climi::bordering (:border-width *render-image-border-width*)
                       output)))))
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (scrolling (:scroll-bar :vertical :height 200) description)))))))))

(defun render-image-update-condition-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (signal-condition-p) clim:*application-frame*
    (setf signal-condition-p
	  (string= (clim:gadget-label selected-gadget) "break"))))

(defun render-image-update-selection (pane item)
  (declare (ignore pane))
  (with-slots (current-selection) clim:*application-frame*
    (setf current-selection item))
  (window-clear (get-frame-pane *application-frame* 'description))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'output) :force-p t))

(defun render-image-display-output (frame pane)
  (declare (ignore pane))
  (let ((output (get-frame-pane frame 'output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane *application-frame* 'description)))
      (when item
        (with-text-style (description (make-text-style :sans-serif :roman :normal))
          (write-string (render-image-test-description item) description)
          (terpri description))
        (if (slot-value *application-frame* 'signal-condition-p)
            (clim:with-drawing-options (output :clipping-region
                                               (clim:make-rectangle* 0 0 *render-image-width* *render-image-height*))
              (clim:draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                    :ink clim:+grey90+)
              (funcall (render-image-test-drawer item) output))
            (handler-case
                (clim:with-drawing-options (output :clipping-region
                                                   (clim:make-rectangle* 0 0 *render-image-width* *render-image-height*))
                  (clim:draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                        :ink clim:+grey90+)
                  (funcall (render-image-test-drawer item) output))
              (condition (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "Error:~a~%" condition)))))))))


(defun run-render-image-tests ()
  (run-frame-top-level
   (make-application-frame
    'render-image-tests)))

(defun render-image-test-make-rgb-image-2d (w h color)
  (let* ((image (clim-render:make-image :rgb w h :two-dim-array))
         (pixels (clim-render:image-pixels image)))
    (dotimes (x w)
      (dotimes (y h)
        (setf (aref pixels y x) color)))
    image))

(defun render-image-test-make-rgb-image-op (w h color)
  (let* ((image (clim-render:make-image :rgb w h :opticl))
         (pixels (clim-render:image-pixels image)))
    (let ((b (first color))
          (g (second color))
          (r (third color)))
      (dotimes (x w)
        (dotimes (y h)
          (setf (opticl:pixel pixels y x) (values r g b)))))
    image))

(defun render-image-test-make-rgb-image (image-class w h color)
  (let* ((image (make-instance image-class :width w :height h))
         (fn (clim-render:image-rgb-set-fn image)))
    (multiple-value-bind (r g b)
        (clim-render:color->octets color)
      (dotimes (x w)
        (dotimes (y h)
          (funcall fn x y r g b))))
    image))

(defun render-image-test-make-rgba-image (image-class w h color)
  (let* ((image (make-instance image-class :width w :height h))
         (fn (clim-render:image-rgba-set-fn image)))
    (multiple-value-bind (r g b)
        (clim-render:color->octets color)
      (dotimes (x w)
        (dotimes (y h)
          (funcall fn x y r g b 255))))
    image))

(defun render-image-test-01-2d (stream)
  (flet ((draw-rect (color w h)
           (let ((image (render-image-test-make-rgb-image-2d 90 70 color)))
             (clim-render:draw-image* stream image w h))))
    (draw-rect #xFFFFFF 10 10)
    (draw-rect #x000000 110 10)
    (draw-rect #x0000F0 10 100)
    (draw-rect #x00F000 110 100)
    (draw-rect #xF00000 210 100)
    (draw-rect #x800080 10 200)
    (draw-rect #x808000 110 200)))

(defun render-image-test-01-op (stream)
  (flet ((draw-rect (color w h)
           (let ((image (render-image-test-make-rgb-image-op 90 70 color)))
             (clim-render:draw-image* stream image w h))))
    (draw-rect (list #xFF #xFF #xFF) 10 10)
    (draw-rect (list #x00 #x00 #x00) 110 10)
    (draw-rect (list #x00 #x00 #xF0) 10 100)
    (draw-rect (list #x00 #xF0 #x00) 110 100)
    (draw-rect (list #xF0 #x00 #x00) 210 100)
    (draw-rect (list #x80 #x00 #x80) 10 200)
    (draw-rect (list #x80 #x80 #x00) 110 200)))

(defun render-image-test-02 (stream image-class)
  (flet ((draw-rect (color w h)
           (let ((image (render-image-test-make-rgb-image image-class 90 70 color)))
             (clim-render:draw-image* stream image w h))))
    (draw-rect +white+ 10 10)
    (draw-rect +black+ 110 10)
    (draw-rect +red+ 10 100)
    (draw-rect +green+ 110 100)
    (draw-rect +blue+ 210 100)
    (draw-rect +purple+ 10 200)
    (draw-rect +olive-drab+ 110 200)))

(defun render-image-test-03 (stream image-class h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  image-class)))
      (clim-render:draw-image* stream image 10 h))))

(defun render-image-test-04 (stream image-class h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :gray)
                  :default image-class)))
      (clim-render:draw-image* stream
                              (clim-render:coerce-image image :rgb)
                              10 10)
      (clim-render:draw-image* stream
                              (clim-render:coerce-image image :gray)
                              10 360))))

(defun render-image-test-05 (stream image-class transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  image-class)))
      (clim-render:draw-image* stream image 0 0
                              :transformation transformation))))

(defun render-image-test-06 (stream image-class transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  image-class)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (clim-render:draw-image* stream image 0 0
                              :transformation transformation
                              :clipping-region clipping-region))))

(defun render-image-test-08 (stream image-class h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  image-class)))
      (draw-design stream (clim-render:make-image-design image) :x 10 :y h))))

(defun render-image-test-09 (stream image-class transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  image-class)))
      (draw-design stream (clim-render:make-image-design image) :x 0 :y 0
                   :transformation transformation))))

(defun render-image-test-10 (stream image-class transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  image-class)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (draw-design stream (clim-render:make-image-design image) :x 0 :y 0
                   :transformation transformation
                   :clipping-region clipping-region))))

(defun render-image-test-12 (stream image-class w h color)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let* ((alpha-image
            (clim-render:coerce-alpha-channel
              (clim-render:read-image path)))
           (image (render-image-test-make-rgba-image image-class
                                                     (clim-render:image-width alpha-image)
                                                     (clim-render:image-height alpha-image)
                                                     color)))
      (clim-render:copy-alpha-channel alpha-image
                             0 0
                             (clim-render:image-width alpha-image)
                             (clim-render:image-height alpha-image)
                             image
                             0 0)
      (clim-render:draw-image* stream image w h))))

(defun render-image-test-13 (stream fg-image-class bg-image-class w h alpha)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let* ((alpha-image
            (clim-render:coerce-alpha-channel
              (clim-render:read-image path)))
           (image
            (clim-render:coerce-image (render-image-test-make-rgba-image 'clim-render:rgba-image
                                                     (clim-render:image-width alpha-image)
                                                     (clim-render:image-height alpha-image)
                                                     +red+)
                          fg-image-class))
           (bg-image
            (clim-render:coerce-image (render-image-test-make-rgba-image 'clim-render:rgba-image
                                                             (clim-render:image-width alpha-image)
                                                             (clim-render:image-height alpha-image)
                                                             +yellow+)
                          bg-image-class)))

      (clim-render:copy-alpha-channel alpha-image
                             0 0
                             (clim-render:image-width alpha-image)
                             (clim-render:image-height alpha-image)
                             image
                             0 0)
      (clim-render:blend-image image
                              0 0
                              (clim-render:image-width alpha-image)
                              (clim-render:image-height alpha-image)
                              bg-image
                              0 0 :alpha alpha)
      (clim-render:draw-image* stream bg-image w h))))

(defun render-image-test-15 (stream image-class cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  :default image-class)))
      (clim-render:draw-image* stream
                              (clim-render:crop-image image cx cy cw ch)
                              w h))))

(defun render-image-test-16 (stream image-class design cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  :default image-class))
          (pixeled-design (clim-render:make-pixeled-design design)))
      (clim-render:fill-image image pixeled-design nil :x cx :y cy :width cw :height ch)
      (clim-render:draw-image* stream
                              image
                              w h))))

(defun render-image-test-17 (stream image-class design cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*))
        (path2 (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (clim-render:coerce-image
                  (clim-render:read-image path :image-class :rgb)
                  :default image-class))
          (pixeled-design (clim-render:make-pixeled-design design))
          (stencil (clim-render:coerce-alpha-channel
                    (clim-render:read-image path2))))
      (clim-render:fill-image image pixeled-design stencil :x cx :y cy :width cw :height ch
                             :stencil-dx (- cx) :stencil-dy (- cy))
      (clim-render:draw-image* stream
                              image
                              w h))))

(define-render-image-test "2d - 01) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-01-2d stream))

(define-render-image-test "op - 01) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-01-op stream))

(define-render-image-test "2d - 02) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-02 stream 'clim-render:rgb-image))

(define-render-image-test "op - 02) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-02 stream 'clim-render:opticl-rgb-image))

(define-render-image-test "op - 03) read rgb" (stream)
    ""
  (render-image-test-03 stream 'clim-render:opticl-rgb-image 10)
  (render-image-test-03 stream 'clim-render:opticl-gray-image 360))


(define-render-image-test "2d - 03) read rgb" (stream)
    ""
  (render-image-test-03 stream 'clim-render:rgb-image 10)
  (render-image-test-03 stream 'clim-render:gray-image 360))

(define-render-image-test "op - 04) read gray" (stream)
    ""
  (render-image-test-04 stream :opticl 10))

(define-render-image-test "2d - 04) read gray" (stream)
    ""
  (render-image-test-04 stream :two-dim-array 10))

(define-render-image-test "op - 05) translate " (stream)
    ""
  (render-image-test-05 stream 'clim-render:opticl-rgb-image
                        (clim:make-translation-transformation 10 10))
  (render-image-test-05 stream 'clim-render:opticl-gray-image
                        (clim:make-translation-transformation 10 360)))

(define-render-image-test "2d - 05) translate" (stream)
    ""
  (render-image-test-05 stream 'clim-render:rgb-image
                        (clim:make-translation-transformation 10 10))
  (render-image-test-05 stream 'clim-render:gray-image
                        (clim:make-translation-transformation 10 360)))

(define-render-image-test "op - 06) clipping " (stream)
    ""
  (render-image-test-06 stream 'clim-render:opticl-rgb-image
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (render-image-test-06 stream 'clim-render:opticl-gray-image
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "2d - 06) clipping" (stream)
    ""
  (render-image-test-06 stream 'clim-render:rgb-image
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (render-image-test-06 stream 'clim-render:gray-image
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "op - 07) with translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-03 stream 'clim-render:opticl-rgb-image 0))
  (with-translation (stream (- 10) 360)
    (render-image-test-03 stream 'clim-render:opticl-gray-image 0)))

(define-render-image-test "2d - 07) with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-03 stream 'clim-render:rgb-image 0))
  (with-translation (stream (- 10) 360)
    (render-image-test-03 stream 'clim-render:gray-image 0)))

(define-render-image-test "op - 08) design draw" (stream)
    ""
  (render-image-test-08 stream 'clim-render:opticl-rgb-image 10)
  (render-image-test-08 stream 'clim-render:opticl-gray-image 360))

(define-render-image-test "2d - 08) design draw" (stream)
    ""
  (render-image-test-08 stream 'clim-render:rgb-image 10)
  (render-image-test-08 stream 'clim-render:gray-image 360))

(define-render-image-test "op - 09) design translate " (stream)
    ""
  (render-image-test-09 stream 'clim-render:opticl-rgb-image
                        (clim:make-translation-transformation 10 10))
  (render-image-test-09 stream 'clim-render:opticl-gray-image
                        (clim:make-translation-transformation 10 360)))

(define-render-image-test "2d - 09) design translate" (stream)
    ""
  (render-image-test-09 stream 'clim-render:rgb-image
                        (clim:make-translation-transformation 10 10))
  (render-image-test-09 stream 'clim-render:gray-image
                        (clim:make-translation-transformation 10 360)))

(define-render-image-test "op - 10) design clipping " (stream)
    ""
  (render-image-test-10 stream 'clim-render:opticl-rgb-image
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (render-image-test-10 stream 'clim-render:opticl-gray-image
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "2d - 10) design clipping" (stream)
    ""
  (render-image-test-10 stream 'clim-render:rgb-image
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (render-image-test-10 stream 'clim-render:gray-image
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "op - 11) design with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-08 stream 'clim-render:opticl-rgb-image 0))
  (with-translation (stream (- 10) 360)
    (render-image-test-08 stream 'clim-render:opticl-gray-image 0)))

(define-render-image-test "2d - 11) design with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-08 stream 'clim-render:rgb-image 0))
  (with-translation (stream (- 10) 360)
    (render-image-test-08 stream 'clim-render:gray-image 0)))

(define-render-image-test "op - 12) alpha" (stream)
    ""
  (render-image-test-12 stream 'clim-render:opticl-rgba-image 10 10 +red+)
  (render-image-test-12 stream 'clim-render:opticl-rgba-image 10 360 +green+))

(define-render-image-test "2d - 12) alpha" (stream)
    ""
  (render-image-test-12 stream 'clim-render:rgba-image 10 10 +red+)
  (render-image-test-12 stream 'clim-render:rgba-image 10 360 +green+))

(define-render-image-test "op - 13) blend" (stream)
    ""
  (render-image-test-13 stream
                        'clim-render:opticl-rgba-image
                        'clim-render:opticl-rgba-image
                        10 10 255)
  (render-image-test-13 stream
                        'clim-render:opticl-rgba-image
                        'clim-render:opticl-rgba-image
                        10 360 128))

(define-render-image-test "2d - 13) blend" (stream)
    ""
  (render-image-test-13 stream
                        'clim-render:rgba-image
                        'clim-render:rgba-image
                        10 10 255)
  (render-image-test-13 stream
                        'clim-render:rgba-image
                        'clim-render:rgba-image
                        10 360 128))

(define-render-image-test "op - 14) blend gray" (stream)
    ""
  (render-image-test-13 stream
                        'clim-render:opticl-rgba-image
                        'clim-render:opticl-gray-image
                        10 10 255)
  (render-image-test-13 stream
                        'clim-render:opticl-rgba-image
                        'clim-render:opticl-gray-image
                        10 360 128))

(define-render-image-test "2d - 14) blend gray" (stream)
    ""
  (render-image-test-13 stream
                        'clim-render:rgba-image
                        'clim-render:gray-image
                        10 10 255)
  (render-image-test-13 stream
                        'clim-render:rgba-image
                        'clim-render:gray-image
                        10 360 128))

(define-render-image-test "op - 14) blend rgb" (stream)
    ""
  (render-image-test-13 stream
                        'clim-render:opticl-rgba-image
                        'clim-render:opticl-rgb-image
                        10 10 255)
  (render-image-test-13 stream
                        'clim-render:opticl-rgba-image
                        'clim-render:opticl-rgb-image
                        10 360 128))

(define-render-image-test "2d - 14) blend rgb" (stream)
    ""
  (render-image-test-13 stream
                        'clim-render:rgba-image
                        'clim-render:rgb-image
                        10 10 255)
  (render-image-test-13 stream
                        'clim-render:rgba-image
                        'clim-render:rgb-image
                        10 360 128))

(define-render-image-test "op - 15) crop" (stream)
    ""
  (render-image-test-15 stream
                        :opticl
                        100 100 100 150
                        10 10)
  (render-image-test-15 stream
                        :opticl
                        150 150 150 100
                        200 10))

(define-render-image-test "2d - 15) crop" (stream)
    ""
  (render-image-test-15 stream
                        :two-dim-array
                        100 100 100 150
                        10 10)
  (render-image-test-15 stream
                        :two-dim-array
                        150 150 150 100
                        200 10))
(define-render-image-test "op - 16) fill color" (stream)
    ""
  (render-image-test-16 stream
                        :opticl
                        +red+
                        100 100 100 150
                        10 10)
  (render-image-test-16 stream
                        :opticl
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))

(define-render-image-test "2d - 16) fill color" (stream)
    ""
  (render-image-test-16 stream
                        :two-dim-array
                        +red+
                        100 100 100 150
                        10 10)
  (render-image-test-16 stream
                        :two-dim-array
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))
(define-render-image-test "op - 17) fill stencil" (stream)
    ""
  (render-image-test-17 stream
                        :opticl
                        +red+
                        100 100 100 150
                        10 10)
  (render-image-test-17 stream
                        :opticl
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))

(define-render-image-test "2d - 17) fill stencil" (stream)
    ""
  (render-image-test-17 stream
                        :two-dim-array
                        +red+
                        100 100 100 150
                        10 10)
  (render-image-test-17 stream
                        :two-dim-array
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))
;;;
;;; indipendent
;;;

(define-render-image-test "zz - 01) output record :draw nil" (stream)
    ""
  (clim:with-output-to-output-record (stream)
    (with-output-recording-options (stream :record t :draw nil)
      (render-image-test-08 stream 'clim-render:rgb-image 10)
      (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t))))

(define-render-image-test "zz - 01) output record moving" (stream)
    ""
  (let ((record
         (clim:with-output-to-output-record (stream)
           (with-output-recording-options (stream :record t :draw t)
             (render-image-test-08 stream 'clim-render:rgb-image 10)
             (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))
    (setf (clim:output-record-position record) (values 10 310))
    (replay record stream)))
