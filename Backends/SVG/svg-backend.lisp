(defpackage #:mcclim-svg
  (:use #:clim #:clime #:climb #:clim-lisp)
  (:local-nicknames (#:alx #:alexandria)))

(in-package #:mcclim-svg)

(defclass svg-port (mcclim-truetype:ttf-port-mixin basic-port)
  ((destination :accessor destination)
   (resources :initform (make-hash-table :test #'equal) :reader resources)))

(defun resource-id (holder resource)
  (let ((ht (resources (port holder))))
    (alx:ensure-gethash resource ht
                        (format nil "r~4,'0d" (hash-table-count ht)))))

(defmacro ensure-resource-id ((medium object) &body body)
  (alx:with-gensyms (foundp)
    `(multiple-value-bind (^resource-id ,foundp)
         (resource-id ,medium ,object)
       (unless ,foundp ,@body)
       ^resource-id)))

(defun parse-server-path (server-path)
  (destructuring-bind (port-type &rest args &key dpi width height)
      server-path
    (declare (ignore dpi width height))
    (list* port-type :id (gensym) args)))

(defmethod find-port-type ((port (eql :svg)))
  (values (find-class 'svg-port) 'parse-server-path))

    ;;; Ensure that the "real" method receives a stream.
(defmethod invoke-with-output-to-drawing-stream
    (cont (port (eql :svg)) destination &rest args &key (preview nil))
  (let* ((args (alx:remove-from-plist args :preview))
         (port (find-port :server-path (list* port args))))
    (unwind-protect
         (etypecase destination
           ((or string pathname)
            (with-open-file (stream destination :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create
                                                :element-type 'character)
              (invoke-with-output-to-drawing-stream cont port stream))
            (when preview
              (when (eq preview t)
                (setf preview "xdg-open"))
              (uiop:launch-program (format nil "~a ~a" preview destination)))
            destination)
           (null
            (with-output-to-string (stream nil :element-type 'character)
              (invoke-with-output-to-drawing-stream cont port stream)))
           ((eql t)
            (let ((stream *standard-output*))
              (invoke-with-output-to-drawing-stream cont port stream))
            nil)
           (stream
            (invoke-with-output-to-drawing-stream cont port destination)))
      (destroy-port port))))

(defun fmt (number)
  (if (integerp number)
      (format nil "~d" number)
      (format nil "~f" number)))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (port svg-port) (destination stream) &rest args)
  (declare (ignore args))
  (destructuring-bind (port-type &key (dpi 96) (width 640) (height 360) &allow-other-keys)
      (port-server-path port)
    (declare (ignore port-type))
    (setf (destination port) destination)
    (let ((medium (make-medium port nil))
          (clip (make-rectangle* 0 0 width height))
          (w-in (format nil "~ain" (fmt (/ width dpi))))
          (h-in (format nil "~ain" (fmt (/ height dpi))))
          (bbox (format nil "0 0 ~a ~a" (fmt width) (fmt height))))
      (with-drawing-options (medium :clipping-region clip)
        (cl-who:with-html-output (destination destination)
          (:svg :version "1.1" :width w-in :height h-in :|viewBox| bbox
                :xmlns "http://www.w3.org/2000/svg"
                :|xmlns:xlink| "http://www.w3.org/1999/xlink"
                (funcall continuation medium)))))))


(defclass svg-medium (mcclim-truetype:ttf-medium-mixin basic-medium)
  ())

(defmethod make-medium ((port svg-port) sheet)
  (make-instance 'svg-medium :stream sheet :port port))

(defmethod medium-drawable ((medium svg-medium))
  (if (medium-sheet medium)
      (call-next-method)
      (destination (port medium))))

(defmacro with-drawing-context ((drawable-var medium mode) &body body)
  (alexandria:with-gensyms (cont)
    `(flet ((,cont (,drawable-var) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-drawing-context (function ,cont) ,medium ,mode))))

(defun url (id)
  (format nil "url(#~a)" id))

(defgeneric medium-clip (medium region)
  (:method ((medium svg-medium) (region (eql +nowhere+)))
    (values :none nil))
  (:method ((medium svg-medium) (region (eql +everywhere+)))
    (values :clip "none"))
  (:method ((medium svg-medium) (region bounding-rectangle))
    (let ((id (ensure-resource-id (medium (cons :clip region))
                (cl-who:with-html-output (drawable (medium-drawable medium))
                  (:defs (:|clipPath| :id ^resource-id (draw-design medium region)))))))
      (values :clip (url id))))
  (:method ((medium svg-medium) (region standard-region-complement))
    (error "SVG: Unsupported clip ~s." 'standard-region-complement))
  (:method ((medium svg-medium) (region standard-region-intersection))
    (error "SVG: Unsupported clip ~s." 'standard-region-intersection)))

(defmethod medium-clip ((medium svg-medium) (region standard-region-complement))
  (let ((id (ensure-resource-id (medium (cons :mask region))
              (cl-who:with-html-output (drawable (medium-drawable medium) :indent t)
                (:defs nil nil
                  (let ((pattern-id
                          (ensure-resource-id (medium (cons :mask-pattern region))
                            (cl-who:htm (:pattern :id ^resource-id :|patternUnits| "userSpaceOnUse"
                                         :x 0 :y 0 :width "100%" :height "100%")
                                        (:g :fill "white" (draw-design medium +everywhere+))
                                        (:g :fill "black" (draw-design medium (region-complement region)))))))
                    (cl-who:htm
                     (:mask :id ^resource-id :|maskUnits| "userSpaceOnUse"
                      :x 0 :y 0 :width "100%" :height "100%"
                      (:rect :x 0 :y 0 :width "100%" :height "100%" :fill (url pattern-id))))))))))
    (values :mask (url id))))

(defmethod medium-clip ((medium svg-medium) (clip standard-region-intersection))
  (let ((mask-id "none"))
    (cl-who:with-html-output (drawable (medium-drawable medium) :indent t)
      (:defs nil nil
        (labels ((add-pattern (region)
                   (ensure-resource-id (medium (cons :mask-pattern region))
                     (cl-who:htm (:pattern :id ^resource-id :|patternUnits| "userSpaceOnUse"
                                  :x 0 :y 0 :width "100%" :height "100%"
                                  (etypecase region
                                    (standard-region-intersection
                                     (error "BUG: not canonical form!"))
                                    (standard-region-complement
                                     (cl-who:htm (:g :fill "white" (draw-design medium +everywhere+)))
                                     (cl-who:htm (:g :fill "black" (draw-design medium (region-complement region)))))
                                    (bounding-rectangle
                                     (cl-who:htm (:g :fill "white" (draw-design medium region)))))))))
                 (add-mask (pattern-id mask-id)
                   (ensure-resource-id (medium (cons :mask pattern-id))
                     (cl-who:htm
                      (:mask :id ^resource-id :|maskUnits| "userSpaceOnUse"
                       :x 0 :y 0 :width "100%" :height "100%"
                       (:rect :x 0 :y 0 :width "100%" :height "100%"
                              :fill pattern-id :mask mask-id))))))
          (loop for region in (region-set-regions clip)
                for pattern-id = (add-pattern region)
                do (setf mask-id (url (add-mask (url pattern-id) mask-id)))))))
    (values :mask mask-id)))

;;; This would be so much nicer had SVG accept RGBA as a fill from the get-go.
(defun uniform-design-values (design)
  (multiple-value-bind (r g b a) (color-rgba design)
    (values (format nil "#~2,'0x~2,'0x~2,'0x"
                    (truncate (* r 255))
                    (truncate (* g 255))
                    (truncate (* b 255)))
            (format nil "~,2f" a))))

(defgeneric medium-design-ink (medium design)
  (:method ((medium svg-medium) (design color))
    (uniform-design-values design))
  (:method ((medium svg-medium) (design opacity))
    (uniform-design-values design))
  (:method ((medium svg-medium) (design climi::uniform-compositum))
    (uniform-design-values design))
  (:method ((medium svg-medium) (design indirect-ink))
    (medium-design-ink medium (indirect-ink-ink design)))
  (:method ((medium svg-medium) design)
    (warn "SVG: Unsupported design ~s." (class-name (class-of design)))
    (medium-design-ink medium +deep-pink+)))

;;; SVG transformation matrix is in column-major order.
(defun svg-transform (transformation)
  (multiple-value-bind (mxx mxy myx myy tx ty)
      (climi::get-transformation transformation)
    (format nil "matrix(~a ~a ~a ~a ~a ~a)"
            (fmt mxx) (fmt myx)
            (fmt mxy) (fmt myy)
            (fmt  tx) (fmt  ty))))

(defun svg-miter-limit (miter-limit-as-angle)
  ;; svg-miter-limit = miter-length / stroke-width = 1 / sin(theta/2)
  (/ 1 (sin (/ miter-limit-as-angle 2))))

(defun svg-parse-text-style-family (text-style-family)
  (case text-style-family
    (:serif "serif")
    (:sans-serif "sans-serif")
    (:fix "monospace")
    (otherwise text-style-family)))

;;; Returns values: font-style, font-weight and font-variant.
(defun svg-parse-text-style-face (text-style-face)
  (etypecase text-style-face
    (symbol
     (ecase text-style-face
       (:italic (values "italic" "normal" "normal"))
       (:bold   (values "normal" "bold"   "normal"))
       (:roman  (values "normal" "normal" "normal"))))
    (list
     (values (if (member :italic text-style-face) "italic" "normal")
             (if (member :bold   text-style-face) "bold"   "normal")
             "normal"))
    (string
     (handler-case (let* ((s1 (position #\- text-style-face))
                          (s2 (position #\- text-style-face :start (1+ s1)))
                          (style   (subseq text-style-face 0 s1))
                          (weight  (subseq text-style-face (1+ s1) s2))
                          (variant (subseq text-style-face (1+ s2))))
                     (values style weight variant))
       (error ()
         (svg-parse-text-style-face
          (text-style-face *undefined-text-style*)))))))

(defvar *configuring-device-p* nil)

(defun invoke-with-drawing-context (cont medium mode)
  (declare (ignorable mode))
  (alx:when-let ((drawable (medium-drawable medium)))
    (when *configuring-device-p*
      (return-from invoke-with-drawing-context
        (funcall cont drawable)))
    (cl-who:with-html-output (drawable)
      (labels ((configure-clip ()
                 (multiple-value-bind (clip value)
                     (medium-clip medium (medium-clipping-region medium))
                   (when clip
                     (cl-who:htm
                      (ecase clip
                        (:clip (cl-who:htm (:g :clip-path value (configure-draw))))
                        (:mask (cl-who:htm (:g :mask      value (configure-draw))))
                        (:none))))))
               (configure-draw ()
                 (multiple-value-bind (paint opacity)
                     (medium-design-ink medium (medium-ink medium))
                   (let ((transformation (svg-transform (medium-device-transformation medium))))
                     (cl-who:htm
                      (ecase mode
                        (:area
                         (cl-who:htm
                          (:g :transform transformation :fill paint :opacity opacity
                              (configure-area))))
                        (:path
                         (cl-who:htm
                          (:g :transform transformation :fill "none" :stroke paint :opacity opacity
                              (configure-path))))
                        (:text
                         (cl-who:htm
                          (:g :transform transformation :fill paint :opacity opacity
                              (configure-text)))))))))
               (configure-area ()
                 (funcall cont drawable))
               (configure-path ()
                 (let* ((line-style (medium-line-style medium))
                        (thickness (fmt (line-style-effective-thickness line-style medium)))
                        (dashes (map 'list #'fmt (line-style-effective-dashes line-style medium)))
                        (cap-shape (ecase (line-style-cap-shape line-style)
                                     ((:butt :no-end-point) "butt")
                                     (:square "square")
                                     (:round "round")))
                        (joint-shape (ecase (line-style-joint-shape line-style)
                                       ((:bevel :none) "bevel")
                                       (:miter "miter")
                                       (:round "round")))
                        (miter-limit (svg-miter-limit (medium-miter-limit medium))))
                   (cl-who:htm
                     (:g :stroke-width thickness
                         :stroke-dasharray (format nil "~{~a~^ ~}" dashes)
                         :stroke-linecap cap-shape
                         :stroke-linejoin joint-shape
                         :miter-limit miter-limit
                         (funcall cont drawable)))))
               (configure-text ()
                 (multiple-value-bind (family face size)
                     (text-style-components (parse-text-style* (medium-text-style medium)))
                   (let ((font-family (svg-parse-text-style-family family)))
                     (multiple-value-bind (font-style font-weight font-variant)
                         (svg-parse-text-style-face face)
                       (cl-who:htm
                         (:g :font-family font-family
                             :font-style font-style
                             :font-weight font-weight
                             :font-variant font-variant
                             :font-size size
                             (funcall cont drawable))))))))
        (let ((*configuring-device-p* t))
          (configure-clip))))))

(defmethod medium-draw-polygon* ((medium svg-medium) coord-seq closed filled)
  (with-drawing-context (drawable medium (if filled :area :path))
    (cl-who:with-html-output (stream drawable)
      (let ((points (format nil "~{~a~^ ~}" (map 'list #'fmt coord-seq))))
        (if (or filled closed)
            (cl-who:htm (:polygon  :points points))
            (cl-who:htm (:polyline :points points)))))))

(defmethod medium-draw-point* ((medium svg-medium) x y)
  (with-drawing-context (drawable medium :area)
    (let* ((line-style (medium-line-style medium))
           (thickness (line-style-effective-thickness line-style medium))
           (radius (/ thickness 2)))
      (case (line-style-cap-shape line-style)
        (:round
         (cl-who:with-html-output (drawable drawable)
           (:circle :cx (fmt x) :cy (fmt y) :r (fmt radius))))
        (:square
         (cl-who:with-html-output (drawable drawable)
           (:rect :x (fmt (- x radius)) :y (fmt (- y radius))
                  :width (fmt thickness) :height (fmt thickness))))
        (otherwise
         (let* ((coord-seq (list (- x radius) y x (- y radius) (+ x radius) y x (+ y radius)))
                (points (format nil "~{~a~^ ~}" (map 'list #'fmt coord-seq))))
           (cl-who:with-html-output (drawable drawable)
             (:polygon :points points))))))))

(defmethod medium-draw-line* ((medium svg-medium) x1 y1 x2 y2)
  (with-drawing-context (drawable medium :path)
    (cl-who:with-html-output (stream drawable)
      (:line :x1 (fmt x1) :y1 (fmt y1) :x2 (fmt x2) :y2 (fmt y2)))))

(defmethod medium-draw-rectangle* ((medium svg-medium) x1 y1 x2 y2 filled)
  (with-drawing-context (drawable medium (if filled :area :path))
    (cl-who:with-html-output (stream drawable)
      (:rect :x (fmt x1) :y (fmt y1) :width (fmt (- x2 x1)) :height (fmt (- y2 y1))))))

(defmethod medium-draw-bezigon* ((medium svg-medium) coord-seq filled)
  (with-drawing-context (drawable medium (if filled :area :path))
    (let* ((coord-seq (coerce coord-seq 'list))
           (points (with-output-to-string (str)
                     (destructuring-bind (x0 y0 . coords) coord-seq
                       (format str "M ~f ~f " x0 y0)
                       (loop for (x1 y1 x2 y2 x3 y3) on coords by (lambda (lst) (nthcdr 6 lst))
                             do (format str "C ~f ~f, ~f ~f, ~f ~f " x1 y1 x2 y2 x3 y3))))))
      (cl-who:with-html-output (drawable)
        (:path :d points)))))

(defvar +angle-transformation+ (make-reflection-transformation* 0 0 1 0))

(defun draw-sliced-ellipse (medium cx cy rx ry trans filled eta1 eta2)
  ;; IMPORTANT compare angles /before/ the transformation.
  (let ((lf (if (< 0 (- eta2 eta1) pi) 0 1))
        (sf 0))
    (climi::with-transformed-angles (+angle-transformation+ nil eta1 eta2)
      (multiple-value-bind (x1 y1)
          (climi::ellipse-point eta1 cx cy rx ry 0)
        (multiple-value-bind (x2 y2)
            (climi::ellipse-point eta2 cx cy rx ry 0)
          (let ((points (format nil "M ~f ~f L ~f ~f A ~f ~f 0 ~a ~a ~f ~f L ~f ~f"
                                cx cy   x1 y1   rx ry   lf sf x2 y2   cx cy)))
            (with-drawing-context (drawable medium (if filled :area :path))
              (cl-who:with-html-output (drawable)
                (:path :d points :transform trans)))))))))

(defun draw-simple-ellipse (medium cx cy rx ry trans filled)
  (with-drawing-context (drawable medium (if filled :area :path))
    (cl-who:with-html-output (drawable)
      (:ellipse :cx (fmt cx) :cy (fmt cy) :rx (fmt rx) :ry (fmt ry) :transform trans))))

(defmethod medium-draw-ellipse* ((medium svg-medium) cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 filled)
  (multiple-value-bind (rx ry rotation)
      (climi::ellipse-normalized-representation* rdx1 rdy1 rdx2 rdy2)
    (let  ((trans (format nil "rotate(~a ~a ~a)" (fmt (/ (* rotation 360) (* 2 pi))) (fmt cx) (fmt cy))))
      (if (< (- eta2 eta1) (* 2 pi))
          (draw-sliced-ellipse medium cx cy rx ry trans filled eta1 eta2)
          (draw-simple-ellipse medium cx cy rx ry trans filled)))))

(defmethod medium-draw-text* ((medium svg-medium) string x y start end
                              align-x align-y toward-x toward-y
                              transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-drawing-context (drawable medium :text)
    (let ((text-anchor
            (ecase align-x
              (:left "start")
              (:center "middle")
              (:right "end")))
          (dominant-baseline
            (ecase align-y
              (:top "text-before-edge")
              (:center "central")
              (:baseline "alphabetic")
              (:bottom "text-after-edge"))))
      (cl-who:with-html-output (stream drawable)
        (:text :x (fmt x)
               :y (fmt y)
               :text-anchor text-anchor
               :dominant-baseline dominant-baseline
               (cl-who:fmt (subseq string start (or end (length string)))))))))
