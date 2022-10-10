(defpackage #:mcclim-svg
  (:use #:clim #:clime #:climb #:clim-lisp)
  (:local-nicknames (#:alx #:alexandria)))

(in-package #:mcclim-svg)

(defclass svg-port (mcclim-truetype:ttf-port-mixin basic-port)
  ((destination :accessor destination)
   (resources :initform (make-hash-table :test #'equal) :reader resources)
   (viewport-width :accessor viewport-width)
   (viewport-height :accessor viewport-height)))

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
  (destructuring-bind (port-type &rest args)
      server-path
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

;;; This version of the method supplies only a medium (there is no sheet output
;;; protocol involved). I'm leaving it here as a reference.
#+ (or)
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
  (:method ((medium svg-medium) (region climi::nowhere-region))
    (values :none nil))
  (:method ((medium svg-medium) (region climi::everywhere-region))
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
              (cl-who:with-html-output (drawable (medium-drawable medium))
                (:defs nil nil
                  (let ((pattern-id
                          (ensure-resource-id (medium (cons :mask-pattern region))
                            (cl-who:htm (:pattern :id ^resource-id :|patternUnits| "userSpaceOnUse"
                                         :x 0 :y 0 :width "100%" :height "100%"
                                         (:g :fill "white" (draw-design medium +everywhere+))
                                         (:g :fill "black" (draw-design medium (region-complement region))))))))
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
               (cl-who:esc (subseq string start (or end (length string)))))))))


;;; Patterns

(defun encode-svg-pattern (pattern &optional (format :jpeg))
  (check-type format (member :png :jpeg))
  (with-output-to-string (stream)
    (format stream "data:image/~(~a~);base64," format)
    (cl-base64:usb8-array-to-base64-stream
     (flexi-streams:with-output-to-sequence (octet-stream)
       (climi::write-bitmap-file pattern octet-stream :format format))
     stream)))

(defmethod medium-design-ink ((medium svg-medium) (pattern image-pattern))
  (let ((id (ensure-resource-id (medium (cons :image pattern))
              (let* ((href (encode-svg-pattern pattern)) ;; :png for alpha channel
                     (width (pattern-width pattern))
                     (height (pattern-height pattern)))
                (cl-who:with-html-output (drawable (medium-drawable medium))
                  (:defs nil nil
                    (:pattern :id ^resource-id :x 0 :y 0 :width "100%" :height "100%"
                              :|patternUnits| "userSpaceOnUse"
                              (:image :|xlink:href| href :x 0 :y 0 :width (fmt width) :height (fmt height)))))))))
    (values (url id) 1.0)))


(defmethod medium-design-ink ((medium svg-medium) (pattern rectangular-tile))
  (let ((id (ensure-resource-id (medium (cons :design-ink pattern))
              (let* ((src-pattern (rectangular-tile-design pattern))
                     (src-id (medium-design-ink medium src-pattern))
                     (src-width (fmt (pattern-width src-pattern)))
                     (src-height (fmt (pattern-height src-pattern)))
                     ;;
                     (tile-width (fmt (pattern-width pattern)))
                     (tile-height (fmt (pattern-height pattern))))
                (cl-who:with-html-output (drawable (medium-drawable medium))
                  (:defs nil nil
                    (:pattern :id ^resource-id :x 0 :y 0 :width tile-width :height tile-height
                              :|patternUnits| "userSpaceOnUse"
                              (:rect :x 0 :y 0 :width src-width :height src-height :fill src-id))))))))
    (values (url id) 1.0)))

(defmethod medium-design-ink ((medium svg-medium) (pattern transformed-pattern))
  (let ((id (ensure-resource-id (medium (cons :design-ink pattern))
              (let* ((transform (svg-transform (transformed-design-transformation pattern)))
                     (src-pattern (transformed-design-design pattern))
                     (src-id (medium-design-ink medium src-pattern))
                     (src-width (fmt (pattern-width src-pattern)))
                     (src-height (fmt (pattern-height src-pattern)))
                     ;;
                     (tilep (typep src-pattern 'rectangular-tile))
                     (width  (if tilep src-width  "100%"))
                     (height (if tilep src-height "100%")))
                (cl-who:with-html-output (drawable (medium-drawable medium))
                  (:defs nil nil
                    (:pattern :id ^resource-id :x 0 :y 0 :width width :height height
                              :|patternUnits| "userSpaceOnUse"
                     :|patternTransform| transform
                     (:rect :x 0 :y 0 :width src-width :height src-height :fill src-id))))))))
    (values (url id) 1.0)))

(defmethod medium-design-ink ((medium svg-medium) (pattern pattern))
  (let ((id (ensure-resource-id (medium (cons :design-ink pattern))
              (let* ((href (encode-svg-pattern pattern :png))
                     (width (pattern-width pattern))
                     (height (pattern-height pattern)))
                (cl-who:with-html-output (drawable (medium-drawable medium))
                  (:defs nil nil
                    (:pattern :id ^resource-id :x 0 :y 0 :width "100%" :height "100%"
                              :|patternUnits| "userSpaceOnUse"
                              (:image :|xlink:href| href :x 0 :y 0 :width (fmt width) :height (fmt height)))))))))
    (values (url id) 1.0)))

;;; Recursive patterns

;;; FIXME this could do some more sophisticated analysis!
;;; FIXME alternative color profiles (rgb, grayscale, alhpa)
;;; FIXME hardcoded viewport size
;;; FIXME detect transparency and collapse to png when feasible
;;;
;;; Some indexed patterns can't be reused at different positions because some of
;;; their palette designs is not uniform. Reusing a cached pattern at different
;;; positions may happenen when:
;;;
;;; - it is tiled
;;; - it is transformed
;;;
;;; Tiling is potentially infinite so if we want to cache a rectangular tile
;;; then we need to compute it for the whole viewport. Alternatively we may skip
;;; caching and create a new picture for each tile, or parametrize cache.
;;;
;;; Transformed pattern has a finite size (transformed rectangle) so it possible
;;; to cache them without creating too costly resources.
;;;
;;; Somewhat problematic part is that a transformed pattern and rectangular
;;; pattern may parent each other so it is necessary to find the leaf child and
;;; only then verify whether it is an index pattern. If there is at least one
;;; tile in the sequence then the overal pattern is infinite.
(defun maybe-collapse-pattern (medium pattern)
  (let ((tile-p nil))
    (labels ((unmoveable-pattern-p (design)
               (typecase design
                 (rectangular-tile
                  (setf tile-p t)
                  (unmoveable-pattern-p (rectangular-tile-design design)))
                 (transformed-pattern
                  (unmoveable-pattern-p (transformed-design-design design)))
                 (otherwise
                  (and (typep design 'climi::indexed-pattern)
                       (some (lambda (p)
                               (not (typep p '(or color opacity climi::uniform-compositum))))
                             (climi::pattern-designs design))
                       design)))))
      (when (unmoveable-pattern-p pattern)
        (if tile-p
            (let* ((port (port medium))
                   (width (viewport-width port))
                   (height (viewport-height port)))
             (climi::%collapse-pattern pattern 0 0 width height))
            (with-bounding-rectangle* (x0 y0 :width width :height height) pattern
              (transform-region (make-translation-transformation x0 y0)
                                (climi::%collapse-pattern pattern x0 y0 width height))))))))

;;; This method is very inefficient (evaluation time and memory) and very
;;; expensive (file size). We are flexing to do the right thing. Normally we'd
;;; use palette in-composition using shaders.
(defmethod medium-design-ink :around ((medium svg-medium) (pattern pattern))
  (alx:if-let ((collapsed (maybe-collapse-pattern medium pattern)))
    (let ((id (ensure-resource-id (medium (cons :design-ink pattern))
                (alx:simple-style-warning
                 "Collapsing the pattern for the viewport - this is very inefficient!")
                (let* ((rht (resources (port medium)))
                       (cid (progn (medium-design-ink medium collapsed)
                                   (resource-id medium (cons :design-ink collapsed)))))
                  (setf (gethash (cons :design-ink pattern) rht) cid)
                  (setf ^resource-id cid)))))
      (values (url id) 1.0))
    (call-next-method)))


;;; Masked composition

;;; Normally we'd use a mask, but this is yet another feature that is handled
;;; differently by every second renderer. That's why we simply flatten the ink
;;; when it is not an uniform compositum.
;;;
;;; CLIM II specification hints that handling only uniform masks is OK.  That
;;; said we still want to support stencils so let's get lazy big time.
(defun compose-stencil  (medium pattern)
  (let* ((port (port medium))
         (pattern* (climi::%collapse-pattern pattern 0 0
                                             (viewport-width port)
                                             (viewport-height port))))
    (medium-design-ink medium pattern*)))

(defmethod medium-design-ink ((medium svg-medium) (design climi::in-compositum))
  (let ((ink (climi::compositum-ink design))
        (mask (climi::compositum-mask design)))
    (alx:if-let ((opacity (ignore-errors (opacity-value mask))))
      (multiple-value-bind (ink-url ink-opacity)
          (medium-design-ink medium ink)
        (values ink-url (* ink-opacity opacity)))
      (compose-stencil medium design))))

(defmethod medium-design-ink ((medium svg-medium) (design climi::out-compositum))
  (let ((ink (climi::compositum-ink design))
        (mask (climi::compositum-mask design)))
    (alx:if-let ((opacity (ignore-errors (opacity-value mask))))
      (multiple-value-bind (ink-url ink-opacity)
          (medium-design-ink medium ink)
        (values ink-url (* ink-opacity (- 1.0 opacity))))
      (compose-stencil medium design))))


;;; Graft

(defclass svg-graft (graft)
  ((density :initarg :dpi    :reader density)
   (region  :initarg :region :reader sheet-native-region)
   (native  :initarg :native :reader sheet-native-transformation)))

(defmethod print-object ((graft svg-graft) stream)
  (print-unreadable-object (graft stream :type t :identity nil)
    (format stream "~ax~a"
            (graft-width graft :units :device)
            (graft-height graft :units :device))))

(defun compute-units-transformation (width height units dpi)
  ;; This is a transformation FROM units TO :device.
  (ecase units
    (:device +identity-transformation+)
    (:inches (make-scaling-transformation dpi dpi))
    (:millimeters (make-scaling-transformation (/ dpi 25.4) (/ dpi 25.4)))
    (:screen (make-scaling-transformation width height))))

;;; The constructor is used as a converter - it is possible to supply any valid
;;; combination of the orientation and units and the created graft will have its
;;; native transformation convert supplied parameters to device parameters:
;;;
;;;   (ORIENTATION, UNITS) -> (:DEFAULT :DEVICE)
;;;
(defmethod climb:make-graft ((port svg-port) &key (orientation :default) (units :device))
  (destructuring-bind (port-type &key (dpi 96) &allow-other-keys)
      (port-server-path port)
    (declare (ignore port-type))
    (let* ((graft (make-instance 'svg-graft :orientation orientation :units units
                                            :mirror (destination port)
                                            :dpi dpi))
           (width (viewport-width port))
           (height (viewport-height port))
           ;; Transform graft units to 1/dpi (for example 1/96in).
           (units-transformation
             (compute-units-transformation width height units dpi))
           (region (transform-region units-transformation
                                     (make-rectangle* 0 0 width height)))
           (height* (bounding-rectangle-height region))
           (orientation-transformation (ecase orientation
                                         (:graphics (compose-transformations
                                                     (make-translation-transformation 0 height*)
                                                     (make-reflection-transformation* 0 0 1 0)))
                                         (:default +identity-transformation+)))
           (native (compose-transformations orientation-transformation units-transformation)))
      (setf (slot-value graft 'region) region)
      (setf (slot-value graft 'native) native)
      graft)))

(defmethod graft-width ((graft svg-graft) &key (units :device))
  (let ((native-width (bounding-rectangle-width (sheet-native-region graft))))
    (ecase units
      (:device native-width)
      (:inches (/ native-width (density graft)))
      (:millimeters (* (/ native-width (density graft)) 25.4))
      (:screen 1))))

(defmethod graft-height ((graft svg-graft) &key (units :device))
  (let ((native-height (bounding-rectangle-height (sheet-native-region graft))))
    (ecase units
      (:device native-height)
      (:inches (/ native-height (density graft)))
      (:millimeters (* (/ native-height (density graft)) 25.4))
      (:screen 1))))

;;; When SCALE-TO-FIT is T then all output is transformed so that its bounding
;;; rectangle is the same as the graft native region. Transformation must
;;; maintain the aspect ratio.
(defun compute-scale-transformation (stream width height)
  (with-bounding-rectangle* (min-x min-y :width bbox-w :height bbox-h)
      (stream-output-history stream)
    (let ((transformation (make-translation-transformation (- min-x) (- min-y))))
      (cond
        ((and (eq width :compute) (eq height :compute))
         (setf width bbox-w)
         (setf height bbox-h))
        ((eq width :compute)
         (let* ((scale (/ height bbox-h))
                (scaling-transformation (make-scaling-transformation scale scale)))
           (setf transformation (compose-transformations scaling-transformation transformation)
                 width (transform-distance scaling-transformation bbox-w 0))))
        ((eq height :compute)
         (let* ((scale (/ width bbox-w))
                (scaling-transformation (make-scaling-transformation scale scale)))
           (setf transformation (compose-transformations scaling-transformation transformation)
                 height (nth-value 1 (transform-distance scaling-transformation 0 bbox-h)))))
        (t
         (let* ((scale (min (/ width bbox-w)
                            (/ height bbox-h)))
                (scaling-transformation (make-scaling-transformation scale scale)))
           (setf transformation (compose-transformations scaling-transformation transformation)))))
      (values transformation width height))))

(defun correct-transformation (port stream width height)
  (multiple-value-bind (scaling graft-w graft-h)
      (compute-scale-transformation stream width height)
    (setf (viewport-width port) graft-w
          (viewport-height port) graft-h
          (sheet-transformation stream) scaling)))

(defun correct-width-or-height (port stream width height)
  (with-bounding-rectangle* (:x2 max-x :y2 max-y) (stream-output-history stream)
    (if (eq width :compute)
        (setf (viewport-width port) max-x)
        (setf (viewport-width port) width))
    (if (eq height :compute)
        (setf (viewport-height port) max-y)
        (setf (viewport-height port) height))))

(defun two-pass-drawing (continuation port &rest args
                         &key (scale-to-fit nil)
                              (width :compute)
                              (height :compute)
                              (orientation :default)
                              (units :device)
                         &allow-other-keys)
  (declare (ignore args))
  (setf (viewport-width port) 1)
  (setf (viewport-height port) 1)
  (let ((fake-graft (make-graft port :units units :orientation orientation))
        (stream (make-instance 'clim-stream-pane :port port :background +white+ :region +everywhere+)))
    (sheet-adopt-child fake-graft stream)
    (with-output-recording-options (stream :record t :draw nil)
      (funcall continuation stream))
    (sheet-disown-child fake-graft stream)
    (if scale-to-fit
        (correct-transformation  port stream width height)
        (correct-width-or-height port stream width height))
    (let* ((graft (make-graft port :units units :orientation orientation))
           (w-in (format nil "~ain" (fmt (graft-width graft :units :inches))))
           (h-in (format nil "~ain" (fmt (graft-height graft :units :inches))))
           (*viewport-w* (graft-width graft :units :device))
           (*viewport-h* (graft-height graft :units :device))
           (bbox (format nil "0 0 ~a ~a" (fmt *viewport-w*) (fmt *viewport-h*))))
      (sheet-adopt-child graft stream)
      (cl-who:with-html-output (destination (destination port))
        (:svg :version "1.1" :width w-in :height h-in :|viewBox| bbox
         :xmlns "http://www.w3.org/2000/svg"
         :|xmlns:xlink| "http://www.w3.org/1999/xlink"
         (stream-replay stream))))))

(defun one-pass-drawing (continuation port &rest args
                         &key (orientation :default) (units :device) width height
                         &allow-other-keys)
  (declare (ignore args))
  (setf (viewport-width port) width)
  (setf (viewport-height port) height)
  (let* ((graft (make-graft port :units units :orientation orientation))
         (w-in (format nil "~ain" (fmt (graft-width graft :units :inches))))
         (h-in (format nil "~ain" (fmt (graft-height graft :units :inches))))
         (bbox (format nil "0 0 ~a ~a" (fmt (viewport-width port)) (fmt (viewport-height port))))
         (region (untransform-region (sheet-native-transformation graft)
                                     (sheet-native-region graft)))
         (stream (make-instance 'clim-stream-pane :port port :background +white+ :region region)))
    (sheet-adopt-child graft stream)
    (cl-who:with-html-output (destination (destination port))
      (:svg :version "1.1" :width w-in :height h-in :|viewBox| bbox
       :xmlns "http://www.w3.org/2000/svg"
       :|xmlns:xlink| "http://www.w3.org/1999/xlink"
       (funcall continuation stream)))))

(defmethod invoke-with-output-to-drawing-stream
    (continuation (port svg-port) (destination stream) &rest args)
  (declare (ignore args))
  (let ((server-path (rest (port-server-path port))))
    (destructuring-bind (&key (scale-to-fit nil) (width :compute) (height :compute)
                         &allow-other-keys)
        server-path
      (setf (destination port) destination)
      (if (or scale-to-fit (eq width :compute) (eq height :compute))
          (apply #'two-pass-drawing continuation port server-path)
          (apply #'one-pass-drawing continuation port server-path)))))
