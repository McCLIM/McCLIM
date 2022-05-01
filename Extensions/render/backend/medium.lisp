(in-package #:mcclim-render)

(defclass render-medium-mixin
    (ttf-medium-mixin multiline-text-medium-mixin basic-medium)
  ())

(defun %medium-stroke-paths (medium paths)
  (when-let ((mirror (medium-drawable medium)))
    (%stroke-paths medium mirror paths
                   (medium-line-style medium)
                   (medium-device-transformation medium)
                   (medium-device-region medium)
                   (transform-region (medium-native-transformation medium)
                                     (medium-ink medium)))))

(defun %medium-fill-paths (medium paths)
  (when-let ((mirror (medium-drawable medium)))
    (%fill-paths mirror paths
                 (medium-device-transformation medium)
                 (medium-device-region medium)
                 (transform-region (medium-native-transformation medium)
                                   (medium-ink medium)))))

(defun %medium-draw-image (target source x y width height to-x to-y)
  (%draw-image target source
               (round x) (round y)
               (round width) (round height)
               (round to-x) (round to-y)))

;;; XXX: used only for medium-draw-text* for now.
(defun %medium-fill-image-mask (medium mask-image x1 y1 x2 y2 mask-dx mask-dy)
  (when-let ((mirror (medium-drawable medium)))
    (%fill-image-mask mirror x1 y1 x2 y2
                      (transform-region (medium-native-transformation medium)
                                        (medium-ink medium))
                      (medium-device-region medium)
                      ;; Stencil
                      mask-image (floor mask-dx) (floor mask-dy))))

(defun %medium-fill-image (medium x1 y1 x2 y2)
  (when-let ((mirror (medium-drawable medium)))
    (%fill-image mirror x1 y1 x2 y2
                 (transform-region (medium-native-transformation medium)
                                   (medium-ink medium))
                 (medium-device-region medium))))

;;; standard medium protocol

(defmethod medium-draw-rectangle* ((medium render-medium-mixin) x1 y1 x2 y2 filled)
  (let ((transformation (medium-device-transformation medium)))
    (if (and filled (rectilinear-transformation-p transformation))
        (climi::with-transformed-positions* (transformation x1 y1 x2 y2)
          (when (< x2 x1) (rotatef x2 x1))
          (when (< y2 y1) (rotatef y2 y1))
          (%medium-fill-image medium x1 y1 x2 y2))
        (%medium-stroke-paths medium (let ((path (make-path x1 y1)))
                                       (line-to path x2 y1)
                                       (line-to path x2 y2)
                                       (line-to path x1 y2)
                                       (close-path path)
                                       (list path))))))

(defmethod medium-draw-polygon* ((medium render-medium-mixin) coord-seq closed filled)
  (let ((x (elt coord-seq 0))
        (y (elt coord-seq 1)))
    (let ((path (make-path x y)))
      (do ((v 2 (+ 2 v)))
          ((>= v (length coord-seq)))
        (let ((x (elt coord-seq v))
              (y (elt coord-seq (1+ v))))
          (line-to path x y)))
      (when closed
        (close-path path))
      (if filled
          (%medium-fill-paths medium (list path))
          (%medium-stroke-paths medium (list path))))))

(defmethod medium-draw-line* ((medium render-medium-mixin) x1 y1 x2 y2)
  (let ((path (make-path x1 y1)))
    (line-to path x2 y2)
    (%medium-stroke-paths medium (list path))))

(defmethod medium-draw-point* ((medium render-medium-mixin) x y)
  (let* ((line-style (medium-line-style medium))
         (thickness (line-style-effective-thickness line-style medium))
         (path (arc x y (max 1 (/ thickness 2)) pi (+ pi (* 2 pi)))))
    (%medium-fill-paths medium (list path))))

#+ (or)
(defun render-draw-circle*
    (medium center-x center-y radius start-angle end-angle filled)
  (let ((path (arc center-x center-y radius (+ pi start-angle) (+ pi end-angle))))
    (if filled
        (%medium-fill-paths medium (list path))
        (%medium-stroke-paths medium (list path)))))

(defmethod medium-draw-ellipse* ((medium render-medium-mixin) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled
                                 &aux (el (make-ellipse*
                                           center-x center-y
                                           radius-1-dx radius-1-dy
                                           radius-2-dx radius-2-dy
                                           :start-angle start-angle
                                           :end-angle end-angle)))
  (multiple-value-bind (cx cy hx hy theta)
      (climi::ellipse-simplified-representation el)
    (declare (ignorable cx cy))
    (let* ((sa (- (* 2 pi) end-angle theta))
           (dalpha (- end-angle start-angle))
           (path (ellipse-arc center-x center-y hx hy theta
                              sa (+ sa dalpha))))
      (when filled
        (line-to path center-x center-y))
      (if filled
          (%medium-fill-paths medium (list path))
          (%medium-stroke-paths medium (list path))))))

(defmethod medium-draw-text* ((medium render-medium-mixin) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore transform-glyphs toward-x toward-y))
  (if (null end)
      (setf end (length string))
      (minf end (length string)))
  (unless (medium-drawable medium)
    (return-from medium-draw-text*))
  (let* ((port (port medium))
         (text-style (medium-text-style medium))
         (font (text-style-mapping port text-style)))
    (ecase align-x
      (:left)
      (:center
       (let ((origin-x (text-size medium string :text-style text-style)))
         (decf x (/ origin-x 2.0))))
      (:right
       (let ((origin-x (text-size medium string :text-style text-style)))
         (decf x origin-x))))
    (ecase align-y
      (:top
       (incf y (font-ascent font)))
      (:baseline)
      (:center
       (let* ((ascent (font-ascent font))
              (descent (font-descent font))
              (height (+ ascent descent))
              (middle (- ascent (/ height 2.0s0))))
         (incf y middle)))
      (:baseline*)
      (:bottom
       (decf y (font-descent font))))
    (let ((codes (font-string-glyph-codes font string :start start :end end))
          (dev-tr (medium-device-transformation medium)))
      (multiple-value-setq (x y)
        (transform-position dev-tr x y))
      (string-primitive-paths medium x y codes dev-tr port font))))
