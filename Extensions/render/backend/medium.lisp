(in-package #:mcclim-render-internals)

(defclass render-medium-mixin (basic-medium
                               climb:multiline-text-medium-mixin
                               climb:font-rendering-medium-mixin)
  ())

(defun maybe-transformation (medium)
  (let ((drawable (if (mediump medium)
                      (medium-drawable medium)
                      medium)))
    (if (typep drawable 'image-pixmap-mixin)
        +identity-transformation+
        (medium-native-transformation medium))))

(defun maybe-region (medium)
  (let ((drawable (if (mediump medium)
                      (medium-drawable medium)
                      medium)))
    (if (typep drawable 'image-pixmap-mixin)
        (make-rectangle* 0 0 (pixmap-width drawable) (pixmap-height drawable))
        (medium-device-region medium))))

(defun %medium-stroke-paths (medium paths)
  (when-let ((%image (mirror->%image (port medium) (medium-drawable medium)))
             (transformation (maybe-transformation medium)))
    (%stroke-paths medium %image paths
                   (medium-line-style medium)
                   transformation
                   (maybe-region medium)
                   (transform-region transformation (medium-ink medium)))))

(defun %medium-fill-paths (medium paths)
  (when-let ((%image (mirror->%image (port medium) (medium-drawable medium)))
             (transformation (maybe-transformation medium)))
    (%fill-paths %image paths
                 transformation
                 (maybe-region medium)
                 (transform-region transformation (medium-ink medium)))))

(defun %medium-draw-image (medium image x y width height to-x to-y)
  (when-let ((%image
              (etypecase medium
                (medium
                 (mirror->%image (port medium) (medium-drawable medium)))
                (image-pixmap-mixin
                 medium)))
             (image
              (etypecase image
                (basic-sheet
                 (image-mirror-image
                  (mirror->%image (port image) (sheet-mirror image))))
                (image-pixmap-mixin
                 (image-mirror-image image))
                (clime:image-pattern
                 image))))
    (%draw-image %image image
                 (round x) (round y)
                 (round width) (round height)
                 (round to-x) (round to-y)
                 (maybe-region medium))))

;;; XXX: used only for medium-draw-text* for now.
(defun %medium-fill-image-mask (medium mask-image from-x from-y width height to-x to-y)
  (when-let ((%image (mirror->%image (port medium) (medium-drawable medium))))
    (%fill-image %image
                 (round from-x) (round from-y)
                 (round width) (round height)
                 (transform-region (maybe-transformation medium)
                                   (medium-ink medium))
                 (maybe-region medium)
                 ;; Stencil
                 mask-image (round to-x) (round to-y))))

(defun %medium-fill-image (medium x y width height)
  (when-let ((%image (mirror->%image (port medium) (medium-drawable medium))))
    (%fill-image %image
                 (round x) (round y)
                 (round width) (round height)
                 (transform-region (maybe-transformation medium)
                                   (medium-ink medium))
                 (maybe-region medium))))

;;; standard medium protocol

(defmethod medium-draw-rectangle* ((medium render-medium-mixin) left top right bottom filled)
  (when (< right left) (rotatef left right))
  (when (< bottom top) (rotatef top bottom))
  (let* ((region (region-intersection
                  (maybe-region medium)
                  (transform-region (maybe-transformation medium)
                                    (make-rectangle* left top right bottom)))))
    (flet ((path ()
             (let ((path (make-path left top)))
               (line-to path right top)
               (line-to path right bottom)
               (line-to path left bottom)
               (close-path path)
               path)))
      (cond ((not filled)
             (%medium-stroke-paths medium (list (path))))
            ((rectanglep region)
             (with-bounding-rectangle* (x1 y1 x2 y2) region
               (%medium-fill-image medium x1 y1 (- x2 x1) (- y2 y1))))
            (t
             (%medium-fill-paths medium (list (path))))))))

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

(defmethod clime:medium-draw-circle* ((medium render-medium-mixin)
                                      center-x center-y radius start-angle end-angle
                                      filled)
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
                              toward-x toward-y transform-glyphs
                              &aux (end (if (null end)
                                            (length string)
                                            (min end (length string)))))
  (declare (ignore toward-x toward-y))
  (setq string (subseq string start end))
  (string-primitive-paths medium x y string align-x align-y transform-glyphs))
