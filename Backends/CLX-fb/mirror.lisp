(in-package #:clim-clx-fb)

(defclass clx-fb-mirror (image-mirror-mixin clx-mirror)
  ((width :initform 0)
   (height :initform 0)
   (xlib-image :initform nil)
   (dirty-xr :initform +nowhere+)
   (clx-image :initform nil)
   (gcontext :initform nil)))

;;; for port
(defmethod mcclim-render::%create-mirror-image :after ((mirror clx-fb-mirror) w h)
  (setf (mcclim-render:image-dirty-region mirror) +nowhere+)
  ;;(let ((data (climi::pattern-array (image-mirror-image mirror))))
  (with-slots (width height clx-image xlib-image) mirror
    (setf width (ceiling w)
          height (ceiling h))
    ;; Fill the image with a recognizable color so that pixels in the
    ;; mirror image which the backend never fills stand out. The
    ;; highest byte should not be used by X, but in case something
    ;; gets messed up in terms of e.g. the pixel format, the color
    ;; will still be recognizable and it will never be transparent.
    (setf xlib-image (make-array (list height width)
                                 :element-type '(unsigned-byte 32)
                                 :initial-element #xc080e0a0))
    (setf clx-image (xlib:create-image :bits-per-pixel 32
                                       :data xlib-image
                                       :depth 24
                                       :width width
                                       :height height
                                       :format :z-pixmap))))

(defun image-mirror-put (width height xmirror gcontext clx-image dirty-r)
  (map-over-region-set-regions
   #'(lambda (region)
       (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
           (region-intersection region
                                (make-rectangle* 0 0 width height))
         (let* ((min-x (floor min-x))
                (min-y (floor min-y))
                (max-x (ceiling max-x))
                (max-y (ceiling max-y))
                (width (- max-x min-x))
                (height (- max-y min-y)))
           (when (and xmirror clx-image)
             (xlib::put-image (window xmirror)
                              gcontext
                              clx-image
                              :src-x (max min-x 0)
                              :src-y (max min-y 0)
                              :x (max min-x 0)
                              :y (max min-y 0)
                              :width  (max 0 (- width (min 0 (- min-x))))
                              :height (max 0 (- height (min 0 (- min-y)))))))))
   dirty-r))

(defun image-mirror-pre-put (width height mirror clx-image xlib-image dirty-r)
  (unless (and mirror clx-image)
    (return-from image-mirror-pre-put))
  (let* ((pixels (clime:pattern-array (image-mirror-image mirror)))
         (w (1- width))
         (h (1- height)))
    (declare (type (simple-array (unsigned-byte 32) 2) pixels xlib-image)
             (type mcclim-render::image-dimension w h)
             (optimize (speed 3) (safety 1)))
    (flet ((put-rect (region)
             (declare (type standard-rectangle region))
             (climi::with-standard-rectangle* (x1 y1 x2 y2) region
               (declare (type fixnum x1 y1 x2 y2))
               (alexandria:maxf x1 0)
               (alexandria:maxf y1 0)
               (alexandria:minf x2 w)
               (alexandria:minf y2 h)
               (loop for y from y1 upto y2 do
                 (loop for x from x1 upto x2 do
                   (setf (aref xlib-image y x) (aref pixels y x)))))))
      (map-over-region-set-regions #'put-rect dirty-r))))

(defun image-mirror-to-x (mirror)
  (declare (optimize speed))
  (with-slots (clx-image xlib-image gcontext width height dirty-xr)
      mirror
    (unless (region-equal dirty-xr +nowhere+)
      (let (reg)
        (with-image-locked (mirror)
          (setf reg dirty-xr)
          (setf dirty-xr +nowhere+)
          (image-mirror-put width height mirror gcontext clx-image reg))))))

(defun %mirror-force-output (mirror)
  (with-slots (dirty-xr width height clx-image xlib-image)
      mirror
    (let ((dirty-region (mcclim-render:image-dirty-region mirror)))
      (unless (region-equal dirty-region +nowhere+)
        (with-image-locked (mirror)
          (unless (region-equal dirty-region +nowhere+)
            (setf dirty-xr (region-union dirty-xr dirty-region)
                  (mcclim-render:image-dirty-region mirror) +nowhere+)
            (image-mirror-pre-put width height mirror clx-image xlib-image dirty-region)))))))
