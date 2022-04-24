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
  (with-slots (mcclim-render::dirty-region) mirror
    (setf mcclim-render::dirty-region +nowhere+))
  ;;(let ((data (climi::pattern-array (image-mirror-image mirror))))
  (with-slots (width height clx-image xlib-image) mirror
    (setf width w
          height h)
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

(declaim (inline xlib-image-data-set-pixel))
(defun xlib-image-data-set-pixel (data x y red green blue)
  (setf (aref data y x)
        (dpb blue (byte 8 0)
             (dpb green (byte 8 8)
                  (dpb red (byte 8 16) 0)))))

(defun image-mirror-pre-put (width height xmirror sheet clx-image xlib-image dirty-r)
  (let* ((pixels (climi::pattern-array (image-mirror-image sheet)))
         (fn (etypecase pixels
               ((simple-array (unsigned-byte 32) 2)
                #'(lambda (region)
                    (declare (optimize (speed 3)))
                    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                        (region-intersection region (make-rectangle* 0 0 (1- width) (1- height)))
                      (declare (type fixnum min-x min-y max-x max-y))
                      (when (and xmirror clx-image)
                        (locally
                              (declare (type (simple-array (unsigned-byte 32) (* *))
                                             pixels
                                             xlib-image))
                          (loop for y from min-y upto max-y do
                            (loop for x from min-x upto max-x do
                              (setf (aref xlib-image y x) (aref pixels y x))))))))))))
    (map-over-region-set-regions fn dirty-r)))

(defun image-mirror-to-x (mirror)
  (declare (optimize speed))
  (with-slots (clx-image xlib-image
               mcclim-render::image-lock gcontext
               mcclim-render::finished-output
               mcclim-render::updating-p
               width height dirty-xr)
      mirror
    (when (not (region-equal dirty-xr +nowhere+))
      (let (reg)
        (clim-sys:with-lock-held (mcclim-render::image-lock)
          (setf reg dirty-xr)
          (setf dirty-xr +nowhere+))
        (image-mirror-put width height mirror gcontext clx-image reg)))))

(defun %mirror-force-output (mirror)
  (with-slots (mcclim-render::image-lock
               mcclim-render::dirty-region
               dirty-xr width height clx-image
               xlib-image)
      mirror
    (unless (region-equal mcclim-render::dirty-region +nowhere+)
      (clim-sys:with-lock-held (mcclim-render::image-lock)
        (unless (region-equal mcclim-render::dirty-region +nowhere+)
          (setf dirty-xr (region-union dirty-xr mcclim-render::dirty-region))
          (image-mirror-pre-put width height mirror mirror clx-image xlib-image dirty-xr)
          (setf mcclim-render::dirty-region +nowhere+))))))
