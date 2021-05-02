(in-package #:clim-clx-fb)

(defclass clx-fb-mirror (image-mirror-mixin)
  ((width :initform 0)
   (height :initform 0)
   (xmirror :initform nil
            :initarg :xmirror)
   (xlib-image :initform nil)
   (dirty-xr :initform +nowhere+)
   (clx-image :initform nil)
   (gcontext :initform nil)))

;;; for port
(defmethod mcclim-render-internals::%create-mirror-image :after ((mirror clx-fb-mirror) w h)
  (with-slots (mcclim-render-internals::dirty-region) mirror
    (setf mcclim-render-internals::dirty-region nil))
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
         (let ((width (round (- max-x min-x)))
               (height (round (- max-y min-y))))
           (when (and xmirror clx-image)
             (xlib::put-image (window xmirror)
                              gcontext
                              clx-image
                              :src-x (round (max min-x 0))
                              :src-y (round (max min-y 0))
                              :x (round (max min-x 0)) :y (round (max min-y 0))
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
                        (do ((y min-y)
                             (x min-x (1+ x)))
                            ((> y max-y))
                          (locally
                              (declare (type (simple-array (unsigned-byte 32) (* *))
                                             pixels
                                             xlib-image))
                            (setf (aref xlib-image y x) (aref pixels y x)))
                          (when (= x max-x)
                            (incf y)
                            (setf x min-x))))))))))
    (map-over-region-set-regions fn dirty-r)))

(defun image-mirror-to-x (mirror)
  (declare (optimize speed))
  (with-slots (xmirror
               clx-image xlib-image
               mcclim-render-internals::image-lock gcontext
               mcclim-render-internals::dirty-region
               mcclim-render-internals::finished-output
               mcclim-render-internals::updating-p
               width height dirty-xr)
      mirror
    (when (not (region-equal dirty-xr +nowhere+))
      (let (reg)
        (clim-sys:with-lock-held (mcclim-render-internals::image-lock)
          (setf reg dirty-xr)
          (setf dirty-xr +nowhere+))
        (image-mirror-put width height xmirror gcontext clx-image reg)))))

(defun %mirror-force-output (mirror)
  (with-slots (mcclim-render-internals::image-lock mcclim-render-internals::dirty-region dirty-xr width height clx-image
               xlib-image xmirror)
      mirror
    (when mcclim-render-internals::dirty-region
      (clim-sys:with-lock-held (mcclim-render-internals::image-lock)
        (when mcclim-render-internals::dirty-region
          (setf dirty-xr (region-union dirty-xr mcclim-render-internals::dirty-region))
          (image-mirror-pre-put width height xmirror mirror clx-image xlib-image dirty-xr)
          (setf mcclim-render-internals::dirty-region nil))))))
