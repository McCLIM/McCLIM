(in-package :clim-mezzano)

;;;
;;; fwidth/fheight are width and height including frame
;;; width and height are the interior width and height available to mcclim
;;; dx/dy are the x and y offsets to the interior available to mcclim
;;;
(defclass mezzano-mirror (image-mirror-mixin)
  ((fwidth     :initform 0)
   (fheight    :initform 0)
   (width      :initform 0)
   (height     :initform 0)
   (dx         :initform 0)
   (dy         :initfomr 0)
   (mez-pixels :initform nil)
   (mez-window :initform nil)
   (mez-frame  :initform nil)
   (mez-dirty-region :initform +nowhere+)
   (skip-count :initform 0)))

(defmethod %create-mirror-image :after ((sheet mezzano-mirror) width height)
  ;; change mirror image size?
  (debug-format "%create-mirror-image :after ((sheet mezzano-mirror) width height)")
  (debug-format "    ~S ~S ~S" sheet width height)
  )

(defgeneric image-mirror-to-mezzano (sheet))

(defmethod image-mirror-to-mezzano ((sheet image-mirror-mixin))
  )

(defun image-mirror-put (mez-window fwidth fheight dirty-r)
  (when mez-window
    ;; (debug-format "image-mirror-put ~S ~S ~S" mez-window width height)
    (mezzano.gui.compositor:damage-window
     mez-window
     0
     0
     fwidth
     fheight)
    ;; (map-over-region-set-regions
    ;;  #'(lambda (region)
    ;;      TODO must take into account interior offset (dx dy) and
    ;;      interior size (width height)
    ;;      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
    ;;          (region-intersection region (make-rectangle* 0 0 width height))
    ;;        (let ((width (round (- max-x min-x)))
    ;;              (height (round (- max-y min-y))))
    ;;          (debug-format "image-mirror-put ~S ~S ~S ~S ~S"
    ;;                        mez-window
    ;;                        (max 0 (- width (min 0 (- min-x))))
    ;;                        (max 0 (- height (min 0 (- min-y))))
    ;;                        width height)
    ;;          (mezzano.gui.compositor:damage-window
    ;;           mez-window
    ;;           (max 0 (- width (min 0 (- min-x))))
    ;;           (max 0 (- height (min 0 (- min-y))))
    ;;           width
    ;;           height))))
    ;;  dirty-r)
    ))

(declaim (inline mez-pixels-data-set-pixel))
(defun mez-pixels-data-set-pixel (data x y red green blue)
  (setf (aref data y x)
	(dpb blue (byte 8 0)
	     (dpb green (byte 8 8)
		  (dpb red (byte 8 16) #xFF000000)))))

(defun image-mirror-pre-put (mirror mez-pixels dx dy width height dirty-r)
  (let ((pixels (image-pixels (image-mirror-image mirror))))
    (declare (type opticl-rgb-image-pixels pixels))
    (map-over-region-set-regions
     #'(lambda (region)
         (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
           (region-intersection region (make-rectangle* 0 0
                                                        (1- width) (1- height)))
           (when mez-pixels
             (opticl:do-region-pixels (y x min-y min-x max-y max-x)
               pixels
               (multiple-value-bind (red green blue)
                   (opticl:pixel pixels y x)
                 (mez-pixels-data-set-pixel mez-pixels (+ dx x) (+ dy y) red green blue))))))
     dirty-r)))

(defmethod image-mirror-to-mezzano ((sheet mezzano-mirror))
  (declare (optimize speed))
  (with-slots (mcclim-render-internals::image-lock
               mcclim-render-internals::dirty-region
               mcclim-render-internals::finished-output
               MCCLIM-RENDER-INTERNALS::updating-p
               fwidth fheight
               mez-window
               mez-dirty-region skip-count) sheet
    (when (not (region-equal mez-dirty-region +nowhere+))
      (let ((reg))
        (climi::with-lock-held (mcclim-render-internals::image-lock)
          (setf reg mez-dirty-region)
          (setf mez-dirty-region +nowhere+))
        (image-mirror-put mez-window fwidth fheight reg)))))

(defmethod clim-backend:port-set-mirror-region
    ((port mezzano-port) (mirror mezzano-mirror) mirror-region)
  (debug-format "clim-backend:port-set-mirror-region ((port mezzano-port) (mirror mezzano-mirror) mirror-region)")
  (debug-format "    ~S ~S ~S" port mirror mirror-region)
  ;; (port-set-mirror-region port (slot-value mirror 'xmirror) mirror-region)
  )

(defmethod clim-backend:port-set-mirror-region
    ((port mezzano-port) mirror mirror-region)
  (debug-format "clim-backend:port-set-mirror-region ((port mezzano-port) mirror mirror-region)")
  (debug-format "    ~S ~S ~S" port mirror mirror-region)
  )


(defmethod clim-backend:port-set-mirror-transformation
    ((port mezzano-port) (mirror mezzano-mirror) mirror-transformation)
  (debug-format "clim-backend:port-set-mirror-transformation ((port mezzano-port) (mirror mezzano-mirror) mirror-transformation)")
  (debug-format "    ~S ~S ~S" port mirror mirror-transformation)
  ;; (port-set-mirror-transformation
  ;;  port
  ;;  (slot-value mirror 'xmirror)
  ;;  mirror-transformation)
  )

(defmethod clim-backend:port-set-mirror-transformation
    ((port mezzano-port) mirror mirror-transformation)
  (debug-format "clim-backend:port-set-mirror-transformation ((port mezzano-port) mirror mirror-transformation)")
  (debug-format "~S ~S ~S" port mirror mirror-transformation)
  )

(defmethod destroy-mirror ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (when (sheet-mirror sheet)
    (let ((mez-window (slot-value (sheet-mirror sheet) 'mez-window)))
      (remhash mez-window (slot-value port 'mez-window->sheet))
      (remhash mez-window (slot-value port 'mez-window->mirror))
      (mezzano.gui.compositor:close-window mez-window)))
  (when (port-lookup-mirror port sheet)
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod mcclim-render-internals::%mirror-force-output ((mirror mezzano-mirror))
  (with-slots (mcclim-render-internals::image-lock
               mcclim-render-internals::dirty-region
               dx dy
               width height
               mez-pixels
               mez-dirty-region) mirror
    (when mcclim-render-internals::dirty-region
      (climi::with-lock-held (mcclim-render-internals::image-lock)
        (when mcclim-render-internals::dirty-region
          (setf mez-dirty-region
                (region-union mez-dirty-region
                              mcclim-render-internals::dirty-region))
          (image-mirror-pre-put mirror mez-pixels dx dy width height mez-dirty-region)
          (setf mcclim-render-internals::dirty-region nil))))))
