(in-package :clim-mezzano)

;;;
;;; fwidth/fheight are width and height including frame
;;; width and height are the interior width and height available to mcclim
;;; dx/dy are the x and y offsets to the interior available to mcclim
;;;
(defclass mezzano-mirror (image-mirror-mixin)
  ((top-levelp :initform nil)
   (fwidth     :initform 0)
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

(defun size-deltas (mez-mirror)
  (with-slots (fwidth fheight width height) mez-mirror
    (values (- fwidth width) (- fheight height))))

(defun resize-mirror (mirror new-width new-height)
  (setf new-width (max 5 new-width))
  (setf new-height (max 5 new-height))
  (with-slots (fwidth fheight width height mez-frame mez-window) mirror
    (when (or (/= width new-width) (/= height new-height))
      (setf fwidth (+ new-width (- fwidth width))
            fheight (+ new-height (- fheight height))
            width new-width
            height new-height)
      (let* ((surface (mos:make-surface fwidth fheight))
             (pixels (mos:surface-pixels surface)))
        (mos:resize-frame mez-frame surface)
        (mos:resize-window mez-window surface)
        (setf (slot-value mirror 'mez-pixels) pixels)
        (mos:draw-frame mez-frame)))))

(defmethod %create-mirror-image :after ((mirror mezzano-mirror) width height)
  (resize-mirror mirror width height))

(defgeneric image-mirror-to-mezzano (sheet))

(defmethod image-mirror-to-mezzano ((sheet image-mirror-mixin))
  )

(defun image-mirror-put (mez-window dx dy width height dirty-r)
  (when mez-window
    ;; (debug-format "image-mirror-put ~S ~S ~S" mez-window width height)
    (mos:damage-window
     mez-window
     dx
     dy
     width
     height)
    ;; (map-over-region-set-regions
    ;;  #'(lambda (region)
    ;;      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
    ;;          (region-intersection region (make-rectangle* 0 0 width height))
    ;;        (let ((width (round (- max-x min-x)))
    ;;              (height (round (- max-y min-y))))
    ;;          ;; (debug-format "image-mirror-put ~S ~S ~S ~S ~S"
    ;;          ;;               mez-window
    ;;          ;;               (max 0 (- width (min 0 (- min-x))))
    ;;          ;;               (max 0 (- height (min 0 (- min-y))))
    ;;          ;;               width height)
    ;;          (mos:damage-window
    ;;           mez-window
    ;;           (+ dx (max 0 (- width (min 0 (- min-x)))))
    ;;           (+ dy (max 0 (- height (min 0 (- min-y)))))
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
             (opticl:do-region-pixels (y x min-y min-x (1+ max-y) (1+ max-x))
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
               dx dy
               width height
               mez-window
               mez-dirty-region skip-count) sheet
    (when (not (region-equal mez-dirty-region +nowhere+))
      (let ((reg))
        (climi::with-lock-held (mcclim-render-internals::image-lock)
          (setf reg mez-dirty-region)
          (setf mez-dirty-region +nowhere+))
        (image-mirror-put mez-window dx dy width height reg)))))

(defmethod port-set-mirror-region
    ((port mezzano-port) (mirror mezzano-mirror) mirror-region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) mirror-region
    (resize-mirror mirror
                   (1+ (ceiling (- max-x min-x)))
                   (1+ (ceiling (- max-y min-y)))))
  (mos:draw-frame (slot-value mirror 'mez-frame)))

(defmethod port-set-mirror-transformation
    ((port mezzano-port) (mirror mezzano-mirror) mirror-transformation)
  (unless (slot-value mirror 'top-levelp)
    (let ((mez-window (slot-value mirror 'mez-window))
          (mez-frame (slot-value mirror 'mez-frame)))
      (multiple-value-bind (x y) (transform-position mirror-transformation 0 0)
        ;; Don't know why this delay is required but without it menus
        ;; are not displayed near the menu bar. 25 Mar. 2018 fittestbits
        (sleep 0.001)
        (setf (mos:window-x mez-window) (floor x)
              (mos:window-y mez-window) (floor y))
        (mos:draw-frame mez-frame)))))

(defmethod destroy-mirror ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-mirror sheet)))
    (when (typep mirror 'mezzano-mirror)
      (let ((mez-window (slot-value mirror 'mez-window)))
        (remhash mez-window (slot-value port 'mez-window->sheet))
        (remhash mez-window (slot-value port 'mez-window->mirror))
        (mos:close-window mez-window)))
    (when (port-lookup-mirror port sheet)
      (port-unregister-mirror port sheet (sheet-mirror sheet)))))

(defmethod port-disable-sheet ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-mirror sheet)))
    (when (and (typep mirror 'mezzano-mirror)
               (eq sheet (port-lookup-sheet port mirror)))
      ;; disabling a top level sheet - close the window and delete mappings
      (let ((mez-window (slot-value mirror 'mez-window)))
        (remhash mez-window (slot-value port 'mez-window->sheet))
        (remhash mez-window (slot-value port 'mez-window->mirror))
        (mos:close-window mez-window)))))

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
