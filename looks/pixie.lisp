(in-package :CLIM-INTERNALS)

(export '(pixie-look pixie/clx-look))

(defclass pixie-look (frame-manager) ())
(defclass pixie/clx-look (pixie-look clim-internals::clx-frame-manager) ())

; our stub inside clim proper
(defmethod make-pane-1 ((fm pixie-look) (frame application-frame) type &rest args)
  (apply #'make-instance
         (or (find-symbol (concatenate 'string "PIXIE-" (symbol-name type)) :climi)
             (find-symbol (concatenate 'string "PIXIE-" (symbol-name type) "-PANE") :climi)
             ; drop back to the built ins...
             (find-symbol (concatenate 'string (symbol-name type) "-PANE") :climi)
             type)
         :frame frame
         :manager fm
         :port (port frame)
         args))

(defparameter slider-button-long-dim 30)
(defparameter slider-button-short-dim 15)

(defclass pixie-slider-pane (slider-pane) (
  (dragging :initform nil)))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-enter-event))
  (with-slots (armed dragging) pane
    (cond
     ((not armed)
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
     (dragging
      (setf dragging :inside)))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-exit-event))
  (with-slots (armed dragging) pane
    (cond
     (dragging
      (setf dragging :outside))
     (armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane))))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-button-press-event))
  (with-slots (dragging) pane
    (setf dragging :inside)))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-motion-event))
  (with-slots (dragging) pane
    (when dragging
      (let ((value (convert-position-to-value pane
					      (if (eq (gadget-orientation pane) :vertical)
						  (pointer-event-y event)
						  (pointer-event-x event)))))
	(setf (gadget-value pane :invoke-callback nil) value)
	(drag-callback pane (gadget-client pane) (gadget-id pane) value)
	(dispatch-repaint pane (sheet-region pane))))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-button-release-event))
  (with-slots (armed dragging) pane
    (when dragging
      (setf (gadget-value pane :invoke-callback t)
	    (convert-position-to-value pane
				       (if (eq (gadget-orientation pane) :vertical)
					   (pointer-event-y event)
					   (pointer-event-x event))))
      (unless (eq dragging :inside)
        (setf armed nil)
        (disarmed-callback pane (gadget-client pane) (gadget-id pane)))
      (setf dragging nil)
      (dispatch-repaint pane (sheet-region pane)))))

; TODO: support labels and value printing
(defmethod handle-repaint ((pane pixie-slider-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((position (convert-value-to-position pane))
	  (slider-button-half-short-dim (ash slider-button-short-dim -1))
	  (slider-button-half-long-dim  (ash slider-button-long-dim -1))
          (background-color (gadget-current-color pane)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
	(display-gadget-background pane +gray83+ 0 0 (- x2 x1) (- y2 y1))
          (multiple-value-bind (channel thumb decoration)
              (case (gadget-orientation pane)
                ((:vertical)
	         (let ((middle (round (- x2 x1) 2)))
                   (values
                     (polygon-points ; Channel
                       (make-rectangle*
                         (- middle slider-button-half-short-dim 1)
                         y1
		         (+ middle slider-button-half-short-dim 2)
                         y2))
                     (polygon-points ; Thumb
                       (make-rectangle*
		         (- middle slider-button-half-short-dim)
                         (- position slider-button-half-long-dim -1)
		         (+ middle slider-button-half-short-dim)
                         (+ position slider-button-half-long-dim -1)))
                     (polygon-points ; Decoration
                       (make-rectangle*
		         (- middle slider-button-half-short-dim -2)
                         (- position 1)
		         (+ middle slider-button-half-short-dim -2)
                         (+ position 1))))))
                ((:horizontal)
	         (let ((middle (round (- y2 y1) 2)))
                   (values
                     (polygon-points ; Channel
                       (make-rectangle*
                         x1
                         (- middle slider-button-half-short-dim 1)
                         x2
		         (+ middle slider-button-half-short-dim 2)))
                     (polygon-points ; Thumb
                       (make-rectangle*
                         (- position slider-button-half-long-dim -1)
		         (- middle slider-button-half-short-dim)
                         (+ position slider-button-half-long-dim -1)
		         (+ middle slider-button-half-short-dim)))
                     (polygon-points ; Decoration
                       (make-rectangle*
                         (- position 1)
		         (- middle slider-button-half-short-dim -2)
                         (+ position 1)
		         (+ middle slider-button-half-short-dim -2)))))))
            ; the drawing code
            (draw-polygon pane channel :filled t :ink +gray76+)
            (draw-bordered-polygon pane channel :style :inset :border-width 1)
            (draw-polygon pane thumb :filled nil :ink +black+)
            (draw-polygon pane thumb :filled t :ink +gray83+)
            (draw-bordered-polygon pane thumb :style :outset :border-width 1)
            (draw-bordered-polygon pane decoration :style :inset :border-width 1))))))

(defmethod convert-value-to-position ((pane pixie-slider-pane))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (multiple-value-bind (good-dim1 good-dim2)
	(case (gadget-orientation pane)
          ((:vertical)
	    (values (+ y1 (ash slider-button-long-dim -1))
		    (- y2 (ash slider-button-long-dim -1))))
          ((:horizontal)
	    (values (+ x1 (ash slider-button-long-dim -1))
		    (- x2 (ash slider-button-long-dim -1)))))
      (+ good-dim1 (/ (* (- (gadget-value pane) (gadget-min-value pane))
			 (- good-dim2 good-dim1))
		      (gadget-range pane))))))

(defclass pixie-menu-bar-pane (menu-bar) ())

(defmethod handle-repaint ((pane pixie-menu-bar-pane) region)
  (with-special-choices (pane)
    (let* ((region (sheet-region pane))
           (frame (polygon-points (bounding-rectangle region))))
      (draw-polygon pane frame :ink +Blue+ :filled t)
      (draw-bordered-polygon pane frame :style :outset :border-width 1))))

(defclass pixie-menu-button-pane (menu-button-pane) ())

(defmethod handle-event ((pane menu-button-pane) (event pointer-enter-event))
  (when (slot-value (slot-value pane 'client) 'armed)
    (arm-branch pane)))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-press-event))
  (arm-branch pane))

(defmethod handle-event ((pane menu-button-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod handle-event ((pane menu-button-pane) (event pointer-ungrab-event))
  (destroy-substructure (menu-root pane)))


(defmethod handle-repaint ((pane pixie-menu-button-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let* ((region (sheet-region pane))
           (frame (polygon-points (bounding-rectangle region))))
      (draw-polygon pane frame :filled t :ink +gray83+)
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
        (cond
          ((slot-value pane 'armed)
           (let ((inset-frame (polygon-points
                                (make-rectangle* (+ x1 2) (+ y1 2) (- x2 2) (- y2 2)))))
             (draw-polygon pane inset-frame :filled t :ink +gray93+)
             (draw-bordered-polygon pane inset-frame
                           :style :outset
                           :border-width 1)))
          (t
           (draw-polygon pane frame :filled t :ink +gray83+)))
        (draw-label* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))))))

(defmethod compose-space ((gadget pixie-menu-button-pane))
  (space-requirement+* (space-requirement+* (compose-label-space gadget :wider 15 :higher 10)
                                            :min-width (* 2 (pane-x-spacing gadget))
                                            :width (* 2 (pane-x-spacing gadget))
                                            :max-width (* 2 (pane-x-spacing gadget))
                                            :min-height (* 2 (pane-y-spacing gadget))
                                            :height (* 2 (pane-y-spacing gadget))
                                            :max-height (* 2 (pane-y-spacing gadget)))
                       :min-width (* 2 *3d-border-thickness*)
                       :width (* 2 *3d-border-thickness*)
                       :max-width (* 2 *3d-border-thickness*)
                       :min-height (* 2 *3d-border-thickness*)
                       :height (* 2 *3d-border-thickness*)
                       :max-height (* 2 *3d-border-thickness*)))

(defclass pixie-menu-button-leaf-pane (pixie-menu-button-pane menu-button-leaf-pane) ())

(defclass pixie-menu-button-submenu-pane (pixie-menu-button-pane menu-button-submenu-pane) ())

(defmethod repaint-sheet ((pane pixie-menu-button-submenu-pane) region)
  (declare (ignore region))
  (with-slots (submenu-frame) pane
    (if submenu-frame
	(menu-draw-highlighted pane)
	(menu-draw-unhighlighted pane))))

; Image pane

; rebuild this with a pixmap repository for general re-use of pixmaps
; within a port/visual combination.

; This is just test/proof-of-concept code :]

(defclass pixie-image-pane (basic-gadget) (
  (image-pathname :initarg :pathname)
  (image-mask-pathname :initarg :mask-pathname :initform nil)
  (image-width    :type integer
                  :reader width
                  :initform 0)
  (image-height   :type integer
                  :reader height
                  :initform 0)
  (image-image    :initform nil)
  (image-pixmap   :initform nil)
  (image-stencil  :initform nil)))

; TODO: allow pixmaps to be realized from unrealized media
(defmethod initialize-instance :after ((pane pixie-image-pane) &rest args)
  (declare (ignore args))
  (with-slots (image-pathname image-image image-width image-height) pane
    (let* ((data (read-image-file image-pathname))
           (image (make-truecolor-image data 255)))
      (destructuring-bind (width height) (array-dimensions data)
        (setf image-width  width
              image-height height
              image-image  image))))
  (with-slots (image-mask-pathname image-stencil) pane
    (when image-mask-pathname
      (let* ((data (read-image-file image-mask-pathname)))
        (setf image-stencil (make-stencil data))))))

(defmethod handle-repaint ((pane pixie-image-pane) region)
  (declare (ignore region))
  (with-slots (image-pixmap image-width image-height) pane
    ; we defer the image loading until after realisation
    ; which will cause a delay in the initial exposure,
    ; CHECKME - should we be able to realize pixmaps derived
    ; from unrealized panes?
    ; Technically we could just do it from the port's root
    ; since we don't switch visuals within a port at this point.
    ; but that is not necessarily a good thing
    (unless image-pixmap
      (with-slots (image-image image-width image-height image-pixmap) pane
        (setf image-pixmap
              (with-output-to-pixmap (medium pane :width image-width :height image-height)
                (draw-image (medium-sheet medium)
                            image-image
                            :clipping-region (make-rectangle* 0 0 image-width image-height))))))
    (copy-from-pixmap image-pixmap 0 0 image-width image-height pane 0 0)))

(defmethod compose-space ((pane pixie-image-pane))
  (with-slots (image-width image-height) pane
    (let ((w image-width)
          (h image-height))
      (make-space-requirement :width     w :height     h
                              :min-width w :min-height h
                              :max-width w :max-height h))))
