(in-package :clim-internals)

;;;
;
; This file is in transition, please don't fix it :]
;
; Much is left purposefully un-refactored until enough is reworked that it can be
; refactored into sensible subcomponents, without becoming a hodge-podge of random
; mixins (as opposed to a hodge-podge of sensible mixins :]) - BTS
;
; This especially applies to colour and event management :\
;
; TODO: move the hardcoded color-settings into proper defaults
;
;;;

;;; TODO: Add units label to slider pane
;;; TODO: Matching repaint method for the list pane
;;; TODO: Is there a locking bug, and does it somehow involve pixie?
;;;       (Or is my computer still haunted?)
;;; TODO: Colors of buttons in clim-fig are wrong

(export '(pixie-look #+clx pixie/clx-look))

(defclass pixie-look (frame-manager) ())
#+clx (defclass pixie/clx-look (pixie-look clim-clx::clx-frame-manager) ())

(defun use-pixie ()
  (setf *default-frame-manager* 
        (make-instance 'pixie/clx-look
                       :port (find-port))))

(defmacro define-pixie-gadget (abstract-type pixie-type &key (enabled t))
  `(defmethod make-pane-1 ((fm pixie-look)
			   (frame application-frame)
			   (type (eql ',abstract-type))
			   &rest args)
    (declare (ignorable fm frame type args))
    ,(if enabled
	 `(apply #'make-instance
           ',pixie-type
           :frame frame
           :manager fm
           :port (port frame)
	   args)
	 `(call-next-method))))

;; Let us please stop playing these stupid symbol games.
#+NIL
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

;;; Scroll button patterns

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +pixie-arrow-pattern+
    #2a((0 0 0 1 0 0 0)
        (0 0 1 1 1 0 0)
        (0 1 1 1 1 1 0)
        (1 1 1 1 1 1 1)))
  
  (flet ((rotate (array)
	   (let ((new-array (make-array (reverse (array-dimensions array)))))
	     (dotimes (i (array-dimension array 0))
	       (dotimes (j (array-dimension array 1))
		 (setf (aref new-array j (- (array-dimension array 0) i 1))
		       (aref array i j))))
	     new-array)))
    (let* ((up    +pixie-arrow-pattern+)
	   (right (rotate up))
	   (down  (rotate right))
	   (left  (rotate down)))
      (macrolet ((def (var) 
		     `(defparameter ,(intern (format nil "~A~A~A"
						     (symbol-name '#:+pixie-)
						     (symbol-name var)
						     (symbol-name '#:-arrow+))
					     (find-package :climi))
		       (make-pattern ,var (list +transparent-ink+ +black+)))))
	(def up)
	(def right)
	(def down)
	(def left)))))

; Standard

; TODO - clean up all of this colour nonsense
; which should involve some sensible ideas about tints vs' inks

(defclass pixie-gadget ()
  ((highlighted      :initarg :highlight
                     :initform +gray93+
                     :reader pane-highlight)
   (paper-color      :initarg :paper-color
                     :initform +white+
                     :reader pane-paper-color)
   (inking-color     :initarg :inking-color
                     :initform +black+
                     :reader pane-inking-color)
   (foreground       :initarg :foreground
                     :initform +gray83+
                     :reader pane-foreground)
   (background       :initarg :background
                     :initform +gray76+
                     :reader pane-background)))

; Convenience

(defun draw-up-box (pane x1 y1 x2 y2 foreground)
  (let ((x2 (- x2 1)))
    (draw-rectangle* pane x1 y1 x2 y2 :ink foreground)
    ;; white outline
    (draw-line* pane x1 y2 x1 y1 :ink +white+)
    (draw-line* pane x2 y1 x1 y1 :ink +white+)
    ;; now for the gray inline
    (let ((x1 (+ x1 1))			; I'd prefer this be zero, so that there isn't
          (y1 (+ y1 1))			; the little sparkling white pixel in both corners
          (x2 (- x2 1))			; (bothersome in the corner of a scroller-pane),
          (y2 (- y2 1)))		; but we may be transformed, so too much work. Bah.
      (draw-line* pane x1 y2 x2 y2 :ink +gray54+)
      (draw-line* pane x2 y1 x2 y2 :ink +gray54+))
    ;; now for the black outline
    (draw-line* pane x1 y2 x2 y2 :ink +black+)
    (draw-line* pane x2 y1 x2 y2 :ink +black+)
    (draw-label* pane x1 y1 x2 y2
                      :ink (pane-inking-color pane))))

(defun draw-down-box (pane x1 y1 x2 y2 foreground)
  (draw-rectangle* pane x1 y1 x2 y2 :ink foreground)
  ;; white outline
  (draw-line* pane x1 y2 x1 y1 :ink +gray58+)
  (draw-line* pane x2 y1 x1 y1 :ink +gray58+)
  ;; now for the black inline
  (let ((x1 (+ x1 1))
        (y1 (+ y1 1))
        (x2 (- x2 2))
        (y2 (- y2 2)))
    (draw-line* pane x1 y1 (+ x2 1) y1 :ink +black+)
    (draw-line* pane x1 y1 x1 (+ y2 1) :ink +black+))
  ;; now for the white outline
  (draw-line* pane x1 y2 x2 y2 :ink +white+)
  (draw-line* pane x2 y1 x2 y2 :ink +white+)
  (draw-label* pane x1 y1 x2 y2
               :ink (pane-inking-color pane)))

; Highlighting

(defmethod gadget-highlight-background ((gadget pixie-gadget))
  +gray93+)

(defmethod effective-gadget-foreground ((gadget pixie-gadget))
  (if (slot-value gadget 'armed)
      +gray93+
      +gray83+))

(defmethod effective-gadget-background ((gadget pixie-gadget))
  (if (slot-value gadget 'armed)
      (gadget-highlight-background gadget)
      (pane-background gadget)))

(defmethod effective-gadget-input-area-color ((gadget pixie-gadget))
  +white+)

(defclass draggable-arming-mixin (arm/disarm-repaint-mixin)
  ()
  (:documentation
   "Mixin class for gadgets, which will be armed, when the mouse enters and 
    disarmed, when the mouse leaves, and manages dragging."))

(defmethod handle-event :before ((pane draggable-arming-mixin) (event pointer-enter-event))
  (declare (ignorable event))
  (with-slots (armed dragging) pane
    (if dragging
        (setf dragging :inside)
        (unless armed
          (arm-gadget pane)))))

(defmethod handle-event :after ((pane draggable-arming-mixin) (event pointer-exit-event))
  (declare (ignorable event))
  (with-slots (armed dragging) pane
    (if dragging
        (setf dragging :outside)
        (when armed
          (disarm-gadget pane)))))

; Slider (refactor into 'thumbed' gadget?)

(defconstant +pixie-slider-pane-thumb-size+  5000.0)
(defconstant +pixie-slider-thumb-half-height+ 17)
(defconstant +pixie-slider-thumb-height+      34)
(defconstant +pixie-slider-thumb-half-width+   8)


(defclass pixie-slider-pane (pixie-gadget draggable-arming-mixin slider-pane)
  ((dragging
    :initform nil)
   (drag-delta
     :initform 0)
   (bounce-value
     :initform 0)
   (thumb-size
     :initarg  :thumb-size
     :initform 1/4
     :accessor gadget-thumb-size)
   (repeating
     :initform 0)
   (was-repeating
     :initform 0))
  (:default-initargs
    :border-style :inset
    :border-width 1))

(define-pixie-gadget slider pixie-slider-pane)

(defmethod compose-space ((pane pixie-slider-pane) &key width height)
  (declare (ignore width height))
  (if (eq (gadget-orientation pane) :vertical)
      (make-space-requirement :min-width   *scrollbar-thickness*
			      :width       *scrollbar-thickness*
                              :max-width   +fill+
			      :min-height  (* 2 *scrollbar-thickness*)
			      :height      (* 4 *scrollbar-thickness*)
                              :max-height  +fill+)
      (make-space-requirement :min-height  *scrollbar-thickness*
			      :height      *scrollbar-thickness* 
			      :max-height  +fill+
			      :min-width   (* 2 *scrollbar-thickness*)
			      :width       (* 4 *scrollbar-thickness*)
                              :max-width   +fill+)))

(defmethod vertical-gadget-orientation-transformation ((pane gadget))
  (ecase (gadget-orientation pane)
    (:vertical   +identity-transformation+)
    (:horizontal (make-transformation 0 1 1 0 0 0))))

;(defun translate-range-value (a mina maxa mino maxo)
;  "When \arg{a} is some value in the range from \arg{mina} to \arg{maxa},
;   proportionally translate the value into the range \arg{mino} to \arg{maxo}."
;  (+ mino (* (/ (- a mina) (- maxa mina)) (- maxo mino))))

(defmethod gadget-thumb-region ((pane pixie-slider-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-bed-region pane)
    (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 1) (+ y1 1) (- x2 1) (- y2 1))
      (multiple-value-bind (minv maxv) (gadget-range* pane)
        (multiple-value-bind (value) (gadget-value pane)
          (let ((half-thumb-size +pixie-slider-thumb-half-height+))
            (let ((ym (translate-range-value value
                          minv maxv
                          (+ y1 half-thumb-size) (- y2 half-thumb-size))))
              (make-rectangle* x1 (- ym half-thumb-size)
                               x2 (+ ym half-thumb-size)))))))))

(defmethod gadget-bed-region ((pane pixie-slider-pane))
  (with-bounding-rectangle* (minx miny maxx maxy)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (let* ((middle     (/ (+ maxx minx) 2))
           (minx       (- middle +pixie-slider-thumb-half-width+))
           (maxx       (+ middle +pixie-slider-thumb-half-width+)))
      (make-rectangle* (+ minx 1) (+ miny 1)
                       (- maxx 1) (- maxy 1)))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-button-release-event))
  (with-slots (armed dragging value bounce-value repeating was-repeating) pane
    (setf was-repeating repeating)
    (when armed
      (setf armed t
           (gadget-value pane :invoke-callback t)
           (convert-position-to-value pane
                                      (if (eq (gadget-orientation pane) :vertical)
                                          (pointer-event-y event)
                                          (pointer-event-x event)))))
    (when dragging
      (unless (eq dragging :inside)
        (setf armed nil
            ; value bounce-value ; this bouncing is more annoying than anything for sliders
            )
        (disarmed-callback pane (gadget-client pane) (gadget-id pane)))
      (setf dragging nil)
      (dispatch-repaint pane (sheet-region pane)))))

(defmethod handle-event ((pane pixie-slider-pane) (event timer-event))
  (let ((token (clim-internals::event-token event)))
    (with-slots (was-repeating repeating) pane
      (unless (eql was-repeating repeating)
        (case token
          ((up-notch)
           (when (< (gadget-value pane) (gadget-max-value pane))
	     #+NIL
             (clim-internals::schedule-timer-event pane token 0.1)
             (incf (gadget-value pane))
             (dispatch-repaint pane (sheet-region pane))))
          ((down-notch)
           (when (> (gadget-value pane) (gadget-min-value pane))
	     #+NIL
             (clim-internals::schedule-timer-event pane token 0.1)
             (decf (gadget-value pane))
             (dispatch-repaint pane (sheet-region pane)))))))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y)
      (transform-position (vertical-gadget-orientation-transformation pane)
                          (pointer-event-x event) (pointer-event-y event))
    
    (with-slots (armed dragging drag-delta value bounce-value repeating) pane
      (incf repeating)
      (let ((thumb (gadget-thumb-region pane)))
        (cond
          ((region-contains-position-p thumb x y)
           ; Thumb
           (setf dragging     :inside
                 armed        t
                 bounce-value value
                 drag-delta   (- y (bounding-rectangle-min-y thumb))))
          ((region-contains-position-p (gadget-bed-region pane) x y)
           ; well, they clicked in the bed, but not on the thumb
           ; move up or down one notch
           (cond
             ((< y (bounding-rectangle-min-y thumb))
	      #+NIL
              (clim-internals::schedule-timer-event pane 'down-notch 0.1)
              ; move toward the min
              (when (> (gadget-value pane) (gadget-min-value pane))
                (decf (gadget-value pane))
                (dispatch-repaint pane (sheet-region pane))))
             ((> y (bounding-rectangle-max-y thumb))
	      #+NIL
              (clim-internals::schedule-timer-event pane 'up-notch 0.1)
              ; move toward the max
              (when (< (gadget-value pane) (gadget-max-value pane))
                (incf (gadget-value pane))
                (dispatch-repaint pane (sheet-region pane)))))))))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-motion-event))
  (with-slots (dragging drag-delta thumb-size) pane
    (multiple-value-bind (x y)
        (transform-position (vertical-gadget-orientation-transformation pane)
                            (pointer-event-x event) (pointer-event-y event))
      (declare (ignore x))
      (when dragging
        (let* ((y-new-thumb-top (- y drag-delta))
               (bed-region      (gadget-bed-region pane))
               (miny            (bounding-rectangle-min-y bed-region))
               (maxy            (bounding-rectangle-max-y bed-region))
               (minv            (gadget-min-value pane))
               (maxv            (gadget-max-value pane))
               (thumb-size      (* thumb-size (- minv maxv)))
               (value           (min maxv
                                     (max minv
                                          (translate-range-value
                                              (+ y-new-thumb-top +pixie-slider-thumb-half-height+)
                                              (+ miny +pixie-slider-thumb-half-height+)
                                              (- maxy +pixie-slider-thumb-half-height+)
                                              minv
                                              maxv)))))
	  (setf (gadget-value pane :invoke-callback nil) value)
	  (drag-callback pane (gadget-client pane) (gadget-id pane) value)
	  (dispatch-repaint pane (sheet-region pane)))))))

;;; Repaint

(defmethod handle-repaint ((pane pixie-slider-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((tr (vertical-gadget-orientation-transformation pane)))
      (let ((transformed-sheet (transform-region tr (sheet-region pane))))
        (with-bounding-rectangle* (minx miny maxx maxy)
            transformed-sheet
          (with-drawing-options (pane :transformation tr)
            ; This region-difference is a bit weird
            ; the gadget-bed-region seems to be being transformed by the with-drawing-options
            ; but the sheet-region itself not, which I guess makes some kind of sense
            ; -- CHECKME
            (with-drawing-options (pane :clipping-region (region-difference
                                                           transformed-sheet
                                                           (gadget-bed-region pane)))
              (draw-rectangle* pane minx miny maxx maxy :filled t :ink *3d-normal-color*))
            ;; draw bed
            (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-bed-region pane)
              (with-drawing-options (pane :clipping-region (region-difference
                                                             transformed-sheet
                                                             (gadget-thumb-region pane)))
                (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 1) (+ y1 1)
                                                           (- x2 1) (- y2 1))
                  (draw-rectangle* pane x1 y1 x2 y2 :ink (pane-background pane)))
                (draw-bordered-polygon pane
                                       (polygon-points (make-rectangle* x1 y1 x2 y2))
                                       :style :inset
                                       :border-width 1)))
            ;; draw thumb
            (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-thumb-region pane)
              (draw-up-box pane x1 y1 x2 y2 (effective-gadget-foreground pane))
              ;; draw decoration in the thumb
              (let* ((middle (/ (+ y1 y2) 2))
                     (y1     (- middle 1))
                     (y2     middle)
                     (x1     (+ x1 2))
                     (x2     (- x2 3)))
                (draw-line* pane x1 y1 x2 y1       :ink +gray58+)
                (draw-line* pane x1 y2 x2 y2 :ink +white+)
                (draw-line* pane x2 y1 x2 y2 :ink +white+)))))))))

; Scrollbar

; We derive from the slider, since the slider is the same, only
; less so.
;;; XXX Probably should derive from scroll-bar too.

(defconstant +pixie-scroll-bar-pane-thumb-size+  5000.0)
(defconstant +pixie-scroll-bar-thumb-half-height+ 17)
(defconstant +pixie-scroll-bar-thumb-height+      34)
(defconstant +pixie-scroll-bar-thumb-half-width+   8)

(defclass pixie-scroll-bar-pane (pixie-slider-pane) (
   (drag-callback
     :initarg :drag-callback
     :initform nil
     :reader scroll-bar-drag-callback)
   (scroll-to-bottom-callback
     :initarg :scroll-to-bottom-callback
     :initform nil
     :reader scroll-bar-scroll-to-bottom-callback)
   (scroll-to-top-callback
     :initarg :scroll-to-top-callback
     :initform nil
     :reader scroll-bar-scroll-to-top-callback)
   (scroll-down-line-callback
     :initarg :scroll-down-line-callback
     :initform nil
     :reader scroll-bar-scroll-down-line-callback)
   (scroll-up-line-callback
     :initarg :scroll-up-line-callback
     :initform nil
     :reader scroll-bar-scroll-up-line-callback)
   (scroll-down-page-callback
     :initarg :scroll-down-page-callback
     :initform nil
     :reader scroll-bar-scroll-down-page-callback)
   (scroll-up-page-callback
     :initarg :scroll-up-page-callback
     :initform nil
     :reader scroll-bar-scroll-up-page-callback)
   (thumb-size
     :initarg :thumb-size
     :initform 1/4
     :accessor gadget-thumb-size))
  (:default-initargs
     :value 0
     :min-value 0
     :max-value 1
     :orientation :vertical))

(define-pixie-gadget scroll-bar pixie-scroll-bar-pane)

(defmethod compose-space ((pane pixie-scroll-bar-pane) &key width height)
  (declare (ignore width height))
  (if (eq (gadget-orientation pane) :vertical)
      (make-space-requirement :min-width   1
			      :width       *scrollbar-thickness*
			      :min-height (* 3 *scrollbar-thickness*)
			      :height     (* 4 *scrollbar-thickness*))
      (make-space-requirement :min-height  1
			      :height      *scrollbar-thickness*
			      :min-width  (* 3 *scrollbar-thickness*)
			      :width      (* 4 *scrollbar-thickness*))))

(defmethod drag-callback ((pane pixie-scroll-bar-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-drag-callback pane) value))

(defmethod scroll-to-top-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-top-callback pane)))

(defmethod scroll-to-bottom-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-bottom-callback pane)))

(defmethod scroll-up-line-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-line-callback pane)))

(defmethod scroll-up-page-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-page-callback pane)))

(defmethod scroll-down-line-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-line-callback pane)))

(defmethod scroll-down-page-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-page-callback pane)))

(defmethod scroll-bar-thumb-size ((pane pixie-scroll-bar-pane))
  (gadget-thumb-size pane))

(defmethod (setf scroll-bar-thumb-size) (value (pane pixie-scroll-bar-pane))
  (setf (gadget-thumb-size pane) value))

(defmethod gadget-up-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (let ((y1 (+ y1 1))
          (y2 (- y2 1))
          (x1 (+ x1 1))
          (x2 (- x2 1)))
      (declare (ignore y2))
      (make-rectangle* x1 y1
                       x2 (+ y1 (- x2 x1))))))

(defmethod gadget-down-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (let ((y1 (+ y1 1))
          (y2 (- y2 1))
          (x1 (+ x1 1))
          (x2 (- x2 1)))
      (declare (ignore y1))
      (make-rectangle* x1 (- y2 (- x2 x1))
                       x2 y2))))

(defmethod gadget-bed-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (make-rectangle* minx (+ miny (- maxx minx) 0)
                     maxx (- maxy (- maxx minx) 0))))

(defmethod gadget-thumb-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-bed-region pane)
    (let ((y1 (+ y1 1))
          (y2 (- y2 1))
          (x1 (+ x1 1))
          (x2 (- x2 1)))
      (multiple-value-bind (minv maxv) (gadget-range* pane)
        (multiple-value-bind (v) (gadget-value pane)
          (let ((ts (gadget-thumb-size pane)))
            (let ((ya (translate-range-value v minv (+ maxv ts) y1 y2))
                  (yb (translate-range-value (+ v ts) minv (+ maxv ts) y1 y2)))
              (make-rectangle* x1 (- ya 1) x2 (+ yb 1)))))))))

(defmethod* (setf scroll-bar-values)
  (min-value max-value thumb-size value (scroll-bar pixie-scroll-bar-pane))
  (setf (slot-value scroll-bar 'min-value) min-value
	(slot-value scroll-bar 'max-value) max-value
	(slot-value scroll-bar 'thumb-size) thumb-size
	(slot-value scroll-bar 'value) value)
  (dispatch-repaint scroll-bar (sheet-region scroll-bar)))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-button-release-event))
  (with-slots (armed dragging repeating was-repeating) pane
    (setf was-repeating repeating)
    (cond
     (dragging
      (unless (eq dragging :inside)
        (setf armed nil)
        (disarmed-callback pane (gadget-client pane) (gadget-id pane)))
      (setf dragging nil)
      (dispatch-repaint pane (sheet-region pane)))
     (t ; we were pressing on one of the arrows
      (when armed
        ; if we were armed, we're still armed, but not :up or :down
        (setf armed t)
        (dispatch-repaint pane (sheet-region pane)))))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event timer-event))
  (let ((token (clim-internals::event-token event)))
    (with-slots (was-repeating repeating) pane
      (unless (eql was-repeating repeating)
	#+NIL
        (clim-internals::schedule-timer-event pane token 0.1)
        (case token
          ((up-line)
           (scroll-up-line-callback pane (gadget-client pane) (gadget-id pane)))
          ((down-line)
           (scroll-down-line-callback pane (gadget-client pane) (gadget-id pane)))
          ((up-page)
           (scroll-up-page-callback pane (gadget-client pane) (gadget-id pane)))
          ((down-page)
           (scroll-down-page-callback pane (gadget-client pane) (gadget-id pane))))))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y)
      (transform-position
                (vertical-gadget-orientation-transformation pane)
                (pointer-event-x event) (pointer-event-y event))
    (with-slots (armed dragging drag-delta repeating) pane
      (incf repeating)
      (let ((thumb (gadget-thumb-region pane)))
        (cond
          ((region-contains-position-p thumb x y)
           ; Thumb
           (setf dragging :inside
                 armed    t
                 drag-delta (- y (bounding-rectangle-min-y thumb))))
          ((region-contains-position-p (gadget-up-region pane) x y)
	   #+NIL
           (clim-internals::schedule-timer-event pane 'up-line 0.1)
           ; Up Arrow
           (scroll-up-line-callback pane (gadget-client pane) (gadget-id pane))
           (setf (slot-value pane 'armed) :up)
           (dispatch-repaint pane +everywhere+))
          ((region-contains-position-p (gadget-down-region pane) x y)
	   #+NIL
           (clim-internals::schedule-timer-event pane 'down-line 0.1)
           ; Down Arrow
           (scroll-down-line-callback pane (gadget-client pane) (gadget-id pane))
           (setf (slot-value pane 'armed) :down)
           (dispatch-repaint pane +everywhere+))
          ((region-contains-position-p (gadget-bed-region pane) x y)
           ; Bed
           (cond
             ((< y (bounding-rectangle-min-y thumb))
	      #+NIL
              (clim-internals::schedule-timer-event pane 'up-page 0.1)
              (scroll-up-page-callback pane (gadget-client pane) (gadget-id pane)))
             (t
	      #+NIL
              (clim-internals::schedule-timer-event pane 'down-page 0.1)
              (scroll-down-page-callback pane (gadget-client pane) (gadget-id pane)))))
          (t
           ; Nowhere (!)
           nil))))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-motion-event))
  (with-slots (dragging drag-delta) pane
    (multiple-value-bind (x y)
        (transform-position (vertical-gadget-orientation-transformation pane)
                            (pointer-event-x event) (pointer-event-y event))
      (declare (ignore x))
      (when dragging
        (let* ((y-new-thumb-top (- y drag-delta))
               (ts (gadget-thumb-size pane))
               (value (min (gadget-max-value pane)
                           (max (gadget-min-value pane)
                                (translate-range-value y-new-thumb-top
                                   (bounding-rectangle-min-y (gadget-bed-region pane))
                                   (bounding-rectangle-max-y (gadget-bed-region pane))
                                   (gadget-min-value pane)
                                   (+ (gadget-max-value pane) ts))))))
	  (setf (gadget-value pane :invoke-callback nil) value)
	  (drag-callback pane (gadget-client pane) (gadget-id pane) value)
	  (dispatch-repaint pane (sheet-region pane)))))))

;;; Repaint

(defmethod handle-repaint ((pane pixie-scroll-bar-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let* ((tr (vertical-gadget-orientation-transformation pane))
           (transformed-sheet (transform-region tr (sheet-region pane))))
      (with-bounding-rectangle* (minx miny maxx maxy)
          transformed-sheet
        ; draw the bed?
        (with-drawing-options (pane :transformation tr)
          (let ((gadget-thumb-region (gadget-thumb-region pane))
                (gadget-down-region  (gadget-down-region pane))
                (gadget-up-region    (gadget-up-region pane))
                (gadget-bed-region   (gadget-bed-region pane)))
            (with-drawing-options (pane :clipping-region (region-difference
                                                           gadget-bed-region
                                                           gadget-thumb-region))
              (multiple-value-bind (x1 y1 x2 y2) (values (+ minx 1) (+ miny 1)
                                                         (- maxx 1) (- maxy 1))
                (draw-rectangle* pane x1 y1 x2 y2 :ink (pane-background pane))))
            (draw-bordered-polygon pane
                                   (polygon-points (make-rectangle* minx miny maxx maxy))
                                   :style :inset
                                   :border-width 1)
            ;; draw up arrow
            (with-bounding-rectangle* (x1 y1 x2 y2) gadget-up-region
	       (if (eq (slot-value pane 'armed) :up)
                   (draw-down-box pane x1 y1 x2 y2 +gray83+)
                   (draw-up-box   pane x1 y1 x2 y2 +gray83+))
                ;; draw decoration in the region
	       (multiple-value-bind (pattern fudge-x fudge-y)
		   (if (eq (gadget-orientation pane) :vertical)
		       (values +pixie-up-arrow+ -1 1)
		       (values +pixie-left-arrow+ -1 1))
		 (draw-pattern* pane pattern
				(+ fudge-x (floor (- (+ x1 x2) (pattern-width  pattern)) 2))
				(+ fudge-y (floor (- (+ y1 y2) (pattern-height pattern)) 2)))))
  
            ;; draw down arrow
            (with-bounding-rectangle* (x1 y1 x2 y2) gadget-down-region
               (if (eq (slot-value pane 'armed) :down)
                   (draw-down-box pane x1 y1 x2 y2 +gray83+)
                   (draw-up-box   pane x1 y1 x2 y2 +gray83+))
                ;; draw decoration in the region
	       (multiple-value-bind (pattern fudge-x fudge-y)
		   (if (eq (gadget-orientation pane) :vertical)
		       (values +pixie-down-arrow+  -1 1)
		       (values +pixie-right-arrow+ -1 2))
		 (draw-pattern* pane pattern
				(+ fudge-x (floor (- (+ x1 x2) (pattern-width  pattern)) 2))
				(+ fudge-y (floor (- (+ y1 y2) (pattern-height pattern)) 2)))))
  
            ;; draw thumb
            (with-bounding-rectangle* (x1 y1 x2 y2) gadget-thumb-region
               (draw-up-box pane x1 y1 x2 y2 (effective-gadget-foreground pane))
               ;; no thumb decoration
               )))))))
  
; Menus

(defclass pixie-menu-bar-pane (pixie-gadget menu-bar) ())

(define-pixie-gadget menu-bar pixie-menu-bar-pane :enabled t)

(defmethod handle-repaint ((pane pixie-menu-bar-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let* ((region (sheet-region pane))
           (frame (polygon-points (bounding-rectangle region))))
      #+NIL      (draw-polygon pane frame :ink +Blue+ :filled t)
      (draw-bordered-polygon pane frame :style :outset :border-width 1))))

(define-pixie-gadget menu-button pixie-menu-button-pane)

(defclass pixie-menu-button-pane (pixie-gadget menu-button-pane)
  ((left-margin :reader left-margin)
   (right-margin :reader right-margin))                
  (:default-initargs
    :align-x :left
    :align-y :center))

(defparameter *pixie-menu-button-left-margin*  26)
(defparameter *pixie-menu-button-right-margin* 26)
(defparameter *pixie-menubar-item-left-margin* 8)
(defparameter *pixie-menubar-item-right-margin* 8)
(defparameter *pixie-menubar-item-spacing* 0)

(defmethod initialize-instance :after ((pane pixie-menu-button-pane)
                                       &rest args &key vertical &allow-other-keys)
  (declare (ignore args))
  (with-slots (left-margin right-margin) pane
    (setf (values left-margin right-margin)
          (if (or (typep (slot-value pane 'client) 'menu-bar)
                  (not vertical))
              (values *pixie-menubar-item-left-margin* *pixie-menubar-item-right-margin*)
              (values *pixie-menu-button-left-margin* *pixie-menu-button-right-margin*)))))

;; What even uses this? All the subclasses have their own handle-repaint methods!
#+NIL
(defmethod handle-repaint ((pane pixie-menu-button-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let* ((region (sheet-region pane))
           (frame (polygon-points (bounding-rectangle region))))
      (draw-polygon pane frame :filled t :ink (effective-gadget-foreground pane))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
        (cond
          ((slot-value pane 'armed)
           (let ((inset-frame (polygon-points
                                (make-rectangle* (+ x1 2) (+ y1 2) (- x2 2) (- y2 2)))))
             (draw-polygon pane inset-frame :filled t :ink (effective-gadget-foreground pane))
             (draw-bordered-polygon pane inset-frame
                           :style :outset
                           :border-width 1)))
          (t
           (draw-polygon pane frame :filled t :ink (effective-gadget-foreground pane))))
        (draw-label* pane (+ x1 (left-margin pane)) y1 (- x2 (right-margin pane)) y2 :ink +red+ #+NIL (pane-inking-color pane))))))

(defmethod compose-space ((gadget pixie-menu-button-pane) &key width height)
  (declare (ignore width height))
  (space-requirement+* (compose-label-space gadget
					    :wider (+ (left-margin gadget)
                                                      (right-margin gadget))
					    :higher (+ 6 (* 2 *3d-border-thickness*)))
                       :min-width  0
                       :width      0
                       :max-width  +fill+
                       :min-height 0
                       :height     0
                       :max-height 0))

(defclass pixie-menu-button-leaf-pane (pixie-menu-button-pane menu-button-leaf-pane) ())
(define-pixie-gadget menu-button-leaf-pane pixie-menu-button-leaf-pane)

(defmethod handle-repaint ((pane pixie-menu-button-leaf-pane) region)
  (declare (ignore region))
  (with-slots (armed) pane
    ; XXX only do this when the gadget is realized.
    (when (sheet-mirror pane)
      (with-special-choices (pane)
        (with-slots (label) pane
          (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
            (let ((w (- x2 x1))
                  (h (- y2 y1)))
              (draw-rectangle* pane -1 -1 x2 y2
                               :ink (if armed +gray93+ +gray83+)
                               :filled t)
              (when armed
                (draw-edges-lines* pane +white+ 0 0 +black+ (1- w) (1- h)))
	      (let ((x1 (+ x1 (left-margin pane)))
		    (x2 (- x2 (right-margin pane))))
		(if (gadget-active-p pane)
		    (draw-label* pane x1 y1 x2 y2 :ink +black+)
		    (draw-engraved-label* pane x1 y1 x2 y2))))))))))

(defclass pixie-menu-button-submenu-pane (pixie-menu-button-pane menu-button-submenu-pane) ())

(define-pixie-gadget menu-button-submenu-pane pixie-menu-button-submenu-pane)
(define-pixie-gadget menu-button-vertical-submenu-pane pixie-menu-button-submenu-pane)


(defmethod compose-space ((gadget pixie-menu-button-submenu-pane) &key width height)
  (declare (ignore width height))
  (if (typep (slot-value gadget 'client) 'menu-bar) ; XXX
      (compose-label-space gadget
			   :wider (+ (left-margin gadget)
				     (right-margin gadget))
			   :higher 10)
      (call-next-method)))

(defmethod handle-repaint ((pane pixie-menu-button-submenu-pane) region)
  (declare (ignore region))
  (with-slots (submenu-frame client) pane
    (when (sheet-mirror pane) ;XXX only do this when the gadget is realized.
      (with-special-choices (pane)
        (with-slots (label) pane
          (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
            (let ((w (- x2 x1))
                  (h (- y2 y1)))
              (draw-rectangle* pane -1 -1 x2 y2
                               :ink (if submenu-frame +gray93+ +gray83+)
                               :filled t)
              (when submenu-frame
                (draw-edges-lines* pane +white+ 0 0 +black+ (1- w) (1- h)))
	      
	      (if (typep client 'menu-button)
		  (let ((pattern +pixie-right-arrow+))
		    (draw-label* pane (+ x1 (left-margin pane)) y1
				 (- x2 (right-margin pane)) y2 :ink +black+)  
		    (draw-pattern* pane pattern (- x2 10) (+ y1 (floor (- h (pattern-height pattern)) 2))))
		  (draw-label* pane
			       (+ x1 (left-margin pane))  y1
			       (- x2 (right-margin pane)) y2
			       :ink +black+)))))))))



; Image pane

; rebuild this with a pixmap repository for general re-use of pixmaps
; within a port/visual combination.

; This is just test/proof-of-concept code :]

#+NIL
(defclass pixie-image-pane (pixie-gadget basic-gadget) (
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
#+NIL
(defmethod initialize-instance :after ((pane pixie-image-pane) &rest args)
  (declare (ignore args))
  (with-slots (image-pathname image-image image-width image-height) pane
    (let* ((data (image:read-image-file image-pathname))
           (image (image:make-truecolor-image data 255)))
      (destructuring-bind (width height) (array-dimensions data)
        (setf image-width  width
              image-height height
              image-image  image))))
  (with-slots (image-mask-pathname image-stencil) pane
    (when image-mask-pathname
      (let* ((data (image:read-image-file image-mask-pathname)))
        (setf image-stencil (make-stencil data))))))

#+NIL
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

#+NIL
(defmethod compose-space ((pane pixie-image-pane) &key width height)
  (declare (ignore width height))
  (with-slots (image-width image-height) pane
    (let ((w image-width)
          (h image-height))
      (make-space-requirement :width     w :height     h
                              :min-width w :min-height h
                              :max-width w :max-height h))))

; Toggle Button (for checkboxes and radio-buttons)

(defclass pixie-toggle-button-pane (pixie-gadget toggle-button-pane) ())

(define-pixie-gadget toggle-button pixie-toggle-button-pane)

(defmethod draw-toggle-button-indicator ((pane pixie-toggle-button-pane) (type (eql :one-of)) value x1 y1 x2 y2)
  (multiple-value-bind (cx cy) (values (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
    (let ((radius (/ (- y2 y1) 2)))
      (draw-circle* pane cx cy radius
                     :start-angle (* 1/4 pi)
                     :end-angle   (* 5/4 pi)
                     :ink *3d-dark-color*)
      (draw-circle* pane cx cy (- radius 1)
                     :start-angle (* 1/4 pi)
                     :end-angle   (* 5/4 pi)
                     :ink (pane-inking-color pane))
      (draw-circle* pane cx cy radius
                     :start-angle (* 5/4 pi)
                     :end-angle   (* 1/4 pi)
                     :ink *3d-light-color*)
      (draw-circle* pane cx cy (- radius 1)
                     :start-angle (* 5/4 pi)
                     :end-angle   (* 1/4 pi)
                     :ink (effective-gadget-foreground pane))
      (draw-circle* pane cx cy (max 1 (- radius 2))
                     :ink (pane-paper-color pane))
      (when value
        (draw-circle* pane cx cy (max 1 (- radius 4))
                       :ink (pane-inking-color pane))))))

(defmethod draw-toggle-button-indicator ((pane pixie-toggle-button-pane) (type (eql :some-of)) value x1 y1 x2 y2)
  (draw-rectangle* pane x1 y1 x2 y2 :ink (pane-paper-color pane))
  (draw-bordered-rectangle* pane x1 y1 x2 y2 :style :inset)
  (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 1) (+ y1 1)
                                             (- x2 2) (- y2 2))
    (draw-line* pane x1 y2 x2 y2 :ink (effective-gadget-foreground pane))
    (draw-line* pane x2 y1 x2 y2 :ink (effective-gadget-foreground pane))
    (draw-line* pane x1 y1 x1 (+ y2 1) :ink (pane-inking-color pane))
    (draw-line* pane x1 y1 (+ x2 1) y1 :ink (pane-inking-color pane)))
  (when value
    (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 3) (+ y1 3)
                                               (- x2 3) (- y2 3))
      (draw-line* pane x1 y1 x2 y2 :ink (pane-inking-color pane) :line-thickness 2)
      (draw-line* pane x2 y1 x1 y2 :ink (pane-inking-color pane) :line-thickness 2))))

(defmethod handle-repaint ((pane pixie-toggle-button-pane) region)
  (declare (ignore region))
  (when (sheet-grafted-p pane)
    (with-special-choices (pane)
      (with-slots (armed) pane
        (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
          (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
          (let* ((as (text-style-ascent (pane-text-style pane) pane))
                 (ds (text-style-descent (pane-text-style pane) pane)) )
            (multiple-value-bind (tx1 ty1 tx2 ty2)
                (values (+ x1 (pane-x-spacing pane))
                        (- (/ (+ y1 y2) 2) (/ (+ as ds) 2))
                        (+ x1 (pane-x-spacing pane) (+ as ds))
                        (+ (/ (+ y1 y2) 2) (/ (+ as ds) 2)))
              (draw-toggle-button-indicator pane (toggle-button-indicator-type pane) (gadget-value pane)
                                            tx1 ty1 tx2 ty2)
              (draw-label* pane (+ tx2 3 (pane-x-spacing pane)) y1 x2 y2
                           :ink (pane-inking-color pane)))))))))

; Push Button

; why does this inherit from slider-pane?
(defclass pixie-push-button-pane  (pixie-gadget push-button-pane slider-pane) (
  (dragging
    :initform nil)))

(define-pixie-gadget push-button pixie-push-button-pane)

(defmethod compose-space ((gadget pixie-push-button-pane) &key width height)
  (declare (ignore width height))
  ;; Why does a button have spacing options, anyway?
  (space-requirement+* (space-requirement+* (compose-label-space gadget)
                                            :min-width (* 2 (pane-x-spacing gadget))
                                            :width (* 2 (pane-x-spacing gadget))
                                            :max-width (* 2 (pane-x-spacing gadget))
                                            :min-height (* 2 (pane-y-spacing gadget))
                                            :height (* 2 (pane-y-spacing gadget))
                                            :max-height (* 2 (pane-y-spacing gadget)))
                       :min-width  (* 8 *3d-border-thickness*)
                       :width      (* 8 *3d-border-thickness*)
                       :max-width  (* 8 *3d-border-thickness*)
                       :min-height (* 4 *3d-border-thickness*)
                       :height     (* 4 *3d-border-thickness*)
                       :max-height (* 4 *3d-border-thickness*)))

; factor out the dragging code into a mixin
(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-enter-event))
  (with-slots (armed dragging) pane
    (cond
     ((not armed)
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
     (dragging
      (setf dragging :inside))))
  (dispatch-repaint pane +everywhere+))

(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-exit-event))
  (with-slots (armed dragging) pane
    (cond
     (dragging
      (setf dragging :outside))
     (armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))
  (dispatch-repaint pane +everywhere+))

(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-button-press-event))
  (with-slots (pressedp dragging) pane
    (setf pressedp t
          dragging :inside)
    (dispatch-repaint pane +everywhere+)))

(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-button-release-event))
  (with-slots (armed pressedp dragging) pane
    (setf pressedp nil)
    (when (and armed (eq dragging :inside))
      (activate-callback pane (gadget-client pane) (gadget-id pane))
      (setf pressedp nil)
      (dispatch-repaint pane +everywhere+))))

(defmethod handle-repaint ((pane pixie-push-button-pane) region)
  (declare (ignore region))
  (with-slots (armed dragging pressedp) pane
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (let ((x1 (+ x1 1))
            (y1 (+ y1 1))
            (x2 (- x2 1))
            (y2 (- y2 1)))
        (let ((x2 (- x2 1))		; Removing this magic weirdness slightly uglifies the 
              (y2 (- y2 1)))		; scroll bar. Not sure why, but FIXME.
          (cond
           ((or (not pressedp)
                (eq dragging :outside))
            (draw-up-box pane x1 y1 x2 y2 (effective-gadget-foreground pane)))
           (pressedp
            (draw-down-box pane x1 y1 x2 y2 (effective-gadget-foreground pane)))))))))

(defclass pixie-submenu-border-pane (submenu-border)
  ()
  (:default-initargs :border-width 2))

(define-pixie-gadget submenu-border pixie-submenu-border-pane)

(defmethod handle-repaint ((pane pixie-submenu-border-pane) region)
  (declare (ignore region))
  (with-slots (border-width) pane
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (draw-rectangle* pane x1 y1 x2 y2 :filled nil :ink +black+)
      ;; Why, having incremented the coordinates, and despite setting
      ;; the border-width to 2, do I now get a single pixel border ?
      ;; It's fine, that's the result I want, but an explanation is in order.
      (draw-bordered-rectangle* pane (1+ x1) (1+ y1) (1- x2) (1- y2)
				:style :outset
				:border-width border-width))))

; Text Area

(defclass pixie-text-field-pane (text-field-pane) ())

;; Why does pixie need its own text area subclass? Leave it disabled for now.
(define-pixie-gadget text-field-pane pixie-text-field-pane :enabled nil)

(defmethod initialize-instance :after ((pane pixie-text-field-pane) &rest rest)
  (unless (getf rest :normal)
    (setf (slot-value pane 'current-color) +white+
	  (slot-value pane 'normal)        +white+)))

(defmethod note-sheet-grafted :after ((pane pixie-text-field-pane))
  (multiple-value-bind (cx cy) (stream-cursor-position pane)
    (setf (cursor-visibility (stream-text-cursor pane)) nil)
    (setf (area pane) (make-instance 'goatee:simple-screen-area
                                            :area-stream pane
                                            :x-position cx
                                            :y-position cy
                                            :initial-contents (slot-value pane 'value)))
    (stream-add-output-record pane (area pane))))

(defmethod handle-repaint ((pane pixie-text-field-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (with-sheet-medium (medium pane)
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
        (display-gadget-background pane (gadget-current-color pane) 0 0 (- x2 x1) (- y2 y1))
        (goatee::redisplay-all (area pane))))))

(defmethod handle-event ((gadget pixie-text-field-pane) (event key-press-event))
  (let ((gesture (convert-to-gesture event))
	(*activation-gestures* *standard-activation-gestures*))
    (when (activation-gesture-p gesture)
      (activate-callback gadget (gadget-client gadget) (gadget-id gadget))
      (return-from handle-event t))
    (goatee:execute-gesture-command gesture
				    (area gadget)
				    goatee::*simple-area-gesture-table*)
    (let ((new-value (goatee::buffer-string (goatee::buffer (area gadget)))))
      (unless (string= (gadget-value gadget) new-value)
	(setf (slot-value gadget 'value) new-value)
	(value-changed-callback gadget 
				(gadget-client gadget) 
				(gadget-id gadget)
				new-value)))))

(defmethod (setf gadget-value) :after (new-value (gadget pixie-text-field-pane)
				       &key invoke-callback)
  (declare (ignore invoke-callback))
  (let* ((area (area gadget))
	 (buffer (goatee::buffer area))
	 (start (goatee::buffer-start buffer))
	 (end (goatee::buffer-end buffer)))
    (goatee::clear-buffer buffer)
    (goatee::insert buffer new-value :position start)
    (goatee::redisplay-area area)))

(defmethod compose-space ((pane pixie-text-field-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let ((as (text-style-ascent (medium-text-style medium) medium))
          (ds (text-style-descent (medium-text-style medium) medium))
          (w  (text-size medium (gadget-value pane))))
      (let ((width w)
            (height (+ as ds)))
        (make-space-requirement :width width :height height
                                :max-width width :max-height height
                                :min-width width :min-height height)))))

;;;; Pixie tab-layout. Reuses implementation of the generic tab-layout-pane.

(define-pixie-gadget clim-tab-layout:tab-layout pixie-tab-layout-pane)
(define-pixie-gadget clim-tab-layout::tab-bar-pane pixie-tab-bar-pane)

(defclass pixie-tab-bar-view (gadget-view)
  ((selected :initform nil
	     :initarg :selected
	     :reader pixie-tab-view-selected-p)))

(defparameter +pixie-tab-bar-view+
  (make-instance 'pixie-tab-bar-view :selected nil))

(defparameter +pixie-selected-tab-bar-view+
  (make-instance 'pixie-tab-bar-view :selected t))



(defclass pixie-tab-layout-pane (clim-tab-layout:tab-layout-pane)
  ()
  (:default-initargs
    :header-display-function 'pixie-display-tab-header))

(defclass pixie-tab-bar-pane (application-pane pixie-gadget)
  ()
  (:default-initargs
    :default-view +pixie-tab-bar-view+
    :background +gray83+
    :text-style (make-text-style :sans-serif 
                                 :roman 
                                 (if (find-package :mcclim-truetype)
                                     :normal
                                     :small))))

(defmethod compose-space ((pane pixie-tab-bar-pane) &key width height)
  (declare (ignore width height))
  (let ((h (+ 6				; padding on the top
	      6				; padding on the bottom
	      (text-style-ascent (pane-text-style pane) pane)
	      (text-style-descent (pane-text-style pane) pane))))
    (make-space-requirement :min-height h :height h :max-height h)))

(defun draw-pixie-tab-bar-bottom (pane)
  (let ((y0 (bounding-rectangle-min-y (sheet-region pane)))
	(y1 (bounding-rectangle-max-y (sheet-region pane))))
    (draw-line* pane 0 (- y1 6) +fill+ (- y1 6) :ink *3d-light-color*)
    (draw-line* pane 0 (- y1 1) +fill+ (- y1 1) :ink *3d-dark-color*)
    #+NIL (draw-line* pane 0 (1- y1) x1 (1- y1)   :ink +gray30+)))

(defmethod draw-output-border-over
    ((shape (eql 'pixie-tab-bar-border)) stream record &key &allow-other-keys)
  (declare (ignore shape stream record)))

(defmethod draw-output-border-under
    ((shape (eql 'pixie-tab-bar-border)) stream record
     &key background enabled &allow-other-keys)
  (with-border-edges (stream record)
    (declare (ignore bottom))
    (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region stream)
      (declare (ignore x0 x1 y0))
      (let ((bottom (- y1 7))
	    (left   (- left 4 (if enabled 2 0)))
	    (right  (+ right 4 (if enabled 2 0)))
	    (top    (- top 2 #+NIL (if enabled 2 0))))
	(draw-rectangle* stream left top right (+ bottom (if enabled 2 1))
			 :filled t :ink background)
	(draw-line* stream (1+ left) (1- top) (- right 1) (1- top) :ink +white+)
	(draw-point* stream left top :ink +white+)
	(draw-line* stream (1- left) bottom (1- left) (1+ top) :ink +white+)
	(draw-line* stream right bottom right top :ink +gray66+)
	(draw-point* stream right top :ink +gray40+)
	(draw-line* stream (1+ right) bottom (1+ right) (1+ top) :ink +gray40+)))))

(define-default-highlighting-method 'pixie-tab-bar-border)

(define-presentation-method present
    (tab-page (type clim-tab-layout:tab-page) stream (view pixie-tab-bar-view) &key)
  (stream-increment-cursor-position stream 5 0)
  (surrounding-output-with-border (stream :shape 'pixie-tab-bar-border
					  :enabled (pixie-tab-view-selected-p view)
					  :highlight-background +gray94+
					  :background +gray83+
					  :move-cursor nil)
    (apply #'invoke-with-drawing-options stream
	   (lambda (rest)
	     (declare (ignore rest))
	     (write-string (clim-tab-layout:tab-page-title tab-page) stream))
	   (clim-tab-layout:tab-page-drawing-options tab-page)))
  (stream-increment-cursor-position stream 6 0))

(defun pixie-display-tab-header (tab-layout pane)
  (draw-pixie-tab-bar-bottom pane)
  (setf (stream-cursor-position pane)
	(values 3 (- (bounding-rectangle-height (sheet-region pane))
		     7
		     (text-style-descent (pane-text-style pane) pane)
		     (text-style-ascent (pane-text-style pane) pane))))  
  (let ((enabled-page-drawers nil))
    (mapc (lambda (page)
	    ;; This gets a little silly, but the tabs are laid out simply by
	    ;; letting the cursor move from left to right. In order to make
	    ;; the selected tab overlap, we can't draw it until after the other
	    ;; tabs. We then draw it slightly larger in each direcetion. But the
	    ;; cursor has to have moved as though it were smaller (so that it
	    ;; overlaps its neighbors), so draw it initially, note its position,
	    ;; and redraw a larger version once everything is done.	    
	    (let ((enabled (sheet-enabled-p (clim-tab-layout:tab-page-pane page))))
	      (when enabled
		(multiple-value-bind (x y) (stream-cursor-position pane)
		  (push (lambda ()
			  (setf (stream-cursor-position pane)
				(values x (- y 2)))
			  (with-output-as-presentation
			      (pane (clim-tab-layout:tab-page-pane page)
				    (clim-tab-layout:tab-page-presentation-type page))
			    (present page 'clim-tab-layout:tab-page :stream pane
				     :view +pixie-selected-tab-bar-view+)))
			enabled-page-drawers)))
	      (let ((record 
		     (with-output-as-presentation
			 (pane (clim-tab-layout:tab-page-pane page)
			       (clim-tab-layout:tab-page-presentation-type page))
		       (present page 'clim-tab-layout:tab-page :stream pane))))
		;; Because piling the presentations on top of each other confuses
		;; CLIM as to which should be highlighted, erase the smaller one.
		;; The cursor has already been moved, so we don't need it.
		(when enabled
		  (delete-output-record record (output-record-parent record))))))
	  (clim-tab-layout:tab-layout-pages tab-layout))
    (mapcar #'funcall enabled-page-drawers)))
