(defpackage #:clim-demo.sheet-geometry
  (:use #:clim-lisp #:clim)
  (:export #:frame))
(in-package #:clim-demo.sheet-geometry)

(defparameter *redisplay-counter* 0)

(defparameter *sheet->stream-transformation*
  (compose-transformation-with-translation
   (make-scaling-transformation .7 .7) 100 100))

(defun <-sheet (object)
  (compose-transformations *sheet->stream-transformation*
                           (sheet-transformation object)))

(defclass tracked-sheet-mixin () ())

(defclass my-pane (basic-pane) ())
(defclass my-bbrd (bboard-pane) ())

(defmethod compose-space ((pane my-pane) &key width height)
  (multiple-value-bind (w h)
      (bounding-rectangle-size
       (transform-region (sheet-device-transformation pane)
                         (sheet-region pane)))
    (if (and width height)
        (make-space-requirement :width width :height height)
        (make-space-requirement :width w :height h))))

(defmethod allocate-space ((pane my-pane) width height)
  ;; Intentionally ignore the space allocation request.
  )

(define-presentation-type translate-handler ())
(define-presentation-type scale-handler     ())
(define-presentation-type rotate-handler    ())

(define-presentation-type move-handler   ())
(define-presentation-type resize-handler ())
(define-presentation-type reset-handler  ())

(defun draw-handler-label (stream text x y align-x align-y &optional transformp)
  (if (null transformp)
      (surrounding-output-with-border
          (stream :shape :rectangle :background +light-yellow+)
        (draw-text* stream text x y :align-x align-x :align-y align-y))
      (surrounding-output-with-border  ;; FIXME :shape :polygon
          (stream :shape :rectangle :background +light-green+)
        (draw-text* stream text x y :align-x align-x :align-y align-y
                                    :transform-glyphs t :text-size :larger))))

(define-presentation-method present
    (object (type my-pane) stream view &key)
  (declare (ignore type view))
  (let* ((region (sheet-region object))
         (transformation (sheet-transformation object))
         (region* (transform-region transformation region)))
    (with-bounding-rectangle* (px1 py1 px2 py2) region*
      (with-bounding-rectangle* (x1 y1 x2 y2) region
        (with-drawing-options (stream :transformation transformation)
          (draw-rectangle* stream x1 y1 x2 y2 :ink +light-blue+ :filled t)
          (draw-design stream region :ink (pane-background object) :filled t))
        (draw-rectangle* stream px1 py1 px2 py2 :ink +red+ :filled nil)
        (with-drawing-options (stream :transformation transformation)
          (draw-arrow* stream -100 0 300 0 :ink +dark-green+ :line-thickness 3)
          (draw-arrow* stream 0 -100 0 300 :ink +dark-green+ :line-thickness 3)
          (with-output-as-presentation (stream object 'translate-handler)
            (draw-handler-label stream "Translate" x1 y1 :left :top t))
          (with-output-as-presentation (stream object 'rotate-handler)
            (draw-handler-label stream "Rotate" x2 0 :right :top t))
          (with-output-as-presentation (stream object 'scale-handler)
            (draw-handler-label stream "Scale" x2 y2 :right :bottom t))))
      (with-output-as-presentation (stream object 'move-handler)
        (draw-handler-label stream "Move" px1 py1 :center :center))
      (with-output-as-presentation (stream object 'resize-handler)
        (draw-handler-label stream "Resize" px2 py2 :center :center))
      (with-output-as-presentation (stream object 'reset-handler)
        (draw-handler-label stream "Reset" px1 py2 :right :top)))))

(define-gesture-name :zoom-in :pointer-scroll (:wheel-up :control))
(define-gesture-name :zoom-out :pointer-scroll (:wheel-down :control))
(define-gesture-name :rotate-left :pointer-scroll (:wheel-up))
(define-gesture-name :rotate-right :pointer-scroll (:wheel-down))
(define-gesture-name :select* :pointer-button-press (:right))

(define-command-table sheet-geometry-table)

;;; This command is used to redisplay the sheet manipulation pane. DND
;;; translators perform side effects in their corresponding feedback function.
(define-command (cmd-nothing :command-table sheet-geometry-table) ())

(defun pick-shape ()
  (case (menu-choose
         '(circle square triangle)
         :printer
         #'(lambda (item stream)
             (clim:with-drawing-options (stream :ink clim:+dark-red+)
               (case item
                 (circle
                  (clim:draw-circle* stream 0 0 10))
                 (square
                  (clim:draw-rectangle* stream 0 0 20 20))
                 (triangle
                  (clim:draw-polygon* stream '(10 8 0 -10 -10 8)))))))
    (circle (make-ellipse* 100 100 100 0 0 100))
    (triangle (make-polygon* '(100 0 200 200 0 200 100 0)))
    (square (make-rectangle* 0 0 200 200))))

(define-command (cmd-reset-sheet :command-table sheet-geometry-table)
    ((object 'my-pane))
  (let ((region (pick-shape)))
    (when (null region)
      (return-from cmd-reset-sheet))
    (setf (sheet-transformation object)
          (make-translation-transformation 50 50))
    (setf (sheet-region object) region)))

(defmacro define-transformation-command (name args transformation)
  (let ((cmd-sheet (alexandria:symbolicate name '-sheet))
        (cmd-region (alexandria:symbolicate name '-region)))
    `(progn
       (define-command
           (,cmd-sheet :command-table sheet-geometry-table)
           ,args
         (setf (sheet-transformation object)
               (compose-transformations
                (sheet-transformation object)
                ,transformation)))
       (define-command
           (,cmd-region :command-table sheet-geometry-table)
           ,args
         (setf (sheet-region object)
               (transform-region ,transformation
                                 (sheet-region object)))))))

(define-transformation-command cmd-translate
    ((object 'my-pane)
     (x 'coordinate)
     (y 'coordinate)
     (origin-x 'coordinate)
     (origin-y 'coordinate))
  (make-translation-transformation (- x origin-x) (- y origin-y)))

(define-transformation-command cmd-scale
    ((object 'my-pane)
     (scale-x 'real)
     (scale-y 'real)
     (origin-x 'coordinate)
     (origin-y 'coordinate))
  (make-scaling-transformation* scale-x scale-y origin-x origin-y))

(define-transformation-command cmd-rotate
    ((object 'my-pane)
     (angle 'real)
     (origin-x 'coordinate)
     (origin-y 'coordinate))
  (make-rotation-transformation* angle origin-x origin-y))

(define-command (cmd-move-sheet :command-table sheet-geometry-table)
    ((object 'my-pane)
     (x 'real)
     (y 'real))
  (move-sheet object x y))

(define-command (cmd-resize-sheet :command-table sheet-geometry-table)
    ((object 'my-pane)
     (w 'real)
     (h 'real))
  (resize-sheet object w h))

(defmacro define-dnd ((name handler gesture pdoc) args &body body)
  (destructuring-bind
      (frame from-presentation stream x0 y0 cx cy feedback) args
    (declare (ignore from-presentation x0 y0 cx cy))
    (multiple-value-bind (forms decls)
        (alexandria:parse-body body)
      `(progn
         (defun ,name ,args
           ,@decls
           (declare (ignorable ,@args))
           (when (eq ,feedback :highlight)
             ,@forms
             (window-clear ,stream)
             (show-hierarchy ,frame ,stream)))
         (define-drag-and-drop-translator ,(alexandria:symbolicate name '-dnd)
             (,handler command blank-area sheet-geometry-table
                       :feedback ,name
                       :menu nil
                       :gesture ,gesture
                       :pointer-documentation ,pdoc)
             (object)
           `(cmd-nothing))))))

(defmacro define-dnd-pair ((name handler cmd) args &body body)
  (let ((name-1 (alexandria:symbolicate name '-feedback-1))
        (name-2 (alexandria:symbolicate name '-feedback-2))
        (cmd-1  (alexandria:symbolicate 'cmd- name '-sheet))
        (cmd-2  (alexandria:symbolicate 'cmd- name '-region)))
    `(progn
       (define-dnd (,name-1 ,handler :select ,(format nil "~a sheet" name)) ,args
         (flet ((,cmd (&rest args) (apply ',cmd-1 args)))
           ,@body))
       (define-dnd (,name-2 ,handler :select* ,(format nil "~a region" name)) ,args
         (flet ((,cmd (&rest args) (apply ',cmd-2 args)))
           ,@body)))))

(define-dnd-pair (translate translate-handler cmd-translate)
    (frame from-presentation stream x0 y0 cx cy feedback)
  (let* ((object (presentation-object from-presentation))
         (<-sheet (<-sheet object))
         (->sheet (invert-transformation <-sheet)))
    (multiple-value-bind (cx cy) (transform-position ->sheet cx cy)
      (multiple-value-bind (x1 y1) (bounding-rectangle-position object)
        (cmd-translate object cx cy x1 y1)))))

(define-dnd-pair (scale scale-handler cmd-scale)
    (frame from-presentation stream x0 y0 cx cy feedback)
  (let* ((object (presentation-object from-presentation))
         (<-sheet (<-sheet object))
         (->sheet (invert-transformation <-sheet))
         (x2 (bounding-rectangle-max-x object))
         (y2 (bounding-rectangle-max-y object)))
    (multiple-value-bind (cx cy) (transform-position ->sheet cx cy)
      (let* ((sx (/ cx x2))
             (sy (/ cy y2)))
        (cmd-scale object sx sy 0 0)))))

(define-dnd (rotate-feedback-1 rotate-handler :select "rotate sheet")
    (frame from-presentation stream x0 y0 cx cy feedback)
  (let* ((object (presentation-object from-presentation))
         (<-sheet (<-sheet object)))
    (multiple-value-bind (cx cy) (untransform-position <-sheet cx cy)
      (let ((angle (clim-internals::atan* cx cy)))
        (cmd-rotate-sheet object angle 0 0)))))

(let ((last-angle 0))
  (define-dnd (rotate-feedback-2 rotate-handler :select* "rotate region")
      (frame from-presentation stream x0 y0 cx cy feedback)
    (let* ((object (presentation-object from-presentation))
           (<-sheet (<-sheet object))
           (->sheet (invert-transformation <-sheet)))
      (multiple-value-bind (cx cy) (transform-position ->sheet cx cy)
        (let ((angle (clim-internals::atan* cx cy)))
          (cmd-rotate-region object (- angle last-angle) 0 0)
          (setf last-angle angle))))))

(define-dnd (move-feedback move-handler :select "Move sheet")
    (frame from-presentation stream x0 y0 cx cy feedback)
  (declare (ignore x0 y0))
  (let* ((object (presentation-object from-presentation))
         (<-sheet *sheet->stream-transformation*)
         (->sheet (invert-transformation <-sheet)))
    (multiple-value-bind (cx cy) (transform-position ->sheet cx cy)
      (cmd-move-sheet object cx cy))))

(define-dnd (resize-feedback resize-handler :select "Resize sheet")
    (frame from-presentation stream x0 y0 cx cy feedback)
  (declare (ignore x0 y0))
  (let* ((object (presentation-object from-presentation))
         (transf (sheet-transformation object))
         (region (sheet-region object))
         (region* (transform-region transf region))
         (<-sheet *sheet->stream-transformation*)
         (->sheet (invert-transformation <-sheet)))
    (multiple-value-bind (x1 y1) (bounding-rectangle-position region*)
      (multiple-value-bind (cx cy) (transform-position ->sheet cx cy)
        (cmd-resize-sheet object (- cx x1) (- cy y1))))))

(define-presentation-translator |-> reset sheet|
    (reset-handler command sheet-geometry-table)
    (object)
  `(cmd-reset-sheet ,object))

(defun show-hierarchy (frame pane)
  (with-bounding-rectangle* (x1 y1 x2 y2) pane
    (declare (ignore x1 y1))
    (draw-arrow* pane 100 25 100 (- y2 25) :ink +red+ :line-thickness 3)
    (draw-arrow* pane 25 100 (- x2 25) 100 :ink +red+ :line-thickness 3)
    ;; FIXME order of operations in WITH-DRAWING-OPTIONS should depend on the
    ;; order in which they appear (i.e first clipping, then transformation).
    ;;
    ;; FIXME text "redisplay counter" is clipped on consecutive displays despite
    ;; being draw outside of the clipping context.
    #+ (or)
    (with-drawing-options
        (pane :clipping-region (make-rectangle* -175 -175 575 575)
              :transformation *sheet->stream-transformation*)
      (present (app* frame)))
    ;;
    ;; FIXME when we use a clipping region, the stream recomputes its size to be
    ;; bigger neverless.
    (with-drawing-options (pane :clipping-region (sheet-region pane))
      (with-drawing-options
          (pane :transformation *sheet->stream-transformation*)
        (loop with bbrd = (find-pane-named frame 'bboard)
              for child in (sheet-children bbrd)
              do (draw-design pane (sheet-region (sheet-parent child))
                              :ink +blue+ :filled nil)
                 (present child))))))

(defun draw-pane (pane)
  (with-scaling (pane 1.5 1.5)
    (draw-rectangle* pane -1000 -1000 1000 1000 :ink +light-sea-green+)
    (draw-rectangle* pane 0 0 100 100 :ink +light-steel-blue+)
    (draw-rectangle* pane 10 10 20 20 :ink +dark-red+)
    (draw-rectangle* pane 80 10 90 20 :ink +dark-cyan+)
    (draw-rectangle* pane 80 80 90 90 :ink +dark-green+)
    (draw-rectangle* pane 10 80 20 90 :ink +dark-blue+)
    (draw-arrow* pane 30 30 70 70 :ink +deep-pink+ :line-thickness 4)
    (medium-finish-output pane)))

(defmethod handle-repaint ((pane my-pane) region)
  (draw-pane pane))

(defmethod handle-event ((pane my-pane) (event pointer-event))
  (let ((x0 (pointer-event-x event))
        (y0 (pointer-event-y event)))
    (cond ((event-matches-gesture-name-p event :zoom-in)
           (cmd-scale-sheet pane 1.1 1.1 x0 y0))
          ((event-matches-gesture-name-p event :zoom-out)
           (cmd-scale-sheet pane .9 .9 x0 y0))
          ((event-matches-gesture-name-p event :rotate-left)
           (cmd-rotate-sheet pane (/ pi -16) x0 y0))
          ((event-matches-gesture-name-p event :rotate-right)
           (cmd-rotate-sheet pane (/ pi 16) x0 y0))
          ((event-matches-gesture-name-p event :select*)
           (cmd-reset-sheet pane))
          ((event-matches-gesture-name-p event :select)
           (prog ((parent (sheet-parent pane))
                  (transf (sheet-transformation pane))
                  dx dy)
              (multiple-value-bind (x y)
                  (bounding-rectangle-position
                   (transform-region transf (sheet-region pane)))
                (multiple-value-bind (x0 y0)
                    (transform-position transf x0 y0)
                  (setf dx (- x0 x) dy (- y0 y)))
                (tracking-pointer (parent)
                  (:pointer-motion (x y)
                    (cmd-move-sheet pane (- x dx) (- y dy))
                    (redisplay-frame-panes *application-frame*))
                  (:pointer-button-release ()
                    (return))))))
          (t
           (return-from handle-event
             (call-next-method)))))
  (redisplay-frame-panes *application-frame*))

(defun make-wrapper (panes)
  (let ((bboard (make-pane 'my-bbrd
                           :contents panes
                           :width 1280
                           :height 300
                           :name 'bboard)))
    (loop for pane in panes
          for x from 50 by 250
          for y from 50 by 250
          do (move-and-resize-sheet pane x y 200 200))
    bboard))

(define-application-frame frame ()
  ()
  (:pointer-documentation t)
  (:geometry :height 720 :width 1920)
  (:panes (app1 (make-pane 'my-pane :width 200 :height 200
                                    :background +deep-pink+
                                    :name 'app1))
          (app2 (make-pane 'my-pane :width 200 :height 200
                                    :background +deep-sky-blue+
                                    :name 'app2))
          (out :application
               :display-function 'show-hierarchy :display-time :command-loop
               :scroll-bars nil :borders nil
               :text-margins '(:bottom 0 :top 0 :left 0 :right 0)))
  (:command-table (sheet-geometry-table))
  (:layouts
   (default
    (horizontally ()
      (720 out)
      (720
       (outlining (:thickness 25 :background +blue+)
         (climi::bordering (:thickness 1 :background +blue+ :foreground +black+)
           (make-wrapper (list app1 app2)))))))))
