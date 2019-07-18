;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :clim-demo)

(defclass canvas-pane (application-pane)
  ((first-point-x :initform nil)
   (first-point-y :initform nil)))

(defclass clim-fig-move-event ()
  ((record :initarg :record :reader record :initform (error "clim-fig-move-event needs a record"))
   (delta-x :initarg :delta-x :reader delta-x :initform 0)
   (delta-y :initarg :delta-y :reader delta-y :initform 0)))

(defmethod print-object ((object clim-fig-move-event) stream)
  (print-unreadable-object (object stream :type T)
   (format stream "moving ~a by (~D,~D)>"
           (record object) (delta-x object) (delta-y object))))

(defun set-status-line (string)
  (setf (gadget-value (clim-fig-status *application-frame*))
	string))

(defun draw-figure (pane x y x1 y1 &key fastp cp-x1 cp-y1 cp-x2 cp-y2)
  (with-slots (line-style current-color fill-mode constrict-mode)
      *application-frame*
    (let* ((radius-x (- x1 x))
           (radius-y (- y1 y))
           (line-style (if fastp (medium-line-style pane) line-style))
           (fill-mode (if fastp nil fill-mode)))
      (when constrict-mode
        (case (clim-fig-drawing-mode *application-frame*)
          ((:line :arrow)
           (if (> (abs radius-x) (abs radius-y))
               (setf y1 y)
               (setf x1 x)))
          ((:rectangle :ellipse)
           (let ((radius-max (max (abs radius-x) (abs radius-y))))
             (setf radius-x (* (signum-1 radius-x) radius-max)
                   radius-y (* (signum-1 radius-y) radius-max)
                   x1 (+ x radius-x)
                   y1 (+ y radius-y))))))
      (case (clim-fig-drawing-mode *application-frame*)
        (:point
         (draw-point* pane x y :ink current-color
                      :line-style line-style))
        (:line
         (draw-line* pane x y x1 y1
                     :ink current-color
                     :line-style line-style))
        (:arrow
         (draw-arrow* pane x y x1 y1
                      :ink current-color
                      :line-style line-style
                      :to-head t :head-width 20 :head-length 20))
        (:rectangle
         (draw-rectangle* pane x y x1 y1 :filled fill-mode
                          :ink current-color
                          :line-style line-style))
        (:ellipse
         (draw-ellipse* pane x y radius-x 0 0 radius-y
                        :filled fill-mode
                        :ink current-color :line-style line-style))
        (:bezier
	  (when fastp
	    (draw-text* pane
			"[Use the middle and right mouse button to set control points]"
			0
			20))
          (if fill-mode
              (let* ((cp-x1 (or cp-x1 x))
                     (cp-y1 (or cp-y1 y1))
                     (cp-x2 (or cp-x2 x1))
                     (cp-y2 (or cp-y2 y)))
                (unless (or (= x cp-x1 x1 cp-x2)
                            (= y cp-y1 y1 cp-y2)) ; Don't draw null beziers.
                  (let ((design (mcclim-bezier::make-bezier-area*
                                 (list x y cp-x1 cp-y1 cp-x2 cp-y2 x1 y1 x1 y1 x y x y))))
                    (mcclim-bezier:draw-bezier-design* pane design
                                                       :ink current-color
                                                       :line-style line-style))
                  (draw-line* pane x y cp-x1 cp-y1 :ink +red+)
                  (draw-line* pane x1 y1 cp-x2 cp-y2 :ink +blue+)))
              (let* ((cp-x1 (or cp-x1 x))
                     (cp-y1 (or cp-y1 y1))
                     (cp-x2 (or cp-x2 x1))
                     (cp-y2 (or cp-y2 y))
                     (design (mcclim-bezier::make-bezier-curve*
                              (list x y cp-x1 cp-y1 cp-x2 cp-y2 x1 y1))))
                (unless (or (= x cp-x1 x1 cp-x2)
                            (= y cp-y1 y1 cp-y2)) ; Don't draw null beziers.
                  (mcclim-bezier:draw-bezier-design* pane design
                                                     :ink current-color
                                                     :line-style line-style)
                  (draw-line* pane x y cp-x1 cp-y1 :ink +red+)
                  (draw-line* pane x1 y1 cp-x2 cp-y2 :ink +blue+)))))))))

(defun signum-1 (value)
  (if (zerop value)
      1
      (signum value)))

(define-presentation-type figure ())

(define-presentation-method highlight-presentation
    ((type figure) record stream state)
  (declare (ignore record stream state))
  nil)

(defun handle-draw-object (pane x1 y1)
  (let* ((pixmap-width (round (bounding-rectangle-width (sheet-region pane))))
         (pixmap-height (round (bounding-rectangle-height (sheet-region pane))))
         (canvas-pixmap (allocate-pixmap pane pixmap-width pixmap-height))
	 cp-x1 cp-y1 cp-x2 cp-y2)
    (copy-to-pixmap pane 0 0 pixmap-width pixmap-height canvas-pixmap)
    (multiple-value-bind (x y)
        (block processor
          (if (eq (slot-value *application-frame* 'drawing-mode) :point)
              (values x1 y1)
              (tracking-pointer (pane)
                (:pointer-motion (&key window x y)
                   (declare (ignore window))
                   (set-status-line (format nil "~:(~A~) from (~D,~D) to (~D,~D)"
                                            (slot-value *application-frame*
                                                        'drawing-mode)
                                            (round x1) (round y1)
                                            (round x) (round y)))
                   (with-output-recording-options (pane :record nil)
                     (copy-from-pixmap canvas-pixmap 0 0
                                       pixmap-width pixmap-height pane 0 0)
                     (draw-figure pane
                                  x1 y1 x y
                                  :fastp t
				  :cp-x1 cp-x1 :cp-y1 cp-y1
				  :cp-x2 cp-x2 :cp-y2 cp-y2)))
		(:pointer-button-release (&key event x y)
                  (when (= (pointer-event-button event)
                           +pointer-left-button+)
                    (return-from processor (values x y))))
                (:pointer-button-press (&key event x y)
		  (cond
		    ((= (pointer-event-button event)
			+pointer-right-button+)
		     (setf cp-x1 x cp-y1 y))
		    ((= (pointer-event-button event)
			+pointer-middle-button+)
		     (setf cp-x2 x cp-y2 y)))))))
      (set-status-line " ")
      (copy-from-pixmap canvas-pixmap 0 0 pixmap-width pixmap-height pane 0 0)
      (deallocate-pixmap canvas-pixmap)
      (let ((new-presentation (with-output-as-presentation (pane nil 'figure)
                                (draw-figure pane x1 y1 x y
		                             :cp-x1 cp-x1 :cp-y1 cp-y1
		                             :cp-x2 cp-x2 :cp-y2 cp-y2))))
        (push new-presentation (clim-fig-undo-list *application-frame*))
        (replay new-presentation *standard-output* (bounding-rectangle new-presentation)))
      (setf (clim-fig-redo-list *application-frame*) nil)
      (deactivate-gadget (find-pane-named *application-frame* 'redo))
      (activate-gadget (find-pane-named *application-frame* 'undo))
      (activate-gadget (find-pane-named *application-frame* 'clear)))))

(defun handle-move-object (pane figure first-point-x first-point-y)
  (multiple-value-bind (figure-x figure-y)
      (output-record-position figure)
    (let ((offset-x (- figure-x first-point-x))
          (offset-y (- figure-y first-point-y)))
     (tracking-pointer (pane)
       (:pointer-motion (&key window x y)
         (declare (ignore window))
         (setf (output-record-position figure)
               (values (+ x offset-x)
                       (+ y offset-y)))
         (window-refresh pane))
       (:pointer-button-release (&key event x y)
         (when (= (pointer-event-button event) +pointer-right-button+)
           (push (make-instance 'clim-fig-move-event
                                :record figure
                                :delta-x (- x first-point-x)
                                :delta-y (- y first-point-y))
                 (clim-fig-undo-list *application-frame*))
           (setf (clim-fig-redo-list *application-frame*) (list))
           (deactivate-gadget (find-pane-named *application-frame* 'redo))
           (window-refresh pane)
           (return-from handle-move-object)))))))

(defun clim-fig ()
  (run-frame-top-level (make-application-frame 'clim-fig)))

(defun make-colored-button (color &key width height)
  (make-pane 'push-button
	     :label " "
	     :activate-callback
             #'(lambda (gadget)
                 (setf (clim-fig-current-color (gadget-client gadget))
                       color))
	     :width width :height height
             :background color :foreground color
	     :normal color :pushed-and-highlighted color
	     :highlighted color))

(defun make-drawing-mode-button (label mode &key width height)
  (make-pane 'push-button
	     :label label
	     :activate-callback
             #'(lambda (gadget)
                 (setf (clim-fig-drawing-mode (gadget-client gadget))
                       mode))
	     :width width :height height))

(defun make-merged-line-style (line-style &key unit thickness joint-shape cap-shape
                               (dashes nil dashes-p))
  (make-line-style :unit (or unit
                             (line-style-unit line-style))
                   :thickness (or thickness
                                 (line-style-thickness line-style))
                   :joint-shape (or joint-shape
                                    (line-style-joint-shape line-style))
                   :cap-shape (or cap-shape
                                  (line-style-cap-shape line-style))
                   :dashes (if dashes-p
                               dashes
                               (line-style-dashes line-style))))

(define-application-frame clim-fig ()
  ((drawing-mode :initform :line :accessor clim-fig-drawing-mode)
   (output-record :accessor clim-fig-output-record)
   (undo-list :initform nil :accessor clim-fig-undo-list)
   (redo-list :initform nil :accessor clim-fig-redo-list)
   (current-color :initform +black+ :accessor clim-fig-current-color)
   (line-style :initform (make-line-style) :accessor clim-fig-line-style)
   (fill-mode :initform nil :accessor clim-fig-fill-mode)
   (constrict-mode :initform nil :accessor clim-fig-constrict-mode)
   (status :initform nil :accessor clim-fig-status))
  (:menu-bar menubar-command-table)
  (:panes
   (canvas (make-pane 'canvas-pane
		      :name 'canvas
                      :display-time nil))
   (line-width-slider :slider
		      :label "Line Width"
		      :value 1
		      :min-value 1
		      :max-value 100
		      :value-changed-callback
                      #'(lambda (gadget value)
                          (declare (ignore gadget))
                          (with-slots (line-style) *application-frame*
                            (setf line-style
                                  (make-merged-line-style line-style
                                                          :thickness (round value)))))
		      :show-value-p t
                      :decimal-places 0
		      :height 50
		      :orientation :horizontal)
   (round-shape-toggle :toggle-button
                       :label "Round Cap/Joint"
                       :value nil
                       :value-changed-callback
                       #'(lambda (gadget value)
                           (declare (ignore gadget))
                           (with-slots (line-style) *application-frame*
                             (let ((cap-shape (if value
                                                  :round
                                                  :butt))
                                   (joint-shape (if value
                                                    :round
                                                    :miter)))
                               (setf line-style
                                     (make-merged-line-style line-style
                                                             :cap-shape cap-shape
                                                             :joint-shape joint-shape))))))
   (fill-mode-toggle :toggle-button
                     :label "Fill"
                     :value nil
                     :value-changed-callback
                     #'(lambda (gadget value)
                         (declare (ignore gadget))
                         (setf (clim-fig-fill-mode *application-frame*) value)))
   (constrict-toggle :toggle-button
                     :label "Constrict"
                     :value nil
                     :value-changed-callback
                     #'(lambda (gadget value)
                         (declare (ignore gadget))
                         (setf (clim-fig-constrict-mode *application-frame*) value)))

   ;; Drawing modes
   (point-button     (make-drawing-mode-button "Point" :point))
   (line-button      (make-drawing-mode-button "Line" :line))
   (arrow-button     (make-drawing-mode-button "Arrow" :arrow))
   (rectangle-button (make-drawing-mode-button "Rectangle" :rectangle))
   (ellipse-button   (make-drawing-mode-button "Ellipse" :ellipse))
   (bezier-button   (make-drawing-mode-button "Bezier" :bezier))

   ;; Colors
   (black-button     (make-colored-button +black+))
   (blue-button      (make-colored-button +blue+))
   (green-button     (make-colored-button +green+))
   (cyan-button      (make-colored-button +cyan+))
   (red-button       (make-colored-button +red+))
   (magenta-button   (make-colored-button +magenta+))
   (yellow-button    (make-colored-button +yellow+))
   (white-button     (make-colored-button +white+))
   (turquoise-button (make-colored-button +turquoise+))
   (grey-button      (make-colored-button +grey+))
   (brown-button     (make-colored-button +brown+))
   (orange-button    (make-colored-button +orange+))

   (undo :push-button
         :label "Undo"
         :active nil
         :activate-callback #'(lambda (x)
                                (declare (ignore x))
                                (com-undo)))
   (redo :push-button
         :label "Redo"
         :active nil
         :activate-callback #'(lambda (x)
                                (declare (ignore x))
                                (com-redo)))
   (clear :push-button
          :label "Clear"
          :active nil
          :activate-callback #'(lambda (x)
                                 (declare (ignore x))
                                 (com-clear)))
   (status :text-field :value "CLIM Fig"
           :editable-p nil))
  (:layouts
   (default
     (vertically ()
       (:fill (horizontally ()
                (vertically (:width 150)
                  (tabling (:height 60)
                    (list black-button blue-button green-button cyan-button)
                    (list red-button magenta-button yellow-button white-button)
                    (list turquoise-button grey-button brown-button orange-button))
                  line-width-slider
                  round-shape-toggle
                  (horizontally () fill-mode-toggle constrict-toggle)
                  point-button line-button arrow-button
                  ellipse-button rectangle-button
	          bezier-button)
                (:fill (scrolling (:width 600 :height 400) canvas))))
       (horizontally (:height 30) clear undo redo)
       status)))
  (:top-level (default-frame-top-level :prompt 'clim-fig-prompt)))

(defmethod frame-standard-output ((frame clim-fig))
  (find-pane-named frame 'canvas))

(define-presentation-to-command-translator add-figure
    (blank-area com-add-figure clim-fig
                :gesture :select ; XXX
                :echo nil
                :tester ((object window)
                         (declare (ignore object))
                         (typep window 'canvas-pane)))
    (object x y)
  (list x y))

(define-presentation-to-command-translator move-figure
    (figure com-move-figure clim-fig
            :gesture :menu ; XXX
            :echo nil)
    (object presentation x y)
  ;; xxx: inv-2016-08-22
  ;; (declare (ignore object))
  (list presentation x y))

(defmethod generate-panes :after (frame-manager (frame clim-fig))
  (declare (ignore frame-manager))
  (setf (clim-fig-output-record frame)
        ;; *standard-output* not bound to the canvas pane yet.
	(stream-current-output-record (frame-standard-output frame))
	(clim-fig-status frame)
	(find-pane-named frame 'status)))

(defun clim-fig-prompt (stream frame)
  (declare (ignore stream frame)))

(define-clim-fig-command com-exit ()
  (frame-exit *application-frame*))

(define-clim-fig-command com-undo ()
  "Undo the previous command, which might have been either 'draw a new object',
   'move an object', or the CLEAR command.

   In the first case, remove the record and add it to the redo list;
   in the second case, move the object back to its previous position;
   to undo a CLEAR, replay the output-history."
  (alexandria:when-let ((latest-undo-entry (pop (clim-fig-undo-list *application-frame*))))
    (cond
      ((typep latest-undo-entry 'clim-fig-move-event)
       (multiple-value-bind (x y)
           (output-record-position (record latest-undo-entry))
         (setf (output-record-position (record latest-undo-entry))
               (values (- x (delta-x latest-undo-entry))
                       (- y (delta-y latest-undo-entry))))
         (window-refresh *standard-output*))
       (push latest-undo-entry (clim-fig-redo-list *application-frame*))
       (activate-gadget (find-pane-named *application-frame* 'redo)))
      ((listp latest-undo-entry)
       (loop for record in latest-undo-entry do
            (stream-add-output-record *standard-output* record)
            (replay record *standard-output* (bounding-rectangle record)))
       (activate-gadget (find-pane-named *application-frame* 'clear))
       (deactivate-gadget (find-pane-named *application-frame* 'redo)))
      (T
       (erase-output-record latest-undo-entry *standard-output*)
       (push latest-undo-entry (clim-fig-redo-list *application-frame*))
       (activate-gadget (find-pane-named *application-frame* 'clear))))
    (unless (clim-fig-undo-list *application-frame*)
      (deactivate-gadget (find-pane-named *application-frame* 'undo))
      (deactivate-gadget (find-pane-named *application-frame* 'clear)))))

(define-clim-fig-command com-redo ()
  (alexandria:when-let ((current-redo-entry (pop (clim-fig-redo-list
                                                *application-frame*))))
    (push current-redo-entry (clim-fig-undo-list *application-frame*))
    (activate-gadget (find-pane-named *application-frame* 'undo))
    (activate-gadget (find-pane-named *application-frame* 'clear))
    (cond
      ((typep current-redo-entry 'clim-fig-move-event)
       (multiple-value-bind (x y)
           (output-record-position (record current-redo-entry))
         (setf (output-record-position (record current-redo-entry))
               (values (+ x (delta-x current-redo-entry))
                       (+ y (delta-y current-redo-entry)))))
       (window-refresh *standard-output*))
      (T (stream-add-output-record *standard-output* current-redo-entry)
         (replay current-redo-entry *standard-output*
                 (bounding-rectangle current-redo-entry))))
    (unless (clim-fig-redo-list *application-frame*)
      (deactivate-gadget (find-pane-named *application-frame* 'redo)))))

(define-clim-fig-command com-clear ()
  (push (coerce (output-record-children (clim-fig-output-record
					 *application-frame*))
		'list)
        (clim-fig-undo-list *application-frame*))
  (setf (clim-fig-redo-list *application-frame*) (list))
  (deactivate-gadget (find-pane-named *application-frame* 'redo))
  (deactivate-gadget (find-pane-named *application-frame* 'clear))
  (window-clear *standard-output*))

(define-clim-fig-command (com-add-figure :name nil) ((x real) (y real))
  (handle-draw-object (find-pane-named *application-frame* 'canvas) x y))

(define-clim-fig-command (com-move-figure :name nil)
    ((figure figure) (x real) (y real))
  (handle-move-object (find-pane-named *application-frame* 'canvas)
                      figure x y))

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("Exit" :command com-exit)))

(make-command-table 'edit-command-table
		    :errorp nil
		    :menu '(("Undo" :command com-undo)
			    ("Redo" :command com-redo)
                            ("Clear" :command com-clear)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Edit" :menu edit-command-table)))
