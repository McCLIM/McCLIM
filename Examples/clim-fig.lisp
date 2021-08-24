;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 by Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;  (c) copyright 2019-2020 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A simple vector drawing application.
;;;

(defpackage #:clim-demo.clim-fig
  (:use #:clim-lisp #:clim)
  (:import-from #:alexandria #:when-let #:curry)
  (:export #:clim-fig)) ; Function and application frame class
(in-package #:clim-demo.clim-fig)

(defclass canvas-pane (application-pane)
  ())

(defclass move-event ()
  ((record :initarg :record :reader record)
   (delta-x :initarg :delta-x :reader delta-x :initform 0)
   (delta-y :initarg :delta-y :reader delta-y :initform 0))
  (:default-initargs
   :record (error "move-event needs a record")))

(defmethod print-object ((object move-event) stream)
  (print-unreadable-object (object stream :type T)
   (format stream "moving ~a by (~D,~D)>"
           (record object) (delta-x object) (delta-y object))))

(defun set-status-line (string)
  (setf (clime:label-pane-label (find-pane-named *application-frame* 'status))
        string))

(defun draw-figure (pane mode x y x1 y1 &key cp-x1 cp-y1 cp-x2 cp-y2)
  (with-slots (line-style current-color fill-mode constrict-mode)
      *application-frame*
    (let* ((radius-x (- x1 x))
           (radius-y (- y1 y)))
      (when constrict-mode
        (case mode
          ((:line :arrow)
           (if (> (abs radius-x) (abs radius-y))
               (setf y1 y)
               (setf x1 x)))
          ((:rectangle :ellipse)
           (let ((radius-max (max (abs radius-x) (abs radius-y))))
             (setf radius-x (* (if (minusp radius-x) -1 1) radius-max)
                   radius-y (* (if (minusp radius-y) -1 1) radius-max)
                   x1 (+ x radius-x)
                   y1 (+ y radius-y))))))
      (case mode
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
         (let* ((cp-x1 (or cp-x1 x))
                (cp-y1 (or cp-y1 y1))
                (cp-x2 (or cp-x2 x1))
                (cp-y2 (or cp-y2 y)))
           (unless (or (= x cp-x1 x1 cp-x2)
                       (= y cp-y1 y1 cp-y2)) ; Don't draw null beziers.
             (let ((design (if fill-mode
                               (mcclim-bezier::make-bezier-area*
                                (list x y cp-x1 cp-y1 cp-x2 cp-y2 x1 y1 x1 y1 x y x y))
                               (mcclim-bezier::make-bezier-curve*
                                (list x y cp-x1 cp-y1 cp-x2 cp-y2 x1 y1)))))
               (draw-design pane design :ink current-color :line-style line-style))
             (draw-line* pane x y cp-x1 cp-y1 :ink +red+)
             (draw-line* pane x1 y1 cp-x2 cp-y2 :ink +blue+))))))))

(define-presentation-type figure ())

(define-presentation-method highlight-presentation
    ((type figure) record stream state)
  (declare (ignore record stream state))
  nil)

(defun handle-draw-object (pane x1 y1)
  (let* ((frame *application-frame*)
         (mode (slot-value frame 'drawing-mode))
         cp-x1 cp-y1 cp-x2 cp-y2
         output-record)
    (flet ((make-figure-output-record (x y)
             ;; Note that this can be NIL if (= x x1) and (= y y1).
             (setf output-record
                   (with-output-to-output-record (pane)
                     (with-output-as-presentation (pane nil 'figure)
                       (draw-figure pane mode x1 y1 x y
                                    :cp-x1 cp-x1 :cp-y1 cp-y1
                                    :cp-x2 cp-x2 :cp-y2 cp-y2))))))
      (case mode
        (:point
         (make-figure-output-record x1 y1)
         (replay output-record pane))
        (t
         (block processor
           (tracking-pointer (pane)
             (:pointer-motion (&key window x y)
               (declare (ignore window))
               (set-status-line
                (format nil "~:(~A~) from (~D,~D) to (~D,~D)~@[ - Use ~
                             the middle and right mouse button to set ~
                             control points~]"
                        mode
                        (round x1) (round y1) (round x) (round y)
                        (eq mode :bezier)))
               (when output-record
                 (repaint-sheet
                  pane
                  (with-bounding-rectangle* (x1 y1 x2 y2) output-record
                    (make-rectangle* (1- x1) (1- y1) (1+ x2) (1+ y2)))))
               (make-figure-output-record x y)
               (when output-record
                 (replay output-record pane)))
             (:pointer-button-release (&key event x y)
               (when (= (pointer-event-button event)
                        +pointer-left-button+)
                 (return-from processor (values x y))))
             (:pointer-button-press (&key event x y)
               (let ((button (pointer-event-button event)))
                 (cond ((= button +pointer-right-button+)
                        (setf cp-x1 x cp-y1 y))
                       ((= button +pointer-middle-button+)
                        (setf cp-x2 x cp-y2 y)))))))))
      (set-status-line " ")
      (when output-record
        (push output-record (undo-list frame))
        (stream-add-output-record pane output-record)
        (setf (redo-list *application-frame*) nil)
        (disable-commands frame 'com-redo)
        (enable-commands frame 'com-undo 'com-clear))
      (change-space-requirements pane))))

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
            (let ((frame *application-frame*))
              (push (make-instance 'move-event :record figure
                                               :delta-x (- x first-point-x)
                                               :delta-y (- y first-point-y))
                    (undo-list frame))
              (setf (redo-list frame) (list))
              (disable-commands frame 'com-redo)
              (window-refresh pane)
              (return-from handle-move-object))))))))

(defun clim-fig ()
  (run-frame-top-level (make-application-frame 'clim-fig)))

(defun make-colored-button (color &key width height)
  (make-pane 'push-button
             :label " "
             :activate-callback
             (lambda (gadget)
               (setf (current-color (gadget-client gadget)) color))
             :width width :height height
             :background color :foreground color
             :normal color :pushed-and-highlighted color
             :highlighted color))

(defun make-drawing-mode-button (label mode)
  (make-pane 'push-button
             :label label
             :activate-callback
             (lambda (gadget)
               (setf (drawing-mode (gadget-client gadget)) mode))))

(defun make-dashes-string (dashes)
  (if dashes
      (with-output-to-string (stream)
        (flet ((add-segment (length character)
                 (write-string (make-string length :initial-element character)
                               stream)))
         (loop for (dash space) on (append dashes dashes) by #'cddr
               do (add-segment dash #\-) (add-segment space #\Space))))
      "none"))

(defun make-merged-line-style (line-style &key unit thickness joint-shape cap-shape
                                               (dashes nil dashes-p))
  (flet ((scale-dashes (dashes factor)
           (map (class-of dashes) (curry #'* factor) dashes)))
    (let* ((old-thickness (line-style-thickness line-style))
           (thickness (or thickness old-thickness))
           (old-dashes (line-style-dashes line-style))
           (dashes (if dashes-p
                       dashes
                       (scale-dashes old-dashes (/ old-thickness)))))
      (make-line-style :unit (or unit
                                 (line-style-unit line-style))
                       :thickness thickness
                       :joint-shape (or joint-shape
                                        (line-style-joint-shape line-style))
                       :cap-shape (or cap-shape
                                      (line-style-cap-shape line-style))
                       :dashes (scale-dashes dashes thickness)))))

(define-application-frame clim-fig ()
  ((drawing-mode :initform :line :accessor drawing-mode)
   (output-record :accessor root-output-record)
   (undo-list :initform nil :accessor undo-list)
   (redo-list :initform nil :accessor redo-list)
   (current-color :initform +black+ :accessor current-color)
   (line-style :initform (make-line-style) :accessor line-style)
   (fill-mode :initform nil :accessor fill-mode)
   (constrict-mode :initform nil :accessor constrict-mode)
   (status :initform nil :accessor status))
  (:menu-bar menubar-command-table)
  (:panes
   (canvas canvas-pane
           :name 'canvas
           :display-time t)
   (line-width-slider :slider
                      :label "Line Width"
                      :value 1
                      :min-value 1
                      :max-value 100
                      :value-changed-callback
                      (lambda (gadget value)
                        (declare (ignore gadget))
                        (with-slots (line-style) *application-frame*
                          (setf line-style
                                (make-merged-line-style line-style
                                                        :thickness (round value)))))
                      :show-value-p t
                      :decimal-places 0
                      :orientation :horizontal)
   (dashes :option-pane
           :value nil
           :items '(nil (2 2) (4 4) (2 4) (4 2))
           :name-key 'make-dashes-string
           :value-changed-callback
           (lambda (gadget value)
             (with-slots (line-style) (gadget-client gadget)
               (setf line-style
                     (make-merged-line-style line-style :dashes value))))
           :text-style (make-text-style :fix nil nil))
   (round-shape-toggle :toggle-button
                       :label "Round Cap/Joint"
                       :value nil
                       :value-changed-callback
                       (lambda (gadget value)
                         (with-slots (line-style) (gadget-client gadget)
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
                     (lambda (gadget value)
                       (setf (fill-mode (gadget-client gadget)) value)))
   (constrict-toggle :toggle-button
                     :label "Constrict"
                     :value nil
                     :value-changed-callback
                     (lambda (gadget value)
                       (setf (constrict-mode (gadget-client gadget)) value)))

   ;; Drawing modes
   (point-button     (make-drawing-mode-button "Point" :point))
   (line-button      (make-drawing-mode-button "Line" :line))
   (arrow-button     (make-drawing-mode-button "Arrow" :arrow))
   (rectangle-button (make-drawing-mode-button "Rectangle" :rectangle))
   (ellipse-button   (make-drawing-mode-button "Ellipse" :ellipse))
   (bezier-button    (make-drawing-mode-button "Bezier" :bezier))

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
         :activate-callback (lambda (x)
                              (declare (ignore x))
                              (com-undo)))
   (redo :push-button
         :label "Redo"
         :active nil
         :activate-callback (lambda (x)
                              (declare (ignore x))
                              (com-redo)))
   (clear :push-button
          :label "Clear"
          :active nil
          :activate-callback (lambda (x)
                               (declare (ignore x))
                               (com-clear)))
   (status :label-pane :label "CLIM Fig"))
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
                  (horizontally (:spacing 4)
                    (labelling (:label "Dashes"))
                    dashes)
                  round-shape-toggle
                  (horizontally () fill-mode-toggle constrict-toggle)
                  point-button line-button arrow-button
                  ellipse-button rectangle-button
                  bezier-button
                  :fill)
                (:fill (scrolling (:width 600 :height 400) canvas))))
       (horizontally (:height 30) clear undo redo)
       status)))
  (:top-level (default-frame-top-level :prompt 'prompt)))

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
  (setf (root-output-record frame)
        ;; *standard-output* not bound to the canvas pane yet.
        (stream-current-output-record (frame-standard-output frame))
        (status frame) (find-pane-named frame 'status)))

(defun prompt (stream frame)
  (declare (ignore stream frame)))

(defmethod note-command-enabled :after (frame-manager (frame clim-fig) command-name)
  (case command-name
    (com-undo (activate-gadget (find-pane-named frame 'undo)))
    (com-redo (activate-gadget (find-pane-named frame 'redo)))
    (com-clear (activate-gadget (find-pane-named frame 'clear)))))

(defmethod note-command-disabled :after (frame-manager (frame clim-fig) command-name)
  (case command-name
    (com-undo (deactivate-gadget (find-pane-named frame 'undo)))
    (com-redo (deactivate-gadget (find-pane-named frame 'redo)))
    (com-clear (deactivate-gadget (find-pane-named frame 'clear)))))

(defun enable-commands (frame &rest command-names)
  (dolist (command-name command-names)
    (setf (command-enabled command-name frame) t)))

(defun disable-commands (frame &rest command-names)
  (dolist (command-name command-names)
    (setf (command-enabled command-name frame) nil)))

(define-clim-fig-command com-exit ()
  (frame-exit *application-frame*))

(define-clim-fig-command com-undo ()
  "Undo the previous command, which might have been either 'draw a new object',
   'move an object', or the CLEAR command.

   In the first case, remove the record and add it to the redo list;
   in the second case, move the object back to its previous position;
   to undo a CLEAR, replay the output-history."
  (when-let ((latest-undo-entry (pop (undo-list *application-frame*))))
    (cond
      ((typep latest-undo-entry 'move-event)
       (multiple-value-bind (x y)
           (output-record-position (record latest-undo-entry))
         (setf (output-record-position (record latest-undo-entry))
               (values (- x (delta-x latest-undo-entry))
                       (- y (delta-y latest-undo-entry))))
         (window-refresh *standard-output*))
       (push latest-undo-entry (redo-list *application-frame*))
       (enable-commands *application-frame* 'com-redo))
      ((listp latest-undo-entry)
       (loop for record in latest-undo-entry do
            (stream-add-output-record *standard-output* record)
            (replay record *standard-output* (bounding-rectangle record)))
       (enable-commands *application-frame* 'com-clear)
       (disable-commands *application-frame* 'com-redo))
      (T
       (erase-output-record latest-undo-entry *standard-output*)
       (push latest-undo-entry (redo-list *application-frame*))
       (enable-commands *application-frame* 'com-clear 'com-redo)))
    (unless (undo-list *application-frame*)
      (disable-commands *application-frame* 'com-undo 'com-clear))))

(define-clim-fig-command com-redo ()
  (when-let ((current-redo-entry (pop (redo-list *application-frame*))))
    (push current-redo-entry (undo-list *application-frame*))
    (enable-commands *application-frame* 'com-undo 'com-clear)
    (cond
      ((typep current-redo-entry 'move-event)
       (multiple-value-bind (x y)
           (output-record-position (record current-redo-entry))
         (setf (output-record-position (record current-redo-entry))
               (values (+ x (delta-x current-redo-entry))
                       (+ y (delta-y current-redo-entry)))))
       (window-refresh *standard-output*))
      (T (stream-add-output-record *standard-output* current-redo-entry)
         (replay current-redo-entry *standard-output*
                 (bounding-rectangle current-redo-entry))))
    (unless (redo-list *application-frame*)
      (disable-commands *application-frame* 'com-redo))))

(define-clim-fig-command com-clear ()
  (push (coerce (output-record-children (root-output-record
                                         *application-frame*))
                'list)
        (undo-list *application-frame*))
  (setf (redo-list *application-frame*) (list))
  (disable-commands *application-frame* 'com-redo 'com-clear)
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
