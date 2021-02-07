;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2000, 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the portable menu bar.
;;;

(in-package #:clim-internals)

(defclass menu-bar-mixin ()
  ((command-table :initarg :command-table :reader command-table)))

(defclass hmenu-pane (menu-bar-mixin hrack-pane) ())
(defclass vmenu-pane (menu-bar-mixin vrack-pane) ())

(defun make-menu-buttons (table client)
  (setf table (find-command-table table))
  (collect (items)
    (map-over-command-table-menu-items
     (lambda (name keystroke item)
       (declare (ignore name keystroke))
       (items (make-menu-button-from-menu-item item client)))
     table
     :inherited (inherit-menu table))
    (items +fill+)))

(defun make-menu-bar (command-table client class)
  (make-pane class :contents (make-menu-buttons command-table client)
                   :command-table command-table))

(defun update-menu-bar (menu-bar client command-table)
  (setf (%pane-contents menu-bar)
        (and command-table (make-menu-buttons command-table client)))
  (change-space-requirements menu-bar))

(defun menu-children (client)
  (when-let ((rack (typecase client
                     (application-frame (frame-menu-bar-pane client))
                     (menu-button-submenu-pane (submenu-pane client))
                     (otherwise nil))))
    (remove-if-not (lambda (elt)
                     (and (typep elt 'menu-button-pane)
                          (gadget-active-p elt)))
                   ;; Composite panes adopt children in
                   ;; order, so they are in reverse order.
                   (reverse (sheet-children rack)))))

(define-gesture-name :menu-exit  :keyboard :escape)
(define-gesture-name :menu-left  :keyboard :left)
(define-gesture-name :menu-right :keyboard :right)
(define-gesture-name :menu-up    :keyboard :up)
(define-gesture-name :menu-down  :keyboard :down)
(define-gesture-name :menu-enter :keyboard #\return)
(define-gesture-name :menu-enter :keyboard #\space :unique nil)

(defun start-menu-bar (menu-bar active-button
                       &aux (first-release t) (frame (pane-frame menu-bar)))
  (unwind-protect (tracking-pointer (menu-bar :multiple-window t)
                    (:pointer-button-press
                     (window)
                     (when (typep window 'menu-button-pane)
                       (arm-gadget window)
                       (setf active-button window)))
                    (:pointer-button-release
                     (window)
                     (typecase window
                       (menu-button-leaf-pane
                        (when (gadget-armed-p window)
                          (with-slots (label client id) window
                            (value-changed-callback window client id label))))
                       (menu-button-submenu-pane
                        (if (gadget-armed-p window)
                            (unless first-release
                              (when (eq frame (gadget-client window))
                                (return-from start-menu-bar)))
                            (arm-gadget window)))
                       (otherwise
                        (return-from start-menu-bar)))
                     (setf first-release nil)
                     (setf active-button window))
                    (:pointer-motion
                     (window)
                     (when (typep window 'menu-button-pane)
                       (cond ((gadget-armed-p window)
                              (mapc #'disarm-gadget (menu-children window)))
                             ((gadget-active-p window)
                              (arm-gadget window))
                             (t
                              (arm-menu-button-callback window)))
                       (setf active-button window)))
                    (:keyboard
                     (event)
                     (labels ((go-top ()
                                (let ((client (gadget-client active-button)))
                                  (if (application-frame-p client)
                                      (mapc #'disarm-gadget (menu-children active-button))
                                      (progn
                                        (setf active-button client)
                                        (go-top)))))
                              (go-parent ()
                                (let ((client (gadget-client active-button))
                                      (active active-button))
                                  (unless (application-frame-p client)
                                    (setf active-button client)
                                    (arm-gadget client)
                                    (mapc #'disarm-gadget (menu-children client))
                                    (unless (eq (box-layout-orientation (sheet-parent active))
                                                (box-layout-orientation (sheet-parent client)))
                                      (go-previous)))))
                              (go-child ()
                                (if-let ((child (first (menu-children active-button))))
                                  (progn
                                    (setf active-button child)
                                    (arm-gadget child))
                                  (progn
                                    (go-top)
                                    (go-next))))
                              (go-next ()
                                (when-let* ((client (gadget-client active-button))
                                            (siblings (menu-children client))
                                            (remainder (member active-button siblings)))
                                  (setf active-button
                                        (if (null (cdr remainder))
                                            (first siblings)
                                            (second remainder)))
                                  (arm-gadget active-button)))
                              (go-previous ()
                                (when-let* ((client (gadget-client active-button))
                                            (siblings (reverse (menu-children client)))
                                            (remainder (member active-button siblings)))
                                  (setf active-button
                                        (if (null (cdr remainder))
                                            (first siblings)
                                            (second remainder)))
                                  (arm-gadget active-button)))
                              (enter ()
                                (etypecase active-button
                                  (menu-button-leaf-pane
                                   (with-slots (label client id) active-button
                                     (value-changed-callback active-button client id label)))
                                  (menu-button-submenu-pane
                                   (when-let ((child (first (menu-children active-button))))
                                     (arm-gadget child)
                                     (setf active-button child))))))
                       (gesture-case event
                         (:menu-exit
                          (return-from start-menu-bar))
                         (:menu-enter
                          (enter))
                         (:menu-left
                          ;; sheet parent is the layout
                          (ecase (box-layout-orientation (sheet-parent active-button))
                            (:horizontal (go-previous))
                            (:vertical (go-parent))))
                         (:menu-right
                          (ecase (box-layout-orientation (sheet-parent active-button))
                            (:horizontal (go-next))
                            (:vertical (go-child))))
                         (:menu-up
                          (ecase (box-layout-orientation (sheet-parent active-button))
                            (:horizontal (go-parent))
                            (:vertical (go-previous))))
                         (:menu-down
                          (ecase (box-layout-orientation (sheet-parent active-button))
                            (:horizontal (go-child))
                            (:vertical (go-next))))))))
    (loop for child in (sheet-children menu-bar)
          do (disarm-gadget child))))

(defclass menu-button-submenu-pane (menu-button-pane)
  ((submenu-pane :reader submenu-pane)
   (submenu-items :reader submenu-items)
   (submenu-frame :reader submenu-frame)))

;;; These methods are responsible for ensuring enough space for a marker, that
;;; shows a triangle at the right edge (visual clue that it may be expanded).
(let* ((left-padding 10)
       (widget-size  5)
       (right-padding 4)
       (widget-width widget-size)
       (widget-height (* 2 widget-size))
       (total-width (+ left-padding widget-width right-padding))
       (total-height widget-height))

  (defmethod compose-space ((gadget menu-button-submenu-pane) &key width height)
    (declare (ignorable width height))
    (when (typep (sheet-parent gadget) 'hmenu-pane)
      (return-from compose-space (call-next-method)))
    (multiple-value-bind (width min-width max-width height min-height max-height)
        (space-requirement-components (call-next-method))
      (declare (ignorable max-width))
      (make-space-requirement :min-width (+ min-width total-width)
                              :width (+ width total-width)
                              :max-width +fill+
                              :min-height (max min-height total-height)
                              :height (max height total-height)
                              :max-height (max max-height total-height))))

  (defmethod handle-repaint ((gadget menu-button-submenu-pane) region)
    (call-next-method)
    (when (typep (sheet-parent gadget) 'hmenu-pane)
      (return-from handle-repaint))
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region gadget)
      (when (and (> (- x2 x1) total-width)
                 (> (- y2 y1) total-height))
        (let* ((center (/ (+ y1 y2) 2))
               (vbase (- center (/ widget-height 2)))
               (hbase (+ (- x2 total-width) left-padding))
               (shape (list hbase vbase
                            (+ hbase widget-size) (+ vbase widget-size)
                            hbase (+ vbase (* 2 widget-size)))))
          (draw-polygon* gadget shape))))))

(defclass menu-button-leaf-pane (menu-button-pane)
  ((command :initform nil :initarg :command)))

(defclass menu-divider-leaf-pane (sheet-leaf-mixin basic-gadget)
  ((label :initform nil :initarg :label)))

(defmethod compose-space ((gadget menu-divider-leaf-pane) &key width height)
  (declare (ignorable width height))
  (if-let ((label (slot-value gadget 'label)))
    (multiple-value-bind (width height)
        (text-size gadget label :text-style (pane-text-style gadget))
      (ecase (box-layout-orientation (sheet-parent gadget))
        (:vertical
         (make-space-requirement :min-width (+ width 4) :min-height (+ height 2)))
        (:horizontal
         (make-space-requirement :min-width (+ width 6) :min-height (+ height 8)))))
    (make-space-requirement :min-width 1 :min-height 1)))

(defmethod handle-repaint ((pane menu-divider-leaf-pane) region)
  (call-next-method)
  (let ((orientation (box-layout-orientation (sheet-parent pane)))
        (line-ink +dark-grey+))
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (if-let ((label (slot-value pane 'label)))
        (let ((text-style (pane-text-style pane)))
          (multiple-value-bind (width height fx fy baseline)
              (text-size pane label :text-style text-style)
            (declare (ignore height fx fy))
            (ecase orientation
              (:vertical
               (let ((tx0 (+ x1 (/ (- (- x2 x1) width) 2)))
                     (ty0 (+ 1 y1 baseline)))
                 (draw-line* pane tx0 (1+ ty0) (+ tx0 width) (1+ ty0) :ink line-ink)
                 (draw-text* pane label tx0 ty0 :text-style text-style)))
              (:horizontal
               (draw-text* pane label (+ x1 2) (- y2 4)
                           :text-style text-style
                           :align-y :bottom)))))
        (ecase orientation
          (:vertical   (draw-line* pane x1 y1 x2 y1 :ink line-ink))
          (:horizontal (draw-line* pane x1 y1 x1 y2 :ink line-ink)))))))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-press-event))
  (arm-gadget pane)
  (start-menu-bar (sheet-parent pane) pane))

(defgeneric arm-menu-button-callback (button)
  ;; Disarm all siblings
  (:method ((button menu-button-pane))
    (let ((rack (sheet-parent button)))
      (mapc #'disarm-gadget (remove button (sheet-children rack)))))
  ;; Show the sub-menu frame
  (:method ((button menu-button-submenu-pane))
    (call-next-method)
    (let ((fm (frame-manager (pane-frame button)))
          (sf (submenu-frame button)))
      (let ((bottomp (typep (sheet-parent button) 'hrack-pane)))
        (with-bounding-rectangle* (xmin ymin xmax ymax) (sheet-region button)
          (multiple-value-bind (x y)
              (transform-position (sheet-delta-transformation button nil)
                                  (if bottomp xmin xmax)
                                  (if bottomp ymax ymin))
            (setf (slot-value sf 'left) x)
            (setf (slot-value sf 'top) y))))
      (adopt-frame fm sf)
      (enable-frame sf))))

(defgeneric disarm-menu-button-callback (button)
  (:method ((button menu-button-pane)))
  ;; Show sub-menu frames
  (:method ((button menu-button-submenu-pane))
    ;; Disarm all children.
    (mapc #'disarm-gadget (menu-children button))
    ;; Hide the sub-menu frame.
    (let* ((sf (submenu-frame button))
           (fm (frame-manager sf)))
      (disown-frame fm sf))))

;;; Menu creation from command tables
(defun make-menu-button-from-menu-item (item client)
  (let* ((name (command-menu-item-name item))
         (type (command-menu-item-type item))
         (value (command-menu-item-value item))
         (text-style (command-menu-item-text-style item))
         (frame (typecase client
                  (application-frame client)
                  (pane (pane-frame client))
                  (otherwise *application-frame*)))
         (manager (frame-manager frame)))
    (flet ((make-sub-pane (class &rest initargs &key &allow-other-keys)
             (apply #'make-pane-1 manager frame class
                    :label name :client client :text-style text-style
                    initargs)))
      (case type
        (:divider
         (setf text-style
               (merge-text-styles text-style '(:sans-serif :roman :smaller)))
         (make-sub-pane 'menu-divider-leaf-pane))
        ((:command :function)
         (let* ((command (extract-menu-item-command item nil))
                (command-name (alexandria:ensure-car command)))
           (make-sub-pane
            'menu-button-leaf-pane
            :value-changed-callback
            (lambda (gadget val)
              (declare (ignore gadget val))
              (throw-object-ptype item 'menu-item))
            :armed-callback 'arm-menu-button-callback
            :disarmed-callback 'disarm-menu-button-callback
            :active (command-enabled command-name frame))))
        (:menu
         (let* ((sub-pane (make-sub-pane
                           'menu-button-submenu-pane
                           :armed-callback 'arm-menu-button-callback
                           :disarmed-callback 'disarm-menu-button-callback))
                (rack (make-menu-bar value sub-pane 'vmenu-pane))
                (border (make-pane 'raised-pane :contents (list rack)))
                (active (remove-if-not (lambda (elt)
                                         (and (typep elt 'menu-button-pane)
                                              (gadget-active-p elt)))
                                       ;; Composite panes adopt children in
                                       ;; order, so they are in reverse order.
                                       (reverse (sheet-children rack)))))
           (setf (slot-value sub-pane 'submenu-pane) rack
                 (slot-value sub-pane 'submenu-items) active
                 (slot-value sub-pane 'submenu-frame) (make-menu-frame border))
           sub-pane))
        (otherwise (error "Don't know how to create a menu button for ~W" type))))))
