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
  ((command-table :initarg :command-table)))

(defclass hmenu-pane (menu-bar-mixin hrack-pane) ())
(defclass vmenu-pane (menu-bar-mixin vrack-pane) ())

(defun make-menu-buttons (table client)
  (setf table (find-command-table table))
  (collect (items)
    (map-over-command-table-menu
     (lambda (item table)
       (declare (ignore table))
       (items (make-menu-button-from-menu-item item client)))
     table
     :inherited (inherit-menu table))
    (items +fill+)))

(defun make-menu-bar (command-table &optional (class 'hmenu-pane))
  (let ((menu-bar (make-pane class)))
    (setf (%pane-contents menu-bar)
          (make-menu-buttons command-table menu-bar))
    menu-bar))

(defun update-menu-bar (menu-bar table)
  (setf (%pane-contents menu-bar)
        (and table (make-menu-buttons table menu-bar)))
  (change-space-requirements menu-bar))

(defclass menu-button-submenu-pane (menu-button-pane)
  ((submenu-pane :initarg :submenu-pane :reader submenu-pane)
   (submenu-frame :initarg :submenu-frame :reader submenu-frame)))

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
    (when (typep (gadget-client gadget) 'hmenu-pane)
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
    (when (typep (gadget-client gadget) 'hmenu-pane)
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
      (make-space-requirement :min-width (+ width 4) :min-height height))
    (make-space-requirement :min-width 1 :min-height 1)))

(defmethod handle-repaint ((pane menu-divider-leaf-pane) region)
  (call-next-method)
  (let ((orientation (box-layout-orientation (gadget-client pane)))
        (line-ink +dark-grey+))
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (if-let ((label (slot-value pane 'label)))
        (let ((text-style (pane-text-style pane)))
          (multiple-value-bind (width height fx fy baseline)
              (text-size pane label :text-style text-style)
            (declare (ignore fx fy))
            (ecase orientation
              (:vertical
               (let ((tx0 (+ x1 (/ (- (- x2 x1) width) 2)))
                     (ty0 (+ 1 y1 baseline)))
                 (draw-line* pane tx0 (1+ ty0) (+ tx0 width) (1+ ty0) :ink line-ink)
                 (draw-text* pane label tx0 ty0 :text-style text-style)))
              (:horizontal
               (draw-line* pane x1 (- y2 height) x1 (- y2 1) :ink line-ink)
               (draw-line* pane (- x2 1) (- y2 height) (- x2 1) (- y2 1) :ink line-ink)
               (draw-text* pane label (+ x1 2) y2 :text-style text-style :align-y :bottom)))))
        (ecase orientation
          (:vertical   (draw-line* pane x1 y1 x2 y1 :ink line-ink))
          (:horizontal (draw-line* pane x1 y1 x1 y2 :ink line-ink)))))))

(defgeneric menu-children (pane)
  (:method (pane)
    nil)
  (:method ((pane menu-bar-mixin))
    (sheet-children pane))
  (:method ((submenu menu-button-submenu-pane))
    (sheet-children (submenu-pane submenu))))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-press-event))
  (let ((root (sheet-parent pane)))
    (unwind-protect
         (catch :exit
           (arm-gadget pane)
           (tracking-pointer (root :multiple-window t)
             (:pointer-button-press
              (window)
              (when (typep window 'menu-button-pane)
                (arm-gadget window)))
             (:pointer-button-release
              (window)
              (when (and (typep window 'menu-button-leaf-pane)
                         (gadget-armed-p window))
                (with-slots (label client id) window
                  (value-changed-callback window client id label)))
              (throw :exit nil))
             (:pointer-motion
              (window)
              (when (typep window 'menu-button-pane)
                (if (gadget-armed-p window)
                    (mapc #'disarm-gadget (menu-children window))
                    (arm-gadget window))))
             (:keyboard
              ()
              (throw :exit nil))))
      (loop for child in (menu-children root)
            do (disarm-gadget child)))))

(defgeneric arm-menu-button-callback (button)
  ;; Disarm all siblings
  (:method ((button menu-button-pane))
    (let ((client (gadget-client button)))
      (mapc #'disarm-gadget (remove button (menu-children client)))))
  ;; Show the sub-menu frame
  (:method ((button menu-button-submenu-pane))
    (call-next-method)
    (let ((fm (frame-manager (pane-frame button)))
          (sf (submenu-frame button)))
      (let ((bottomp (typep (gadget-client button) 'hrack-pane)))
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
         (frame (pane-frame client))
         (manager (frame-manager frame)))
    (flet ((make-sub-pane (class &rest initargs &key &allow-other-keys)
             (apply #'make-pane-1 manager frame class
                    :label name :client client :text-style text-style
                    :armed-callback 'arm-menu-button-callback
                    :disarmed-callback 'disarm-menu-button-callback
                    initargs)))
      (case type
        (:command
         (let ((command-name (if (consp value) (car value) value)))
           (if (command-enabled command-name frame)
               (make-sub-pane 'menu-button-leaf-pane
                              :value-changed-callback
                              (lambda (gadget val)
                                (declare (ignore gadget val))
                                (throw-object-ptype item 'menu-item)))
               (let ((pane (make-sub-pane 'menu-button-leaf-pane
                                          :value-changed-callback
                                          (lambda (gadget val)
                                            (declare (ignore gadget val))
                                            nil))))
                 (deactivate-gadget pane)
                 pane))))
        (:function
         (make-sub-pane 'menu-button-leaf-pane
                        :value-changed-callback
                        (lambda (gadget val)
                          (declare (ignore gadget val))
                          ;; FIXME: the spec requires us to pass a gesture to the
                          ;; function, but value-changed-callback doesn't provide
                          ;; one, so we pass NIL for now.
                          ;; FIXME: We don't have a numeric argument, either.
                          (let ((command (funcall value nil nil)))
                            (throw-object-ptype command 'command)))))
        (:divider
         (setf text-style
               (merge-text-styles text-style '(:sans-serif :roman :smaller)))
         (make-sub-pane 'menu-divider-leaf-pane))
        (:menu
         (let* ((rack (make-menu-bar value 'vmenu-pane))
                (border (make-pane 'raised-pane :contents (list rack))))
           (make-sub-pane 'menu-button-submenu-pane
                          :submenu-pane rack
                          :submenu-frame (make-menu-frame border))))
        (otherwise (error "Don't know how to create a menu button for ~W" type))))))
