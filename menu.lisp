1;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :clim-internals)

(defmethod stream-force-output ((pane menu-button-pane))
  (with-sheet-medium (medium pane)
    (medium-force-output medium)))

(defmethod menu-root ((button menu-button-pane))
  (menu-root (gadget-client button)))

(defmethod arm-menu ((button menu-button-pane))
  (with-slots (client armed id) button
    (unless armed
      (arm-menu client)
      (mapc #'disarm-menu (menu-children client))
      (arm-gadget button t))
    (dispatch-repaint button (sheet-region button))))

(defmethod disarm-menu ((button menu-button-pane))
  (with-slots (client armed id) button
    (when armed
      (disarm-gadget button)
      (dispatch-repaint button (sheet-region button))
      (stream-force-output button))))

(defun menu-draw-highlighted (gadget)
  (when (sheet-mirror gadget)           ;XXX only do this when the gadget is realized.
    (with-special-choices (gadget)
      (with-slots (label) gadget
        (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region gadget)
          (let ((w (- x2 x1))
                (h (- y2 y1)))
            (draw-rectangle* gadget -1 -1 x2 y2
                             :ink (gadget-highlighted-color gadget)
                             :filled t)
            (draw-edges-lines* gadget +white+ 0 0 +black+ (1- w) (1- h))
            (draw-label* gadget x1 y1 x2 y2)))))))

(defun menu-draw-unhighlighted (gadget)
  (when (sheet-mirror gadget)           ;XXX only do this when the gadget is realized.
    (with-special-choices (gadget)
      (with-slots (label) gadget
        (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region gadget)
          (let ((w (- x2 x1))
                (h (- y2 y1)))
            (draw-rectangle* gadget -1 -1 w h ;-1 -1 x2 y2
                             :ink +background-ink+
                             :filled t)
            (draw-label* gadget x1 y1 x2 y2)))))))

(defmethod handle-event ((pane menu-button-pane) (event pointer-enter-event))
  (when (slot-value (slot-value pane 'client) 'armed)
    (arm-branch pane)))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-press-event))
  (arm-branch pane))

(defmethod handle-event ((pane menu-button-pane) (event pointer-ungrab-event))
  (destroy-substructure (menu-root pane)))

;;; menu-button-leaf-pane

(defclass menu-button-leaf-pane (menu-button-pane)
  ((command :initform nil :initarg :command)))

(defmethod arm-branch ((button menu-button-leaf-pane))
  (with-slots (client) button
    (arm-menu client)
    (mapc #'destroy-substructure (menu-children client))
    (arm-menu button)))

(defmethod destroy-substructure ((button menu-button-leaf-pane))
  (disarm-gadget button))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-button-release-event))
  (with-slots (armed label client id) pane
    (when armed
      (unwind-protect
	   (value-changed-callback pane client id label)
	(disarm-menu pane)
	(destroy-substructure (menu-root pane))))))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-exit-event))
  (disarm-menu pane))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-ungrab-event))
  (destroy-substructure (menu-root pane)))

;;; menu-button-submenu-pane

(defclass menu-button-submenu-pane (menu-button-pane)
  ((frame-manager :initform nil :initarg :frame-manager)
   (submenu-frame :initform nil)
   (bottomp :initform nil :initarg :bottomp)
   (command-table :initform nil :initarg :command-table)))

(defmethod menu-children ((submenu menu-button-submenu-pane))
  (with-slots (submenu-frame) submenu
    (if submenu-frame
	(sheet-children (first (sheet-children (frame-panes submenu-frame))))
	'())))

(defclass submenu-border (border-pane) ())

(defclass submenu-border-pane (raised-pane)
  ()
  (:default-initargs :border-width 2 :background *3d-normal-color*))

(defun make-menu-buttons (command-table-name client)
  "Map over the available menu items in the command table with
name `command-table-name', taking inherited menu items into
account, and create a list of menu buttons."
  (let ((menu-buttons '()))
    (map-over-command-table-menu-items
     #'(lambda (name gesture item)
         (declare (ignore name gesture))
         (push (make-menu-button-from-menu-item
                item client :command-table command-table-name :vertical t)
               menu-buttons))
     command-table-name)
    (nreverse menu-buttons)))

(defun create-substructure (sub-menu client)
  (let* ((frame *application-frame*)
	 (manager (frame-manager frame))
	 (command-table-name (slot-value sub-menu 'command-table))
	 (items (make-menu-buttons command-table-name client))
	 (rack (make-pane-1 manager frame 'vrack-pane
			    :background *3d-normal-color* :contents items))
	 (raised (make-pane-1 manager frame 'submenu-border :contents (list rack))))
    (with-slots (bottomp) sub-menu
      (multiple-value-bind (xmin ymin xmax ymax)
	  (bounding-rectangle* (sheet-region sub-menu))
	(multiple-value-bind (x y)
	    (transform-position (sheet-delta-transformation sub-menu nil)
				(if bottomp xmin xmax)
				(if bottomp ymax ymin))
	  (with-slots (frame-manager submenu-frame) sub-menu
	    (setf frame-manager manager
		  submenu-frame (make-menu-frame raised :left x :top y))
	    (adopt-frame manager submenu-frame)
	    (with-sheet-medium (medium raised)
	      (medium-force-output medium))))))))

(defmethod destroy-substructure ((sub-menu menu-button-submenu-pane))
  (with-slots (frame-manager submenu-frame) sub-menu
    (when submenu-frame
      (mapc #'destroy-substructure (menu-children sub-menu))
      (disown-frame frame-manager submenu-frame)
      (disarm-gadget sub-menu)
      (dispatch-repaint sub-menu +everywhere+)
      (setf submenu-frame nil) )))

(defmethod arm-branch ((sub-menu menu-button-submenu-pane))
  (with-slots (client frame-manager submenu-frame) sub-menu
    (arm-menu client)
    (if submenu-frame
	(progn (mapc #'destroy-substructure (menu-children sub-menu))
	       (mapc #'disarm-menu (menu-children sub-menu)))
	(progn
	  (mapc #'destroy-substructure (menu-children client))
	  (create-substructure sub-menu sub-menu)))
    (arm-menu sub-menu)))

(defmethod handle-event ((pane menu-button-submenu-pane) (event pointer-button-release-event))
  (destroy-substructure (menu-root pane)))

;;; menu-button-vertical-submenu-pane
(defclass menu-button-vertical-submenu-pane (menu-button-submenu-pane) ())

(let* ((left-padding 10)
       (widget-size  5)
       (right-padding 4)
       (widget-width widget-size)
       (widget-height (* 2 widget-size))
       (total-width (+ left-padding widget-width right-padding))
       (total-height widget-height))

  (defmethod compose-space ((gadget menu-button-vertical-submenu-pane) &key width height)
    (declare (ignorable width height))
    (multiple-value-bind (width min-width max-width height min-height max-height)
        (space-requirement-components (call-next-method))
      (declare (ignorable max-width))
      (make-space-requirement :min-width (+ min-width total-width)
                              :width (+ width total-width)
                              :max-width +fill+
                              :min-height (max min-height total-height)
                              :height (max height total-height)
                              :max-height (if (zerop max-height) ; make-space-requirements default maximums are zero..
                                              0
                                              (max max-height total-height)))))

  (defmethod handle-repaint ((pane menu-button-vertical-submenu-pane) region)
    (call-next-method)
    (multiple-value-bind (x1 y1 x2 y2)
        (bounding-rectangle* (sheet-region pane))
      (when (and (> (- x2 x1) total-width)
                 (> (- y2 y1) total-height))
        (let* ((center (/ (+ y1 y2) 2))
               (vbase (- center (/ widget-height 2)))
               (hbase (+ (- x2 total-width) left-padding))
               (shape (list hbase vbase
                            (+ hbase widget-size) (+ vbase widget-size)
                            hbase (+ vbase (* 2 widget-size)))))
          (draw-polygon* pane shape :ink +black+))))))

;;; menu-divider-leaf-pane

(defclass menu-divider-leaf-pane (standard-gadget)
  ((label :initform nil :initarg :label)))

(defparameter *labelled-divider-text-style* (make-text-style :sans-serif :roman :small))

(defmethod destroy-substructure ((object menu-divider-leaf-pane)))
(defmethod arm-menu ((object menu-divider-leaf-pane)))
(defmethod disarm-menu ((object menu-divider-leaf-pane)))

(defmethod compose-space ((gadget menu-divider-leaf-pane) &key width height)
  (declare (ignorable width height))
  (flet ((make-sr (w h)
           (make-space-requirement :min-width w   :width w
                                   :min-height h  :height h :max-height h)))
    (let ((label (slot-value gadget 'label)))
      (if label
          (multiple-value-bind (width height fx fy baseline)
              (text-size gadget label :text-style *labelled-divider-text-style*)
            (declare (ignore fx fy height baseline))
            (make-sr width (+ 0
                              (text-style-ascent *labelled-divider-text-style* gadget)
                              (text-style-descent *labelled-divider-text-style* gadget))))
          (make-sr 0 4)))))


(defmethod handle-repaint ((pane menu-divider-leaf-pane) region)
  (let ((label (slot-value pane 'label)))   
    (multiple-value-bind (x1 y1 x2 y2)
        (bounding-rectangle* (sheet-region pane))
      (declare (ignore y2))
      (if label
          (multiple-value-bind (width height fx fy baseline)
              (text-size pane label :text-style *labelled-divider-text-style*)
            (declare (ignore height fx fy))
            (let ((tx0 (+ x1 (/ (- (- x2 x1) width) 2)))
                  (ty0 (+ 1 y1 baseline)))
            (draw-line* pane tx0 (1+ ty0) (+ tx0 width) (1+ ty0) :ink *3d-dark-color*)
            (draw-text* pane label tx0 ty0
                        :text-style *labelled-divider-text-style*)))
          (progn
            (draw-line* pane x1 (1+ y1) x2 (1+ y1) :ink *3d-dark-color*)
            (draw-line* pane x1 (+ 2 y1) x2 (+ 2 y1) :ink *3d-light-color*))))))


;;; Menu creation from command tables

(defparameter *enabled-text-style*  (make-text-style :sans-serif :roman :normal))
(defparameter *disabled-text-style* (make-text-style :sans-serif :roman :normal))

(defun make-menu-button-from-menu-item (item client
					&key (bottomp nil)
                                        (vertical nil)
					command-table
					(presentation-type 'menu-item))
  (declare (ignore command-table))
  (let ((name (command-menu-item-name item))
	(type (command-menu-item-type item))
	(value (command-menu-item-value item))
	(frame *application-frame*)
	(manager (frame-manager *application-frame*)))
    (case type
      (:command
       (let ((command-name (if (consp value) (car value) value)))
         (if (command-enabled command-name frame)
             (make-pane-1 manager frame 'menu-button-leaf-pane
                          :label name
                          :text-style *enabled-text-style*
                          :client client
                          :vertical vertical
                          :value-changed-callback
                          #'(lambda (gadget val)
                              (declare (ignore gadget val))
                              (throw-object-ptype item presentation-type)))
             (let ((pane (make-pane-1 manager frame 'menu-button-leaf-pane
                            :label name
                            :text-style *disabled-text-style*
                            :client client
                            :vertical vertical
                            :value-changed-callback
                            #'(lambda (gadget val)
                                (declare (ignore gadget val))
                                nil))))
               (deactivate-gadget pane)
               pane))))
      (:function
        (make-pane-1 manager frame 'menu-button-leaf-pane
                     :label name
                     :text-style *enabled-text-style*
                     :client client
                     :vertical vertical
                     :value-changed-callback
                     #'(lambda (gadget val)
                         (declare (ignore gadget val))
                         ;; FIXME: the spec requires us to pass a gesture to the
                         ;; function, but value-changed-callback doesn't provide
                         ;; one, so we pass NIL for now.
                         ;; FIXME: We don't have a numeric argument, either.
                         (let ((command (funcall value nil nil)))
                           (throw-object-ptype command 'command)))))
      (:divider
       (make-pane-1 manager frame 'menu-divider-leaf-pane
                    :label name
                    :vertical vertical
                    :client client))
      (:menu
        (make-pane-1 manager frame (if vertical
                                       'menu-button-vertical-submenu-pane
                                       'menu-button-submenu-pane)
		     :label name
		     :client client
                     :vertical vertical
		     :frame-manager manager
		     :command-table value
		     :bottomp bottomp))
      (otherwise (error "Don't know how to create a menu button for ~W" type)))))

;;
;; MENU-BAR
;;
(defclass menu-button-hrack-pane (hrack-pane) ())

(defclass menu-bar (menu-button-hrack-pane
                    permanent-medium-sheet-output-mixin)
  ((items :initform nil)
   (armed :initform nil)))

(defmethod initialize-instance :after ((pane menu-bar)
				       &rest args
				       &key
				       &allow-other-keys)
  (declare (ignore args))
  (setf (slot-value pane 'items) (copy-list (sheet-children pane)))
  (loop for child in (menu-children pane)
	do (setf (gadget-client child) pane)))

(defmethod menu-children ((menu-bar menu-bar))
  (slot-value menu-bar 'items))

(defmethod menu-root ((object menu-bar))
  object)

(defmethod destroy-substructure ((object menu-bar))
  (loop for child in (menu-children object)
	do (progn (destroy-substructure child)
		  (dispatch-repaint child (sheet-region child))))
  (setf (slot-value object 'armed) nil))

(defmethod arm-menu ((object menu-bar))
  (setf (slot-value object 'armed) t))

(defmethod disarm-menu ((object menu-bar))
  (setf (slot-value object 'armed) nil))

(defun make-menu-bar (command-table 
		      &key width height
		           (max-width +fill+) max-height
			   min-width min-height)
  (with-slots (menu) (find-command-table command-table)
    (make-pane-1 *pane-realizer* *application-frame*
		 'menu-bar
		 :background *3d-normal-color*
		 :width width :height height
		 :max-width max-width :max-height max-height
		 :min-width min-width :min-height min-height
		 :contents
		 (append
		  (loop for item in menu
		      collect 
                        (make-menu-button-from-menu-item
			 item nil
			 :bottomp t
			 :vertical nil
			 :command-table command-table))
		  (list +fill+)))))

(defmethod handle-repaint ((pane menu-bar) region)
  (declare (ignore region))
  (with-slots (border-width) pane
    (multiple-value-call #'draw-bordered-rectangle*
      pane
      (bounding-rectangle* (sheet-region pane))
      :style :outset
      :border-width 2)))

(defmethod compose-space ((pane menu-bar) &key width height)
  (declare (ignore width height))
  (space-requirement+
   (call-next-method)
   (make-space-requirement :height 4 :max-height 4 :min-height 4)))

(defmethod box-layout-mixin/horizontally-allocate-space
    ((pane menu-bar) real-width real-height)
  (with-slots (x-spacing) pane
    (let ((widths
	   (box-layout-mixin/horizontally-allocate-space-aux*
	    pane real-width real-height))
	  (x 2))
      (loop
	  for child in (box-layout-mixin-clients pane)
	  for width in widths
	  do
	    (when (box-client-pane child)
	      (layout-child (box-client-pane child)
			    :expand
			    :expand
			    x
			    2
			    width
			    (- real-height 4)))
	    (incf x width)
	    (incf x x-spacing)))))

(defmethod display-command-table-menu ((command-table standard-command-table)
                                       (stream fundamental-output-stream)
                                       &rest args
                                       &key max-width max-height n-rows n-columns
                                       x-spacing y-spacing initial-spacing
                                       row-wise (cell-align-x :left)
                                       (cell-align-y :top) (move-cursor t))
  (formatting-item-list (stream :max-width max-width :max-height max-height :n-rows n-rows
                                :n-columns n-columns :x-spacing x-spacing :y-spacing y-spacing
                                :initial-spacing initial-spacing :row-wise row-wise
                                :move-cursor move-cursor)
    (map-over-command-table-menu-items
     #'(lambda (item-name accelerator item)
         (declare (ignore accelerator))
         (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
           (cond ((eq (command-menu-item-type item) :menu)
                  (with-text-style (stream (make-text-style :serif '(:bold :italic) nil))
                    (write-string item-name stream)
                    (terpri stream))
                  (surrounding-output-with-border (stream)
                    (apply #'display-command-table-menu
                           (find-command-table (command-menu-item-value item))
                           stream args)))
                 ((eq (command-menu-item-type item) :command)
                  (let ((name (command-menu-item-name item)))
                    (with-output-as-presentation (stream (command-menu-item-value item) 'command)
                      (write-string name stream)))))))
     command-table)))

(defmethod display-command-menu (frame (stream fundamental-output-stream)
                                 &rest args &key
                                 (command-table (frame-command-table frame))
                                 initial-spacing row-wise max-width
                                 max-height n-rows n-columns
                                 (cell-align-x :left) (cell-align-y :top))
  (declare (ignore initial-spacing row-wise max-width max-height
                   n-rows n-columns cell-align-x cell-align-y))
  (with-keywords-removed (args (:command-table))
    (apply #'display-command-table-menu command-table stream args)))
