;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

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

(in-package :CLIM-INTERNALS)

(defmethod menu-root ((button menu-button-pane))
  (menu-root (gadget-client button)))

(defmethod arm-menu ((button menu-button-pane))
  (with-slots (client armed id) button
    (unless armed
      (arm-menu client)
      (mapc #'disarm-menu (menu-children client))
      (setf armed t)
      (armed-callback button client id))
    (dispatch-repaint button (sheet-region button))))

(defmethod disarm-menu ((button menu-button-pane))
  (with-slots (client armed id) button
    (when armed
      (setf armed nil)
      (disarmed-callback button client id)
      (dispatch-repaint button (sheet-region button)))))

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
            (draw-edges-lines* gadget 0 0 (1- w) (1- h)) ;(- w 2) (- h 2)
            (draw-text* gadget label (round w 2) (round h 2)
                        :align-x :center :align-y :center)))))))

(defun menu-draw-unhighlighted (gadget)
  (when (sheet-mirror gadget)           ;XXX only do this when the gadget is realized.
    (with-special-choices (gadget)
      (with-slots (label) gadget
        (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region gadget)
          (let ((w (- x2 x1))
                (h (- y2 y1)))
            (draw-rectangle* gadget -1 -1 w h ;-1 -1 x2 y2
                             :ink (gadget-normal-color gadget)
                             :filled t)
            (draw-text* gadget label (round w 2) (round h 2)
                        :align-x :center :align-y :center)))))))

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
  (with-slots (armed) button
      (setf armed nil)))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-button-release-event))
  (with-slots (armed label client id) pane
    (when armed
      (value-changed-callback pane client id label)
      (disarm-menu pane)
      (destroy-substructure (menu-root pane)))))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-exit-event))
  (disarm-menu pane))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-ungrab-event))
  (destroy-substructure (menu-root pane)))

(defmethod handle-repaint ((pane menu-button-leaf-pane) region)
  (declare (ignore region))
  (with-slots (armed) pane
    (if armed
	(menu-draw-highlighted pane)
	(menu-draw-unhighlighted pane))))

;;; menu-button-submenu-pane

(defclass menu-button-submenu-pane (menu-button-pane)
  ((frame-manager :initform nil :initarg :frame-manager)
   (submenu-frame :initform nil)
   (bottomp :initform nil :initarg :bottomp)
   (command-table :initform nil :initarg :command-table)))

(defmethod menu-children ((submenu menu-button-submenu-pane))
  (with-slots (submenu-frame) submenu
    (if submenu-frame
	(sheet-children (first (sheet-children (frame-pane submenu-frame))))
	'())))

(defun create-substructure (sub-menu client)
  (let* ((frame *application-frame*)
	 (manager (frame-manager frame))
	 (items (mapcar #'(lambda (item)
			    (make-menu-button-from-menu-item item client))
			(slot-value (find-command-table (slot-value sub-menu 'command-table)) 'menu)))
	 (rack (make-pane-1 manager frame 'vrack-pane
			    :background +grey80+ :contents items))
	;(raised (make-pane-1 manager frame 'raised-pane :contents (list rack)))
	 (raised (make-pane-1 manager frame 'raised-pane :border-width 2 :background +gray80+ :contents (list rack))))
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
	    (adopt-frame manager submenu-frame)))))))

(defmethod destroy-substructure ((sub-menu menu-button-submenu-pane))
  (with-slots (frame-manager submenu-frame) sub-menu
    (when submenu-frame
      (mapc #'destroy-substructure (menu-children sub-menu))
      (disown-frame frame-manager submenu-frame)
      (setf submenu-frame nil))))

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

(defmethod handle-repaint ((pane menu-button-submenu-pane) region)
  (declare (ignore region))
  (with-slots (submenu-frame) pane
    (if submenu-frame
	(menu-draw-highlighted pane)
	(menu-draw-unhighlighted pane))))

;; Menu creation from command tables

;; for now, accept only types :command and :menu, and only 
;; command names as values of :command
(defun make-menu-button-from-menu-item (item client &key (bottomp nil))
  (let ((name (command-menu-item-name item))
	(type (command-menu-item-type item))
	(value (command-menu-item-value item))
	(frame *application-frame*)
	(manager (frame-manager *application-frame*)))
    (if (eq type :command)
	(make-pane-1 manager frame 'menu-button-leaf-pane
		     :label name
		     :client client
		     :value-changed-callback
		     #'(lambda (gadget val)
			 (declare (ignore gadget val))
			 (funcall value)))
	(make-pane-1 manager frame 'menu-button-submenu-pane
		     :label name
		     :client client
		     :frame-manager manager
		     :command-table value
		     :bottomp bottomp))))

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
  (loop for child in (sheet-children pane)
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
    (progn ;;raising () ;; XXX temporary medicine as RAISED is borken --GB
      (make-pane-1 *pane-realizer* *application-frame*
          'menu-bar
	  :background +grey80+						
	  :width width :height height
	  :max-width max-width :max-height max-height
	  :min-width min-width :min-height min-height
	  :contents
	  (loop for item in menu
		collect 
		 (make-menu-button-from-menu-item item nil :bottomp t))))))
