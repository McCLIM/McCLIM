;;; -*- Mode: Lisp; Package: CLIM-GRAPHIC-FORMS; -*-

;;; (c) 2006-2007 Jack D. Unrue (jdunrue (at) gmail (dot) com)
;;; based on the null backend by:
;;;  (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-graphic-forms)

;;;
;;; base widget behaviors
;;;

(defmethod activate-gadget ((widget gfw-widget-pane-mixin))
  (with-slots (active-p) widget
    (unless active-p
      (gfw:enable (sheet-mirror widget) t)))
  (call-next-method))

(defmethod deactivate-gadget ((widget gfw-widget-pane-mixin))
  (with-slots (active-p) widget
    (unless active-p
      (gfw:enable (sheet-mirror widget) nil)))
  (call-next-method))

;;;
;;; menus
;;;

(defun append-menu-items (port menu-pane)
  (let ((table-name (command-table menu-pane)))
    (when table-name
      (let ((table (find-command-table table-name)))
        (dolist (thing (slot-value table 'climi::menu))
          (let* ((sub-table-name (if (eql (command-menu-item-type thing) :menu)
                                   (command-table-name thing)
                                   nil))
                 (sub-pane (climi::make-menu-button-from-menu-item
                             thing nil :command-table sub-table-name)))
            (if (eql (command-menu-item-type thing) :command)
              (setf (gadget-label sub-pane) (climi::command-menu-item-name thing)
                    (item sub-pane) thing)
              (if (climi::command-menu-item-name thing)
                  (setf (label sub-pane) (climi::command-menu-item-name thing))))
            (setf (sheet-parent sub-pane) menu-pane)
            (realize-mirror port sub-pane))))))
  (dolist (menu-item (contents menu-pane))
    (unless (integerp menu-item)
      (setf (sheet-parent menu-item) menu-pane)
      (realize-mirror port menu-item))))

(defmethod make-pane-2 ((type (eql 'climi::menu-bar)) &rest initargs)
  (apply #'make-instance 'gfw-menu-bar-pane initargs))

(defmethod realize-mirror ((port graphic-forms-port) (pane gfw-menu-bar-pane))
  (let* ((top-level (sheet-mirror (sheet-parent (sheet-parent pane))))
         (mirror (gfw:menu-bar top-level)))
    (setf (sheet mirror) pane)
    (climi::port-register-mirror port pane mirror)
    (append-menu-items port pane)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (pane gfw-menu-bar-pane))
  (let ((mirror (climi::port-lookup-mirror port pane)))
    (climi::port-unregister-mirror port pane mirror)))

(defmethod make-pane-2 ((type (eql 'climi::menu-button-submenu-pane)) &rest initargs)
  (apply #'make-instance 'gfw-menu-pane initargs))

(defmethod realize-mirror ((port graphic-forms-port) (pane gfw-menu-pane))
  (let* ((parent (sheet-mirror (sheet-parent pane)))
         (mirror (make-instance 'gfw-menu :sheet pane :handle (gfs::create-popup-menu))))
    (gfw:append-submenu parent (label pane) mirror nil)
    (climi::port-register-mirror port pane mirror)
    (append-menu-items port pane)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (pane gfw-menu-pane))
  (let ((mirror (climi::port-lookup-mirror port pane)))
    (climi::port-unregister-mirror port pane mirror)))

(defmethod make-pane-2 ((type (eql 'climi::menu-button-leaf-pane)) &rest initargs)
  (apply #'make-instance 'gfw-menu-item-pane initargs))

(defmethod realize-mirror ((port graphic-forms-port) (pane gfw-menu-item-pane))
  (let* ((menu (sheet-mirror (sheet-parent pane)))
         (mirror (gfw:append-item menu (gadget-label pane) *pane-dispatcher* nil nil 'gfw-menu-item)))
    (setf (sheet mirror) pane)
    (climi::port-register-mirror port pane mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (pane gfw-menu-item-pane))
  (let ((mirror (climi::port-lookup-mirror port pane)))
    (climi::port-unregister-mirror port pane mirror)))

(defmethod realize-mirror ((port graphic-forms-port) (pane climi::menu-divider-leaf-pane))
  (let* ((menu (sheet-mirror (sheet-parent pane)))
         (mirror (gfw:append-separator menu)))
    (climi::port-register-mirror port pane mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (pane climi::menu-divider-leaf-pane))
  (let ((mirror (climi::port-lookup-mirror port pane)))
    (climi::port-unregister-mirror port pane mirror)))

;;;
;;; other gadgets
;;;

(defmethod realize-mirror ((port graphic-forms-port) (gadget push-button))
  #+nil (gfs::debug-format "realizing ~a~%" gadget)
  (let* ((parent-mirror (sheet-mirror (sheet-parent gadget)))
         (mirror (make-instance 'gfw-button
				:sheet gadget
				:parent parent-mirror
				:dispatcher *pane-dispatcher*
				:style '(:push-button))))
    (if (gadget-label gadget)
      (setf (gfw:text mirror) (gadget-label gadget)))
    (climi::port-register-mirror port gadget mirror)
    mirror))

(defmethod realize-mirror ((port graphic-forms-port) (gadget toggle-button))
  #+nil (gfs::debug-format "realizing ~a~%" gadget)
  (let* ((parent-mirror (sheet-mirror (sheet-parent gadget)))
         (mirror (make-instance 'gfw-button :parent parent-mirror :style '(:check-box))))
    (if (gadget-label gadget)
      (setf (gfw:text mirror) (gadget-label gadget)))
    (climi::port-register-mirror port gadget mirror)
    mirror))

(defmethod realize-mirror ((port graphic-forms-port) (gadget scroll-bar))
  #+nil (gfs::debug-format "realizing ~a~%" gadget)
  (let* ((parent-mirror (sheet-mirror (sheet-parent gadget)))
         (mirror (make-instance 'gfw-scroll-bar :parent parent-mirror :style :vertical)))
    (climi::port-register-mirror port gadget mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (gadget value-gadget))
  (let ((mirror (climi::port-lookup-mirror port gadget)))
    (climi::port-unregister-mirror port gadget mirror)))

(defmethod destroy-mirror ((port graphic-forms-port) (gadget action-gadget))
  (let ((mirror (climi::port-lookup-mirror port gadget)))
    (climi::port-unregister-mirror port gadget mirror)))

;;;
;;; layout
;;;

(defmethod compose-space ((gadget action-gadget) &key width height)
  (declare (ignore width height))
  (let ((mirror (climi::port-lookup-mirror (port gadget) gadget))
        (pref-size (gfs:make-size :width 100 :height 100)))
    (if mirror
      (setf pref-size (gfw:preferred-size mirror -1 -1))
      (progn
        (setf mirror (make-instance 'gfw:button :parent (sheet-mirror (sheet-parent gadget)) :text (gadget-label gadget)))
        (setf pref-size (gfw:preferred-size mirror -1 -1))
        (gfs:dispose mirror)
        (setf mirror nil)))
    (make-space-requirement :width (gfs:size-width pref-size)
                            :height (gfs:size-height pref-size))))
