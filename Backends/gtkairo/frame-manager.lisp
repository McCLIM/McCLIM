;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)
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

(in-package :clim-gtkairo)

(defclass gtkairo-frame-manager (frame-manager)
  ())

;; fixme!  we're supposed to dispatch on the abstract name, not resolve
;; it to the (incorrect) concrete generic class name and dispatch on that.
(defun resolve-abstract-pane-name (type)
  (when (get type 'climi::concrete-pane-class-name)
    (setf type (get type 'climi::concrete-pane-class-name)))
  (class-name
   (or (find-class
	(intern (concatenate 'string (symbol-name type) "-PANE") :climi)
	nil)
       (if (keywordp type)
	   (find-class (intern (symbol-name type) :climi))
	   (find-class type)))))

(defmethod make-pane-1
    ((fm gtkairo-frame-manager) (frame application-frame) type &rest initargs)
  (apply #'make-pane-2
	 (resolve-abstract-pane-name type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 initargs))

;; make CMUCL happy
(defgeneric make-pane-2 (type &rest args &key &allow-other-keys))

(defmethod make-pane-2 (type &rest initargs)
  (apply #'make-instance type initargs))

(defmethod make-pane-2 ((type (eql 'push-button-pane)) &rest initargs)
  (apply #'make-instance 'gtk-button initargs))

(defmethod make-pane-2
    ((type (eql 'climi::menu-button-leaf-pane)) &rest initargs)
  (apply #'make-instance 'gtk-nonmenu initargs))

(defmethod make-pane-2
    ((type (eql 'climi::menu-button-submenu-pane)) &rest initargs)
  (apply #'make-instance 'gtk-menu initargs))

(defmethod make-pane-2 ((type (eql 'climi::menu-bar)) &rest initargs)
  (apply #'make-instance 'gtk-menu-bar initargs))

;;;(defmethod make-pane-2 ((type (eql 'clim:check-box-pane)) &rest initargs)
;;;  (apply #'make-instance gtkairo-check-box-pane initargs))
;;;(defmethod make-pane-2 ((type (eql 'clim:radio-box-pane)) &rest initargs)
;;;  (apply #'make-instance gtkairo-radio-box-pane initargs))

(defmethod make-pane-2 ((type (eql 'clim:slider-pane))
			&rest initargs
			&key orientation)
  (apply #'make-instance
	 (if (eq orientation :vertical)
	     'gtk-vscale
	     'gtk-hscale)
	 initargs))

(defmethod make-pane-2 ((type (eql 'clim:scroll-bar-pane))
			&rest initargs
			&key orientation)
  (apply #'make-instance
	 (if (eq orientation :vertical)
	     'gtk-vscrollbar
	     'gtk-hscrollbar)
	 initargs))

(defmethod make-pane-2 ((type (eql 'clim:toggle-button-pane))
			&rest initargs
			&key indicator-type)
  (apply #'make-instance
	 (ecase indicator-type
	   (:one-of 'gtk-radio-button)
	   ((:some-of nil) 'gtk-check-button))
	 initargs))

(defmethod make-pane-2 ((type (eql 'clim:generic-list-pane)) &rest initargs)
  (apply #'make-instance 'gtk-list initargs))

(defmethod make-pane-2
    ((type (eql 'clim-tab-layout:tab-layout-pane)) &rest initargs)
  (apply #'make-instance 'gtk-tab-layout initargs))

(defmethod make-pane-2 ((type (eql 'clim:label-pane)) &rest initargs)
  (apply #'make-instance 'gtk-label-pane initargs))

(defmethod make-pane-2 ((type (eql 'clim:generic-option-pane)) &rest initargs)
  (apply #'make-instance 'gtk-option-pane initargs))

(defmethod adopt-frame :after
    ((fm gtkairo-frame-manager) (frame application-frame))
  ())

(defmethod note-space-requirements-changed :after ((graft gtkairo-graft) pane)
  ())

;;; ";; Temporary kludge." says the CLX backend.  Ha-ha.
(defmethod adopt-frame
    :before
    ((fm gtkairo-frame-manager) (frame climi::menu-frame))
  (when (eq (slot-value frame 'climi::top) nil)
    (multiple-value-bind (x y)
	(%gdk-display-get-pointer)
      (setf (slot-value frame 'climi::left) x
	    (slot-value frame 'climi::top) y))))

(defmethod adopt-frame
    :after
    ((fm gtkairo-frame-manager) (frame climi::menu-frame))
  (port-enable-sheet (car climi::*all-ports*)
		     (slot-value frame 'climi::top-level-sheet)))

(defmethod frame-manager-menu-choose
    ((frame-manager gtkairo-frame-manager)
     items
     &key associated-window printer presentation-type
	  (default-item nil default-item-p)
	  text-style label cache unique-id id-test cache-value cache-test
	  max-width max-height n-rows n-columns x-spacing y-spacing row-wise
	  cell-align-x cell-align-y scroll-bars pointer-documentation)
  (declare
   ;; XXX hallo?
   (ignore presentation-type default-item default-item-p
	   text-style label cache unique-id id-test cache-value
	   cache-test max-width max-height n-rows n-columns x-spacing
	   y-spacing row-wise cell-align-x cell-align-y scroll-bars
	   pointer-documentation))
  (let* ((frame (if associated-window
		    (pane-frame associated-window)
		    *application-frame*))
	 (port (port frame))
	 (sheet (make-instance 'dummy-context-menu-sheet))
	 (menu (make-context-menu port sheet items :printer printer)))
    (invoke-later
     (lambda ()
       (invoke-later (lambda () (gdk_pointer_ungrab GDK_CURRENT_TIME)))
       (gtk_menu_popup menu
		       (cffi:null-pointer)
		       (cffi:null-pointer)
		       (cffi:null-pointer)
		       (cffi:null-pointer)
		       *last-seen-button*
		       (gtk_get_current_event_time))))
    (let ((event (event-read sheet)))
      ;; `deactivate' is signalled on the menu before `clicked' on the item,
      ;; so let's make sure we have processed all events before deciding
      ;; whether the was a `clicked' or not
      (gtk-main-iteration port)
      (when (typep (event-peek sheet) 'context-menu-clicked-event)
	(setf event (event-read sheet)))
      (etypecase event
	(context-menu-clicked-event
	  (values (event-value event) (event-itemspec event) event))
	(context-menu-cancelled-event
	  nil)))))
