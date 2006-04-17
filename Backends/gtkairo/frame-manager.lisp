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

(defun frob-stupid-type-spec (type)
  (when (get type 'climi::concrete-pane-class-name)
    (setf type (get type 'climi::concrete-pane-class-name)))
  (class-name
   (or (find-class
	(intern (concatenate 'string (symbol-name type) "-PANE") :climi)
	nil)
       (find-class type))))

(defmethod make-pane-1
    ((fm gtkairo-frame-manager) (frame application-frame) type &rest initargs)
  (apply #'make-pane-2
	 (frob-stupid-type-spec type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 initargs))

(defmethod make-pane-2 (type &rest initargs)
  (apply #'make-instance type initargs))

(defmethod make-pane-2 ((type (eql 'push-button-pane)) &rest initargs)
  (apply #'make-instance 'gtk-button initargs))

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
  ;; doesn't really work yet
  (call-next-method)
  #+(or)
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
