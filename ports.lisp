;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-display-variable (s)
  "Given a string in standard X11 display format host-name:display-number:screen-number,
returns a list in CLIM X11 format (:x11 :host host-name :display-id display-number
:screen-id screen-number)."
  (let* ((colon (position #\: s))
	 (dot (position #\. s :start colon))
	 (host-name (if (zerop colon) "localhost" (subseq s 0 colon)))
	 (display-number (parse-integer s :start (1+ colon) :end dot))
	 (screen-number (if dot (parse-integer s :start (1+ dot)) 0)))
    (list :x11 :host host-name :display-id display-number :screen-id screen-number)))

(defun get-environment-variable (string)
  #+excl (sys:getenv string)
  #+cmu (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (sys::getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #-(or excl cmu clisp sbcl) (error "GET-ENVIRONMENT-VARIABLE not implemented")))
		
(defvar *default-server-path*
    #+unix (parse-display-variable (get-environment-variable "DISPLAY")))

(defvar *all-ports* nil)

(defclass port ()
  ((server-path :initform nil
		:initarg :server-path
		:reader port-server-path)
   (properties :initform nil
	       :initarg :properties)
   (grafts :initform nil
	   :accessor port-grafts)
   (frame-managers :initform nil
		   :reader frame-managers)
   (sheet->mirror :initform (make-hash-table :test #'eq))
   (mirror->sheet :initform (make-hash-table :test #'eq))
   (pixmap->mirror :initform (make-hash-table :test #'eq))
   (mirror->pixmap :initform (make-hash-table :test #'eq))
   (keyboard-input-focus :initform nil
			 :initarg :keyboard-input-focus
			 :accessor port-keyboard-input-focus)
   )
  )

(defun find-port (&key (server-path *default-server-path*))
  (loop for port in *all-ports*
      if (equal server-path (port-server-path port))
      do (return port)
      finally (let ((port-type (get (first server-path) :port-type))
		    port)
		(if (null port-type)
		    (error "Don't know how to make a port of type ~S" server-path))
		(setq port (funcall 'make-instance port-type :server-path server-path))
		(push port *all-ports*)
		(return port))))

(defmethod port-lookup-mirror ((port port) (sheet mirrored-sheet-mixin))
  (gethash sheet (slot-value port 'sheet->mirror)))

(defmethod port-lookup-sheet ((port port) mirror)
  (gethash mirror (slot-value port 'mirror->sheet)))

(defmethod port-register-mirror ((port port) (sheet mirrored-sheet-mixin) mirror)
  (setf (gethash sheet (slot-value port 'sheet->mirror)) mirror)
  (setf (gethash mirror (slot-value port 'mirror->sheet)) sheet)
  nil)

(defmethod port-unregister-mirror ((port port) (sheet mirrored-sheet-mixin) mirror)
  (remhash sheet (slot-value port 'sheet->mirror))
  (remhash mirror (slot-value port 'mirror->sheet))
  nil)

(defmethod realize-mirror ((port port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to realize the mirror of a generic mirrored-sheet"))

(defmethod destroy-mirror ((port port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to destroy the mirror of a generic mirrored-sheet"))

(defmethod mirror-transformation ((port port) mirror)
  (declare (ignore mirror))
  (error "MIRROR-TRANSFORMATION is not implemented for generic ports"))

(defmethod port-properties ((port port) indicator)
  (with-slots (properties) port
    (getf properties indicator)))

(defmethod (setf port-properties) (value (port port) indicator)
  (with-slots (properties) port
    (setf (getf properties indicator) value)))

(defmethod get-next-event ((port port) &key wait-function timeout)
  (declare (ignore wait-function timeout))
  (error "Calling GET-NEXT-EVENT on a PORT protocol class"))

(defmethod process-next-event ((port port) &key wait-function timeout)
  (let ((event (get-next-event port :wait-function wait-function :timeout timeout)))
    (cond
     ((null event) nil)
     ((eq event :timeout) (values nil :timeout))
     (t
      (distribute-event port event)
      t))))

(defmethod distribute-event ((port port) event)
  (cond
   ((typep event 'keyboard-event)
    (dispatch-event (or (port-keyboard-input-focus port)
			(event-sheet event)) event))
   ((typep event 'window-event)
;    (dispatch-event (window-event-mirrored-sheet event) event))
    (dispatch-event (event-sheet event) event))
   ((typep event 'pointer-event)
    (dispatch-event (event-sheet event) event))
   ((typep event 'timer-event)
    (error "Where do we send timer-events?"))
   (t
    (error "Unknown event ~S received in DISTRIBUTE-EVENT" event))))

(defun map-over-ports (function)
  (mapc function *all-ports*))

(defmethod restart-port ((port port))
  (reset-watcher port :restart)
  nil)

(defmethod destroy-port ((port port))
  (reset-watcher port :destroy)
  (setf *all-ports* (remove port *all-ports*)))

(defmethod add-watcher ((port port) watcher)
  (declare (ignore watcher))
  nil)

(defmethod delete-watcher ((port port) watcher)
  (declare (ignore watcher))
  nil)

(defmethod reset-watcher ((port port) how)
  (declare (ignore how))
  nil)

(defmethod make-graft ((port port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'graft
		 :port port :mirror nil
		 :orientation orientation :units units)))
    (push graft (port-grafts port))
    graft))

(defmethod make-medium ((port port) sheet)
  (make-instance 'basic-medium :port port :graft (graft sheet) :sheet sheet))

;;; Pixmap

(defmethod port-lookup-mirror ((port port) (pixmap pixmap))
  (gethash pixmap (slot-value port 'pixmap->mirror)))

(defmethod port-lookup-pixmap ((port port) mirror)
  (gethash mirror (slot-value port 'mirror->pixmap)))

(defmethod port-register-mirror ((port port) (pixmap pixmap) mirror)
  (setf (gethash pixmap (slot-value port 'pixmap->mirror)) mirror)
  (setf (gethash mirror (slot-value port 'mirror->pixmap)) pixmap)
  nil)

(defmethod port-unregister-mirror ((port port) (pixmap pixmap) mirror)
  (remhash pixmap (slot-value port 'pixmap->mirror))
  (remhash mirror (slot-value port 'mirror->pixmap))
  nil)

(defmethod realize-mirror ((port port) (pixmap mirrored-pixmap))
  (declare (ignorable port pixmap))
  (error "Don't know how to realize the mirror on a generic port"))

(defmethod destroy-mirror ((port port) (pixmap mirrored-pixmap))
  (declare (ignorable port pixmap))
  (error "Don't know how to destroy the mirror on a generic port"))

(defmethod port-allocate-pixmap ((port port) sheet width height)
  (declare (ignore sheet width height))
  (error "ALLOCATE-PIXMAP is not implemented for generic PORTs"))

(defmethod port-deallocate-pixmap ((port port) pixmap)
  (declare (ignore pixmap))
  (error "DEALLOCATE-PIXMAP is not implemented for generic PORTs"))
