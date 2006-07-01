;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
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

(in-package :clim-internals)

(defvar *default-server-path* nil)

(defvar *server-path-search-order* '(:genera :ms-windows :gtkairo :clx :x11 :opengl :beagle :null))

(defun find-default-server-path ()
  (loop for port in *server-path-search-order*
	if (get port :port-type)
	   do (return-from find-default-server-path (list port))
	finally (error "No CLIM backends have been loaded!")))

(defvar *all-ports* nil)

(defclass basic-port (port)
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
   #+ignore (keyboard-input-focus :initform nil ;; nuked this, see below
			 :initarg :keyboard-input-focus
			 :accessor port-keyboard-input-focus)
   (event-process
    :initform nil
    :initarg  :event-process
    :accessor port-event-process
    :documentation "In a multiprocessing environment, the particular process
                    reponsible for calling PROCESS-NEXT-EVENT in a loop.")

   (lock
    :initform (make-recursive-lock "port lock")
    :accessor port-lock)
   (event-count :initform 0)
   (text-style-mappings :initform (make-hash-table :test #'eq)
                        :reader port-text-style-mappings)
   (pointer-sheet :initform nil :accessor port-pointer-sheet
		  :documentation "The sheet the pointer is over, if any")
   ))

;; Keyboard focus is now managed per-frame rather than per-port,
;; which makes a lot of sense (less sense in the presense of
;; multiple top-level windows, but no one does that yet). The CLIM
;; spec suggests this in a "Minor Issue". So, redirect
;; PORT-KEYBOARD-INPUT-FOCUS to the current application frame
;; for compatibility.

;; Note: This would prevent you from using the function the
;; function to query who currently has the focus. I don't
;; know if this is an intended use or not.

;; The big picture:
;;   PORT-KEYBOARD-INPUT-FOCUS is defined by CLIM 2.0
;;   Our default method on this delegates to KEYBOARD-INPUT-FOCUS
;;    on the current application frame.
;;   %SET-PORT-KEYBOARD-FOCUS is the function which
;;    should be implemented in a McCLIM backend and
;;    does the work of changing the focus.
;;   A method on (SETF KEYBOARD-INPUT-FOCUS) brings them together,
;;    calling %SET-PORT-KEYBOARD-FOCUS.

(defgeneric port-keyboard-input-focus (port))
(defgeneric (setf port-keyboard-input-focus) (focus port))

(defmethod port-keyboard-input-focus (port)
  (declare (ignore port))
  (when *application-frame*
    (keyboard-input-focus *application-frame*)))

(defmethod (setf port-keyboard-input-focus) (focus port)
  (when focus
    (if (pane-frame focus)
        (setf (keyboard-input-focus (pane-frame focus)) focus)
        (%set-port-keyboard-focus port focus))))

;; This is not in the CLIM spec, but since (setf port-keyboard-input-focus)
;; now calls (setf keyboard-input-focus), we need something concrete the
;; backend can implement to set the focus.    
(defmethod %set-port-keyboard-focus (port focus &key timestamp)
  (declare (ignore focus timestamp))  
  (warn "%SET-PORT-KEYBOARD-FOCUS is not implemented on ~W" port))
  

(defun find-port (&key (server-path *default-server-path*))
  (if (null server-path)
      (setq server-path (find-default-server-path)))
  (if (atom server-path)
      (setq server-path (list server-path)))
  (setq server-path (funcall (get (first server-path) :server-path-parser) server-path))
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

(defmethod initialize-instance :after ((port basic-port) &rest args)
  (declare (ignorable args))
  )

(defmethod destroy-port :before ((port basic-port))
  (when (and *multiprocessing-p* (port-event-process port))
    (destroy-process (port-event-process port))
    (setf (port-event-process port) nil)))

(defmethod port-lookup-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (gethash sheet (slot-value port 'sheet->mirror)))

(defmethod port-lookup-sheet ((port basic-port) mirror)
  (gethash mirror (slot-value port 'mirror->sheet)))

(defmethod port-register-mirror ((port basic-port) (sheet mirrored-sheet-mixin) mirror)
  (setf (gethash sheet (slot-value port 'sheet->mirror)) mirror)
  (setf (gethash mirror (slot-value port 'mirror->sheet)) sheet)
  nil)

(defmethod port-unregister-mirror ((port basic-port) (sheet mirrored-sheet-mixin) mirror)
  (remhash sheet (slot-value port 'sheet->mirror))
  (remhash mirror (slot-value port 'mirror->sheet))
  nil)

(defmethod realize-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to realize the mirror of a generic mirrored-sheet"))

(defmethod destroy-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to destroy the mirror of a generic mirrored-sheet"))

(defmethod mirror-transformation ((port basic-port) mirror)
  (declare (ignore mirror))
  (error "MIRROR-TRANSFORMATION is not implemented for generic ports"))

(defmethod port-properties ((port basic-port) indicator)
  (with-slots (properties) port
    (getf properties indicator)))

(defmethod (setf port-properties) (value (port basic-port) indicator)
  (with-slots (properties) port
    (setf (getf properties indicator) value)))

(defmethod get-next-event ((port basic-port) &key wait-function timeout)
  (declare (ignore wait-function timeout))
  (error "Calling GET-NEXT-EVENT on a PORT protocol class"))

(defmethod get-next-event :after ((port basic-port) &key wait-function timeout)
  (declare (ignore wait-function timeout))
  (with-slots (event-count) port
    (incf event-count)))

(defmethod process-next-event ((port basic-port) &key wait-function timeout)
  (let ((event (get-next-event port
			       :wait-function wait-function
			       :timeout timeout)))
    (cond
     ((null event) nil)
     ((eq event :timeout) (values nil :timeout))
     (t
      (distribute-event port event)
      t))))

(defmethod distribute-event ((port basic-port) event)
  (cond
   ((typep event 'keyboard-event)
    (dispatch-event (or #+ignore(port-keyboard-input-focus port) (event-sheet event))
		    event))
   ((typep event 'window-event)
;   (dispatch-event (window-event-mirrored-sheet event) event)
    (dispatch-event (event-sheet event) event))
   ((typep event 'pointer-event)
    (dispatch-event (event-sheet event) event))
   ((typep event 'window-manager-delete-event)
    ;; not sure where this type of event should get sent - mikemac
    ;; This seems fine; will be handled by the top-level-sheet-pane - moore
    (dispatch-event (event-sheet event) event))
   ((typep event 'timer-event)
    (error "Where do we send timer-events?"))
   (t
    (error "Unknown event ~S received in DISTRIBUTE-EVENT" event))))

(defmacro with-port-locked ((port) &body body)
  (let ((fn (gensym "CONT.")))
    `(labels ((,fn ()
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-port-locked ,port #',fn))))

(defmethod invoke-with-port-locked ((port basic-port) continuation)
  (with-recursive-lock-held ((port-lock port))
    (funcall continuation)))

(defun map-over-ports (function)
  (mapc function *all-ports*))

(defmethod restart-port ((port basic-port))
  (reset-watcher port :restart)
  nil)

(defmethod destroy-port ((port basic-port))
  (reset-watcher port :destroy))

(defmethod destroy-port :around ((port basic-port))
  (unwind-protect
       (call-next-method)
    (setf *all-ports* (remove port *all-ports*))))

(defmethod add-watcher ((port basic-port) watcher)
  (declare (ignore watcher))
  nil)

(defmethod delete-watcher ((port basic-port) watcher)
  (declare (ignore watcher))
  nil)

(defmethod reset-watcher ((port basic-port) how)
  (declare (ignore how))
  nil)

(defmethod make-graft ((port basic-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'graft
		 :port port :mirror nil
		 :orientation orientation :units units)))
    (push graft (port-grafts port))
    graft))

#||
(defmethod make-medium ((port basic-port) sheet)
  (make-instance 'basic-medium :port port :graft (graft sheet) :sheet sheet))
||#

;;; Pixmap

(defmethod port-lookup-mirror ((port basic-port) (pixmap pixmap))
  (gethash pixmap (slot-value port 'pixmap->mirror)))

(defmethod port-lookup-pixmap ((port basic-port) mirror)
  (gethash mirror (slot-value port 'mirror->pixmap)))

(defmethod port-register-mirror ((port basic-port) (pixmap pixmap) mirror)
  (setf (gethash pixmap (slot-value port 'pixmap->mirror)) mirror)
  (setf (gethash mirror (slot-value port 'mirror->pixmap)) pixmap)
  nil)

(defmethod port-unregister-mirror ((port basic-port) (pixmap pixmap) mirror)
  (remhash pixmap (slot-value port 'pixmap->mirror))
  (remhash mirror (slot-value port 'mirror->pixmap))
  nil)

(defmethod realize-mirror ((port basic-port) (pixmap mirrored-pixmap))
  (declare (ignorable port pixmap))
  (error "Don't know how to realize the mirror on a generic port"))

(defmethod destroy-mirror ((port basic-port) (pixmap mirrored-pixmap))
  (declare (ignorable port pixmap))
  (error "Don't know how to destroy the mirror on a generic port"))

(defmethod port-allocate-pixmap ((port basic-port) sheet width height)
  (declare (ignore sheet width height))
  (error "ALLOCATE-PIXMAP is not implemented for generic PORTs"))

(defmethod port-deallocate-pixmap ((port basic-port) pixmap)
  (declare (ignore pixmap))
  (error "DEALLOCATE-PIXMAP is not implemented for generic PORTs"))


(defgeneric port-force-output (port)
  (:documentation "Flush the output buffer of PORT, if there is one.")) 

(defmethod port-force-output ((port basic-port))
  (values))

(defgeneric port-grab-pointer (port pointer sheet)
  (:documentation "Grab the specified pointer, for implementing TRACKING-POINTER."))

(defgeneric port-ungrab-pointer (port pointer sheet)
  (:documentation "Ungrab the specified pointer, for implementing TRACKING-POINTER."))

(defmethod port-grab-pointer ((port basic-port) pointer sheet)
  (declare (ignorable port pointer sheet))
  (warn "Port ~A has not implemented pointer grabbing." port))

(defmethod port-ungrab-pointer ((port basic-port) pointer sheet)
  (declare (ignorable port pointer sheet))
  (warn "Port ~A  has not implemented pointer grabbing." port))

(defgeneric set-sheet-pointer-cursor (port sheet cursor)
  (:documentation "Sets the cursor associated with SHEET. CURSOR is a symbol, as described in the Franz user's guide."))

(defmethod set-sheet-pointer-cursor ((port basic-port) sheet cursor)
  (declare (ignore sheet cursor))
  (warn "Port ~A has not implemented sheet pointer cursors." port))
