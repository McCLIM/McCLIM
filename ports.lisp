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

;; - CLX is the de-facto reference backend.
;; - Prefer Graphic-Forms and Gtkairo over CLX, since they get installed only
;;   on explicit user request anyway.
;; - If both are present, use Graphics-Forms in favour of Gtkairo, since
;;   it is the native Windows backend.
;; - Beagle should be treated like Graphic-Forms in the long term, but is
;;   currently lacking a maintainer, so let's leave it near the end.
;; - OpenGL and Null are in this list mostly to document their existence,
;;   and neither is currently a complete backend we would want to make
;;   a default.  Put them after CLX, so that they won't actually be reached.
(defvar *server-path-search-order*
    '(:graphic-forms :gtkairo :clx :opengl :beagle :null))

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
		  :documentation "The sheet the pointer is over, if any")))

(defmethod port-keyboard-input-focus (port)
  (when (null *application-frame*)
    (error "~S called with null ~S" 
           'port-keyboard-input-focus '*application-frame*))
  (port-frame-keyboard-input-focus port *application-frame*))
(defmethod (setf port-keyboard-input-focus) (focus port)
  (when (null *application-frame*)
    (error "~S called with null ~S" 
           '(setf port-keyboard-input-focus) '*application-frame*))
  (unless (eq *application-frame* (pane-frame focus))
    (error "frame mismatch in ~S" '(setf port-keyboard-input-focus)))
  (setf (port-frame-keyboard-input-focus port *application-frame*) focus))

(defgeneric port-frame-keyboard-input-focus (port frame))
(defgeneric (setf port-frame-keyboard-input-focus) (focus port frame))

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
    (dispatch-event (event-sheet event) event))
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

;;;;
;;;; Font listing extension
;;;;

(defgeneric port-all-font-families
    (port &key invalidate-cache &allow-other-keys)
  (:documentation
   "Returns the list of all FONT-FAMILY instances known by PORT.
With INVALIDATE-CACHE, cached font family information is discarded, if any."))

(defgeneric font-family-name (font-family)
  (:documentation
   "Return the font family's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style family for this port."))

(defgeneric font-family-port (font-family)
  (:documentation "Return the port this font family belongs to."))

(defgeneric font-family-all-faces (font-family)
  (:documentation
   "Return the list of all font-face instances for this family."))

(defgeneric font-face-name (font-face)
  (:documentation
   "Return the font face's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style face for this port."))

(defgeneric font-face-family (font-face)
  (:documentation "Return the font family this face belongs to."))

(defgeneric font-face-all-sizes (font-face)
  (:documentation
   "Return the list of all font sizes known to be valid for this font,
if the font is restricted to particular sizes.  For scalable fonts, arbitrary
sizes will work, and this list represents only a subset of the valid sizes.
See font-face-scalable-p."))

(defgeneric font-face-scalable-p (font-face)
  (:documentation
   "Return true if this font is scalable, as opposed to a bitmap font.  For
a scalable font, arbitrary font sizes are expected to work."))

(defgeneric font-face-text-style (font-face &optional size)
  (:documentation
   "Return an extended text style describing this font face in the specified
size.  If size is nil, the resulting text style does not specify a size."))

(defclass font-family ()
  ((font-family-port :initarg :port :reader font-family-port)
   (font-family-name :initarg :name :reader font-family-name))
  (:documentation "The protocol class for font families.  Each backend
defines a subclass of font-family and implements its accessors.  Font
family instances are never created by user code.  Use port-all-font-families
to list all instances available on a port."))

(defmethod print-object ((object font-family) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A" (font-family-name object))))

(defclass font-face ()
  ((font-face-family :initarg :family :reader font-face-family)
   (font-face-name :initarg :name :reader font-face-name))
  (:documentation "The protocol class for font faces  Each backend
defines a subclass of font-face and implements its accessors.  Font
face instances are never created by user code.  Use font-family-all-faces
to list all faces of a font family."))

(defmethod print-object ((object font-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A, ~A"
	    (font-family-name (font-face-family object))
	    (font-face-name object))))

;;; fallback font listing implementation:

(defclass basic-font-family (font-family) ())
(defclass basic-font-face (font-face) ())

(defmethod port-all-font-families ((port basic-port) &key invalidate-cache)
  (declare (ignore invalidate-cache))
  (flet ((make-basic-font-family (name)
	   (make-instance 'basic-font-family :port port :name name)))
    (list (make-basic-font-family "FIX")
	  (make-basic-font-family "SERIF")
	  (make-basic-font-family "SANS-SERIF"))))

(defmethod font-family-all-faces ((family basic-font-family))
  (flet ((make-basic-font-face (name)
	   (make-instance 'basic-font-face :family family :name name)))
    (list (make-basic-font-face "ROMAN")
	  (make-basic-font-face "BOLD")
	  (make-basic-font-face "BOLD-ITALIC")
	  (make-basic-font-face "ITALIC"))))

(defmethod font-face-all-sizes ((face basic-font-face))
  (list 1 2 3 4 5 6 7))

(defmethod font-face-scalable-p ((face basic-font-face))
  nil)

(defmethod font-face-text-style ((face basic-font-face) &optional size)
  (make-text-style
   (find-symbol (string-upcase (font-family-name (font-face-family face)))
		:keyword)
   (if (string-equal (font-face-name face) "BOLD-ITALIC")
       '(:bold :italic)
       (find-symbol (string-upcase (font-face-name face)) :keyword))
   (ecase size
     ((nil) nil)
     (1 :tiny)
     (2 :very-small)
     (3 :small)
     (4 :normal)
     (5 :large)
     (6 :very-large)
     (7 :huge))))
