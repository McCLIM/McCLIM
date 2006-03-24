;;; -*- Mode: Lisp; Package: CLIM-NULL; -*-

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

(in-package :clim-null)

(defclass null-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass null-port (basic-port)
  ((id)
   (pointer :accessor port-pointer :initform (make-instance 'null-pointer))
   (window :initform nil :accessor null-port-window)))

(defun parse-null-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :null :port-type) 'null-port)
(setf (get :null :server-path-parser) 'parse-null-server-path)

(defmethod initialize-instance :after ((port null-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "NULL-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'null-frame-manager :port port)
	(slot-value port 'climi::frame-managers)))

(defmethod print-object ((object null-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defmethod port-set-mirror-region ((port null-port) mirror mirror-region)
  ())
                                   
(defmethod port-set-mirror-transformation
    ((port null-port) mirror mirror-transformation)
  ())

(defmethod realize-mirror ((port null-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod destroy-mirror ((port null-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod mirror-transformation ((port null-port) mirror)
  ())


(defmethod port-set-sheet-region ((port null-port) (graft graft) region)
  ())

(defmethod port-set-sheet-transformation
    ((port null-port) (graft graft) transformation)
  ())

(defmethod port-set-sheet-transformation
    ((port null-port) (sheet mirrored-sheet-mixin) transformation)
  ())

(defmethod port-set-sheet-region
    ((port null-port) (sheet mirrored-sheet-mixin) region)
  ())

(defmethod port-enable-sheet ((port null-port) (mirror mirrored-sheet-mixin))
  ())

(defmethod port-disable-sheet ((port null-port) (mirror mirrored-sheet-mixin))
  ())

(defmethod destroy-port :before ((port null-port))
  ())

(defmethod port-motion-hints ((port null-port) (mirror mirrored-sheet-mixin))
  ())

(defmethod (setf port-motion-hints)
    (value (port null-port) (sheet mirrored-sheet-mixin))
  value)

(defmethod get-next-event
    ((port null-port) &key wait-function (timeout nil))
  ())

(defmethod make-graft
    ((port null-port) &key (orientation :default) (units :device))
  (make-instance 'null-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port null-port) sheet)
  (make-instance 'null-medium :sheet sheet))

(defmethod text-style-mapping
    ((port null-port) text-style &optional character-set)
  ())

(defmethod (setf text-style-mapping)
    (font-name (port null-port)
     (text-style text-style) &optional character-set)
  ())

(defmethod port-character-width ((port null-port) text-style char)
  ())

(defmethod port-string-width ((port null-port) text-style string &key (start 0) end)
  ())

(defmethod port-mirror-width ((port null-port) sheet)
  ())

(defmethod port-mirror-height ((port null-port) sheet)
  ())

(defmethod graft ((port null-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port null-port) sheet width height)
  ())

(defmethod port-deallocate-pixmap ((port null-port) pixmap)
  #+nil
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer null-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer null-pointer))
  ())

(defmethod port-modifier-state ((port null-port))
  ())

(defmethod synthesize-pointer-motion-event ((pointer null-pointer))
  ())

;;; Set the keyboard input focus for the port.

(defmethod %set-port-keyboard-focus (focus (port null-port) &key timestamp)
  ())

(defmethod port-force-output ((port null-port))
  ())

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port null-port) pointer sheet)
  ())

(defmethod port-ungrab-pointer ((port null-port) pointer sheet)
  ())

(defmethod distribute-event :around ((port null-port) event)
  ())

(defmethod set-sheet-pointer-cursor ((port null-port) sheet cursor)
  ())        

(defmethod bind-selection ((port null-port) window &optional time)
  ())

(defmethod release-selection ((port null-port) &optional time)
  ())

(defmethod request-selection ((port null-port) requestor time)
  ())

(defmethod get-selection-from-event ((port null-port) event)
  ())

(defmethod send-selection ((port null-port) event string)
  nil)
