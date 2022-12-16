;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2005 by Christophe Rhodes <c.rhodes@gold.ac.uk>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-null)

(defclass null-port (basic-port)
  ((id)
   (window :initform nil :accessor null-port-window))
  (:default-initargs :pointer (make-instance 'standard-pointer)))

(defmethod find-port-type ((type (eql :null)))
  (values 'null-port 'identity))

(defmethod initialize-instance :after ((port null-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "NULL-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'null-frame-manager :port port)
        (slot-value port 'climi::frame-managers)))

(defmethod print-object ((object null-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defmethod port-set-mirror-geometry ((port null-port) sheet region)
  (bounding-rectangle* region))

(defmethod realize-mirror ((port null-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod destroy-mirror ((port null-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod port-enable-sheet ((port null-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port null-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod port-shrink-sheet ((port null-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port null-port))
  nil)

(defmethod process-next-event ((port null-port) &key wait-function (timeout nil))
  (cond ((maybe-funcall wait-function)
         (values nil :wait-function))
        ((not (null timeout))
         (sleep timeout)
         (if (maybe-funcall wait-function)
             (values nil :wait-function)
             (values nil :timeout)))
        ((not (null wait-function))
         (loop do (sleep 0.1)
               until (funcall wait-function)
               finally (return (values nil :wait-function))))
        (t
         (error "Game over. Listening for an event on Null backend."))))

(defmethod make-graft
    ((port null-port) &key (orientation :default) (units :device))
  (make-instance 'null-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port null-port) sheet)
  (make-instance 'null-medium :port port :sheet sheet))

(defmethod text-style-mapping
    ((port null-port) (text-style text-style) &optional character-set)
  (declare (ignore port text-style character-set))
  nil)

(defmethod (setf text-style-mapping) (font-name
                                      (port null-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

(defmethod graft ((port null-port))
  (first (climi::port-grafts port)))

(defmethod port-modifier-state ((port null-port))
  nil)

(defmethod (setf port-keyboard-input-focus) (focus (port null-port))
  focus)

(defmethod port-keyboard-input-focus ((port null-port))
  nil)

(defmethod port-force-output ((port null-port))
  nil)

(defmethod distribute-event :around ((port null-port) event)
  (declare (ignore event))
  nil)

(defmethod set-sheet-pointer-cursor ((port null-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)
