(in-package :climi)

;;;
;;;  Functions dealing with copying to the clipboard.
;;;

(deftype representation-type-name () '(member :string :html))

(defgeneric copy-to-clipboard (port object)
  (:documentation "Copy OBJECT to the clipboard."))

(defmethod copy-to-clipboard ((port clim:port) object)
  "Fallback implementation if the port does not support copying to the clipboard."
  nil)

(defgeneric representation-type-supported-p (object type)
  (:documentation "Returns true if OBJECT can be converted TYPE."))

(defmethod representation-type-supported-p (object type)
  "Default implementation that simply calls CONVERT-CLIPBOARD-OBJECT."
  (climi::convert-clipboard-object object type))

(defgeneric convert-clipboard-object (object type)
  (:documentation "Convert OBJECT to representation type TYPE.

If the conversion is expensive, the method
REPRESENTATION-TYPE-SUPPORTED-P can be implemented in addition to this
method."))

(defmethod convert-clipboard-object (object type)
  nil)

(defmethod convert-clipboard-object ((object string) (type (eql :string)))
  object)

;;;
;;;  The following functions implement the standard API to request
;;;  content from the clipboard.
;;;

(defclass clipboard-send-event (climi::window-event)
  ((content :initarg :content
            :reader clipboard-event-content)))

(defgeneric request-clipboard-content-from-port (port pane type)
  (:documentation "Backend implementation of REQUEST-CLIPBOARD-CONTENT.")
  (:method ((port clim:port) pane type)
    (error "Clipboard not implemented for port: ~s" port)))

(defun request-clipboard-content (pane type)
  (request-clipboard-content-from-port (port pane) pane type))
