(in-package :climi)

;;;
;;;  Functions dealing with copying to the clipboard.
;;;

(deftype representation-type-name () '(member :string :html))

(defgeneric copy-to-clipboard-with-port (port sheet object)
  (:documentation "Method to be implemented by backends."))

(defmethod copy-to-clipboard-with-port ((port clim:port) (sheet clim:sheet) object)
  "Fallback implementation if the implementation does not support a clipboard.."
  nil)

(defun copy-to-clipboard (sheet object)
  "Copy OBJECT to the clipboard.
SHEET is the owner of the clipboard, and it is not guaranteed that the
content of the clipboard will be available after the sheet has been
removed."
  (copy-to-clipboard-with-port (port sheet) sheet object))

#+nil
(progn
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
    object))

;;;
;;;  The following functions implement the standard API to request
;;;  content from the clipboard.
;;;

(defclass clipboard-send-event (climi::window-event)
  ((content :initarg :content
            :reader clipboard-event-content)
   (type    :initarg :type
            :reader clipboard-event-type)))

(defgeneric request-clipboard-content-from-port (port pane type)
  (:documentation "Backend implementation of REQUEST-CLIPBOARD-CONTENT.")
  (:method ((port clim:port) pane type)
    (error "Clipboard not implemented for port: ~s" port)))

(defun request-clipboard-content (pane type)
  (request-clipboard-content-from-port (port pane) pane type))
