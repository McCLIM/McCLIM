(in-package :climi)

;;;
;;;  Functions dealing with copying to the clipboard.
;;;

(deftype representation-type-name () '(member :string :html :image))

(defgeneric copy-to-clipboard-with-port (port sheet clipboard-p object presentation-type)
  (:documentation "Backend implementation of the copy to selection or clipboard.

When this function is called, the backend is responsible for placing
OBJECT on the selection or clipboard. Afterwards, other applications
should be able to paste this object.

The object has a presentation type, indicated by the parameter
PRESENTATION-TYPE. It is the responsibility of the backend to use the
function CONVERT-CLIPBOARD-CONTENT to retrieve the underlying data
from the object when needed. Note that the conversion operation may be
expensive (in particular for images) so it is recommended that the a
full conversion only be performed when a request to copy the content
is actually performed. Use the :CHECK-ONLY argument to check whether a
conversion is supported.

PORT is the port that the sheet belongs to. It should be the primary
argument that individual sheets specialise on.

SHEET is the owner of the selection or clipboard.

If CLIPBOARD-P is true, this call is a request to copy to the
clipboard, if false it's a copy to the selection.

OBJECT is the object that should be copied.

PRESENTATION-TYPE is the CLIM presentation type for OBJECT. It should
be passed to CONVERT-CLIPBOARD-CONTENT when the actual clipboard data
is retrieved.")
  (:method ((port clim:port) (sheet clim:sheet) clipboard-p object presentation-type)
    nil))

(defun copy-to-clipboard (sheet object &key presentation-type)
  "Copy OBJECT to the clipboard.
SHEET is the owner of the clipboard, and it is not guaranteed that the
content of the clipboard will be available after the sheet has been
removed."
  (copy-to-clipboard-with-port (port sheet) sheet t object presentation-type))

(defun copy-to-selection (sheet object &key presentation-type)
  "Copy OBJECT to the selection.
SHEET is the owner of the selection, and it is not guaranteed that the
content of the selection will be available after the sheet has been
removed."
  (copy-to-clipboard-with-port (port sheet) sheet nil object presentation-type))

(defgeneric clear-clipboard-with-port (port sheet clipboard-p)
  (:documentation "Backend implementation of clear selection or clipboard.

When this function is called, the backend should ensure that the
content of the selection or the clipboard is no longer available for
pasting.

PORT is the port that the sheet belongs to. It should be the primary
argument that individual sheets specialise on.

SHEET is the owner of the selection or clipboard.

Clear the clipboard if CLIPBOARD-P is true, otherwise the selection.")
  (:method ((port clim:port) (sheet clim:sheet) clipboard-p)
    nil))

(defun clear-clipboard (sheet)
  "Clear the content of the clipboard.
After this function is called, external programs will not be able to
paste the content."
  (clear-clipboard-with-port (port sheet) sheet t))

(defun clear-selection (sheet)
  "Clear the content of the selection.
After this function is called, external programs will not be able to
paste the content. However, selected text in the user interface may
still be highlighted."
  (clear-clipboard-with-port (port sheet) sheet nil))

(defgeneric local-selection-content (port)
  (:documentation "Returns the content of the selection in this Lisp image.
If the global selection is currently held, the value returned is the
same as what would be sent in response to a REQUEST-CLIPBOARD-CONTENT
call.

If content is available, this function should return two values: The
content of the selection, and the presentation-type of the object.
Otherwise NIL,NIL should be returned.

This function is used by the copy selection to clipboard operation.
This is in order to present the same beahviour as most non-CLIM
applications exhibit: They are are able perform the copy
operation (usually invoked by control-c) even when not owning the
system-wide selection.

The simplest implementation of this function would simply return the
content of the selection, or NIL,NIL if it is not held.")
  (:method ((port clim:port))
    nil))

;;;
;;;  The following functions implement the standard API to request
;;;  content from the clipboard.
;;;

(defgeneric request-clipboard-content-with-port (port pane clipboard-p type)
  (:documentation "Backend implementation of REQUEST-CLIPBOARD-CONTENT.")
  (:method ((port clim:port) pane clipboard-p type)
    (error "Clipboard not implemented for port: ~s" port)))

(defmethod request-clipboard-content-with-port :around (port name clipboard-p type)
  (unless (or (typep type 'climi::representation-type-name)
              (and (listp type)
                   (every (lambda (v) (typep v 'climi::representation-type-name)) type)))
    (error "Invalid type: ~s" type))
  (call-next-method))

(defun request-selection-content (pane type)
  (request-clipboard-content-with-port (port pane) pane nil type))

(defun request-clipboard-content (pane type)
  (request-clipboard-content-with-port (port pane) pane t type))
