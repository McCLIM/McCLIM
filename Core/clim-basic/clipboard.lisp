(in-package #:climi)

;;;  This file contains the definition of the clipboard protocol.
;;;
;;;  This implementation supports two separate ways to transfer data
;;;  between applications.
;;;
;;;    Selection:
;;;
;;;      The currently selected data (for example using shift-drag in
;;;      a stream pane). X11 allows an application to request the
;;;      content of the selection, usually by pressing middle-click to
;;;      paste. Other windowing systems might not allow one
;;;      application to request the content of the selection from
;;;      another application.
;;;
;;;    Clipboard:
;;;
;;;      This is the standard clipboard which is used to copy data
;;;      between applications. Most GNOME applications use Control-V
;;;      to paste the content of the clipboard into the current
;;;      application.
;;;
;;;  To update the selection or clipboard, the functions
;;;  COPY-TO-SELECTION and COPY-TO-CLIPBOARD are used. These functions
;;;  take two required arguments: The sheet that should act as the
;;;  owner of the selection or clipboard, and the object itself.
;;;
;;;  To request the content of the selection or clipboard, the
;;;  functions REQUEST-SELECTION-CONTENT and REQUEST-CLIPBOARD-CONTENT
;;;  are used. The arguments are the sheet to which the result should
;;;  be delivered, and a keyword or list of keywords specifying the
;;;  accepted data types.
;;;
;;;  Currently supported data types when requesting selection or
;;;  clipboard content are: :STRING, :HTML, :IMAGE.

;;;
;;;  Functions dealing with copying to the clipboard.
;;;

(deftype representation-type-name () '(member :string :html :image))

(defgeneric copy-to-selection (port sheet object &key presentation-type)
  (:documentation "Copy OBJECT to the selection.

Please refer to the documentation for COPY-TO-CLIPBOARD for details.")
  (:method ((port clim:port) (sheet clim:sheet) object &key presentation-type)
    (declare (ignore presentation-type))
    nil))

(defgeneric copy-to-clipboard (port sheet object &key presentation-type)
  (:documentation "Copy OBJECT to the clipboard.

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

OBJECT is the object that should be copied.

PRESENTATION-TYPE is the CLIM presentation type for OBJECT. It should
be passed to CONVERT-CLIPBOARD-CONTENT when the actual clipboard data
is retrieved.")
  (:method ((port clim:port) (sheet clim:sheet) object &key presentation-type)
    (declare (ignore presentation-type))
    nil))

(defgeneric clear-selection (port sheet)
  (:documentation "Clear the content of the selection.

Please refer to the documentation for CLEAR-CLIPBOARD for details.")
  (:method ((port clim:port) (sheet clim:sheet))
    nil))

(defgeneric clear-clipboard (port sheet)
  (:documentation "Clear the content of the clipboard.

When this function is called, the backend should ensure that the
content of the selection or the clipboard is no longer available for
pasting.

PORT is the port that the sheet belongs to. It should be the primary
argument that individual sheets specialise on.

SHEET is the owner of the selection or clipboard.

Clear the clipboard if CLIPBOARD-P is true, otherwise the selection.")
  (:method ((port clim:port) (sheet clim:sheet))
    nil))

(defgeneric local-selection-content (port)
  (:documentation "Returns the content of the selection in this Lisp image.
If the global selection is currently held, the value returned is the
same as what would be sent in response to a REQUEST-SELECTION-CONTENT
call.

If content is available, this function should return two values: The
content of the selection, and the presentation-type of the object.
Otherwise (VALUES NIL NIL) should be returned.

This function is used by the copy selection to clipboard operation.
This is in order to present the same beahviour as most non-CLIM
applications exhibit: They are able to perform the copy
operation (usually invoked by control-c) even when not owning the
system-wide selection.

The simplest implementation of this function would simply return the
content of the selection, or (VALUES NIL NIL) if it is not held.")
  (:method ((port clim:port))
    nil))

;;;
;;;  The following functions implement the standard API to request
;;;  content from the clipboard.
;;;

(defgeneric request-selection-content (port sheet type)
  (:documentation "Request the content of the selection.

Please refer to the documentation for REQUEST-CLIPBOARD-CONTENT for
details.")
  (:method ((port clim:port) (sheet clim:sheet) type)
    nil))

(defgeneric request-clipboard-content (port sheet type)
  (:documentation "Request the content of the clipboard.

After calling this function, if the clipboard is active and can be
presented as the requested type, an event of type CLIPBOARD-SEND-EVENT
will be delivered to SHEET. This event will contain the requested data
in one of the requested formats.

TYPE is a list of keywords (or a single keyword, in which case it is
interpreted as a list of a single element) that indicates the type of
data that can be accepted. If the clipboard content can be represented
in more than one format, the first matching type will be used.")
  (:method ((port clim:port) (sheet clim:sheet) type)
    nil))
