(in-package #:climi)

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
