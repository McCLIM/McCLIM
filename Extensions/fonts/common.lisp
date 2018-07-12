(defpackage :mcclim-font
  (:use :cl)
  (:export #:find-replacement-text-styles
           #:find-replacement-fonts-from-port))

(in-package :mcclim-font)

(defgeneric find-replacement-fonts-from-port (port text-style string)
  (:method (port text-style string)
    (list (cons string '(nil nil)))))

(defun find-replacement-text-styles (stream string &key text-style)
  "Find replacement fonts for characters in STRING if they were drawn on STREAM.
Use TEXT-STYLE if non-NIL, or the current text style of the stream if
NIL. This function returns a list of individual parts of the original
string together with the appropriate text style to use. The returned
valus is of the form:

    (STRING FAMILY FACE)

If FAMILY and FACE are NIL, this indicates that the base text style
contains all the characters of the given string.

Example: Assume that the text GREEK is written in greek, and the
default font does not contain any greek characters, and this function
is called as follows:

  (find-replacement-text-styles \"abcGREEKdef\")

The return value would then be:
  ((\"abc\" nil nil)
   (\"GREEK\" \"Greek Font Family\" \"Greek Font Style\")
   (\"def\" nil nil))

The second and third values of each element are appropriate for
arguments to CLIM:MAKE-TEXT-STYLE."
  (clim:with-sheet-medium (medium stream)
    (find-replacement-fonts-from-port (clim:port medium) (or text-style (clim:medium-text-style stream)) string)))
