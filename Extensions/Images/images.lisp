;;; -*- Mode: Lisp; Package: MCCLIM-IMAGES -*-

;;;  (c) copyright 2008 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

(in-package :mcclim-images)

(defvar *image-readers* (make-hash-table :test 'equalp)
  "A hash table mapping lowercase image format names to a
function that can read an image of that format. The functions
will be called with at least one argument, the pathname of the
file to be read, and any keyword arguments provided by the
user.")

(defun image-format-supported (format)
  "Return true if `format' is supported by `load-image'."
  (not (null (gethash format *image-readers*))))

(define-condition unsupported-image-format (error)
  ((%format :reader image-format
            :initarg :image-format
            :initform (error "The image format must be supplied")
            :documentation "The image format that cannot be loaded"))
  (:report (lambda (condition stream)
             (format
              stream "Cannot read image of unknown format \"~A\""
              (image-format condition))))
  (:documentation "This exception is signalled when
`load-image-of-type' is called on an image of a type that no
reader has been defined for."))

(defun unsupported-image-format (format)
  "Signal an error of type `unsupprted-image-format' for the
image format `format'."
  (error 'unsupported-image-format :image-format format))

(defun load-image (image-pathname &rest args &key)
  "Load an image from `image-pathname', with the format of the
image being the pathname-type of `image-pathname'. `Args' can be
any keyword-arguments, they will be passed on to the image reader
function for the relevant image format. If the image format is
not recognised, an error of type `unsupprted-image-format' will
be signalled."
  (apply #'load-image-of-format (pathname-type image-pathname)
         image-pathname args))

(defun load-image-of-format (format image-pathname &rest args &key)
  "Load an image of format `format' from `image-pathname'. `Args'
can be any keyword-arguments, they will be passed on to the image
reader function for `format'. If the image format is not
recognised, an error of type `unsupprted-image-format' will be
signalled."
  (apply (or (gethash format *image-readers*)
             (unsupported-image-format format))
         image-pathname args))

(defmacro define-image-reader (image-format (&rest args) &body body)
  `(setf (gethash ,image-format *image-readers*)
         #'(lambda (,@args)
             ,@body)))
