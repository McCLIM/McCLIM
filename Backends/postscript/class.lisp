;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)

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

;;; TODO:
;;; - (?) WITH-OUTPUT-TO-POSTSCRIPT-STREAM should bind its first argument
;;;   to stream, not to medium.

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-POSTSCRIPT-STREAM should offer a :PAPER-SIZE option.
;;; - How can one ask for the dimensions of a postscript device (aka paper-size)?
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;; - font metrics are missing
;;;
;;;--GB

(in-package :CLIM-POSTSCRIPT)

(defvar *default-postscript-title* "")

(defvar *default-postscript-for*
  #+unix (or (get-environment-variable "USER")
             "Unknown")
  #-unix "")

(defclass postscript-medium (basic-medium)
  ((file-stream :initarg :file-stream :reader postscript-medium-file-stream)
   (title :initarg :title)
   (for :initarg :for)
   (orientation :initarg :orientation)
   (current-page :initform 1)
   (document-fonts :initform '())
   (graphics-state-stack :initform '())))

(defun make-postscript-medium (file-stream device-type
                               multi-page scale-to-fit
                               orientation header-comments)
  (declare (ignore device-type multi-page scale-to-fit))
  (let ((title (or (getf header-comments :title)
                   *default-postscript-title*))
        (for (or (getf header-comments :for)
                 *default-postscript-for*)))
    (make-instance 'postscript-medium
                   :sheet (make-postscript-graft)
                   :file-stream file-stream
                   :title title :for for
                   :orientation orientation)))

(defmacro postscript-medium-graphics-state (medium)
  `(first (slot-value ,medium 'graphics-state-stack)))
