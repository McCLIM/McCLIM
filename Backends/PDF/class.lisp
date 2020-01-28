;;; -*- Mode: Lisp; Package: CLIM-PDF -*-

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

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-PDF-STREAM should offer a :PAPER-SIZE option.
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;;
;;;--GB

(in-package :clim-pdf)

;;;; Medium

(defclass pdf-medium (clim-postscript-font:postscript-font-medium) ())

(defmacro pdf-medium-graphics-state (medium)
  `(first (slot-value (medium-sheet ,medium) 'graphics-state-stack)))

(defun pdf-medium-file-stream (medium)
  (clim-pdf-stream-file-stream (medium-sheet medium)))

;;;; Stream

(defvar *default-title* "")

(defvar *default-author*
  (or #+unix (get-environment-variable "USER")
      "Unknown"))

(defclass clim-pdf-stream (sheet-leaf-mixin
                           sheet-mute-input-mixin
                           permanent-medium-sheet-output-mixin
                           sheet-mute-repainting-mixin
                           ;; ?
                           mirrored-sheet-mixin
                           ;; FIXME: Tim Moore suggested (2006-02-06,
                           ;; mcclim-devel) that this might better be
                           ;; a superclass of
                           ;; STANDARD-OUTPUT-RECORDING-STREAM.  This
                           ;; should be revisited when we grow another
                           ;; non-interactive backend (maybe a cl-pdf
                           ;; backend?).  -- CSR.
                           climi::updating-output-stream-mixin
                           basic-sheet
                           standard-extended-output-stream
                           standard-output-recording-stream)
  ((file-stream :initarg :file-stream :reader clim-pdf-stream-file-stream)
   (title :initarg :title)
   (author :initarg :author)
   (subject :initarg :subject)
   (orientation :initarg :orientation :accessor orientation)
   (paper :initarg :paper)
   (transformation :initarg :transformation
                   :initform nil
                   :accessor sheet-native-transformation)
   (current-page :initform 0)
   (document-fonts :initform '())
   (graphics-state-stack :initform '())
   (pages  :initform nil :accessor pdf-pages)))

(defun make-clim-pdf-stream (file-stream port device-type
                             multi-page scale-to-fit
                             orientation header-comments)
  (declare (ignore multi-page scale-to-fit))
  (unless device-type (setq device-type :a4))
  (let ((region (paper-region device-type orientation)))
    (make-instance 'clim-pdf-stream
                   :file-stream file-stream
                   :port port
                   :title (getf header-comments :title *default-title*)
                   :author (getf header-comments :author *default-author*)
                   :subject (getf header-comments :subject)
                   :orientation orientation
                   :paper device-type
                   :native-region region
                   :region region)))

;;;; Port

(defclass pdf-port (clim-postscript-font:postscript-font-port)
  ((stream :reader pdf-port-stream)))
