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

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-POSTSCRIPT-STREAM should offer a :PAPER-SIZE option.
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;;
;;;--GB

(in-package :clim-postscript)

;;;; Medium

(defclass postscript-medium (basic-medium)
  ((device-fonts :initform nil
		 :accessor device-fonts)))

(defmacro postscript-medium-graphics-state (medium)
  `(first (slot-value (medium-sheet ,medium) 'graphics-state-stack)))

(defun postscript-medium-file-stream (medium)
  (postscript-stream-file-stream (medium-sheet medium)))


;;;; Stream
(defvar *default-postscript-title* "")

(defvar *default-postscript-for*
  #+unix (or (get-environment-variable "USER")
             "Unknown")
  #-unix "")

(defclass postscript-stream 
    (basic-sheet
     sheet-leaf-mixin sheet-mute-input-mixin 
     permanent-medium-sheet-output-mixin sheet-mute-repainting-mixin
     ;; ?
     mirrored-sheet-mixin
     ;; FIXME: Tim Moore suggested (2006-02-06, mcclim-devel) that
     ;; this might better be a superclass of
     ;; STANDARD-OUTPUT-RECORDING-STREAM.  This should be revisited
     ;; when we grow another non-interactive backend (maybe a cl-pdf
     ;; backend?).  -- CSR.
     climi::updating-output-stream-mixin
     standard-extended-output-stream standard-output-recording-stream)
  ((file-stream :initarg :file-stream :reader postscript-stream-file-stream)
   (title :initarg :title)
   (for :initarg :for)
   (orientation :initarg :orientation)
   (paper :initarg :paper)   
   (transformation :initarg :transformation
                   :reader sheet-native-transformation)
   (current-page :initform 0)
   (document-fonts :initform '())
   (graphics-state-stack :initform '())
   (pages  :initform nil :accessor postscript-pages)))

(defun make-postscript-stream (file-stream port device-type
                               multi-page scale-to-fit
                               orientation header-comments)
  (declare (ignore multi-page scale-to-fit))
  (unless device-type (setq device-type :a4))
  (let ((title (or (getf header-comments :title)
                   *default-postscript-title*))
        (for (or (getf header-comments :for)
                 *default-postscript-for*))
        (region (case device-type
                  ((:eps) +everywhere+)
                  (t (paper-region device-type orientation))))
        (transform (make-postscript-transformation device-type orientation)))
    (make-instance 'postscript-stream
                   :file-stream file-stream
                   :port port
                   :title title :for for
                   :orientation orientation
                   :paper device-type
                   :native-region region
                   :region region
                   :transformation transform)))


;;;; Port

(defclass postscript-port (basic-port)
  ((stream #| :initarg :stream |#
           #| :initform (error "Unspecified stream.") |#
           ;; I think this is right, but BASIC-PORT accepts only
           ;; :SERVER-PATH initarg. -- APD, 2002-06-06

           :reader postscript-port-stream)))

;;; FIXME!!! The following method should be removed. -- APD, 2002-06-06
(defmethod initialize-instance :after ((port postscript-port)
                                       &rest initargs
                                       &key server-path)
  (declare (ignore initargs))
  (destructuring-bind (ps &key stream) server-path
    (assert (eq ps :ps))
    (check-type stream stream)
    (setf (slot-value port 'stream) stream)))