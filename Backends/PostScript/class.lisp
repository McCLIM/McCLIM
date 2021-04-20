;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) Copyright 2001 by Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) Copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;  (c) Copyright 2002 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

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

(defclass postscript-medium (postscript-font-medium) ())

(defmacro postscript-medium-graphics-state (medium)
  `(first (slot-value (medium-sheet ,medium) 'graphics-state-stack)))

;;;; Stream
(defvar *default-postscript-title* "")

(defvar *default-postscript-for*
  #+unix (or (get-environment-variable "USER")
             "Unknown")
  #-unix "")

(defclass postscript-stream (sheet-leaf-mixin
                             sheet-parent-mixin
                             sheet-transformation-mixin
                             sheet-mute-input-mixin
                             sheet-mute-repainting-mixin
                             climi::updating-output-stream-mixin
                             basic-sheet
                             standard-extended-output-stream
                             permanent-medium-sheet-output-mixin
                             standard-output-recording-stream)
  ((port :initform nil :initarg :port :accessor port)
   (title :initarg :title)
   (for :initarg :for)
   (current-page :initform 0)
   (document-fonts :initform '())
   (graphics-state-stack :initform '())
   (pages  :initform nil :accessor postscript-pages)))

(defun make-postscript-stream (port device-type
                               multi-page scale-to-fit
                               orientation header-comments)
  (declare (ignore multi-page scale-to-fit))
  (unless device-type (setq device-type :a4))
  (let ((title (or (getf header-comments :title)
                   *default-postscript-title*))
        (for (or (getf header-comments :for)
                 *default-postscript-for*))
        (region (paper-region device-type orientation)))
    (make-instance 'postscript-stream
                   :port port
                   :title title :for for
                   :region region)))


;;;; Port

(defclass postscript-port (postscript-font-port)
  ((stream :initform nil
           :accessor postscript-port-stream)
   (device-type :initform :a4 :initarg :device-type
                :accessor device-type
                :type keyword)
   (page-orientation :initform :portrait :initarg :page-orientation
                     :accessor page-orientation
                     :type (member :landscape :portrait))))

(defmethod climb:make-graft
    ((port postscript-port) &key (orientation :default) (units :device))
  (make-instance 'postscript-graft
                 :port port
                 :mirror (postscript-port-stream port)
                 :orientation orientation
                 :units units))

(defmethod initialize-instance :after ((port postscript-port) &key)
  (let* ((options (cdr (port-server-path port)))
         (stream (getf options :stream))
         (device-type (getf options :device-type :a4))
         (page-orientation (getf options :page-orientation :portrait)))
    (setf (postscript-port-stream port) stream
          (device-type port) device-type
          (page-orientation port) page-orientation))
  (climb:make-graft port))
