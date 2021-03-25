;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Frame managers.
;;;

(in-package #:clim-internals)

(defvar *default-frame-manager* nil)

(defclass standard-frame-manager (frame-manager)
  ((port
    :initarg :port
    :reader port)
   (frames
    :initform nil
    :reader frame-manager-frames)))
