;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2005 by Christophe Rhodes <c.rhodes@gold.ac.uk>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-null)

(defclass null-frame-manager (standard-frame-manager)
  ())

(defmethod adopt-frame :after
    ((fm null-frame-manager) (frame application-frame))
  ())

(defmethod note-space-requirements-changed :after ((graft null-graft) pane)
  ())
