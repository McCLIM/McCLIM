;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Special variables used by the inspector.
;;;

(cl:in-package #:clouseau)

;;; Bound to a type specifier indicating which conditions should be
;;; handled (by printing an error message). Signaled conditions of
;;; other types are not handled and potentially cause the debugger to
;;; be invoked.
(defvar *handle-errors* t)
