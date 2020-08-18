;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Inspection method for `pathname' objects.
;;;

(cl:in-package #:clouseau)

;;; TODO should be immutable

;;; Object inspection methods

(defmethod inspect-object-using-state :after ((object pathname)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (write-char #\Space stream)
  (badge stream (cond ((wild-pathname-p object)
                       "wild")
                      ((ignore-errors (probe-file object))
                       "exists in filesystem")
                      (t
                       "does not exist in filesystem"))))
