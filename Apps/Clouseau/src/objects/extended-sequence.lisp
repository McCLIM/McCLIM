;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Inspection method for extended sequences.
;;;

(cl:in-package #:clouseau)

(defclass inspected-extended-sequence (inspected-instance
                                       inspected-sequence)
  ())

(defmethod object-state-class :around ((object standard-object) (place t))
  (if (typep object 'sequence)
      'inspected-extended-sequence
      (call-next-method)))
