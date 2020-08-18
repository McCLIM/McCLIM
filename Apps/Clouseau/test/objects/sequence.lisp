;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Smoke test for inspecting sequences.
;;;

(cl:in-package #:clouseau.test)

(def-suite* :clouseau.objects.sequence
  :in :clouseau)

(defclass my-sequence (sequence standard-object)
  ())

(defmethod sequence:length ((sequence my-sequence))
  3)

(defmethod sequence:elt ((sequence my-sequence) (index t))
  (case index
    (0 :a)
    (1 :b)
    (2 :c)))

(test sequence.object-state-class.smoke
  "Test `object-state-class' for extended sequences."

  (object-state-class-cases
   (list (make-instance 'my-sequence) 'clouseau::inspected-extended-sequence)))
