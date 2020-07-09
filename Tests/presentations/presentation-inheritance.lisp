(cl:in-package #:clim-tests)
(in-suite :mcclim.presentations)

;;; When a presentation type is defined with the same name as an
;;; existing class, its inherit-from must match the superclasses of
;;; the class itself. Otherwise an error should be signaled.
(defclass pti.0001.class () ())
(test pti.0001.inherit-expression
  (fails
    (signals error
      (define-presentation-type pti.0001.class ()
        :inherit-from 'expression))))

;;; The presentation type PTI.0002.CLASS* should inherit its default
;;; behavior for PRESENTATION-TYPEP from its supertype PTI.0002.CLASS
;;; attached to the CLOS class.
(defclass pti.0002.class () ())
(defclass pti.0002.other () ())
(define-presentation-type pti.0002.class  ())
(define-presentation-type pti.0002.class* () :inherit-from 'pti.0002.class)
(test pti.0002.inherit-class
  (let ((foo (make-instance 'pti.0002.class)))
    (is (presentation-typep foo 'pti.0002.class))
    (fails
      (handler-case (is (presentation-typep foo 'pti.0002.class*))
        (error () (fail "Function behavior is not inherited."))))
    (is (not (presentation-typep foo 'pti.0002.other)))))
