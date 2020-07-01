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
  (let ((foo (make-instance 'pti.0002.class))
        (bar (make-instance 'pti.0002.other)))
    (is (presentation-typep foo 'pti.0002.class))
    (is (presentation-typep foo 'pti.0002.class*))
    (fails (is (not (presentation-typep bar 'pti.0002.class*))))
    (is (presentation-typep foo 'pti.0002.class*))
    (is (not (presentation-typep foo 'pti.0002.other)))))

(test pti.0003.inheritance-validity
  (define-presentation-type pti.0003.foo ())
  (define-presentation-type pti.0003.bar ())
  (finishes
    (define-presentation-type pti.0003.qux ()
      :inherit-from 'pti.0003.foo))
  (finishes
    (define-presentation-type pti.0003.qux ()
      :inherit-from '(and pti.0003.foo
                          pti.0003.bar)))
  (signals error
    (define-presentation-type pti.0003.qux ()
      :inherit-from '(and pti.0003.foo
                          (and pti.0003.bar pti.0003.foo))))
  (signals error
    (define-presentation-type pti.0003.qux ()
      :inherit-from '(not string)))
  (signals error
    (define-presentation-type pti.0003.qux ()
      :inherit-from '(satisfies list)))
  (signals error
    (define-presentation-type pti.0003.qux ()
      :inherit-from '(or pti.0003.foo pti.0003.bar)))
  (signals error
    (define-presentation-type pti.0003.qux ()
      :inherit-from '(and (or pti.0003.foo pti.0003.bar))))
  (signals error
    (define-presentation-type presentations.invalid-inheritance.qux ()
      :inherit-from '(and pti.0003.foo pti.0003.bar (satisfies (list)))))
  (signals error
    (define-presentation-type presentations.invalid-inheritance.qux ()
      :inherit-from '(and pti.0003.foo (not pti.0003.bar)))))
