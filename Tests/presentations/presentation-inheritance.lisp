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
  (define-presentation-type pti.0003.super1 ())
  (define-presentation-type pti.0003.super2 ())
  (finishes
    (define-presentation-type pti.0003.sub1 ()
      :inherit-from 'pti.0003.super1))
  (finishes
    (define-presentation-type pti.0003.sub2 ()
      :inherit-from '(and pti.0003.super1
                          pti.0003.super2)))
  (compilation-signals error
    (define-presentation-type pti.0003.sub3 ()
      :inherit-from '(and pti.0003.super1
                      (and pti.0003.super2 pti.0003.super1))))
  (compilation-signals error
    (define-presentation-type pti.0003.sub4 ()
      :inherit-from '(not string)))
  (compilation-signals error
    (define-presentation-type pti.0003.sub5 ()
      :inherit-from '(satisfies list)))
  (compilation-signals error
    (define-presentation-type pti.0003.sub6 ()
      :inherit-from '(or pti.0003.super1 pti.0003.suyper2)))
  (compilation-signals error
    (define-presentation-type pti.0003.sub7 ()
      :inherit-from '(and (or pti.0003.super1 pti.0003.super2))))
  (compilation-signals error
    (define-presentation-type pti.0003.sub8 ()
      :inherit-from '(and pti.0003.super1 pti.0003.super2 (satisfies (list)))))
  (compilation-signals error
    (define-presentation-type pti.0003.sub9 ()
      :inherit-from '(and pti.0003.super1 (not pti.0003.super2)))))
