(cl:in-package #:clim-tests)
(in-suite :mcclim.presentations)

;;; Presentation methods depend on the presentation generic function
;;; being defined during macroexpansion of the method macros.
;;; Ultimately we'd like to fix this inconvenience.

(define-presentation-generic-function %pgf1 pgf1
  (climi::type-key type))

(test pgf-specializers
  (finishes
    (define-default-presentation-method pgf1 (type)    (list :default type))
    (define-presentation-method pgf1 ((type (eql t)))  (list :eql-t   type))
    (define-presentation-method pgf1 ((type symbol))   (list :symbol  type))
    (define-presentation-method pgf1 ((type sequence)) (list :seq-int type)))
  (macrolet ((call-spec (spec)
               `(funcall-presentation-generic-function pgf1 ',spec)))
    (is (equal '(:default         expression) (call-spec         expression)))
    (is (equal '(:eql-t                    t) (call-spec                  t)))
    (is (equal '(:symbol              symbol) (call-spec             symbol)))
    (is (equal '(:seq-int (sequence integer)) (call-spec (sequence integer))))))
