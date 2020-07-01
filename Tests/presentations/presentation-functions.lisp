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

(define-presentation-type pgf-warning.type1 ())
(define-presentation-type pgf-warning.type2 (arg1)      :options (arg1 arg2))
(define-presentation-type pgf-warning.type3 (type arg1) :options (arg1 arg2))
(define-presentation-type pgf-warning.type4 ()          :options (type))
(macrolet ((define-it (name args)
             `(define-presentation-generic-function ,(gensym) ,name ,args)))
  (define-it pgf-warning.fn1 (climi::type-key type))
  (define-it pgf-warning.fn2 (climi::type-key climi::parameters type))
  (define-it pgf-warning.fn3 (climi::type-key climi::parameters climi::options type)))

(test pgf-warning
  (macrolet ((signals-style-warning (expected form)
               (let ((fail1 "~%~S~%~% not expected to signal STYLE-WARNING")
                     (fail2 "~%~S~%~% expected to signal STYLE-WARNING"))
                 `(let ((sig nil))
                    (handler-bind ((style-warning
                                     (lambda (c)
                                       (setf sig t)
                                       (muffle-warning c))))
                      (compile nil '(lambda () ,form))
                      (if (eq sig ,expected)
                          (pass)
                          (if sig
                              (fail ,fail1 ',form)
                              (fail ,fail2 ',form))))))))
    ;; method/parameters/options doesn't coincide
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn1
             ((type pgf-warning.type1))))
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn2
             ((type pgf-warning.type1))))
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn3
             ((type pgf-warning.type1))))
    ;; parameters/options coincide
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn1
             ((type pgf-warning.type2))))
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn2
             ((type pgf-warning.type2))))
    (signals-style-warning
     t (define-presentation-method pgf-warning.fn3
           ((type pgf-warning.type2))))
    ;; method/parameters and parameters/options coincide
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn1
             ((type pgf-warning.type3))))
    (signals-style-warning
     t (define-presentation-method pgf-warning.fn2
             ((type pgf-warning.type3))))
    (signals-style-warning
     t (define-presentation-method pgf-warning.fn3
           ((type pgf-warning.type3))))
    ;; method/options coincide
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn1
             ((type pgf-warning.type4))))
    (signals-style-warning
     nil (define-presentation-method pgf-warning.fn2
           ((type pgf-warning.type4))))
    (signals-style-warning
     t (define-presentation-method pgf-warning.fn3
           ((type pgf-warning.type4))))))
