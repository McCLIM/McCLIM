(cl:in-package #:clim-tests)

(def-suite* :mcclim.presentations
  :in :mcclim)


;;; Presentation Types
;;; ============================================================================

(defparameter *presentation-type-supertypes*
  '(;; 23.8.1
    (t)
    ;; NIL is a special case
    (null t) (boolean t) (symbol t) (keyword symbol t) (blank-area t)
    ;; 23.8.2
    (number t) (complex number t) (real number t) (rational real number t)
    (integer rational real number t) (ratio rational real number t)
    (float real number t)
    ;; 23.8.3
    (character t) (string t)
    ;; 23.8.4
    (pathname t)
    ;; 23.8.5
    ((completion (42)) t)
    ;; not allowed abbreviations
    ;; (member t) ((member-sequence nil) t) ((member-alist nil) t)
    ((subset-completion nil) t)
    ;; (subset t) ((subset-sequence nil) t) ((subset-alist nil) t)
    ;; 23.8.6
    ((sequence t) t) (sequence-enumerated t)
    ;; 23.8.7
    ;;   OR, AND
    ;; 23.8.8
    ;;   ((token-or-type nil t) t) ((null-or-type t) t) ((type-or-string t) t)
    ;; 23.8.9
    (expression t)
    (form expression t)))

;;; The function STUPID-SUBTYPEP is used by translators to quickly determine
;;; whether a particular translator is potentially applicable to a given
;;; presentation (see 23.7.2 FIND-PRESENTATION-TRANSLATORS). Specification
;;; says, that type parameters should not be taken into account. That leads to
;;; a conclusion:
;;;
;;;   (stupid-subtypep a b) ; -> nil
;;;
;;; implies
;;;
;;;   (presentation-subtypep a b) ; -> (nil t)
;;;
;;; reverse implication is not true. For example:
;;;
;;;   (presentation-subtypep '(integer 1 2) '(integer 2 4)) ; -> (nil t)
;;;   (stupid-subtypep       '(integer 1 2) '(integer 2 4)) ; -> t
;;;
;;; McCLIM makes an exception for types OR, AND, COMPLETION and
;;; SUBSET-COMPLETION and sometimes accesses parameters. This is because
;;; subtypep relation these cases may be an exception, because type A may be a
;;; subtype of type B despite not inheriting from it.

(defun expect-t-t (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is (and yesp surep)
        "Expected (T T), got (~s ~s) for~%~
         ~2@T~2@T(presentation-subtypep ~s ~s)"
        yesp surep type supertype))
  #+mcclim
  ;; we can do this because *presentation-type-supertypes* doesn't do
  ;; clever things with type parameters
  (is-true (climi::stupid-subtypep type supertype)
           "(~S ~S ~S) did not return a true value"
           'climi::stupid-subtypep type supertype))

(defun expect-nil-t (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is (and (null yesp) surep)
        "Expected (NIL T) but got (~s ~s) for~%~
         ~2@T(presentation-subtypep ~s ~s)"
        yesp surep type supertype)))

(defun expect-nil-nil (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is (and (null yesp) (null surep))
        "Expected (NIL NIL) but got (~s ~s) for~%~
         ~2@T(presentation-typetypep ~s ~s)"
        yesp surep type supertype))
  ;; stupid-subtypep must be conservative in what it reports as possibly
  ;; acceptable.
  #+mcclim
  (is-true (climi::stupid-subtypep type supertype)
           "(~S ~S ~S) did not return a true value"
           'climi::stupid-subtypep type supertype))

(defun expect-t-nil (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is (and yesp (null surep))
        "Expected (T NIL), got (~s ~s)~%for ~s~%and ~s"
        yesp surep type supertype))
  ;; stupid-subtypep must be conservative in what it reports as possibly
  ;; acceptable.
  #+mcclim
  (is-true (climi::stupid-subtypep type supertype)))

(defun constantly-t (object)
  (declare (ignore object))
  t)

(test presentations.type-relations.1
  (loop for (type . supertypes) in *presentation-type-supertypes*
        do (expect-t-t type type)
        do (expect-t-t nil type)
           ;; The following assertion is not true for empty sets and meta
           ;; types, but the test data does not contain such examples. For
           ;; instance:
           ;;
           ;; (presentation-subtypep '(completion nil) nil) -> t t
           ;; (presentation-subtypep '(and (integer 1 2)
           ;;                              (integer 3 4)) nil) -> nil nil
        do (expect-nil-t type nil)
        do (mapcar (lambda (x) (expect-t-t type x)) supertypes)))

(test presentations.type-relations.2
  (loop for (type) in *presentation-type-supertypes*
        do (expect-t-t type `(and ,type))
        do (expect-t-t `(and ,type) type)
        do (expect-t-t `(and ,type) `(and ,type))
        do (expect-t-t type `(or ,type))
        do (expect-t-t `(or ,type) type)
        do (expect-t-t `(or ,type) `(or ,type))
        do (expect-t-t `(or ,type) `(and ,type))
        do (expect-t-t `(and ,type) `(or ,type))))

(test presentations.type-relations.3
  (loop for (type) in *presentation-type-supertypes*
        do (expect-t-t `(and ,type (satisfies constantly-t)) type)
        unless (eq (presentation-type-name type) 'completion)
          ;; This assertion is not true for COMPLETION, because in this case
          ;; we may test for each element.
          do (expect-nil-nil type `(and ,type (satisfies constantly-t)))
        do (expect-t-t `(and ,type (not nil)) type)
        do (expect-nil-nil type `(and ,type (not nil)))))

(test presentations.type-relations.4
  (expect-t-t '(or integer symbol) '(or integer symbol))
  (expect-t-t '(or integer symbol) '(or symbol integer))

  (expect-t-t '(or real complex) 'number)
  (fails (expect-t-t 'number '(or real complex))))

(test presentations.type-relations.5
  (let ((type (expand-presentation-type-abbreviation '(member "a" 3))))
    (fails (expect-t-t type '(or string integer)))))

;;; COMPLETION
(test presentations.type-relations.6
  (expect-t-t '(completion (1 2 3)) 'integer)
  (expect-nil-t '(completion (1 2 3)) 'string)
  (expect-nil-t '(completion (1 2 3 "foo")) 'integer)
  ;; See FIXME in presentation-type-functions.lisp for OR supertype.
  (fails (expect-t-t '(completion (1 2 3 "foo")) '(or string integer)))
  (expect-t-t '(completion (1 2 3)) '(and number (satisfies integerp)))
  (expect-nil-t '(completion (1 2 3 3.14)) '(and number (satisfies integerp)))
  (expect-t-t '(completion (3)) '(and (satisfies integerp)))
  ;; completion vs completion
  (expect-t-t '(completion (3)) '(completion (3 4 5)))
  (expect-nil-t '(completion (3 4)) '(completion (3 5)))
  ;; other presentation vs completion
  (expect-nil-nil 'integer '(completion (1 2 3 "foo")))
  (expect-nil-t 'integer '(completion ("foo" "bar"))))

;;; SUBSET-COMPLETION
(test presentations.type-relations.7
  (expect-t-t '(subset-completion (1 2 3 4)) '(sequence integer))
  (expect-nil-t '(subset-completion (1 2 3 4.3)) '(sequence integer))
  (expect-t-t '(subset-completion (1 2 "fam")) '(sequence t))
  (expect-nil-t '(subset-completion (1 2)) 'integer)
  ;; subset-completion vs subset-completion
  (expect-t-t '(subset-completion (1 2)) '(subset-completion (1 2 3)))
  (expect-nil-t '(subset-completion (1 2)) '(subset-completion (1))))

(test presentations.type-relations.sequence-enumerated
  ;; vs SEQUENCE-ENUMERATED
  (expect-t-t '(sequence-enumerated) '(sequence-enumerated))
  (expect-t-t '(sequence-enumerated integer integer) '(sequence-enumerated integer real))
  (expect-nil-t '(sequence-enumerated integer real) '(sequence-enumerated integer integer))
  (expect-nil-t '(sequence-enumerated integer) '(sequence-enumerated integer integer))
  (expect-nil-t '(sequence-enumerated integer integer) '(sequence-enumerated integer))
  ;; vs SEQUENCE
  (expect-t-t '(sequence-enumerated) '(sequence integer))
  (expect-t-t '(sequence-enumerated integer integer) '(sequence integer))
  (expect-nil-t '(sequence-enumerated real integer) '(sequence integer))
  (expect-nil-t '(sequence integer) '(sequence-enumerated real integer))
  (expect-nil-t '(sequence integer) '(sequence-enumerated integer integer)))

;;; This is a test for an issue where a parametrized class without
;;; defined PRESENTATION-TYPEP method doesn't error.
(test presentations.typep.1
  (defclass foo () ())
  (define-presentation-type foo (a))
  (is (not (presentation-typep 3 '(foo 3))))
  (signals error (presentation-typep (make-instance 'foo) '(foo 3))))

;;; Presentation type specifiers, names, parameters and options

(defclass class-presentation-type () ())

(define-presentation-type simple-presentation-type ())

(define-presentation-type presentation-type-with-parameters (foo bar))

(define-presentation-type presentation-type-with-options ()
  :options (baz fez))

(test presentations.type-parameters.smoke
  (flet ((expect (expected-parameters type-name)
           (let ((result (presentation-type-parameters type-name)))
             (is (equal expected-parameters result)
                 "~@<Expected ~S to have parameters ~:S, but got ~
                  ~:S.~@:>"
                 type-name expected-parameters result))))
    (expect '()        'class-presentation-type)
    (expect '()        (find-class 'class-presentation-type))
    (expect '()        'simple-presentation-type)
    (expect '(foo bar) 'presentation-type-with-parameters)
    (expect '()        'presentation-type-with-options)))

(test presentations.type-options.smoke
  (flet ((expect (expected-options type-name)
           (let ((result (presentation-type-options type-name)))
             (is (equal expected-options result)
                 "~@<Expected ~S to have options ~:S, but got ~
                  ~:S.~@:>"
                 type-name expected-options result))))
    (expect '()        'class-presentation-type)
    (expect '()        (find-class 'class-presentation-type))
    (expect '()        'simple-presentation-type)
    (expect '()        'presentation-type-with-parameters)
    (expect '(baz fez) 'presentation-type-with-options)))

(test presentations.forward-referenced-class
  (flet ((make-defclass-lambda (name super)
           (compile nil `(lambda ()
                           (defclass ,name (,super) ()))))
         (make-defptype-lambda (name super)
           (compile nil `(lambda ()
                           (define-presentation-type ,name ()
                             :inherit-from ',super)))))
    (flet ((do-test (root-type leaf-type)
             (let ((root (gensym))
                   (leaf (gensym)))
               ;; Undefined types do not have prototypes.
               (signals error (climi::prototype-or-error root))
               (signals error (climi::prototype-or-error leaf))
               ;; We may define a type with a forward-referenced supertype.
               (finishes
                 (funcall (ecase leaf-type
                            (:class (make-defclass-lambda leaf root))
                            (:ptype (make-defptype-lambda leaf root)))))
               ;; However it can't be finalized due to a forward reference.
               (signals error (climi::prototype-or-error leaf))
               ;; Define the forward-referenced super.
               (finishes
                 (funcall (ecase root-type
                            (:class (make-defclass-lambda root t))
                            (:ptype (make-defptype-lambda root t)))))
               (finishes (climi::prototype-or-error root))
               (finishes (climi::prototype-or-error leaf)))))
      (do-test :class :ptype)
      (do-test :ptype :ptype)
      ;; Anything fancy going on here?
      #+ (or) (do-test :class :class)
      ;; Standard classes does not inherit from the presentation-type classes.
      #+ (or) (do-test :ptype :class))))
