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
        "Expected (T T), got (~s ~s)~%for ~s~%and ~s"
        yesp surep type supertype))
  #+mcclim
  ;; we can do this because *presentation-type-supertypes* doesn't do
  ;; clever things with type parameters
  (is-true (climi::stupid-subtypep type supertype)))

(defun expect-nil-t (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is (and (null yesp) surep)
        "Expected (NIL T), got (~s ~s)~%for ~s~%and ~s"
        yesp surep type supertype)))

(defun expect-nil-nil (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is (and (null yesp) (null surep))
        "Expected (NIL NIL), got (~s ~s)~%for ~s~%and ~s"
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
  (expect-nil-t '(completion (3 4)) '(completion (3 5))))

;;; SUBSET-COMPLETION
(test presentations.type-relations.7
  (expect-t-t '(subset-completion (1 2 3 4)) '(sequence integer))
  (expect-nil-t '(subset-completion (1 2 3 4.3)) '(sequence integer))
  (expect-t-t '(subset-completion (1 2 "fam")) '(sequence t))
  (expect-nil-t '(subset-completion (1 2)) 'integer)
  ;; subset-completion vs subset-completion
  (expect-t-t '(subset-completion (1 2)) '(subset-completion (1 2 3)))
  (expect-nil-t '(subset-completion (1 2)) '(subset-completion (1))))

;;; This is a test for an issue where a parametrized class without
;;; defined PRESENTATION-TYPEP method doesn't error.
(test presentations.typep.1
  (defclass foo () ())
  (define-presentation-type foo (a))
  (is (not (presentation-typep 3 '(foo 3))))
  (signals error (presentation-typep (make-instance 'foo) '(foo 3))))
