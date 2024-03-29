;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Functions and presentation generic functions implementing the
;;; presentation type predicates and utilities.
;;;

(in-package #:clim-internals)

(define-presentation-generic-function %describe-presentation-type
    describe-presentation-type
  (type-key parameters options type stream plural-count ))

;;; Support for the default method on describe-presentation-type: if a CLOS
;;; class has been defined as a presentation type, get description out of the
;;; presentation type.

(defmethod description ((class standard-class))
  (let ((name (class-name class)))
    (if-let ((ptype (find-presentation-type name nil)))
      (description ptype)
      (make-default-description name))))

(defun default-describe-presentation-type (description stream plural-count)
  (when (symbolp description)
    (setq description (make-default-description (symbol-name description))))
  (cond ((eql 1 plural-count)
         (format stream "~:[a~;an~] ~A"
                   (find (char description 0) "aeiouAEIOU")
                   description))
        ((numberp plural-count)
         (format stream "~D ~A~P" plural-count description plural-count))
        (plural-count
         (format stream "~As" description))
        (t (write-string description stream))))

(define-default-presentation-method describe-presentation-type
    (type stream plural-count)
  (with-presentation-type-decoded (name parameters options)
      type
    (declare (ignore name parameters))
    (let ((description (or (getf options :description)
                           (description (class-of type-key)))))
      (default-describe-presentation-type description
                                          stream
                                          plural-count))))

(defun describe-presentation-type
    (type &optional (stream *standard-output*) (plural-count 1))
  (flet ((describe-it (stream)
           (funcall-presentation-generic-function
            describe-presentation-type type stream plural-count)))
    (if stream
        (describe-it stream)
        (with-output-to-string (s)
          (describe-it s)))))

(define-presentation-generic-function
    %presentation-type-specifier-p
  presentation-type-specifier-p
  (type-class type))

(define-default-presentation-method presentation-type-specifier-p (type)
  (declare (ignore type))
  t)

(defun presentation-type-specifier-p (object)
  "Return true if `object' is a valid presentation type specifier,
otherwise return false."
  ;; Apparently, this funtion has to handle arbitrary objects.
  (let ((name (presentation-type-name object)))
    (when (and (typep name '(or symbol class))
               (get-ptype-metaclass name))
      (funcall-presentation-generic-function presentation-type-specifier-p object))))

(define-presentation-generic-function %presentation-typep presentation-typep
  (type-key parameters object type))

;;; The following two methods are defined for inheritance and combined, they
;;; implement the default behavior of the presentation method
;;; PRESENTATION-TYPEP. "basic" presentation types include all objects and
;;; "clos" presentation types error, because the method must be implemented
;;; for parametrized standard classes. -- jd 2020-07-09

(define-presentation-method presentation-typep (object (type t))
  (declare (ignore object))
  t)

(define-presentation-method presentation-typep (object (type standard-object))
  (declare (ignore object))
  (or (presentation-type-class-p (find-presentation-type-class type))
      (error "The presentation type ~s doesn't implement a ~s method."
             type 'presentation-typep)))

(defun presentation-typep (object type)
  (with-presentation-type-decoded (name parameters) type
    (case name
      ((t) t)                           ; super type
      ((nil) nil)                       ; empty type
      (otherwise
       (let ((clos-class (find-class name nil))) ; Don't error out.
         (when (and clos-class (typep clos-class 'standard-class))
           (unless (typep object name)
             (return-from presentation-typep nil))
           (when (null parameters)
             (return-from presentation-typep t))))
       (funcall-presentation-generic-function presentation-typep object type)))))

(define-presentation-generic-function
    %presentation-subtypep
    presentation-subtypep
  (type-key type putative-supertype))

(defun map-completion-items (function completion-type-parameters)
  (destructuring-bind (sequence &key (value-key 'identity)
                                &allow-other-keys)
      completion-type-parameters
    (map nil (lambda (element)
               (funcall function (funcall value-key element)))
         sequence)))

(defun every-completion-item (function completion-type-parameters)
  (map-completion-items (lambda (element)
                          (when (not (funcall function element))
                            (return-from every-completion-item nil)))
                        completion-type-parameters)
  t)

(defun some-completion-item (function completion-type-parameters)
  (map-completion-items (lambda (element)
                          (when (funcall function element)
                            (return-from some-completion-item t)))
                        completion-type-parameters)
  nil)

;;; PRESENTATION-SUBTYPEP suffers from some of the same problems as
;;; CL:SUBTYPEP, most (but sadly not all) of which were solved in H. Baker "A
;;; Decision Procedure for SUBTYPEP"; additionally, it suffers from the
;;; behaviour being underspecified, as CLIM documentation did not have the
;;; years of polish that CLtS did.
;;;
;;; So you might wonder why, instead of copying or using directly some decent
;;; Public Domain subtype code (such as that found in SBCL, implementing
;;; CL:SUBTYPEP), there's this slightly wonky implementation here.  Well, some
;;; of the answer lies in the fact that the subtype relationships answered by
;;; this predicate are not in fact analogous to CL's type system.  The major
;;; use of PRESENTATION-SUBTYPEP seems to be for determining whether a
;;; presentation is applicable as input to a translator (including the default
;;; translator, transforming an object to itself); actually, the first step is
;;; taken by STUPID-SUBTYPEP, but that I believe is simply intended to be a
;;; short-circuiting conservative version of PRESENTATION-SUBTYPEP.
;;;
;;; Most presentation types in CLIM are hierarchically arranged by
;;; single-inheritance, and SUBTYPEP relations on the hierarchy are easy to
;;; determine: simply walk up the hierarchy until you find the putative
;;; supertype (in which case the answer is T, T unless the type's parameters
;;; are wrong) or you find the universal supertype (in which case the answer
;;; is NIL, T.  There are numerous wrinkles, however...
;;;
;;; (1) the NIL presentation type is the universal subtype, breaking the
;;;     single-inheritance of the hierarchy.  This isn't too bad, because it
;;;     can be special-cased.
;;;
;;; (2) union types can be constructed, destroying the single-inheritance
;;;     hierarchy (when used as a subtype).
;;;
;;; (3) union types can give rise to ambiguity.  For example, is the NUMBER
;;;     presentation type subtypep (OR REAL COMPLEX)?  What about (INTEGER 3
;;;     6) subtypep (OR (INTEGER 3 4) (INTEGER 5 6))?  Is (OR A B) subtypep
;;;     (OR B A)?  The answer to this last question is not obvious, as the two
;;;     types have different ACCEPT behaviour if A and B have any Lisp objects
;;;     in common, even if the presentation types are hierarchically
;;;     unrelated...
;;;
;;; (4) intersection types can be constructed, destroying the
;;;     single-inheritance hierarchy (when used as a supertype).  This is
;;;     partially mitigated by the explicit documentation that the first type
;;;     in the AND type's parameters is privileged and treated specially by
;;;     ACCEPT.
;;;
;;; Given these difficulties, I'm aiming for roughly expected behaviour from
;;; STUPID- and PRESENTATION-SUBTYPEP, rather than something which has a
;;; comprehensive understanding of presentation types and the Lisp object
;;; universe (as this would be unachievable anyway: the user can write
;;; arbitrary PRESENTATION-TYPEP functions); PRESENTATION-SUBTYPEP should not
;;; be thought of as a predicate over sets of Lisp objects, but simply a
;;; formal predicate over a graph of names.  This gives rise to the
;;; implementation below for OR and AND types, and the hierarchical walk for
;;; all other types.  -- CSR, 2007-01-10
;;;
;;; (5) the COMPLETION presentation type ("one of") is equivalent to the
;;;     Common Lisp type MEMBER.
;;;
;;;     - when both types are COMPLETION then "normal" rules apply
;;;
;;;     - when MAYBE-SUBTYPE is COMPLETION then use individual objects to
;;;       determine its relation with MAYBE-SUPERTYPE
;;;
;;;     - when MAYBE-SUPERTYPE is COMPLETION return (values t nil), because we
;;;       can't tell whether the object presented with MAYBE-SUBTYPE is not
;;;       one of the completion possibilities
;;;
;;; (6) the SUBSET-COMPLETION presentation type ("some of") doesn't have
;;;     equivalent Common Lisp type. When it is used as MAYBE-SUBTYPE and
;;;     MAYBE-SUPERTYPE is SEQUENCE, then individual objects are used to
;;;     determine whether they satisfy the sequence element type.
;;;
;;; Both types depend on the result of calling PRESENTATION-TYPE-OF or
;;; PRESENTATION-TYPEP on member objects and can't be determined based on the
;;; single-inheritance lattice alone. -- jd 2020-07-10
;;;
(defun presentation-subtypep (maybe-subtype maybe-supertype)
  ;; special shortcuts: the universal subtype is privileged (and
  ;; doesn't in fact fit into a hierarchical lattice); the universal
  ;; supertype is easy to identify.
  (when (or (eql maybe-subtype nil) (eql maybe-supertype t))
    (return-from presentation-subtypep (values t t)))
  (when (eql maybe-subtype maybe-supertype)
    (return-from presentation-subtypep (values t t)))
  (with-presentation-type-decoded (maybe-supertype-name maybe-supertype-parameters)
      maybe-supertype
    (with-presentation-type-decoded (maybe-subtype-name maybe-subtype-parameters)
        maybe-subtype
      (cond
        ;; DO NOT BE TEMPTED TO REARRANGE THESE CLAUSES
        ((eq maybe-subtype-name 'or)
         (dolist (or-type maybe-subtype-parameters
                          (return-from presentation-subtypep (values t t)))
           (multiple-value-bind (yesp surep)
               (presentation-subtypep or-type maybe-supertype)
             (unless yesp
               (return-from presentation-subtypep (values yesp surep))))))
        ((eq maybe-supertype-name 'and)
         (let ((result t))
           (dolist (and-type maybe-supertype-parameters
                             (return-from presentation-subtypep
                               (values result result)))
             (cond
               ((and (consp and-type) (eq (car and-type) 'satisfies))
                (if (eq maybe-subtype-name 'completion)
                    (let ((predicate (second and-type)))
                      (unless (every-completion-item predicate maybe-subtype-parameters)
                        (return-from presentation-subtypep (values nil t))))
                    (setq result nil)))
               ((and (consp and-type) (eq (car and-type) 'not))
                (multiple-value-bind (yp sp)
                    (presentation-subtypep maybe-subtype (cadr and-type))
                  (declare (ignore sp))
                  (if yp
                      (return-from presentation-subtypep (values nil t))
                      (setq result nil))))
               (t (multiple-value-bind (yp sp)
                      (presentation-subtypep maybe-subtype and-type)
                    (unless yp
                      (if sp
                          (return-from presentation-subtypep (values nil t))
                          (setq result nil)))))))))
        ((eq maybe-supertype-name 'or)
         (assert (not (eq maybe-subtype-name 'or)))
         ;; FIXME: this would be the right method were it not for the
         ;; fact that there can be unions 'in disguise' in the
         ;; subtype; examples:
         ;;
         ;;   (PRESENTATION-SUBTYPEP 'NUMBER '(OR REAL COMPLEX))
         ;;   (PRESENTATION-SUBTYPEP '(INTEGER 3 6)
         ;;                          '(OR (INTEGER 2 5) (INTEGER 4 7)))
         ;; Sorry about that.
         (let ((surep t))
           (dolist (or-type maybe-supertype-parameters
                            (return-from presentation-subtypep (values nil surep)))
             (multiple-value-bind (yp sp)
                 (presentation-subtypep maybe-subtype or-type)
               (cond
                 (yp (return-from presentation-subtypep (values t t)))
                 ((not sp) (setq surep nil)))))))
        ((eq maybe-subtype-name 'and)
         (assert (not (eq maybe-supertype-name 'and)))
         (multiple-value-bind (yp sp)
             (presentation-subtypep (car maybe-subtype-parameters) maybe-supertype)
           (declare (ignore sp))
           (return-from presentation-subtypep (values yp yp))))
        ((and (eq maybe-subtype-name 'completion)
              (not (eq maybe-supertype-name 'completion)))
         (return-from presentation-subtypep
           (values (every-completion-item
                    (alexandria:rcurry #'presentation-typep maybe-supertype)
                    maybe-subtype-parameters)
                   t)))
        ((and (not (eq maybe-subtype-name 'completion))
              (eq maybe-supertype-name 'completion))
         (return-from presentation-subtypep
           (values nil
                   (not (some-completion-item
                         (alexandria:rcurry #'presentation-typep maybe-subtype)
                         maybe-supertype-parameters)))))
        ((and (eq maybe-subtype-name 'subset-completion)
              (eq maybe-supertype-name 'sequence))
         (let ((element-type (first maybe-supertype-parameters)))
           (return-from presentation-subtypep
             (values (every-completion-item
                      (alexandria:rcurry #'presentation-typep element-type)
                      maybe-subtype-parameters)
                     t))))
        ((and (eq maybe-subtype-name 'sequence-enumerated)
              (eq maybe-supertype-name 'sequence))
         (let ((sub-element-types maybe-subtype-parameters)
               (super-element-type (first maybe-supertype-parameters)))
           (return-from presentation-subtypep
             (values (every (alexandria:rcurry #'presentation-subtypep
                                               super-element-type)
                            sub-element-types)
                     t))))))
    (map-over-presentation-type-supertypes
     #'(lambda (name massaged)
         (when (eq name maybe-supertype-name)
           (return-from presentation-subtypep
             (funcall-presentation-generic-function presentation-subtypep
                                                    massaged
                                                    maybe-supertype))))
     maybe-subtype))
  (values nil t))

;;; This is to implement the requirement on presentation translators for doing
;;; subtype calculations without reference to type parameters. We are generous
;;; in that we return T when we are unsure, to give translator testers a
;;; chance to accept or reject the translator. This is essentially
;;;
;;;   (multiple-value-bind (yesp surep)
;;;       (presentation-subtypep maybe-subtype maybe-supertype)
;;;     (or yesp (not surep)))
;;;
;;; except faster. The above relation should not be taken literally. The
;;; actual relation between predicates is
;;;
;;;   (unless (stupid-subtypep maybe-subtype maybe-supertype)
;;;     (multiple-value-bind (yesp surep)
;;;         (presentation-subtypep maybe-subtype maybe-supertype)
;;;       (assert (and (null yesp) surep))))
;;;
;;; for example the first relation is not preserved with
;;;
;;;   (presentation-subtypep '(integer 1 2) '(integer 2 4)) ; -> (nil t)
;;;   (stupid-subtypep       '(integer 1 2) '(integer 2 4)) ; -> t
;;;
(defun stupid-subtypep (maybe-subtype maybe-supertype)
  "Return T if MAYBE-SUBTYPE is a presentation subtype of
MAYBE-SUPERTYPE, regardless of parameters."
  (when (or (eq maybe-subtype nil) (eq maybe-supertype t))
    (return-from stupid-subtypep t))
  (when (eql maybe-subtype maybe-supertype)
    (return-from stupid-subtypep t))
  (let ((maybe-subtype-name (presentation-type-name maybe-subtype))
        (maybe-supertype-name (presentation-type-name maybe-supertype)))
    ;; In most cases, meta types can only be compared meaningfully by
    ;; considering the presentation type parameters, but
    ;; STUPID-SUBTYPEP is not supposed to do that, so return the
    ;; conservative answer.
    ;;
    ;; SEQUENCE, SEQUENCE-ENUMERATED and SUBSET-COMPLETION are special
    ;; in that certain combinations do not have to be handled
    ;; conservatively.
    (when (or (member maybe-subtype-name '(or and completion))
              (member maybe-supertype-name '(or and completion))
              (and (eq maybe-supertype-name 'sequence)
                   (member maybe-subtype-name '(subset-completion
                                                sequence-enumerated))))
      (return-from stupid-subtypep t))
    ;; Normal types.
    (let ((subtype-meta (get-ptype-metaclass maybe-subtype-name))
          (maybe-supertype-meta (get-ptype-metaclass maybe-supertype-name)))
      (unless (and subtype-meta maybe-supertype-meta)
        (return-from stupid-subtypep nil))
      (map-over-ptype-superclasses (lambda (super)
                                     (when (eq maybe-supertype-meta super)
                                       (return-from stupid-subtypep t)))
                                   maybe-subtype-name)
      nil)))

(define-default-presentation-method presentation-subtypep
    (type maybe-supertype)
  (with-presentation-type-decoded (name params)
      type
    (declare (ignore name))
    (with-presentation-type-decoded (super-name super-params)
        maybe-supertype
      (declare (ignore super-name))
      (if (equal params super-params)
          (values t t)
          (values nil nil)))))

(define-presentation-generic-function
    %map-over-presentation-type-supertypes
  map-over-presentation-type-supertypes
  (type-key function type))

;;; Define the method for presentation and clos types
(define-default-presentation-method map-over-presentation-type-supertypes
    (function type)
  (let ((type-name (presentation-type-name type)))
    (map-over-ptype-superclasses
     #'(lambda (super)
         (let ((super-name (type-name super)))
           (funcall function
                    super-name
                    (funcall (expansion-function super)
                             (translate-specifier-for-type type-name
                                                           super-name
                                                           type)))))
     type-name)))

(defun map-over-ptype-superclasses (function type)
  (let* ((type-name (presentation-type-name type))
         (type-meta (get-ptype-metaclass type-name))
         (type-is-ptype (presentation-type-class-p type-meta)))
    (unless type-meta
      (return-from map-over-ptype-superclasses nil))
    (loop
      for super-meta in (safe-cpl type-meta)
      ;; structure classes?
      when (and (or (typep super-meta 'standard-class)
                    (eq super-meta *builtin-t-class*))
                (not (and type-is-ptype
                          (eq super-meta *standard-object-class*))))
        do (funcall function super-meta))))

(defun map-over-presentation-type-supertypes (function type)
  (funcall-presentation-generic-function
   map-over-presentation-type-supertypes function type))
