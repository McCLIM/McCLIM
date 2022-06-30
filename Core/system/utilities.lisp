;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2018 Cyrus Harmon <ch-lisp@bobobeach.com>
;;;  (c) copyright 2017-2020 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2019, 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Utilities used through the McCLIM codebase. Things that are in
;;; alexandria should not be duplicated here.

(in-package #:clim-internals)

(defun get-environment-variable (string)
  #+excl (sys:getenv string)
  #+(or cmu scl) (cdr (assoc string ext:*environment-list* :test #'string=))
  #+(or clisp abcl) (ext:getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #+openmcl (ccl::getenv string)
  #+lispworks (lw:environment-variable string)
  #+ecl (ext:getenv string)
  #+clasp (ext:getenv string)
  #-(or abcl ecl excl cmu scl clisp sbcl openmcl lispworks clasp)
  (error "GET-ENVIRONMENT-VARIABLE not implemented"))

;;; It would be nice to define this macro in terms of letf, but that
;;; would change the top-levelness of the enclosed forms.

#+excl
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (excl:package-definition-lock (find-package :common-lisp)) nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (excl:package-definition-lock (find-package :common-lisp)) t))))

#+clisp
(defmacro with-system-redefinition-allowed (&body body)
  `(ext:without-package-lock ("COMMON-LISP")
     ,@body))

#+openmcl
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setq ccl::*warn-if-redefine-kernel* nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setq ccl::*warn-if-redefine-kernel* t))))

#+cmu
(eval-when (:compile-toplevel :execute)
  (when (find-symbol "PACKAGE-LOCK" :ext)
    (pushnew 'clim-internals::package-locks *features*)))

#+(and cmu clim-internals::package-locks)
(eval-when (:load-toplevel)
  (unless (find-symbol "PACKAGE-LOCK" :ext)
    (error "Binary incompatibility: your CMUCL does not have package locks")))

#+cmu
(defmacro with-system-redefinition-allowed (&body body)
  #+clim-internals::package-locks
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (ext:package-definition-lock (find-package :common-lisp)) nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (ext:package-definition-lock (find-package :common-lisp)) t)))
  #-clim-internals::package-locks
  `(progn ,@body))

#+sbcl
(eval-when (:compile-toplevel :execute)
  (when (find-symbol "UNLOCK-PACKAGE" :sb-ext)
    (pushnew 'clim-internals::package-locks *features*)))

#+sbcl
(defmacro with-system-redefinition-allowed (&body body)
  #+clim-internals::package-locks
  `(progn
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (sb-ext:unlock-package :common-lisp))
    ,@body
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (sb-ext:lock-package :common-lisp)))
  #-clim-internals::package-locks
  `(progn
     ,@body))

#+ecl
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (ext:package-lock (find-package :common-lisp) nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (ext:package-lock (find-package :common-lisp) t))))

#-(or ecl excl openmcl cmu sbcl clisp)
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     ,@body))

(deftype function-designator ()
  `(or function symbol (cons (eql setf) (cons symbol null))))

;;; The command object is specified to be a list where the first argument is
;;; the command name (a symbol). However we allow symbols to be put in menus
;;; and drei inserts sometimes a literal function instead of a symbol.
(deftype command-designator ()
  `(or (cons function-designator) function-designator))

(defun 2+ (x)
  (+ x 2))

(defun 2- (x)
  (- x 2))

(declaim (inline lerp unlerp))

(defun lerp (v a b)
  (+ (* (- 1 v) a) (* v b)))

(defun unlerp (v a b)
  "Inverse linear interpolate (lerp).

Given an interpolated value V and two extreme values A and B, return
the blending factor. More precisely, return c such that

  V = (1 - c) A + c B.

When A = B, return 0.5."
  (if (= a b)
      0.5
      (/ (- v a) (- b a))))


(defun check-letf-form (form)
  (assert (and (listp form)
               (= 2 (length form)))))

(defun valueify (list)
  (if (and (consp list)
           (endp (rest list)))
      (first list)
      `(values ,@list)))

(defmacro letf ((&rest forms) &body body &environment env)
  "LETF ({(Place Value)}*) Declaration* Form* During evaluation of the
Forms, SETF the Places to the result of evaluating the Value forms.
The places are SETF-ed in parallel after all of the Values are
evaluated."
  (mapc #'check-letf-form forms)
  (let* (init-let-form save-old-values-setf-form
         new-values-set-form old-values-set-form
         update-form)
    (loop for (place new-value) in forms
          for (vars vals store-vars writer-form reader-form)
            = (multiple-value-list (get-setf-expansion place env))
          for old-value-names = (mapcar (lambda (var)
                                          (declare (ignore var))
                                          (gensym))
                                        store-vars)
          nconc (mapcar #'list vars vals)
            into temp-init-let-form
          nconc (copy-list store-vars)
            into temp-init-let-form
          nconc (copy-list old-value-names)
            into temp-init-let-form
          nconc `(,(valueify old-value-names) ,reader-form)
            into temp-save-old-values-setf-form
          nconc `(,(valueify store-vars) ,new-value)
            into temp-new-values-set-form
          nconc `(,(valueify store-vars) ,(valueify old-value-names))
            into temp-old-values-set-form
          collect writer-form
            into temp-update-form
          finally (setq init-let-form temp-init-let-form
                        save-old-values-setf-form temp-save-old-values-setf-form
                        new-values-set-form temp-new-values-set-form
                        old-values-set-form temp-old-values-set-form
                        update-form (cons 'progn temp-update-form)))
    `(let* ,init-let-form
       (setf ,@save-old-values-setf-form)
       (unwind-protect
            (progn (setf ,@new-values-set-form)
                   ,update-form
                   (progn ,@body))
         (setf ,@old-values-set-form)
         ,update-form))))

(defun map-repeated-sequence (result-type n function sequence)
  "Like CL:MAP, but applies \\arg{function} to \\arg{n} consecutive
elements of \\arg{sequence}. All the function's return values will be
gathered into the output sequence. \\arg{result-type} can also be NIL,
in which case the function is only applied for effect.

Examples:

 (map-repeated-sequence 'list 2 #'list '(1 2 3 4 5 6)) => ((1 2) (3 4) (5 6))
 (map-repeated-sequence 'list 2 #'+ '(1 2 3 4 5 6)) => (3 7 11)
 (map-repeated-sequence 'vector 3 #'+ '(1 2 3 4 5 6)) => #(6 15)

 (map-repeated-sequence 'list 2 #'floor '(2 1 4 3 6 5))
 => (2 0 1 1 1 1)

 (map-repeated-sequence 'list 2 #'cons '(color red weight 17 name fred))
 => ((COLOR . RED) (WEIGHT . 17) (NAME . FRED))

 (map-repeated-sequence 'list 1 #'(lambda (p) (values (car p) (cdr p)))
                        '((color . red) (weight . 17) (name . fred)))
 => (COLOR RED WEIGHT 17 NAME FRED)

Note:
 Be careful, since this function is quite sensible to the number of values
 returned by \\arg{function}.
"
  (assert (>= n 1))
  (cond ((eq result-type 'nil)
         ;; just map for effect
         (cond ((vectorp sequence)
                (loop for i from 0 below (length sequence) by n
                      do (apply function
                                (loop for j from 0 below n
                                      collect (aref sequence (+ i j))))))
               ((listp sequence)
                (let ((q sequence))
                  (loop until (null q)
                        do (apply function
                                  (loop for j from 0 below n
                                        collect (pop q))))))))
        (t
         ;; Otherwise, we (for now) take the easy route of calling
         ;; COERCE.
         (coerce
          (cond ((vectorp sequence)
                 (loop for i from 0 below (length sequence) by n
                       nconc (multiple-value-list
                              (apply function
                                     (loop for j from 0 below n
                                           collect (aref sequence (+ i j)))))))
                ((listp sequence)
                 (let ((q sequence))
                   (loop until (null q)
                         nconc (multiple-value-list
                                (apply function
                                       (loop for j from 0 below n
                                             collect (pop q))))))))
          result-type))))

(defmacro do-sequence ((vars sequence &optional result-form) &body body)
  "Iterate over SEQUENCE.  VARS is a list of symbols (or a single
symbol).  At each iteration the variables in VARS are bound to the
initial elements of the sequence.  The iteration is then \"stepped\"
by the number of variables in VARS."
  `(do-sequence* (,vars ,sequence nil ,result-form) ,@body))

(defmacro do-sequence* ((vars sequence step &optional result-form) &body body)
  "Iterate over SEQUENCE.  VARS is a list of symbols (or a single symbol).  At
each iteration the variables in VARS are bound to the initial elements of the
sequence.  The iteration is then \"stepped\" by the specified number."
  (flet ((list-stepper (n)
           (case n
             (1 'cdr)
             (2 'cddr)
             (3 'cdddr)
             (4 'cddddr)
             (t `(lambda (list) (nthcdr ,n list))))))
    (with-gensyms (body-fun i iter-limit)
      (once-only (sequence)
        (let* ((vars        (alexandria:ensure-list vars))
               (count       (length vars))
               (step        (or step count))
               (vector-args (loop for j from 0 below count
                                  collect `(aref ,sequence (+ ,i ,j)))))
          `(block nil
             (flet ((,body-fun ,vars
                      (tagbody
                         ,@body)))
               (declare (dynamic-extent (function ,body-fun)))
               (etypecase ,sequence
                 (list
                  (loop with ,iter-limit = (- (length ,sequence) ,count)
                        for ,i of-type alexandria:array-index from 0 by ,step
                        for ,vars on ,sequence by (function ,(list-stepper step))
                        until (> ,i ,iter-limit)
                        do (,body-fun ,@vars)))
                 (vector
                  (loop with ,iter-limit = (- (length ,sequence) ,count)
                        for ,i of-type alexandria:array-index from 0 by ,step
                        until (> ,i ,iter-limit)
                        do (,body-fun ,@vector-args)))))
             ,@(when result-form
                 `((let ,vars ; bind variables to NIL
                     (declare (ignorable ,@vars))
                     ,result-form)))))))))

;;;;
;;;; meta functions
;;;;

;; these are as in Dylan

(defun curry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append args more))))

(define-compiler-macro curry (fun &rest args)
  `(lambda (&rest more)
     (apply ,fun ,@args more)))

(defun always (x)
  #'(lambda (&rest more)
      (declare (ignore more))
      x))

(define-compiler-macro always (x)
  (let ((g (gensym)))
    `(let ((,g ,x))
       (lambda (&rest more)
         (declare (ignore more))
         ,g))))

(defun clamp (val min max)
  (max (min val max) min))

;;; Convenience macros

(define-modify-macro maxf (&rest args) max)
(define-modify-macro minf (&rest args) min)
(define-modify-macro nconcf (&rest args) nconc)
(define-modify-macro orf (&rest args) or)
(define-modify-macro clampf (min max) clamp)

;;; Anaphoric

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro aand (&rest args)
  (cond ((endp args) t)
        ((endp (rest args)) (first args))
        (t `(aif ,(first args) (aand ,@(rest args))))))

;;; Lexical (curbed from alexandria)
(defmacro if-let (bindings &body (then-form &optional else-form))
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body forms)
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro when-let* (bindings &body body)
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(let (,(car bindings))
                      (when ,(caar bindings)
                        ,(bind (cdr bindings) body)))
                   `(progn ,@body))))
      (bind binding-list body))))

;;; curbed from uiop
(defmacro nest (&rest things)
  (reduce #'(lambda (outer inner) `(,@outer ,inner))
          things :from-end t))

(declaim (inline maybe-funcall maybe-apply))

(defun maybe-funcall (function &rest args)
  "If FUNCTION is not NIL, funcall it."
  (when function (apply function args)))

(defun maybe-apply (function &rest args)
  "If FUNCTION is not NIL, apply it."
  (when function (apply #'apply function args)))

;;; Remove keyword pairs from an argument list, consing as little as
;;; possible.
(defun remove-keywords (arg-list keywords)
  (let ((clean-tail arg-list))
    ;; First, determine a tail in which there are no keywords to be removed.
    (loop for arg-tail on arg-list by #'cddr
          for (key) = arg-tail
          do (when (member key keywords :test #'eq)
               (setq clean-tail (cddr arg-tail))))
    ;; Cons up the new arg list until we hit the clean-tail, then nconc that on
    ;; the end.
    (loop for arg-tail on arg-list by #'cddr
          for (key value) = arg-tail
          if (eq arg-tail clean-tail)
            nconc clean-tail
            and do (loop-finish)
          else if (not (member key keywords :test #'eq))
                 nconc (list key value)
          end)))

(defmacro with-keywords-removed ((var keywords &optional (new-var var))
                                 &body body)
  "Binds NEW-VAR (defaults to VAR) to VAR with the keyword arguments specified
in KEYWORDS removed."
  `(let ((,new-var (remove-keywords ,var ',keywords)))
     ,@body))

(defun symbol-concat (&rest symbols)
  (intern (apply #'concatenate 'string (mapcar #'string symbols))))

(defun stream-designator-symbol (symbol default)
  "Maps T to DEFAULT, barfs if argument does not look good.
   To be used in the various WITH-... macros."
  (cond ((eq symbol 't)
         default)
        ((symbolp symbol)
         symbol)
        (t
         (error "~S Can not be a stream designator for ~S" symbol default))))

(defun declare-ignorable-form (variables)
  #+CMU
  ;; CMUCL barfs if you declare a special variable ignorable, work
  ;; around that.
  `(declare (ignorable
             ,@(remove-if (lambda (symbol)
                            (eq :special (lisp::info lisp::variable lisp::kind symbol)))
                          variables)))
  #-CMU
  `(declare (ignorable ,@variables)))

;;; spread version:

(defun declare-ignorable-form* (&rest variables)
  (declare-ignorable-form variables))

(defun gen-invoke-trampoline (fun to-bind to-pass body)
  "Macro helper function, generates the LABELS / INVOKE-WITH-... ideom."
  (let ((cont (gensym ".CONT.")))
    `(labels ((,cont (,@to-bind)
               ,(declare-ignorable-form to-bind)
               ,@body))
      (declare (dynamic-extent #',cont))
      (,fun ,@to-bind #',cont ,@to-pass))))

;;; Macro writing

;;; This is a utility which is intended to be used by macro writers to
;;; help application programmers. It allows the CL implementation to
;;; associate conditions signaled during macroexpansion with sub-forms
;;; of the whole macro form. In practice this means tools like SLIME
;;; will highlight that sub-form when reporting the error. This macro
;;; is most useful in complex macros such as
;;; `define-application-frame'.
(defmacro with-current-source-form ((form &rest more-forms) &body body)
  "Associate errors signaled while executing BODY with FORM.
MORE-FORMS are used when FORM is unsuitable (for example some
implementations cannot associate errors with atoms). MORE-FORMS should
be forms containing FORM."
  #-sbcl (declare (ignore form more-forms))
  #+sbcl `(sb-ext:with-current-source-form (,form ,@more-forms) ,@body)
  #-sbcl `(progn ,@body))


(defun delete-1 (item list &key (test #'eql) (key #'identity))
  "Delete 1 ITEM from LIST. Second value is T if item was deleted."
  (loop
    for tail on list
    and tail-prev = nil then tail
    for (list-item) = tail
    if (funcall test item (funcall key list-item))
      do (return-from delete-1
           (if tail-prev
               (progn
                 (setf (cdr tail-prev) (cdr tail))
                 (values list t))
               (values (cdr tail) t)))
    finally (return (values list nil))))

(defun copy-sequence-into-vector (sequence)
  (typecase sequence
    (vector (copy-seq sequence))
    (t (coerce sequence 'vector))))

(defun rebind-arguments (arg-list)
  "Create temporary variables for non keywords in a list of
  arguments. Returns two values: a binding list for let, and a new
  argument list with the temporaries substituted in."
  (loop
     for arg in arg-list
     for var = (gensym)
     if (keywordp arg)
       collect arg into new-arg-list
     else
       collect `(,var ,arg) into bindings
       and collect var into new-arg-list
     end
     finally (return (values bindings new-arg-list))))

;;; Taken from a stackoverflow thread[1] then extended per suggestion in peer
;;; review[2]. Use with care (should work for "ordinary" classes).
;;;
;;; [1] https://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects#11068536
;;; [2] https://github.com/McCLIM/McCLIM/pull/833#discussion_r322010160
(defun shallow-copy-object (original &optional (new-class (class-of original)))
  (let ((copy (allocate-instance new-class)))
    (mapc (lambda (slot &aux (slot-name (c2mop:slot-definition-name slot)))
            (when (and (slot-exists-p original slot-name)
                       (slot-boundp original slot-name))
              (setf (slot-value copy slot-name)
                    (slot-value original slot-name))))
          (c2mop:class-slots new-class))
    copy))

(defmacro dolines ((line string &optional result) &body body)
  "Iterates over lines in string separated by #\newline."
  (with-gensyms (substr end)
    (once-only (string)
      `(do* ((,substr ,string (subseq ,substr (1+ ,end)))
             (,end  #1=(position #\newline ,substr) #1#)
             (,line #2=(subseq ,substr 0 ,end) #2#))
            ((null ,end) ,@body ,result)
         ,@body))))

;;;; The Collect macro:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-normal-expander (n-value fun forms)
    `(progn
       ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
       ,n-value))

  (defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
         ,@(mapcar #'(lambda (form)
                       `(let ((,n-res (cons ,form nil)))
                          (cond (,n-tail
                                 (setf (cdr ,n-tail) ,n-res)
                                 (setq ,n-tail ,n-res))
                                (t
                                 (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                   forms)
         ,n-value))))

(defmacro collect (collections &body body)
  (let (macros binds)
    (dolist (spec collections)
      (cond ((atom spec)
             (setf spec (list spec)))
            ((not (<= 1 (length spec) 3))
             (error "Malformed collection specifier: ~S." spec)))
      (let ((n-value (gensym))
            (name (first spec))
            (default (second spec))
            (kind (or (third spec) 'collect)))
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
            (let ((n-tail (gensym)))
              (if default
                  (push `(,n-tail (last ,n-value)) binds)
                  (push n-tail binds))
              (push `(,name (&rest args)
                            (collect-list-expander ',n-value ',n-tail args))
                    macros))
            (push `(,name (&rest args)
                          (collect-normal-expander ',n-value ',kind args))
                  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

;;;
;;; pretty printing
;;;
;;; This is a simplified version of the approach used by David
;;; Lichteblau for pretty-printing objects in cxml-stp/node.lip
(defgeneric slots-for-pprint-object (node)
  (:documentation "A generic function that returns the slot names of
  objects to be pretty-printed by simple-pprint-object. Providers of
  print-object methods that intend to use simple-pprint-object should
  provide their own methods that return the appropriate slot-names.")
  (:method-combination append)
  (:method append ((object t)) nil))

;;; print :slot-name slot-value for each slot
(defgeneric simple-pprint-object-args (stream object)
  (:method (stream object)
    (let ((slots (mapcan (lambda (slot)
                           (let ((value (slot-value object slot)))
                             (list (list slot value))))
                         (slots-for-pprint-object object))))
      (loop for (slot-name slot-value) in slots
         do
           (write-char #\Space stream)
           (pprint-newline :fill stream)
           (write-char #\: stream)
           (princ slot-name stream)
           (write-char #\Space stream)
           (unless (atom slot-value)
             (princ "'" stream))
           (write slot-value :stream stream)))))

(defgeneric simple-pprint-object (stream object)
  (:method (stream object)
    (pprint-logical-block (stream (list object) :prefix "#.(" :suffix ")")
      (write (intern "MAKE-INSTANCE" *package*) :stream stream)
      (write-char #\Space stream)
      (write-char #\' stream)
      (write (class-name (class-of object)) :stream stream)
      (simple-pprint-object-args stream object))))

(defmacro maybe-print-readably ((self sink) &body body)
  `(cond
     ((and *print-readably* (not *read-eval*))
      (error "cannot readably print object of type ~A when not *read-eval*." (type-of ,self)))
     ((and *print-pretty* *print-readably*)
      (simple-pprint-object ,sink ,self))
     (t ,@body)))
