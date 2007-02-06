;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Functionality designed to aid development of Common Lisp code.

(in-package :drei-lisp-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler note hyperlinking

(defun make-compiler-note (note-list)
  (let ((severity (getf note-list :severity))
        (message (getf note-list :message))
        (location (getf note-list :location))
        (references (getf note-list :references))
        (short-message (getf note-list :short-message)))
    (make-instance
     (ecase severity
       (:error 'error-compiler-note)
       (:read-error 'read-error-compiler-note)
       (:warning 'warning-compiler-note)
       (:style-warning 'style-warning-compiler-note)
       (:note 'note-compiler-note))
     :message message :location location
     :references references :short-message short-message)))

(defclass compiler-note ()
  ((message :initarg :message :initform nil :accessor message)
   (location :initarg :location :initform nil :accessor location)
   (references :initarg :references :initform nil :accessor references)
   (short-message :initarg :short-message :initform nil :accessor short-message))
  (:documentation "The base for all compiler-notes."))

(defclass error-compiler-note (compiler-note) ())

(defclass read-error-compiler-note (compiler-note) ())

(defclass warning-compiler-note (compiler-note) ())

(defclass style-warning-compiler-note (compiler-note) ())

(defclass note-compiler-note (compiler-note) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code interrogation/form analysis

(defparameter +cl-arglist-keywords+
  lambda-list-keywords)

(defparameter +cl-garbage-keywords+
  '(&whole &environment))

(defun arglist-keyword-p (arg)
  "Return T if `arg' is an arglist keyword. NIL otherwise."
  (when (member arg +cl-arglist-keywords+)
    t))

(defun split-arglist-on-keywords (arglist)
  "Return an alist keying lambda list keywords of `arglist'
to the symbols affected by the keywords."
  (let ((sing-result '())
        (env (position '&environment arglist)))
    (when env
      (push (list '&environment (elt arglist (1+ env))) sing-result)
      (setf arglist (remove-if (constantly t) arglist :start env :end (+ env 2))))
    (when (eq '&whole (first arglist))
      (push (subseq arglist 0 2) sing-result)
      (setf arglist (cddr arglist)))
    (do ((llk '(&mandatory &optional &key &allow-other-keys &aux &rest &body))
         (args (if (arglist-keyword-p (first arglist))
                   arglist
                   (cons '&mandatory arglist))
               (cdr args))
         (chunk '())
         (result '()))
        ((null args)
         (when chunk (push (nreverse chunk) result))
         (nreverse (nconc sing-result result)))
      (if (member (car args) llk)
          (progn
            (when chunk (push (nreverse chunk) result))
            (setf chunk (list (car args))))
          (push (car args) chunk)))))

(defun find-optional-argument-values (arglist provided-args &optional
                                      (split-arglist
                                       (split-arglist-on-keywords
                                        arglist)))
  "Return an association list mapping symbols of optional or
  keyword arguments from `arglist' to the specified values in
  `provided-args'. `Split-arglist' should be either a split
  arglist or nil, in which case it will be calculated from
  `arglist'."
  (flet ((get-args (keyword)
           (rest (assoc keyword split-arglist))))
    (let* ((mandatory-args-count (length (get-args '&mandatory)))
           (optional-args-count (length (get-args '&optional)))
           (keyword-args-count (length (get-args '&key)))
           (provided-args-count (length provided-args))
           (nonmandatory-args-count (+ keyword-args-count
                                       optional-args-count)))
      ;; First we check whether any optional arguments have even been
      ;; provided.
      (when (> provided-args-count
               mandatory-args-count)
        ;; We have optional arguments.
        (let (
              ;; Find the part of the provided arguments that concern
              ;; optional arguments.
              (opt-args-values (subseq provided-args
                                       mandatory-args-count
                                       (min provided-args-count
                                            nonmandatory-args-count)))
              ;; Find the part of the provided arguments that concern
              ;; keyword arguments.
              (keyword-args-values (subseq provided-args
                                           (min (+ mandatory-args-count
                                                   optional-args-count)
                                                provided-args-count))))
          (append (mapcar #'cons
                          (mapcar #'unlisted (get-args '&optional))
                          opt-args-values)

                  (loop
                     ;; Loop over the provided keyword symbols and
                     ;; values in the argument list. Note that
                     ;; little checking is done to ensure that the
                     ;; given symbols are valid - this is not a
                     ;; compiler, so extra mappings do not
                     ;; matter.
                     for (keyword value) on keyword-args-values by #'cddr
                     if (keywordp keyword)
                     collect (let ((argument-symbol
                                    (unlisted (find (symbol-name keyword)
                                                    (get-args '&key)
                                                    :key #'(lambda (arg)
                                                             (symbol-name (fully-unlisted arg)))
                                                    :test #'string=))))
                               ;; We have to find the associated
                               ;; symbol in the argument list... ugly.
                               (cons argument-symbol
                                     value)))))))))

(defun find-affected-simple-arguments (arglist current-arg-index preceding-arg
                                       &optional (split-arglist (split-arglist-on-keywords arglist)))
  "Find the simple arguments of `arglist' that would be affected
  if an argument was intered at index `current-arg-index' in the
  arglist. If `current-arg-index' is nil, no calculation will be
  done (this function will just return nil). `Preceding-arg'
  should either be nil or the argument directly preceding
  point. `Split-arglist' should either be a split arglist or nil,
  in which case `split-arglist' will be computed from
  `arglist'. This function returns two values: The primary value
  is a list of symbols that should be emphasized, the secondary
  value is a list of symbols that should be highlighted."
  (when current-arg-index
    (flet ((get-args (keyword)
             (rest (assoc keyword split-arglist))))
      (let ((mandatory-argument-count (length (get-args '&mandatory))))
        (cond ((> mandatory-argument-count
                  current-arg-index)
               ;; We are in the main, mandatory, positional arguments.
               (let ((relevant-arg (elt (get-args '&mandatory)
                                        current-arg-index)))
                 ;; We do not handle complex argument lists here, only
                 ;; pure standard arguments.
                 (unless (and (listp relevant-arg)
                              (< current-arg-index mandatory-argument-count))
                   (values nil (list (unlisted relevant-arg))))))
              ((> (+ (length (get-args '&optional))
                     (length (get-args '&mandatory)))
                  current-arg-index)
               ;; We are in the &optional arguments.
               (values nil
                       (list (unlisted (elt (get-args '&optional)
                                            (- current-arg-index
                                               (length (get-args '&mandatory))))))))
              (t
               (let ((body-or-rest-args (or (get-args '&rest)
                                            (get-args '&body)))
                     (key-arg (find (format nil "~A" preceding-arg)
                                    (get-args '&key)
                                    :test #'string=
                                    :key #'(lambda (arg)
                                             (symbol-name (fully-unlisted arg))))))
                 ;; We are in the &body, &rest or &key arguments.
                 (values
                  ;; Only emphasize the &key
                  ;; symbol if we are in a position to add a new
                  ;; keyword-value pair, and not just in a position to
                  ;; specify a value for a keyword.
                  (when (and (null key-arg)
                             (get-args '&key))
                    '(&key))
                  (append (when key-arg
                            (list (unlisted key-arg)))
                          body-or-rest-args)))))))))

(defun analyze-arglist-impl (arglist current-arg-indices preceding-arg provided-args)
  "The implementation for `analyze-arglist'."
  (let* ((split-arglist (split-arglist-on-keywords arglist))
         (user-supplied-arg-values (find-optional-argument-values
                                    arglist
                                    provided-args
                                    split-arglist))
         (mandatory-argument-count
          (length (rest (assoc '&mandatory split-arglist))))
         
         (current-arg-index (or (first current-arg-indices)
                                0))
         ret-arglist
         emphasized-symbols
         highlighted-symbols)
    ;; First, we find any standard arguments that should be
    ;; highlighted or emphasized, more complex, destructuring
    ;; arguments will be handled specially.
    (multiple-value-bind (es hs)
        (find-affected-simple-arguments arglist
                                        ;; If `current-arg-indices' is
                                        ;; nil, that means that we do
                                        ;; not have enough information
                                        ;; to properly highlight
                                        ;; symbols in the arglist.
                                        (and current-arg-indices
                                             current-arg-index)
                                        preceding-arg
                                        split-arglist)
      (setf emphasized-symbols es)
      (setf highlighted-symbols hs))
    ;; We loop over the arglist and build a new list, and if we have a
    ;; default value for a given argument, we insert it into the
    ;; list. Also, whenever we encounter a list in a mandatory
    ;; argument position, we assume that it is a destructuring arglist
    ;; and recursively call `analyze-arglist' on it to find the
    ;; arglist and emphasized and highlighted symbols for it.
    (labels ((generate-arglist (arglist)
               (loop
                  for arg-element in arglist
                  for arg-name = (unlisted arg-element)
                  for index from 0
                    
                  if (and (listp arg-element)
                          (> mandatory-argument-count
                             index))
                  collect (multiple-value-bind (arglist
                                                sublist-emphasized-symbols
                                                sublist-highlighted-symbols)
                              (analyze-arglist arg-element
                                               (rest current-arg-indices)
                                               preceding-arg
                                               (when (< index (length provided-args))
                                                 (listed (elt provided-args index))))
                            ;; Unless our `current-arg-index'
                            ;; actually refers to this sublist, its
                            ;; highlighted and emphasized symbols
                            ;; are ignored. Also, if
                            ;; `current-arg-indices' is nil, we do
                            ;; not have enough information to
                            ;; properly highlight symbols in the
                            ;; arglist.
                            (when (and current-arg-indices
                                       (= index current-arg-index))
                              (if (and (rest current-arg-indices))
                                  (setf emphasized-symbols
                                        (union (mapcar #'unlisted
                                                       sublist-emphasized-symbols)
                                               emphasized-symbols)
                                        highlighted-symbols
                                        (union sublist-highlighted-symbols
                                               highlighted-symbols))
                                  (setf emphasized-symbols
                                        (union (mapcar #'unlisted
                                                       arg-element)
                                               emphasized-symbols))))
                            arglist)
                  else if (assoc arg-name user-supplied-arg-values)
                  collect (list arg-name
                                (rest (assoc
                                       arg-name
                                       user-supplied-arg-values)))
                  else
                  collect arg-element)))
      (setf ret-arglist (generate-arglist arglist)))
    (list ret-arglist emphasized-symbols highlighted-symbols)))

(defun analyze-arglist (arglist current-arg-indices
                        preceding-arg provided-args)
  "Analyze argument list and provide information for highlighting
it. `Arglist' is the argument list that is to be analyzed,
`current-arg-index' is the index where the next argument would be
written (0 is just after the operator), `preceding-arg' is the
written argument preceding point and `provided-args' is a list of
the args already written.

Three values are returned: 

* An argument list with values for &optional and &key arguments
inserted from `provided-args'.

* A list of symbols that should be emphasized.

* A list of symbols that should be highlighted."
  (apply #'values (analyze-arglist-impl
                   arglist
                   current-arg-indices
                   preceding-arg
                   provided-args)))

(defun cleanup-arglist (arglist)
  "Remove elements of `arglist' that we are not interested in."
  (loop
     for arg in arglist
     with in-&aux                       ; If non-NIL, we are in the
                                        ; &aux parameters that should
                                        ; not be displayed.
                    
     with in-garbage                    ; If non-NIL, the next
                                        ; argument is a garbage
                                        ; parameter that should not be
                                        ; displayed.
     if in-garbage
     do (setf in-garbage nil)
     else if (not in-&aux)
     if (eq arg '&aux)
     do (setf in-&aux t)
     else if (member arg +cl-garbage-keywords+ :test #'eq)
     do (setf in-garbage t)
     else
     collect arg))

(defun find-argument-indices-for-operand (syntax operand-form operator-form)
  "Return a list of argument indices for `argument-form' relative
  to `operator-form'. These lists take the form of (n m p), which
  means (list-aref form-operand-list n m p). A list of argument
  indices can have arbitrary length (but they are practically
  always at most 2 elements long). "
  (declare (ignore syntax))
  (let ((operator (first-form (children operator-form))))
    (labels ((worker (operand-form &optional the-first)
               ;; Cannot find index for top-level-form.
               (when (parent operand-form)
                 (let ((form-operand-list
                        (remove-if #'(lambda (form)
                                       (or (not (formp form))
                                           (eq form operator)))
                                   (children (parent operand-form)))))

                   (let ((operand-position (position operand-form form-operand-list))
                         (go-on (not (eq operator-form (parent operand-form)))))
                     ;; If we find anything, we have to increment the
                     ;; position by 1, since we consider the existance
                     ;; of a first operand to mean point is at operand
                     ;; 2. Likewise, a position of nil is interpreted
                     ;; as 0.
                     (cons (if operand-position
                               (if (or the-first)
                                   (1+ operand-position)
                                   operand-position)
                               0)
                           (when go-on
                             (worker (parent operand-form)))))))))
      (nreverse (worker operand-form t)))))

(defun find-operand-info (syntax mark-or-offset operator-form)
  "Returns two values: The operand preceding `mark-or-offset' and
  the path from `operator-form' to the operand."
  (as-offsets ((offset mark-or-offset))
    (let* ((preceding-arg-token (form-before syntax offset))
           (indexing-start-arg
            (let* ((candidate-before preceding-arg-token)
                   (candidate-after (when (null candidate-before)
                                      (let ((after (form-after syntax offset)))
                                        (when after
                                          (parent after)))))
                   (candidate-around (when (null candidate-after)
                                       (form-around syntax offset)))
                   (candidate (or candidate-before
                                  candidate-after
                                  candidate-around)))
              (if (or (and candidate-before
                           (typep candidate-before 'incomplete-list-form))
                      (and (null candidate-before)
                           (form-list-p (or candidate-after candidate-around))))
                  ;; HACK: We should not attempt to find the location of
                  ;; the list form itself, so we create a new parser
                  ;; symbol, attach the list form as a parent and try to
                  ;; find the new symbol. That way we can get a list of
                  ;; argument-indices to the first element of the list
                  ;; form, even if it is empty or incomplete.
                  (let ((obj (make-instance 'parser-symbol)))
                    (setf (parent obj) candidate)
                    obj)
                  candidate)))
           (argument-indices (find-argument-indices-for-operand
                              syntax
                              indexing-start-arg
                              operator-form))
           (preceding-arg-obj (when preceding-arg-token
                                (form-to-object syntax preceding-arg-token
                                                 :no-error t))))
      (values preceding-arg-obj argument-indices))))

(defun valid-operator-p (operator)
  "Check whether or not `operator' is a valid
  operator. `Operator' is considered a valid operator if it is a
  symbol bound to a function, or if it is a lambda expression."
  (cond ((symbolp operator)
         (or (fboundp operator)
             (macro-function operator)
             (special-operator-p operator)))
        ((listp operator)
         (eq (first operator) 'cl:lambda))))

(defun indices-match-arglist (arglist arg-indices)
  "Check whether the argument indices `arg-indices' could refer
  to a direct argument for the operator with the argument list
  `arglist'. Returns T if they could, NIL otherwise. This
  functions does not care about the argument quantity, only their
  structure."
  (let* ((index (first arg-indices))
         (pure-arglist (remove-if #'arglist-keyword-p arglist))
         (arg (when (< index (length pure-arglist))
                (elt pure-arglist index))))
    (cond ((or (and (>= index (or (position-if #'arglist-keyword-p arglist)
                                  (1+ index)))
                    (not (null (rest arg-indices))))
               (and (not (null (rest arg-indices)))
                    (> (length pure-arglist)
                       index)
                    (not (listp (elt pure-arglist index)))))
           nil)
          ((and (not (null arg))
                (listp arg)
                (rest arg-indices))
           (indices-match-arglist arg (rest arg-indices)))
          (t t))))

(defun direct-arg-p (syntax operator-form arg-form)
  "Is `arg-form' a direct argument to `operator-form'? A \"direct
argument\" is defined as an argument that would be directly bound
to a symbol when evaluating the operators body, or as an argument
that would be a direct component of a &body or &rest argument."
  (let ((operator (form-to-object syntax operator-form)))
    (and
     ;; An operator is not an argument to itself.
     (not (eq arg-form
              (first-form (children (parent operator-form)))))
     ;; An operator must be valid.
     (valid-operator-p operator)
     ;; The argument must match the operators argument list.
     (indices-match-arglist
      (arglist (image syntax)
               operator)
      (nth-value 1 (find-operand-info
                    syntax
                    (start-offset arg-form)
                    (parent operator-form)))))))

(defun find-direct-operator (syntax arg-form)
  "Check whether `arg-form' is a direct argument to one of its
parents. If it is, return the form with the operator that
`arg-form' is a direct argument to. If not, return NIL."
  (labels ((recurse (form)
             ;; Check whether `arg-form' is a direct argument to
             ;; the operator of `form'.
             (when (parent form)
               (if (direct-arg-p syntax (first-form (children form)) arg-form)
                   form
                   (recurse (parent form))))))
    (recurse (parent arg-form))))

(defun find-applicable-form (syntax arg-form)
  "Find the enclosing form that has `arg-form' as a valid
argument. Return NIL if none can be found."
  ;; The algorithm for finding the applicable form:
  ;;
  ;; From `arg-form', we wander up the tree looking at enclosing
  ;; forms, until we find a a form with an operator, the
  ;; form-operator, that has `arg-form' as a direct argument (this is
  ;; checked by comparing argument indices for `arg-form', relative to
  ;; form-operator, with the arglist ofform-operator). However, if
  ;; form-operator itself is a direct argument to one of its parents,
  ;; we ignore it (unless form-operators form-operator is itself a
  ;; direct argument, etc). This is so we can properly handle
  ;; nested/destructuring argument lists such as those found in
  ;; macros.
  (labels ((recurse (candidate-form)
             (if (and (direct-arg-p syntax (first-form (children candidate-form))
                                    arg-form)
                      (not (find-applicable-form syntax (first-form (children candidate-form)))))
                 candidate-form
                 (unless (form-at-top-level-p candidate-form)
                   (recurse (parent candidate-form))))))
    (unless (form-at-top-level-p arg-form)
      (recurse (parent arg-form)))))

(defun relevant-keywords (arglist arg-indices)
  "Return a list of the keyword arguments that it would make
  sense to use at the position `arg-indices' relative to the
  operator that has the argument list `arglist'."
  (let* ((key-position (position '&key arglist))
         (rest-position (position '&rest arglist))
         (cleaned-arglist (remove-if #'arglist-keyword-p
                                     arglist))
         (index (first arg-indices))
         (difference (+ (- (length arglist)
                           (length cleaned-arglist))
                        (if rest-position 1 0))))
    (cond ((and (null key-position)
                (rest arg-indices)
                (> (length cleaned-arglist)
                   index)
                (listp (elt cleaned-arglist index)))
           ;; Look in a nested argument list.
           (relevant-keywords (elt cleaned-arglist index)
                              (rest arg-indices)))
          ((and (not (null key-position))
                (>= (+ index
                       difference) 
                    key-position)
                (let ((offset (- index (- key-position (1- difference)))))
                  (or (evenp offset) (zerop key-position))))
           (mapcar #'unlisted (subseq cleaned-arglist
                                      (+ (max (- key-position
                                                 difference)
                                              (- (if rest-position 2 1)))
                                         (if rest-position 2 1))
                                      (if rest-position
                                          (1- (length cleaned-arglist))
                                          (length cleaned-arglist))))))))

(defgeneric possible-completions (syntax operator string package operands indices)
  (:documentation "Get the applicable completions for completing
`string' (which should a string of the, possibly partial, symbol
name to be completed) in `package', which is part of a form with
the operator `operator' (which should be a valid operator
object), and which has the operands `operands'. `Indices' should
be the argument indices from the operator to `token' (see
`find-argument-indices-for-operands').")
  (:method ((syntax lisp-syntax) operator (string string)
            (package package) (operands list) (indices list))
    (let ((completions (first (simple-completions (get-usable-image syntax)
                                                  string package))))
      ;; Welcome to the ugly mess! Part of the uglyness is that we
      ;; depend on Swank do to our nonobvious completion (m-v-b ->
      ;; multiple-value-bind).
      (or (when (valid-operator-p operator)
            (let* ((relevant-keywords
                    (relevant-keywords (arglist-for-form syntax operator operands) indices))
                   (keyword-completions (mapcar #'(lambda (a)
                                                    (string-downcase (format nil ":~A" a)))
                                                relevant-keywords)))
              (when relevant-keywords
                ;; We need Swank to get the concrete list of
                ;; possibilities, but after that, we need to filter
                ;; out anything that is not a relevant keyword
                ;; argument. ALSO, if `string' is blank, Swank will
                ;; "helpfully" not put any keyword symbols in
                ;; `completions', thus ruining this entire scheme. SO,
                ;; we have to force Swank to give us a list of keyword
                ;; symbols and use that instead of `completions'. Joy!
                (intersection (mapcar #'string-downcase
                                      (if (string= string "")
                                          (first (simple-completions (get-usable-image syntax)
                                                                     ":" package))
                                          completions))
                 keyword-completions
                 :key #'string-downcase
                 :test #'string=))))
          completions))))

(defgeneric complete-argument-of-type (argument-type syntax string all-completions)
  (:documentation "")
  (:method (argument-type syntax string all-completions)
    all-completions))

(defgeneric modify-argument-list (argument-type syntax arglist arguments arg-position)
  (:documentation "")
  (:method (syntax argument-type arglist arguments arg-position)
    arglist))

(defmacro define-argument-type (name (&optional inherit-from)
                                &rest options)
  "Define an argument type for use in `define-form-traits'."
  (let ((completion-code (rest (assoc :completion options)))
        (modification-code (rest (assoc :arglist-modification options))))
    (assert (or (null completion-code) (= (length (first completion-code)) 3)))
    (assert (or (null modification-code) (= (length (first modification-code)) 4)))
    `(progn
       ,(if (or completion-code inherit-from)
            (let ((lambda-list (if completion-code
                                   (first completion-code)
                                   '(argument-type syntax token all-completions))))
              `(defmethod complete-argument-of-type ((argument-type (eql ',name))
                                                     ,@lambda-list)
                 ,@(or (rest completion-code)
                       `((complete-argument-of-type ',inherit-from ,@lambda-list)))))
            ;; If no completion rule has been specified for this
            ;; type, we must check whether an earlier definition had
            ;; completion rules - if so, remove the method
            ;; implementing the rules.
            `(let ((method (find-method #'complete-argument-of-type nil `((eql ,name) t t t) nil)))
               (when method
                 (remove-method #'complete-argument-of-type method))))
       ,(if (or modification-code inherit-from)
            (let ((lambda-list (if modification-code
                                   (first modification-code)
                                   '(syntax arglist arguments arg-position))))
             `(defmethod modify-argument-list ((argument-type (eql ',name))
                                               ,@lambda-list)
                ,@(or (rest modification-code)
                      `((modify-argument-list ',inherit-from ,@lambda-list)))))
            ;; If no arglist modification rule has been specified
            ;; for this type, we must check whether an earlier
            ;; definition had arglist modification rules - if so,
            ;; remove the method implementing the rules.
            `(let ((method (find-method #'modify-argument-list nil '((eql ,name) t t t t) nil)))
               (when method
                 (remove-method #'modify-argument-list method)))))))

(define-argument-type class-name ()
  (:completion (syntax string all-completions)
               (let ((all-lower (every #'lower-case-p string)))
                 (loop for completion in all-completions
                    when (find-class (ignore-errors (read-from-string completion))
                                     nil)
                    collect (if all-lower
                                (string-downcase completion)
                                completion))))
  (:arglist-modification (syntax arglist arguments arg-position)
                         (if (and (> (length arguments) arg-position)
                                  (listp (elt arguments arg-position))
                                  (> (length (elt arguments arg-position)) 1)
                                  (eq (first (elt arguments arg-position)) 'cl:quote)
                                  (find-class (second (elt arguments arg-position)) nil))
                             (nconc arglist
                                     (cons '&key (get-class-keyword-parameters
                                                  (get-usable-image syntax)
                                                  (elt arguments arg-position))))
                             arglist)))

(define-argument-type package-designator ()
  (:completion (syntax string all-completions)
               (declare (ignore all-completions))
               (let ((keyworded (and (plusp (length string))
                                     (char= (aref string 0) #\:)))
                     (all-upper (every #'upper-case-p string)))
                 (loop for package in (list-all-packages)
                    for package-name = (if keyworded
                                           (concatenate 'string ":" (package-name package))
                                           (package-name package))
                    when (search string package-name
                                 :test #'char-equal
                                 :end2 (min (length string)
                                            (length package-name)))
                    collect (if all-upper
                                package-name
                                (string-downcase package-name))))))

(defmacro define-form-traits ((operator &rest arguments)
                              &key no-typed-completion no-smart-arglist)
  "Define \"traits\" for a form with the operator that is eql to
`operator'. Traits is a common designator for
intelligent (type-aware) completion and intelligent modification
of argument lists (for example, adding keyword arguments for the
initargs of the class being instantiated to the arglist of
`make-instance').

`Arguments' is a lambda-list-like list that describes the types
of the operands of `operator'. You can use the lambda-list
keywords `&rest' and `&key' to tie all, or specific keyword
arguments, to types.

If `no-typed-completion' or `no-smart-arglist' is non-NIL, no
code for performing typed completion or smart arglist
modification will be generated, respectively."
  ;; FIXME: This macro should also define indentation rules.
  (labels ((process-keyword-arg-descs (arguments)
             ;; We expect `arguments' to be a plist mapping keyword
             ;; symbols to type/class designators/names.
             `((t
                (let* ((keyword-indices (loop
                                           for (car . cdr) on indices
                                           if (null cdr)
                                           collect (1+ car)
                                           else collect car))
                       (keyword (apply #'list-aref operands keyword-indices))
                       (type (getf ',arguments keyword)))
                  (if (null type)
                      (call-next-method)
                      (complete-argument-of-type type syntax string all-completions))))))
           (process-arg-descs (arguments index)
             (let ((argument (first arguments)))
               (cond ((null argument)
                      nil)
                     ((eq argument '&rest)
                      `(((>= (first indices) ,index)
                         (complete-argument-of-type ',(second arguments) syntax string all-completions))))
                     ((eq argument '&key)
                      (process-keyword-arg-descs (rest arguments)))
                     ((listp argument)
                      (cons `((= (first indices) ,index)
                              ,(if (eq (first argument) 'quote)
                                   `(cond ((eq (first (apply #'list-aref operands indices)) 'quote)
                                           (complete-argument-of-type ',(second argument) syntax string all-completions))
                                          (t (call-next-method)))
                                   `(cond ((not (null (rest indices)))
                                           (pop indices)
                                           (cond ,@(build-completions-cond-body argument)))
                                          (t (call-next-method)))))
                            (process-arg-descs (rest arguments)
                                               (1+ index))))
                     (t
                      (cons `((= (first indices) ,index)
                              (complete-argument-of-type ',argument syntax string all-completions))
                            (process-arg-descs (rest arguments)
                                               (1+ index)))))))
           (build-completions-cond-body (arguments)
             (append (process-arg-descs arguments 0)
                     '((t (call-next-method))))))
    `(progn
       (defmethod possible-completions (syntax (operator (eql ',operator)) string package operands indices)
         ,(if no-typed-completion
              '(call-next-method)
              `(let* ((*package* package)
                      (all-completions (call-next-method)))
                 (cond ,@(build-completions-cond-body arguments)))))
       ,(unless no-smart-arglist
                `(defmethod arglist-for-form (syntax (operator (eql ',operator)) &optional arguments)
                   (declare (ignorable arguments))
                   (let ((arglist (call-next-method))
                         (arg-position 0))
                     (declare (ignorable arg-position))
                     ,@(loop for arg in arguments
                          collect `(setf arglist
                                         (modify-argument-list
                                          ',(unlisted arg #'second)
                                          syntax arglist arguments arg-position))
                          collect '(incf arg-position))
                     arglist))))))

(defmacro with-code-insight (mark-or-offset syntax (&key operator preceding-operand
                                                         form preceding-operand-indices
                                                         operands)
                             &body body)
  "Evaluate `body' with the provided symbols lexically bound to
  interesting details about the code at `mark'. If `mark' is not
  within a form, everything will be bound to nil."
  ;; This macro is pretty complicated and deals with tons of border
  ;; issues. If it at all possible, please don't modify it. It is the
  ;; main interface for syntax module code to get high-level
  ;; information about the forms in the buffer, so if it breaks, most
  ;; of the user-oriented functionality of this syntax module breaks
  ;; as well.
  (let ((operator-sym (or operator (gensym)))
        (preceding-operand-sym (or preceding-operand (gensym)))
        (operands-sym (or operands (gensym)))
        (form-sym (or form (gensym)))
        (operand-indices-sym (or preceding-operand-indices (gensym))))
    (once-only (mark-or-offset syntax)
      `(declare (ignorable ,mark-or-offset ,syntax))
      `(let* ((,form-sym
               ;; Find a form with a valid (fboundp) operator.
               (let ((immediate-form
                      (preceding-form ,syntax ,mark-or-offset)))
                 (unless (null immediate-form)
                   (or (find-applicable-form ,syntax immediate-form)
                       ;; If nothing else can be found, and `arg-form'
                       ;; is the operator of its enclosing form, we use
                       ;; the enclosing form.
                       (when (and (not (form-at-top-level-p immediate-form))
                                  (eq (first-form (children (parent immediate-form))) immediate-form))
                         (parent immediate-form))))))
              ;; If we cannot find a form, there's no point in looking
              ;; up any of this stuff.
              (,operator-sym (when (and ,form-sym (form-list-p ,form-sym))
                               (form-to-object ,syntax (form-operator ,syntax ,form-sym))))
              (,operands-sym (when (and ,form-sym (form-list-p ,form-sym))
                               (mapcar #'(lambda (operand)
                                           (when operand
                                             (form-to-object ,syntax operand)))
                                       (form-operands ,syntax ,form-sym)))))
         (declare (ignorable ,form-sym ,operator-sym ,operands-sym))
         (multiple-value-bind (,preceding-operand-sym ,operand-indices-sym)
             (when ,form-sym (find-operand-info ,syntax ,mark-or-offset ,form-sym))
           (declare (ignorable ,preceding-operand-sym ,operand-indices-sym))
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Form trait definitions

(define-form-traits (make-instance 'class-name))
(define-form-traits (find-class 'class-name)
    :no-smart-arglist t)
(define-form-traits (change-class t 'class-name))
(define-form-traits (make-pane 'class-name))
(define-form-traits (make-instances-obsolete 'class-name)
    :no-smart-arglist t)
(define-form-traits (typep t 'class-name))
(define-form-traits (in-package package-designator))
(define-form-traits (clim-lisp:defclass t (&rest class-name))
    :no-smart-arglist t)
(define-form-traits (cl:defclass t (&rest class-name))
    :no-smart-arglist t)
(define-form-traits (define-application-frame t (&rest class-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameter hinting

(defgeneric operator-for-display (operator)
  (:documentation "Return what should be displayed whenever
  `operator' is displayed as an operator.")
  (:method (operator)
    operator))

(defmethod operator-for-display ((operator list))
  (case (first operator)
    ('cl:lambda '|Lambda-Expression|)))

(defun display-arglist-to-stream (stream operator arglist
                                  &optional emphasized-symbols
                                  highlighted-symbols)
  "Display the operator and arglist to stream, format as
  appropriate."
  ;; FIXME: This is fairly ugly.
  (labels ((display-symbol (symbol)
             (with-text-style
                 (stream
                  `(nil
                    ,(cond ((member symbol
                                    highlighted-symbols)
                            :bold)
                           ((member symbol
                                    emphasized-symbols)
                            :italic))
                    nil))
               (format stream "~A" symbol)))
           (display-list (list)
             (if (and (eq (first list) 'quote)
                      (= (length list) 2))
                 (progn
                   (format stream "'")
                   (display-argument (second list)))
                 (progn
                   (format stream "(")
                   (display-argument (first list))
                   (dolist (arg (rest list))
                     (format stream " ")
                     (display-argument arg))
                   (format stream ")"))))
           (display-argument (arg)
             (if (and (listp arg)
                      (not (null arg)))
                 (display-list arg)
                 (display-symbol arg))))
    (display-argument (cons (operator-for-display operator)
                            arglist))))

(defun show-arglist-silent (operator &optional
                            current-arg-indices
                            preceding-arg arguments)
  "Display the arglist for `operator' in the minibuffer, do not
complain if `operator' is not bound to, or is not, a function.

`Current-arg-index' and `preceding-arg' are used to add extra
information to the arglist display. `Arguments' should be either
nil or a list of provided arguments in the form housing symbol.

Returns NIL if an arglist cannot be displayed."
  (multiple-value-bind (arglist emphasized-symbols highlighted-symbols)
      (analyze-arglist
       (arglist-for-form (syntax *current-buffer*) operator arguments)
       current-arg-indices
       preceding-arg
       arguments)
    (esa:with-minibuffer-stream (minibuffer)
      (display-arglist-to-stream minibuffer operator
                                 arglist emphasized-symbols
                                 highlighted-symbols))))

(defun show-arglist (symbol)
  (unless (and (fboundp symbol)
               (show-arglist-silent symbol))
    (esa:display-message "Function ~a not found." symbol)))

(defun show-arglist-for-form-at-mark (mark syntax)
  "Display the argument list for the operator of `form'. The
list need not be complete. If an argument list cannot be
retrieved for the operator, nothing will be displayed."
  (with-code-insight mark syntax (:operator operator
                                            :preceding-operand preceding-operand
                                            :preceding-operand-indices preceding-operand-indices
                                            :operands operands)
    (when (valid-operator-p operator) 
      (show-arglist-silent operator preceding-operand-indices preceding-operand operands))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Symbol completion

;;; The following helper stuff is from Swank.

(defun longest-completion (completions)
  "Return the longest completion of `completions', which must be a
list of sequences."
  (untokenize-completion
   (mapcar #'longest-common-prefix
           (transpose-lists (mapcar #'tokenize-completion completions)))))

(defun tokenize-completion (string)
  "Return all substrings of STRING delimited by #\-."
  (loop with end
     for start = 0 then (1+ end)
     until (> start (length string))
     do (setq end (or (position #\- string :start start) (length string)))
     collect (subseq string start end)))

(defun untokenize-completion (tokens)
  (format nil "~{~A~^-~}" tokens))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun transpose-lists (lists)
  "Turn a list-of-lists on its side.
If the rows are of unequal length, truncate uniformly to the shortest.

For example:
\(transpose-lists '((ONE TWO THREE) (1 2)))
  => ((ONE 1) (TWO 2))"
  (cond ((null lists) '())
        ((some #'null lists) '())
        (t (cons (mapcar #'car lists)
                 (transpose-lists (mapcar #'cdr lists))))))

;;; The interface used by the commands.

(defgeneric frame-clear-completions (frame)
  (:documentation "Clear the display completions for `frame'.")
  (:method (frame)
    nil))

(defun clear-completions ()
  (frame-clear-completions *application-frame*))

(defun find-completions (syntax mark-or-offset string)
  "Find completions for the symbol denoted by the string `string'
at `mark-or-offset'. Two values will be returned: the common
leading string of the completions and a list of the possible
completions as strings."
  (let* ((result (with-code-insight mark-or-offset syntax
                     (:operator operator
                                :operands operands
                                :preceding-operand-indices indices)
                   (let ((completions (possible-completions
                                       syntax operator string
                                       (package-at-mark syntax mark-or-offset)
                                       operands indices)))
                     (list completions (longest-completion completions)))))
         (set (first result))
         (longest (second result)))
    (values longest set)))

(defun find-fuzzy-completions (syntax mark-or-offset string)
    "Find completions for the symbol denoted by the string
`string' at `mark-or-offset'. Two values will be returned: the
common leading string of the completions and a list of the
possible completions as strings. This function uses fuzzy logic
to find completions based on `string'."
  (let* ((set (fuzzy-completions (get-usable-image syntax) string
                                 (package-at-mark syntax mark-or-offset)
                                 10))
         (best (caar set)))
    (values best set)))

(defun complete-symbol-at-mark-with-fn (syntax mark &key (completion-finder #'find-completions)
                                        (complete-blank t))
  "Attempt to find and complete the symbol at `mark' using the
function `completion-finder' to get the list of completions. If
the completion is ambiguous, a list of possible completions will
be displayed. If no symbol can be found at `mark', return NIL. If
there is no symbol at `mark' and `complete-blank' is true (the
default), all symbols available in the current package will be
shown. If `complete-blank' is true, nothing will be shown and the
function will return NIL."
  (let* ((token (form-around syntax (offset mark)))
         (useful-token (and (not (null token))
                            (form-token-p token)
                            (not (= (start-offset token)
                                    (offset mark))))))
    (when (or useful-token complete-blank)
      (multiple-value-bind (longest completions)
          (funcall completion-finder syntax
                   (cond (useful-token
                          (start-offset (fully-quoted-form token)))
                         ((and (form-quoted-p token)
                               (form-incomplete-p token))
                          (start-offset token))
                         (t (offset mark)))
                   (if useful-token
                       (form-string syntax token)
                       ""))
        (cond ((null completions)
               (esa:display-message "No completions found")
               nil)
              ((endp (rest completions))
               (replace-symbol-at-mark syntax mark longest)
               t)
              (t (replace-symbol-at-mark
                  syntax mark
                  (or (menu-choose (mapcar
                                    #'(lambda (completion)
                                        (if (listp completion)
                                            (cons completion
                                                  (first completion))
                                            completion))
                                    completions)
                                   :label "Possible completions"
                                   :scroll-bars :vertical)
                      longest))
                 t))))))

(defun complete-symbol-at-mark (syntax mark &optional (complete-blank t))
  "Attempt to find and complete the symbol at `mark'. If the
  completion is ambiguous, a list of possible completions will be
  displayed. If no symbol can be found at `mark', return nil."
  (complete-symbol-at-mark-with-fn syntax mark :complete-blank complete-blank))

(defun fuzzily-complete-symbol-at-mark (syntax mark &optional (complete-blank t))
  "Attempt to find and complete the symbol at `mark' using fuzzy
  completion. If the completion is ambiguous, a list of possible
  completions will be displayed. If no symbol can be found at
  `mark', return nil."
  (complete-symbol-at-mark-with-fn syntax mark
                                   :completion-finder #'find-fuzzy-completions
                                   :complete-blank complete-blank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation and compilation

(defun eval-string (syntax string)
  "Evaluate all expressions in STRING and return a list of
results."
  (with-input-from-string (stream string)
    (loop for form = (read stream nil stream)
       while (not (eq form stream))
       collecting (multiple-value-list
                   (eval-form-for-drei (get-usable-image syntax)
                                          form)))))

(defun eval-region (start end syntax)
  ;; Must be (mark>= end start).
  (with-syntax-package (syntax start)
    (let ((*read-base* (base syntax)))
      (let* ((string (buffer-substring (buffer start)
                                       (offset start)
                                       (offset end)))
             (values (multiple-value-list
                      (eval-string syntax string)))
             ;; Enclose each set of values in {}.
             (result (apply #'format nil "~{{~:[No values~;~:*~{~S~^,~}~]}~}"
                            values)))
        (esa:display-message result)))))
