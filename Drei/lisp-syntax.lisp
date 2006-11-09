;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX -*-

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

;;; Alternative syntax module for analysing Common Lisp

(in-package :drei-lisp-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience functions and macros.

(defun usable-package (package-designator)
  "Return a usable package based on `package-designator'."
  (or (find-package package-designator)
      *package*))

(defmacro evaluating-interactively (&body body)
  `(handler-case (progn ,@body)
     (end-of-file ()
       (esa:display-message "Unbalanced parentheses in form."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The command table.

(define-syntax-command-table lisp-table
    :errorp nil
    :inherit-from '(editor-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the syntax object

(define-syntax lisp-syntax (fundamental-syntax)
  ((stack-top :initform nil)
   (potentially-valid-trees)
   (lookahead-lexeme :initform nil :accessor lookahead-lexeme)
   (current-state)
   (current-start-mark)
   (current-size)
   (package-list :accessor package-list
                 :documentation "An alist mapping the end offset
of (in-package) forms to a string of the package designator in
the form. The list is sorted with the earliest (in-package) forms
last (descending offset).")
   (base :initform nil
         :documentation "The base which numbers in the buffer are
expected to be in.")
   (option-specified-package :accessor option-specified-package
                             :initform nil
                             :documentation "The package
specified in the attribute line (may be overridden
by (in-package) forms). This may be either a string (the name of
the intended package) or a package object.")
   (image :accessor image
          :initform nil
          :documentation "An image object (or NIL) that
determines where and how Lisp code in the buffer of the
syntax should be run."))
  (:name "Lisp")
  (:pathname-types "lisp" "lsp" "cl")
  (:command-table lisp-table))

(defgeneric base (syntax)
  (:documentation "Get the base `syntax' should interpret numbers
  in.")
  (:method ((syntax lisp-syntax))
    (or (slot-value syntax 'base)
        *read-base*)))

(defmethod (setf base) (base (syntax lisp-syntax))
  (setf (slot-value syntax 'base) base))

(define-option-for-syntax lisp-syntax "Package" (syntax package-name)
  (let ((specified-package (find-package package-name)))
    (setf (option-specified-package syntax) (or specified-package package-name))))

(define-option-for-syntax lisp-syntax "Base" (syntax base)
  (let ((integer-base (parse-integer base :junk-allowed t)))
    (when integer-base
      (if (typep integer-base '(integer 2 36))
          (setf (base syntax) integer-base)
          (esa:display-message "Invalid base specified: outside the interval 2 to 36.")))))

(defmethod current-attributes-for-syntax append ((syntax lisp-syntax))
  (list (cons :package (or (if (packagep (option-specified-package syntax))
                               (package-name (option-specified-package syntax))
                               (option-specified-package syntax))
                           (package-name (package-at-mark
                                          syntax
                                          (or (caar (last (package-list syntax)))
                                              0)))))
        (cons :base (format nil "~A" (base syntax)))))

(defmethod initialize-instance :after ((syntax lisp-syntax) &rest args)
  (declare (ignore args))
  (with-slots (buffer scan) syntax
     (setf scan (clone-mark (low-mark buffer) :left))))

(defmethod name-for-info-pane ((syntax lisp-syntax) &key pane)
  (format nil "Lisp~@[:~(~A~)~]"
          (provided-package-name-at-mark syntax (point pane))))

(defmethod display-syntax-name ((syntax lisp-syntax) (stream extended-output-stream) &key pane)
  (princ "Lisp:" stream)                ; FIXME: should be `present'ed
                                        ; as something.
  (let ((package-name (provided-package-name-at-mark syntax (point pane))))
    (if (find-package package-name)
        (with-output-as-presentation (stream (find-package package-name) 'expression)
          (princ package-name stream))
        (with-text-face (stream :italic)
          (princ package-name stream)))))

(defgeneric default-image ()
  (:documentation "The default image for when the current syntax
  does not mandate anything itself (for example if it is not a
  Lisp syntax).")
  (:method ()
    t))

(defgeneric get-usable-image (syntax)
  (:documentation "Get usable image object from `syntax'.")
  (:method (syntax)
    (default-image))
  (:method ((syntax lisp-syntax))
    (or (image syntax)
        (default-image))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Swank interface functions.

(defgeneric eval-string-for-drei (image string package)
  (:documentation "Evaluate `string' in `package'. A single value
is returned: The result of evaluating `string'.")
  (:method (image string package)
    (let ((*package* package))
      (eval-form-for-drei image (read-from-string string)))))

(defgeneric eval-form-for-drei (image form)
  (:documentation "Evaluate `string' in `package'. A single value
is returned: The result of evaluating `string'.")
  (:method (image form)
    (declare (ignore image))
    (eval form)))

(defgeneric compile-string-for-drei (image string package buffer buffer-mark)
  (:documentation "Compile and evaluate `string' in
`package'. Two values are returned: The result of evaluating
`string' and a list of compiler notes. `Buffer' and `buffer-mark'
will be used for hyperlinking the compiler notes to the source
code.")
  (:method (image string package buffer buffer-mark)
    (declare (ignore image string package buffer buffer-mark))
    (error "Backend insufficient for this operation")))

(defgeneric compile-form-for-drei (image form buffer buffer-mark)
  (:documentation "Compile and evaluate `form', which must be a
valid Lisp form. Two values are returned: The result of
evaluating `string' and a list of compiler notes. `Buffer' and
`buffer-mark' will be used for hyperlinking the compiler notes to
the source code.")
  (:method (image form buffer buffer-mark)
    (compile-string-for-drei image
                                (let ((*print-base* (base (syntax buffer))))
                                  (write-to-string form))
                                *package* buffer buffer-mark)))

(defgeneric compile-file-for-drei (image filepath package &optional load-p)
  (:documentation "Compile the file at `filepath' in
`package'. If `load-p' is non-NIL, also load the file at
`filepath'. Two values will be returned: the result of compiling
the file and a list of compiler notes.")
  (:method (image filepath package &optional load-p)
    (declare (ignore image filepath package load-p))
    (error "Backend insufficient for this operation")))

(defgeneric macroexpand-for-drei (image form &optional full-p)
  (:documentation "Macroexpand `form' and return result.")
  (:method (image form &optional full-p)
    (declare (ignore image))
    (funcall (if full-p
                 #'macroexpand
                 #'macroexpand-1)
             form)))

(defgeneric find-definitions-for-drei (image symbol)
  (:documentation "Return list of definitions for `symbol'.")
  (:method (image symbol)
    (declare (ignore image symbol))))

(defgeneric get-class-keyword-parameters (image class)
  (:documentation "Get a list of keyword parameters (possibly
along with any default values) that can be used in a
`make-instance' form for `class'.")
  (:method (image class)
    (declare (ignore image class))))

(defgeneric arglist (image symbol)
  (:documentation "Get plain arglist for symbol.")
  (:method (image symbol)
    (declare (ignore image symbol))))

(defgeneric simple-completions (image string default-package)
  (:documentation "Return a list of simple symbol-completions for
`string' in `default-package'.")
  (:method (image string default-package)
    (declare (ignore image string default-package))))

(defgeneric fuzzy-completions (image symbol-name default-package &optional limit)
  (:documentation "Return a list of fuzzy completions for `symbol-name'.")
  (:method (image symbol-name default-package &optional limit)
    (declare (ignore image symbol-name default-package limit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defgeneric skip-inter (syntax state scan)
  (:documentation "advance scan until the beginning of a new
    lexeme.  Return T if one can be found and NIL otherwise."))

(defgeneric lex (syntax state scan)
  (:documentation "Return the next lexeme starting at scan."))

(defclass lexer-state ()
  ()
  (:documentation "These states are used to determine how the lexer 
    should behave."))

(defmacro define-lexer-state (name superclasses &body body)
  `(defclass ,name (,@superclasses lexer-state)
      ,@body))

(define-lexer-state lexer-error-state ()
  ()
  (:documentation "In this state, the lexer returns error lexemes
    consisting of entire lines of text"))

(define-lexer-state lexer-toplevel-state ()
  ()
  (:documentation "In this state, the lexer assumes it can skip 
    whitespace and should recognize ordinary lexemes of the language
    except for the right parenthesis"))

(define-lexer-state lexer-list-state (lexer-toplevel-state)
  ()
  (:documentation "In this state, the lexer assumes it can skip 
    whitespace and should recognize ordinary lexemes of the language"))

(define-lexer-state lexer-string-state ()
  ()
  (:documentation "In this state, the lexer is working inside a string 
    delimited by double quote characters."))

(define-lexer-state lexer-line-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a line 
    comment (starting with a semicolon."))

(define-lexer-state lexer-long-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a long
    comment delimited by #| and |#."))

(define-lexer-state lexer-escaped-token-state ()
  ()
  (:documentation "In this state, the lexer is accumulating a token
    and an odd number of multiple escapes have been seen."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this should go in syntax.lisp or lr-syntax.lisp
;;; and should inherit from parse-tree

(defclass parser-symbol ()
  ((start-mark :initform nil :initarg :start-mark :reader start-mark)
   (size :initform nil :initarg :size)
   (parent :initform nil :accessor parent)
   (children :initform '() :initarg :children :reader children)
   (preceding-parse-tree :initform nil :reader preceding-parse-tree)
   (parser-state :initform nil :initarg :parser-state :reader parser-state)))

(defmethod start-offset ((state parser-symbol))
  (let ((mark (start-mark state)))
    (when mark
      (offset mark))))

(defmethod end-offset ((state parser-symbol))
  (with-slots (start-mark size) state
     (when start-mark
       (+ (offset start-mark) size))))

(defgeneric action (syntax state lexeme))
(defgeneric new-state (syntax state parser-symbol))

(defclass parser-state () ())

(defmacro define-parser-state (name superclasses &body body)
  `(progn 
     (defclass ,name ,superclasses
	  ,@body)
     (defvar ,name (make-instance ',name))))

(defclass lexeme (parser-symbol) ())

(defmethod print-object ((lexeme lexeme) stream)
  (print-unreadable-object (lexeme stream :type t :identity t)
    (format stream "~s ~s" (start-offset lexeme) (end-offset lexeme))))

(defclass nonterminal (parser-symbol) ())

(defmethod initialize-instance :after ((parser-symbol nonterminal) &rest args)
  (declare (ignore args))
  (with-slots (children start-mark size) parser-symbol
     (loop for child in children
	   do (setf (parent child) parser-symbol))
     (let ((start (find-if-not #'null children :key #'start-offset))
	   (end (find-if-not #'null children :key #'end-offset :from-end t)))
       (when start
	 (setf start-mark (slot-value start 'start-mark)
	       size (- (end-offset end) (start-offset start)))))))  

;;; until here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lisp-nonterminal (nonterminal) ())     
(defclass form (lisp-nonterminal) ())
(defclass complete-form-mixin () ())
(defclass incomplete-form-mixin () ())

(defclass comment (lisp-nonterminal) ())

(defclass lisp-lexeme (lexeme)
  ((ink)
   (face)))

(defclass error-lexeme (lisp-lexeme) ())
(defclass left-parenthesis-lexeme (lisp-lexeme) ())
(defclass simple-vector-start-lexeme (lisp-lexeme) ())
(defclass right-parenthesis-lexeme (lisp-lexeme) ())
(defclass quote-lexeme (lisp-lexeme) ())
(defclass backquote-lexeme (lisp-lexeme) ())
(defclass comma-lexeme (lisp-lexeme) ())
(defclass comma-at-lexeme (lisp-lexeme) ())
(defclass comma-dot-lexeme (lisp-lexeme) ())
(defclass dot-lexeme (lisp-lexeme) ())
(defclass form-lexeme (form lisp-lexeme) ())
(defclass incomplete-character-lexeme (form-lexeme incomplete-form-mixin) ())
(defclass complete-character-lexeme (form-lexeme complete-form-mixin) ())
(defclass function-lexeme (lisp-lexeme) ())
(defclass line-comment-start-lexeme (lisp-lexeme) ())
(defclass long-comment-start-lexeme (lisp-lexeme) ())
(defclass comment-end-lexeme (lisp-lexeme) ())
(defclass string-start-lexeme (lisp-lexeme) ())
(defclass string-end-lexeme (lisp-lexeme) ())
(defclass word-lexeme (lisp-lexeme) ())
(defclass delimiter-lexeme (lisp-lexeme) ())
(defclass text-lexeme (lisp-lexeme) ())
(defclass sharpsign-equals-lexeme (lisp-lexeme) ())
(defclass sharpsign-sharpsign-lexeme (form-lexeme) ())
(defclass reader-conditional-positive-lexeme (lisp-lexeme) ())
(defclass reader-conditional-negative-lexeme (lisp-lexeme) ())
(defclass uninterned-symbol-lexeme (lisp-lexeme) ())
(defclass readtime-evaluation-lexeme (lisp-lexeme) ())
(defclass array-start-lexeme (lisp-lexeme) ())
(defclass structure-start-lexeme (lisp-lexeme) ())
(defclass pathname-start-lexeme (lisp-lexeme) ())
(defclass undefined-reader-macro-lexeme (lisp-lexeme) ())
(defclass bit-vector-lexeme (form-lexeme) ())
(defclass number-lexeme (form-lexeme complete-form-mixin) ())
(defclass token-mixin () ())
(defclass literal-object-lexeme (form-lexeme) ())
(defclass complete-token-lexeme (token-mixin form-lexeme complete-form-mixin) ())
(defclass multiple-escape-start-lexeme (lisp-lexeme) ())
(defclass multiple-escape-end-lexeme (lisp-lexeme) ())
(defclass incomplete-lexeme (lisp-lexeme incomplete-form-mixin) ())
(defclass unmatched-right-parenthesis-lexeme (lisp-lexeme) ())

(defmethod skip-inter ((syntax lisp-syntax) state scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop when (end-of-buffer-p scan)
	    do (return nil)
	  until (not (whitespacep syntax (object-after scan)))
	  do (fo)
	  finally (return t))))

(defmethod lex :around (syntax state scan)
  (when (skip-inter syntax state scan)
    (let* ((start-offset (offset scan))
	   (lexeme (call-next-method))
	   (new-size (- (offset scan) start-offset)))
      (with-slots (start-mark size) lexeme
	 (setf (offset scan) start-offset)
	 (setf start-mark scan
	       size new-size))
      lexeme)))		  

(defmethod lex ((syntax lisp-syntax) (state lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\( (fo) (make-instance 'left-parenthesis-lexeme))
	(#\) (fo) (make-instance 'unmatched-right-parenthesis-lexeme))
	(#\' (fo) (make-instance 'quote-lexeme))
	(#\; (fo)
	     (loop until (or (end-of-buffer-p scan)
			     (end-of-line-p scan)
			     (not (eql (object-after scan) #\;)))
		   do (fo))
	     (make-instance 'line-comment-start-lexeme))
	(#\" (fo) (make-instance 'string-start-lexeme))
	(#\` (fo) (make-instance 'backquote-lexeme))
	(#\, (fo)
	     (cond ((end-of-buffer-p scan)
		    (make-instance 'incomplete-lexeme))
		   (t
		    (case (object-after scan)
		      (#\@ (fo) (make-instance 'comma-at-lexeme))
		      (#\. (fo) (make-instance 'comma-dot-lexeme))
		      (t (make-instance 'comma-lexeme))))))
	(#\# (fo)
	     (cond ((end-of-buffer-p scan)
		    (make-instance 'incomplete-lexeme))
		   (t 
		    (let ((prefix 0))
		      (loop until (end-of-buffer-p scan)
			    while (digit-char-p (object-after scan))
			    do (setf prefix
				     (+ (* 10 prefix)
					(digit-char-p (object-after scan))))
			       (fo))
		    (if (end-of-buffer-p scan)
			(make-instance 'incomplete-lexeme)
			(case (object-after scan)
			  ((#\Backspace #\Tab #\Newline #\Linefeed 
			    #\Page #\Return #\Space #\))
			   (fo)
			   (make-instance 'error-lexeme))
			  (#\\ (fo)
			       (cond ((end-of-buffer-p scan)
				      (make-instance 'incomplete-character-lexeme))
				     ((not (constituentp (object-after scan)))
				      (fo) (make-instance 'complete-character-lexeme))
				     (t (loop until (end-of-buffer-p scan)
					   while (constituentp (object-after scan))
					   do (fo))
					(make-instance 'complete-character-lexeme))))
			  (#\' (fo)
			       (make-instance 'function-lexeme))
			  (#\( (fo)
			       (make-instance 'simple-vector-start-lexeme))
			  (#\* (fo)
			       (loop until (end-of-buffer-p scan)
				     while (or (eql (object-after scan) #\1)
					       (eql (object-after scan) #\0))
				     do (fo))
			       (if (and (not (end-of-buffer-p scan))
					(constituentp (object-after scan)))
				   (make-instance 'error-lexeme)
				   (make-instance 'bit-vector-lexeme)))
			  (#\: (fo)
			       (make-instance 'uninterned-symbol-lexeme))
			  (#\. (fo)
			       (make-instance 'readtime-evaluation-lexeme))
			  ((#\B #\b #\O #\o #\X #\x)
			   (let ((radix
				  (ecase (object-after scan)
				    ((#\B #\b) 2)
				    ((#\O #\o) 8)
				    ((#\X #\x) 16))))
			     (fo)
			     (loop until (end-of-buffer-p scan)
				   while (digit-char-p (object-after scan) radix)
				   do (fo)))
			   (if (and (not (end-of-buffer-p scan))
				    (constituentp (object-after scan)))
			       (make-instance 'error-lexeme)
			       (make-instance 'number-lexeme)))
			  ((#\R #\r)
			   (fo)
			   (cond
			     ((<= 2 prefix 36)
			      (loop until (end-of-buffer-p scan)
				    while (digit-char-p (object-after scan) prefix)
				    do (fo))
			      (if (and (not (end-of-buffer-p scan))
				       (constituentp (object-after scan)))
				  (make-instance 'error-lexeme)
				  (make-instance 'number-lexeme)))
			     (t (make-instance 'error-lexeme))))
			  ;((#\C #\c) )
			  ((#\A #\a) (fo)
			   (make-instance 'array-start-lexeme))
			  ((#\S #\s) (fo)
			   (cond ((and (not (end-of-buffer-p scan))
				       (eql (object-after scan) #\())
				  (fo)
				  (make-instance 'structure-start-lexeme))
				 ((end-of-buffer-p scan)
				  (make-instance 'incomplete-lexeme))
				 (t (make-instance 'error-lexeme))))
			  ((#\P #\p) (fo)
			   (make-instance 'pathname-start-lexeme))
			  (#\= (fo)
			       (make-instance 'sharpsign-equals-lexeme))
			  (#\# (fo)
			       (make-instance 'sharpsign-sharpsign-lexeme))
			  (#\+ (fo)
			       (make-instance 'reader-conditional-positive-lexeme))
			  (#\- (fo)
			       (make-instance 'reader-conditional-negative-lexeme))
			  (#\| (fo)
			       (make-instance 'long-comment-start-lexeme))
			  (#\< (fo)
			       (make-instance 'error-lexeme))
			  (t (fo) (make-instance 'undefined-reader-macro-lexeme))))))))
	(#\| (fo) (make-instance 'multiple-escape-start-lexeme))
	(t (cond ((or (constituentp object)
                      (eql object #\\))
                  (lex-token syntax scan))
                 (t (fo) (make-instance 'literal-object-lexeme))))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-list-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\) (fo) (make-instance 'right-parenthesis-lexeme))
	(t (call-next-method))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-string-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\") (fo) (make-instance 'string-end-lexeme))
	    ((eql object #\\)
	     (fo)
	     (unless (end-of-buffer-p scan)
	       (fo))
	     (make-instance 'delimiter-lexeme))
	    ((constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-long-comment-state) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\|)
	     (fo)
	     (cond ((or (end-of-buffer-p scan)
			(not (eql (object-after scan) #\#)))
		    (make-instance 'delimiter-lexeme))
		   (t (fo) (make-instance 'comment-end-lexeme))))
	    ((eql object #\#)
	     (fo)
	     (cond ((or (end-of-buffer-p scan)
			(not (eql (object-after scan) #\|)))
		    (make-instance 'delimiter-lexeme))
		   (t (fo) (make-instance 'long-comment-start-lexeme))))
	    ((constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod skip-inter ((syntax lisp-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (or (end-of-line-p scan)
		    (not (whitespacep syntax (object-after scan))))
	  do (fo)
	  finally (return t))))

(defmethod lex ((syntax lisp-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (cond ((end-of-line-p scan)
	   (make-instance 'comment-end-lexeme))
	  ((constituentp (object-after scan))
	   (loop until (or (end-of-buffer-p scan)
			   (not (constituentp (object-after scan))))
		 do (fo))
	   (make-instance 'word-lexeme))
	  (t (fo) (make-instance 'delimiter-lexeme)))))

(defun lex-token (syntax scan)
  ;; May need more work. Can recognize symbols and numbers. This can
  ;; get very ugly and complicated (out of necessity I believe).
  (let ((could-be-number t)
        sign-seen dot-seen slash-seen nondot-seen number-seen exponent-seen)
    (flet ((fo () (forward-object scan))
           (return-token-or-number-lexeme ()
             (return-from lex-token
               (if (and could-be-number
                        (if exponent-seen
                            nondot-seen t))
                   (if nondot-seen
                       (make-instance 'number-lexeme)
                       (make-instance 'dot-lexeme))
                   (make-instance 'complete-token-lexeme))))
           (this-object ()
             (object-after scan)))
      (tagbody
       START
         (when (end-of-buffer-p scan)
           (return-token-or-number-lexeme))
         (when (constituentp (object-after scan))
           (when (not (eql (this-object) #\.))
             (setf nondot-seen t))
           (cond ((or (eql (this-object) #\+)
                      (eql (this-object) #\-))
                  (when (or sign-seen number-seen slash-seen)
                    (setf could-be-number nil))
                  (setf sign-seen t))
                 ((eql (this-object) #\.)
                  (when (or dot-seen exponent-seen)
                    (setf could-be-number nil))
                  (setf dot-seen t))
                 ((member (this-object)
                          '(#\e #\f #\l #\s #\d)
                          :test #'equalp)
                  (when exponent-seen
                    (setf could-be-number nil))
                  (setf exponent-seen t)
                  (setf number-seen nil)
                  (setf sign-seen nil))
                 ((eql (this-object) #\/)
                  (when (or slash-seen dot-seen exponent-seen)
                    (setf could-be-number nil))
                  (setf slash-seen t))
                 ((not (digit-char-p (this-object)
                                     (base syntax)))
                  (setf could-be-number nil))
                 (t (setf number-seen t)))
           (fo)
           (go START))
         (when (eql (object-after scan) #\\)
           (fo)
           (when (end-of-buffer-p scan)
             (return-from lex-token (make-instance 'incomplete-lexeme)))
           (fo)
           (go START))
         (when (eql (object-after scan) #\|)
           (fo)
           (return-from lex-token (make-instance 'multiple-escape-start-lexeme)))
         (return-token-or-number-lexeme)))))

(defmethod lex ((syntax lisp-syntax) (state lexer-escaped-token-state) scan)
  (let ((bars-seen 0))
    (macrolet ((fo () `(forward-object scan)))
      (tagbody
       start
	 (when (end-of-buffer-p scan)
	   (return-from lex (make-instance 'text-lexeme)))
	 (when (eql (object-after scan) #\\)
	   (fo)
	   (when (end-of-buffer-p scan)
	     (return-from lex (make-instance 'incomplete-lexeme)))
	   (fo)
	   (go start))
	 (when (eql (object-after scan) #\|)
	   (incf bars-seen)
	   (fo)
	   (go start))
         (if (evenp bars-seen)
             (unless (whitespacep syntax (object-after scan))
               (fo)
               (go start))
             (when (constituentp (object-after scan))
               (fo)
               (go start)))
	 (return-from lex 
	   (if (oddp bars-seen)
	       (make-instance 'multiple-escape-end-lexeme)
	       (make-instance 'text-lexeme)))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-error-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (end-of-line-p scan)
	  do (fo))
    (make-instance 'error-lexeme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nonterminals

(defclass line-comment (lisp-nonterminal) ())
(defclass long-comment (lisp-nonterminal) ())  
(defclass error-symbol (lisp-nonterminal) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defmacro define-lisp-action ((state lexeme) &body body)
  `(defmethod action ((syntax lisp-syntax) (state ,state) (lexeme ,lexeme))
     ,@body))

(defmacro define-new-lisp-state ((state parser-symbol) &body body)
  `(defmethod new-state ((syntax lisp-syntax) (state ,state) (tree ,parser-symbol))
     ,@body))

(defun pop-one (syntax)
  (with-slots (stack-top current-state) syntax
     (with-slots (preceding-parse-tree parser-state) stack-top
	(prog1 stack-top
	       (setf current-state parser-state
		     stack-top preceding-parse-tree)))))

(defun pop-number (syntax how-many)
  (loop with result = '()
	repeat how-many
	do (push (pop-one syntax) result)
	finally (return result)))

(defmacro reduce-fixed-number (symbol nb-children)
  `(let ((result (make-instance ',symbol :children (pop-number syntax ,nb-children))))
     (when (zerop ,nb-children)
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))

(defun pop-until-type (syntax type)
  (with-slots (stack-top) syntax
     (loop with result = '()
	   for child = stack-top
	   do (push (pop-one syntax) result)
	   until (typep child type)
	   finally (return result))))

(defmacro reduce-until-type (symbol type)
  `(let ((result (make-instance ',symbol
		    :children (pop-until-type syntax ',type))))
     (when (null (children result))
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))

(defun pop-all (syntax)
  (with-slots (stack-top) syntax
     (loop with result = '()
	   until (null stack-top)
	   do (push (pop-one syntax) result)
	   finally (return result))))

(defmacro reduce-all (symbol)
  `(let ((result (make-instance ',symbol :children (pop-all syntax))))
     (when (null (children result))
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))     

(define-parser-state error-state (lexer-error-state parser-state) ())
(define-parser-state error-reduce-state (lexer-toplevel-state parser-state) ())

(define-lisp-action (error-reduce-state (eql nil))
  (throw 'done nil)) 

;;; the default action for any lexeme is shift
(define-lisp-action (t lisp-lexeme)
  lexeme)

;;; the action on end-of-buffer is to reduce to the error symbol
(define-lisp-action (t (eql nil))
  (reduce-all error-symbol))

;;; the default new state is the error state
(define-new-lisp-state (t parser-symbol) error-state)

;;; the new state when an error-state 
(define-new-lisp-state (t error-symbol) error-reduce-state)


;;;;;;;;;;;;;;;; Top-level 

#| rules
   form* -> 
   form* -> form* form
|#

;;; parse trees
(defclass form* (lisp-nonterminal) ())

(define-parser-state |form* | (lexer-toplevel-state parser-state) ())
(define-parser-state form-may-follow (lexer-toplevel-state parser-state) ())
(define-parser-state |initial-state | (form-may-follow) ())

(define-new-lisp-state (|initial-state | form) |initial-state |)
(define-new-lisp-state (|initial-state | comment) |initial-state |)
;; skip over unmatched right parentheses
(define-new-lisp-state (|initial-state | unmatched-right-parenthesis-lexeme) |initial-state |)

(define-lisp-action (|initial-state | (eql nil))
  (reduce-all form*))

(define-new-lisp-state (|initial-state | form*) |form* | )
  
(define-lisp-action (|form* | (eql nil))
  (throw 'done nil))

;;;;;;;;;;;;;;;; List

#| rules
   form -> ( form* )
|#

;;; parse trees
(defclass list-form (form) ())
(defclass complete-list-form (list-form complete-form-mixin) ())
(defclass incomplete-list-form (list-form incomplete-form-mixin) ())

(define-parser-state |( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow left-parenthesis-lexeme) |( form* |)
(define-new-lisp-state (|( form* | form) |( form* |)
(define-new-lisp-state (|( form* | comment) |( form* |)
(define-new-lisp-state (|( form* | right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ( form* )
(define-lisp-action (|( form* ) | t)
  (reduce-until-type complete-list-form left-parenthesis-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|( form* | (eql nil))
  (reduce-until-type incomplete-list-form left-parenthesis-lexeme))

;;;;;;;;;;;;;;;; Cons cell
;; Also (foo bar baz . quux) constructs.
;; (foo bar . baz quux) flagged as an error (too aggressively?).

;;; parse trees
(defclass cons-cell-form (form) ())
(defclass complete-cons-cell-form (cons-cell-form complete-list-form) ())
(defclass incomplete-cons-cell-form (cons-cell-form incomplete-list-form) ())

(define-parser-state |( form* dot-lexeme |
    (lexer-list-state form-may-follow) ())
(define-parser-state |( form* dot-lexeme form |
    (lexer-list-state form-may-follow) ())
(define-parser-state |( form* dot-lexeme form ) |
    (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|( form* | dot-lexeme)
  |( form* dot-lexeme |)
(define-new-lisp-state (|( form* dot-lexeme | form)
  |( form* dot-lexeme form |)
(define-new-lisp-state (|( form* dot-lexeme | comment)
  |( form* dot-lexeme |)
(define-new-lisp-state (|( form* dot-lexeme form | right-parenthesis-lexeme)
  |( form* dot-lexeme form ) |)
(define-new-lisp-state (|( form* dot-lexeme form | comment)
  |( form* dot-lexeme form |)

(define-lisp-action (|( form* dot-lexeme form ) | t)
  (reduce-until-type complete-cons-cell-form left-parenthesis-lexeme))

;;; Reduce at end of buffer.
(define-lisp-action (|( form* dot-lexeme | (eql nil))
  (reduce-until-type incomplete-cons-cell-form left-parenthesis-lexeme))
(define-lisp-action (|( form* dot-lexeme form | (eql nil))
  (reduce-until-type incomplete-cons-cell-form left-parenthesis-lexeme))

;;;;;;;;;;;;;;;; Simple Vector

;;; parse trees
(defclass simple-vector-form (list-form) ())
(defclass complete-simple-vector-form (complete-list-form simple-vector-form) ())
(defclass incomplete-simple-vector-form (incomplete-list-form simple-vector-form) ())

(define-parser-state |#( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |#( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow simple-vector-start-lexeme) |#( form* |)
(define-new-lisp-state (|#( form* | form) |#( form* |)
(define-new-lisp-state (|#( form* | comment) |#( form* |)
(define-new-lisp-state (|#( form* | right-parenthesis-lexeme) |#( form* ) |)

;;; reduce according to the rule form -> #( form* )
(define-lisp-action (|#( form* ) | t)
  (reduce-until-type complete-simple-vector-form simple-vector-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#( form* | (eql nil))
  (reduce-until-type incomplete-simple-vector-form simple-vector-start-lexeme))

;;;;;;;;;;;;;;;; String

;;; parse trees
(defclass string-form (form) ())
(defclass complete-string-form (string-form complete-form-mixin) ())
(defclass incomplete-string-form (string-form incomplete-form-mixin) ())

(define-parser-state |" word* | (lexer-string-state parser-state) ())
(define-parser-state |" word* " | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|" word* | word-lexeme) |" word* |)
(define-new-lisp-state (|" word* | delimiter-lexeme) |" word* |)
(define-new-lisp-state (form-may-follow string-start-lexeme) |" word* |)
(define-new-lisp-state (|" word* | string-end-lexeme) |" word* " |)

;;; reduce according to the rule form -> " word* "
(define-lisp-action (|" word* " | t)
  (reduce-until-type complete-string-form string-start-lexeme))

;;; reduce at the end of the buffer 
(define-lisp-action (|" word* | (eql nil))
  (reduce-until-type incomplete-string-form string-start-lexeme))

;;;;;;;;;;;;;;;; Line comment

;;; parse trees
(defclass line-comment-form (comment) ())

(define-parser-state |; word* | (lexer-line-comment-state parser-state) ())
(define-parser-state |; word* NL | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow line-comment-start-lexeme) |; word* |)
(define-new-lisp-state (|; word* | word-lexeme) |; word* |)
(define-new-lisp-state (|; word* | delimiter-lexeme) |; word* |)
(define-new-lisp-state (|; word* | comment-end-lexeme) |; word* NL |)

;;; reduce according to the rule form -> ; word* NL
(define-lisp-action (|; word* NL | t)
  (reduce-until-type line-comment-form line-comment-start-lexeme))

;;;;;;;;;;;;;;;; Long comment

;;; parse trees
(defclass long-comment-form (comment) ())
(defclass complete-long-comment-form (long-comment-form complete-form-mixin) ())
(defclass incomplete-long-comment-form (long-comment-form incomplete-form-mixin) ())

(define-parser-state |#\| word* | (lexer-long-comment-state parser-state) ())
(define-parser-state |#\| word* \|# | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|#\| word* | word-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | delimiter-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | long-comment-start-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | long-comment-form) |#\| word* |)
(define-new-lisp-state (form-may-follow long-comment-start-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | comment-end-lexeme) |#\| word* \|# |)

;;; reduce according to the rule form -> #| word* |#
(define-lisp-action (|#\| word* \|# | t)
  (reduce-until-type complete-long-comment-form long-comment-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#\| word* | (eql nil))
  (reduce-until-type incomplete-long-comment-form long-comment-start-lexeme))

;;;;;;;;;;;;;;;; Token (number or symbol)

;;; parse trees
(defclass token-form (form token-mixin) ())
(defclass complete-token-form (token-form complete-form-mixin) ())
(defclass incomplete-token-form (token-form incomplete-form-mixin) ())

(define-parser-state | m-e-start text* | (lexer-escaped-token-state parser-state) ())
(define-parser-state | m-e-start text* m-e-end | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow multiple-escape-start-lexeme) | m-e-start text* |)
(define-new-lisp-state (| m-e-start text* | text-lexeme) | m-e-start text* |)
(define-new-lisp-state (| m-e-start text* | multiple-escape-end-lexeme) | m-e-start text* m-e-end |)

;;; reduce according to the rule form -> m-e-start text* m-e-end
(define-lisp-action (| m-e-start text* m-e-end | t)
  (reduce-until-type complete-token-form multiple-escape-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (| m-e-start text* | (eql nil))
  (reduce-until-type incomplete-token-form multiple-escape-start-lexeme))

;;;;;;;;;;;;;;;; Quote

;;; parse trees
(defclass quote-form (form) ())
(defclass complete-quote-form (quote-form complete-form-mixin) ())
(defclass incomplete-quote-form (quote-form incomplete-form-mixin) ())

(define-parser-state |' | (form-may-follow) ())
(define-parser-state |' form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow quote-lexeme) |' |)
(define-new-lisp-state (|' | form) |' form |)
(define-new-lisp-state (|' | comment) |' |)
(define-new-lisp-state (|' | unmatched-right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ' form
(define-lisp-action (|' form | t)
  (reduce-until-type complete-quote-form quote-lexeme))

(define-lisp-action (|' | right-parenthesis-lexeme)
  (reduce-until-type incomplete-quote-form quote-lexeme))
(define-lisp-action (|' | unmatched-right-parenthesis-lexeme)
  (reduce-until-type incomplete-quote-form quote-lexeme))
(define-lisp-action (|' | (eql nil))
  (reduce-until-type incomplete-quote-form quote-lexeme))

;;;;;;;;;;;;;;;; Backquote

;;; parse trees
(defclass backquote-form (form) ())
(defclass complete-backquote-form (backquote-form complete-form-mixin) ())
(defclass incomplete-backquote-form (backquote-form incomplete-form-mixin) ())

(define-parser-state |` | (form-may-follow) ())
(define-parser-state |` form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow backquote-lexeme) |` |)
(define-new-lisp-state (|` | form) |` form |)
(define-new-lisp-state (|` | comment) |` |)
(define-new-lisp-state (|` | unmatched-right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ` form
(define-lisp-action (|` form | t)
  (reduce-until-type complete-backquote-form backquote-lexeme))

(define-lisp-action (|` | right-parenthesis-lexeme)
  (reduce-until-type incomplete-backquote-form backquote-lexeme))
(define-lisp-action (|` | unmatched-right-parenthesis-lexeme)
  (reduce-until-type incomplete-backquote-form backquote-lexeme))
(define-lisp-action (|` | (eql nil))
  (reduce-until-type incomplete-backquote-form backquote-lexeme))

;;;;;;;;;;;;;;;; Comma

;;; parse trees
(defclass comma-form (form) ())
(defclass comma-at-form (form) ())
(defclass comma-dot-form (form) ())

(define-parser-state |, | (form-may-follow) ())
(define-parser-state |, form | (lexer-toplevel-state parser-state) ())
(define-parser-state |,@ | (form-may-follow) ())
(define-parser-state |,@ form | (lexer-toplevel-state parser-state) ())
(define-parser-state |,. | (form-may-follow) ())
(define-parser-state |,. form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow comma-lexeme) |, |)
(define-new-lisp-state (form-may-follow comma-at-lexeme) |,@ |)
(define-new-lisp-state (form-may-follow comma-dot-lexeme) |,. |)
(define-new-lisp-state (|, | form) |, form |)
(define-new-lisp-state (|, | comment) |, |)
(define-new-lisp-state (|,@ | form) |,@ form |)
(define-new-lisp-state (|,@ | comment) |,@ |)
(define-new-lisp-state (|,. | form) |,. form |)
(define-new-lisp-state (|,. | comment) |,. |)

;;; reduce according to the rule form -> , form
(define-lisp-action (|, form | t)
  (reduce-until-type comma-form comma-lexeme))
(define-lisp-action (|,@ form | t)
  (reduce-until-type comma-at-form comma-at-lexeme))
(define-lisp-action (|,. form | t)
  (reduce-until-type comma-dot-form comma-dot-lexeme))

;;;;;;;;;;;;;;;; Function

;;; parse trees
(defclass function-form (form) ())

(define-parser-state |#' | (form-may-follow) ())
(define-parser-state |#' form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow function-lexeme) |#' |)
(define-new-lisp-state (|#' | form) |#' form |)
(define-new-lisp-state (|#' | comment) |#' |)

;;; reduce according to the rule form -> #' form
(define-lisp-action (|#' form | t)
  (reduce-until-type function-form function-lexeme))

;;;;;;;;;;;;;;;; Reader conditionals

;;; parse trees
(defclass reader-conditional-form (form) ())
(defclass reader-conditional-positive-form (reader-conditional-form) ())
(defclass reader-conditional-negative-form (reader-conditional-form) ())

(define-parser-state |#+ | (form-may-follow) ())
(define-parser-state |#+ form | (form-may-follow) ())
(define-parser-state |#+ form form | (lexer-toplevel-state parser-state) ())
(define-parser-state |#- | (form-may-follow) ())
(define-parser-state |#- form | (form-may-follow) ())
(define-parser-state |#- form form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow reader-conditional-positive-lexeme) |#+ |)
(define-new-lisp-state (|#+ | form) |#+ form |)
(define-new-lisp-state (|#+ form | form) |#+ form form |)
(define-new-lisp-state (|#+ | comment) |#+ |)
(define-new-lisp-state (|#+ form | comment) |#+ form |)
(define-new-lisp-state (form-may-follow reader-conditional-negative-lexeme) |#- |)
(define-new-lisp-state (|#- | form) |#- form |)
(define-new-lisp-state (|#- form | form) |#- form form |)
(define-new-lisp-state (|#- | comment) |#- |)
(define-new-lisp-state (|#- form | comment) |#- form |)
  
(define-lisp-action (|#+ form form | t)
  (reduce-until-type reader-conditional-positive-form reader-conditional-positive-lexeme))

(define-lisp-action (|#- form form | t)
  (reduce-until-type reader-conditional-negative-form reader-conditional-negative-lexeme))

;;;;;;;;;;;;;;;; uninterned symbol

;;; parse trees
(defclass uninterned-symbol-form (form) ())

(define-parser-state |#: | (form-may-follow) ())
(define-parser-state |#: form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow uninterned-symbol-lexeme) |#: |)
(define-new-lisp-state (|#: | form) |#: form |)

;;; reduce according to the rule form -> #: form
(define-lisp-action (|#: form | t)
  (reduce-fixed-number uninterned-symbol-form 2))

;;;;;;;;;;;;;;;; readtime evaluation

;;; parse trees
(defclass readtime-evaluation-form (form) ())

(define-parser-state |#. | (form-may-follow) ())
(define-parser-state |#. form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow readtime-evaluation-lexeme) |#. |)
(define-new-lisp-state (|#. | form) |#. form |)
(define-new-lisp-state (|#. | comment) |#. |)

;;; reduce according to the rule form -> #. form
(define-lisp-action (|#. form | t)
  (reduce-until-type readtime-evaluation-form readtime-evaluation-lexeme))

;;;;;;;;;;;;;;;; sharpsign equals

;;; parse trees
(defclass sharpsign-equals-form (form) ())

(define-parser-state |#= | (form-may-follow) ())
(define-parser-state |#= form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow sharpsign-equals-lexeme) |#= |)
(define-new-lisp-state (|#= | form) |#= form |)
(define-new-lisp-state (|#= | comment) |#= |)

;;; reduce according to the rule form -> #= form
(define-lisp-action (|#= form | t)
  (reduce-until-type sharpsign-equals-form sharpsign-equals-lexeme))

;;;;;;;;;;;;;;;; array

;;; parse trees
(defclass array-form (form) ())

(define-parser-state |#A | (form-may-follow) ())
(define-parser-state |#A form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow array-start-lexeme) |#A |)
(define-new-lisp-state (|#A | form) |#A form |)
(define-new-lisp-state (|#A | comment) |#A |)

;;; reduce according to the rule form -> #A form
(define-lisp-action (|#A form | t)
  (reduce-until-type array-form array-start-lexeme))

;;;;;;;;;;;;;;;; structure

;;; parse trees
(defclass structure-form (list-form) ())
(defclass complete-structure-form (complete-list-form) ())
(defclass incomplete-structure-form (incomplete-list-form) ())

(define-parser-state |#S( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |#S( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow structure-start-lexeme) |#S( form* |)
(define-new-lisp-state (|#S( form* | form) |#S( form* |)
(define-new-lisp-state (|#S( form* | right-parenthesis-lexeme) |#S( form* ) |)

;;; reduce according to the rule form -> #S( form* )
(define-lisp-action (|#S( form* ) | t)
  (reduce-until-type complete-structure-form structure-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#S( form* | (eql nil))
  (reduce-until-type incomplete-structure-form structure-start-lexeme))


;;;;;;;;;;;;;;;; pathname

;;; NB: #P need not be followed by a string,
;;;  as it could be followed by a #. construct instead (or some other reader macro)

;;; parse trees
(defclass pathname-form (form) ())

(define-parser-state |#P | (form-may-follow) ())
(define-parser-state |#P form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow pathname-start-lexeme) |#P |)
(define-new-lisp-state (|#P | form) |#P form |)
(define-new-lisp-state (|#P | comment) |#P |)

;;; reduce according to the rule form -> #P form
(define-lisp-action (|#P form | t)
  (reduce-until-type pathname-form pathname-start-lexeme))

;;;;;;;;;;;;;;;; undefined reader macro

;;; parse trees
(defclass undefined-reader-macro-form (form) ())

(define-parser-state |#<other> | (form-may-follow) ())
(define-parser-state |#<other> form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow undefined-reader-macro-lexeme) |#<other> |)
(define-new-lisp-state (|#<other> | form) |#<other> form |)

;;; reduce according to the rule form -> #<other> form
(define-lisp-action (|#<other> form | t)
  (reduce-fixed-number undefined-reader-macro-form 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser step

(defgeneric parser-step (syntax))

(defmethod parser-step ((syntax lisp-syntax))
  (with-slots (lookahead-lexeme stack-top current-state scan) syntax
     (setf lookahead-lexeme (lex syntax current-state (clone-mark scan :right)))
     (let* ((new-parser-symbol (action syntax current-state lookahead-lexeme))
	    (new-state (new-state syntax current-state new-parser-symbol)))
       (with-slots (parser-state parser-symbol preceding-parse-tree children) new-parser-symbol
	  (setf parser-state current-state
		current-state new-state
		preceding-parse-tree stack-top
		stack-top new-parser-symbol)))
     (setf (offset scan) (end-offset stack-top))))

(defun prev-tree (tree)
  (assert (not (null tree)))
  (if (null (children tree))
      (preceding-parse-tree tree)
      (car (last (children tree)))))

(defun next-tree (tree)
  (assert (not (null tree)))
  (if (null (parent tree))
      nil
      (let* ((parent (parent tree))
	     (siblings (children parent)))
	(cond ((null parent) nil)
	      ((eq tree (car (last siblings))) parent)
	      (t (loop with new-tree = (cadr (member tree siblings :test #'eq))
		       until (null (children new-tree))
		       do (setf new-tree (car (children new-tree)))
		       finally (return new-tree)))))))	 

(defun find-last-valid-lexeme (parse-tree offset)
  (cond ((or (null parse-tree) (null (start-offset parse-tree))) nil)
	((> (start-offset parse-tree) offset)
	 (find-last-valid-lexeme (preceding-parse-tree parse-tree) offset))
	((not (typep parse-tree 'lexeme))
	 (find-last-valid-lexeme (car (last (children parse-tree))) offset))
	((>= (end-offset parse-tree) offset)
	 (find-last-valid-lexeme (preceding-parse-tree parse-tree) offset))
	(t parse-tree)))  

(defun find-first-potentially-valid-lexeme (parse-trees offset)
  (cond ((null parse-trees) nil)
	((or (null (start-offset (car parse-trees)))
	     (< (end-offset (car parse-trees)) offset))
	 (find-first-potentially-valid-lexeme (cdr parse-trees) offset))
	((not (typep (car parse-trees) 'lexeme))
	 (find-first-potentially-valid-lexeme (children (car parse-trees)) offset))
	((<= (start-offset (car parse-trees)) offset)
	 (loop with tree = (next-tree (car parse-trees))
	       until (or (null tree) (> (start-offset tree) offset))
	       do (setf tree (next-tree tree))
	       finally (return tree)))
	(t (car parse-trees))))

(defun parse-tree-equal (tree1 tree2)
  (and (eq (class-of tree1) (class-of tree2))
       (eq (parser-state tree1) (parser-state tree2))
       (= (end-offset tree1) (end-offset tree2))))
  
(defmethod print-object ((mark mark) stream)
  (print-unreadable-object (mark stream :type t :identity t)
    (format stream "~s" (offset mark))))

(defun parse-patch (syntax)
  (with-slots (current-state stack-top scan potentially-valid-trees) syntax
     (parser-step syntax)
     (finish-output *trace-output*)
     (cond ((parse-tree-equal stack-top potentially-valid-trees)
	    (unless (or (null (parent potentially-valid-trees))
			(eq potentially-valid-trees
			    (car (last (children (parent potentially-valid-trees))))))
	      (loop for tree = (cadr (member potentially-valid-trees
					     (children (parent potentially-valid-trees))
					     :test #'eq))
		      then (car (children tree))
		    until (null tree)
		    do (setf (slot-value tree 'preceding-parse-tree)
			     stack-top))
	      (setf stack-top (prev-tree (parent potentially-valid-trees))))
	    (setf potentially-valid-trees (parent potentially-valid-trees))
	    (setf current-state (new-state syntax (parser-state stack-top) stack-top))
	    (setf (offset scan) (end-offset stack-top)))
	   (t (loop until (or (null potentially-valid-trees)
			      (>= (start-offset potentially-valid-trees)
				  (end-offset stack-top)))
		    do (setf potentially-valid-trees
			     (next-tree potentially-valid-trees)))))))	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defmethod update-syntax-for-display (buffer (syntax lisp-syntax) top bot)
  nil)

(defun package-at-mark (syntax mark-or-offset)
  "Get the specified Lisp package for the syntax. First, an
attempt will be made to find the package specified in
the (in-package) preceding `mark-or-offset'. If none can be
found, return the package specified in the attribute list. If no
package can be found at all, or the otherwise found packages are
invalid, return the CLIM-USER package."
  (as-offsets ((offset mark-or-offset))
   (let* ((designator (rest (find offset (package-list syntax)
                                  :key #'first
                                  :test #'>=))))
     (or (handler-case (find-package designator)
           (type-error ()
             nil))
         (let ((osp (option-specified-package syntax)))
          (typecase osp
            (package osp)
            (string osp)))
         (find-package (option-specified-package syntax))
         (find-package :clim-user)))))

(defun provided-package-name-at-mark (syntax mark-or-offset)
  "Get the name of the specified Lisp package for the
syntax. This will return a normalised version of
whatever (in-package) form precedes `mark-or-offset', even if the
package specified in that form does not exist. If no (in-package)
form can be found, return the package specified in the attribute
list. If no such package is specified, return \"CLIM-USER\"."
  (as-offsets ((offset mark-or-offset))
    (flet ((normalise (designator)
             (typecase designator
               (symbol
                (symbol-name designator))
               (string
                designator)
               (package
                (package-name designator)))))
     (let* ((designator (rest (find offset (package-list syntax)
                                    :key #'first
                                    :test #'>=))))
       (normalise (or designator
                      (option-specified-package syntax)
                      :clim-user))))))

(defmacro with-syntax-package ((syntax offset) &body
                               body)
  "Evaluate `body' with `*package*' bound to a valid package,
  preferably taken from `syntax' based on `offset'."
  `(let ((*package* (package-at-mark ,syntax ,offset)))
     ,@body))

(defun need-to-update-package-list-p (buffer syntax)
  (let ((low-mark-offset (offset (low-mark buffer)))
        (high-mark-offset (offset (high-mark buffer))))
    (flet ((test (x)
             (let ((start-offset (start-offset x))
                   (end-offset (end-offset x)))
              (when (and (or (<= start-offset
                                 low-mark-offset
                                 end-offset
                                 high-mark-offset)
                             (<= low-mark-offset
                                 start-offset
                                 high-mark-offset
                                 end-offset)
                             (<= low-mark-offset
                                 start-offset
                                 end-offset
                                 high-mark-offset)
                             (<= start-offset
                                 low-mark-offset
                                 high-mark-offset
                                 end-offset))
                         (typep x 'complete-list-form))
                (let ((candidate (first-form (children x))))
                  (and (form-token-p candidate)
                       (eq (token-to-object syntax candidate
                                            :no-error t)
                           'cl:in-package)))))))
      (with-slots (stack-top) syntax
        (or (not (slot-boundp syntax 'package-list))
            (loop
               for child in (children stack-top)
               when (test child)
               do (return t))
            (loop
               for (offset . nil) in (package-list syntax)
               unless (let ((form (form-around syntax offset)))
                        (form-list-p form))
               do (return t)))))))

(defun update-package-list (buffer syntax)
  (declare (ignore buffer))
  (setf (package-list syntax) nil)
  (flet ((test (x)
           (when (form-list-p x)
             (let ((candidate (first-form (children x))))
               (and (form-token-p candidate)
                    (eq (token-to-object syntax candidate
                                         :no-error t)
                        'cl:in-package)))))
         (extract (x)
           (let ((designator (second-form (children x))))
             (token-to-object syntax designator
                              :no-error t))))
    (with-slots (stack-top) syntax
      (loop for child in (children stack-top)
         when (test child)
         do (push (cons (end-offset child)
                        (extract child))
                  (package-list syntax))))))

(defmethod update-syntax (buffer (syntax lisp-syntax))
  (let* ((low-mark (low-mark buffer))
	 (high-mark (high-mark buffer)))
    (when (mark<= low-mark high-mark)
      (catch 'done
	(with-slots (current-state stack-top scan potentially-valid-trees) syntax
          (setf potentially-valid-trees
                (if (null stack-top)
                    nil
                    (find-first-potentially-valid-lexeme (children stack-top)
                                                         (offset high-mark))))
          (setf stack-top (find-last-valid-lexeme stack-top (offset low-mark)))
          (setf (offset scan) (if (null stack-top) 0 (end-offset stack-top))
                current-state (if (null stack-top)
                                  |initial-state |
                                  (new-state syntax
                                             (parser-state stack-top)
                                             stack-top)))
          (loop do (parse-patch syntax)))))
    (when (need-to-update-package-list-p buffer syntax)
      (update-package-list buffer syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; accessing parser forms

(defun first-noncomment (list)
  "Returns the first non-comment in list."
  (find-if-not #'comment-p list))

(defun rest-noncomments (list)
  "Returns the remainder of the list after the first non-comment,
stripping leading comments."
  (loop for rest on list
	count (not (comment-p (car rest)))
	  into forms
	until (= forms 2)
	finally (return rest)))

(defun nth-noncomment (n list)
  "Returns the nth non-comment in list."
  (loop for item in list
	count (not (comment-p item))
	  into forms
	until (> forms n)
	finally (return item)))

(defun elt-noncomment (list n)
  "Returns the nth non-comment in list."
  (nth-noncomment n list))

(defun second-noncomment (list)
  "Returns the second non-comment in list."
  (nth-noncomment 1 list))

(defun third-noncomment (list)
  "Returns the third non-comment in list."
  (nth-noncomment 2 list))

(defun rest-forms (list)
  "Returns the remainder of the list after the first form,
stripping leading non-forms."
  (loop for rest on list
     count (formp (car rest))
       into forms
     until (= forms 2)
     finally (return rest)))

(defun nth-form (n list)
  "Returns the nth form in list or `nil'."
  (loop for item in list
     count (formp item)
       into forms
     until (> forms n)
     finally (when (> forms n)
               (return item))))

(defun elt-form (list n)
  "Returns the nth form in list or `nil'."
  (nth-form n list))

(defun first-form (list)
  "Returns the first form in list."
  (nth-form 0 list))

(defun second-form (list)
  "Returns the second form in list."
  (nth-form 1 list))

(defun third-form (list)
  "Returns the third formw in list."
  (nth-form 2 list))

(defgeneric form-operator (syntax form)
  (:documentation "Return the operator of `form' as a
  token. Returns nil if none can be found.")
  (:method (form syntax) nil))

(defmethod form-operator (syntax (form list-form))
  (first-form (rest (children form))))

(defmethod form-operator (syntax (form complete-quote-form))
  (first-form (rest (children (second (children form))))))

(defmethod form-operator (syntax (form complete-backquote-form))
  (first-form (rest (children (second (children form))))))

(defgeneric form-operands (syntax form)
  (:documentation "Returns the operands of `form' as a list of
  tokens. Returns nil if none can be found.")
  (:method (form syntax) nil))

(defmethod form-operands (syntax (form list-form))
  (remove-if-not #'formp (rest-forms (children form))))

(defun form-toplevel (form syntax)
  "Return the top-level form of `form'."
  (if (null (parent (parent form)))
      form
      (form-toplevel (parent form) syntax)))

(defgeneric form-operator-p (token syntax)
  (:documentation "Return true if `token' is the operator of its form. Otherwise,
  return nil.")
  (:method (token syntax)
    (with-accessors ((pre-token preceding-parse-tree)) token
      (cond ((typep pre-token 'left-parenthesis-lexeme)
             t)
            ((comment-p pre-token)
             (form-operator-p pre-token syntax))
            (t nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful functions for selecting forms based on the mark.

(defun expression-at-mark (mark-or-offset syntax)
  "Return the form at `mark-or-offset'. If `mark-or-offset' is just after,
or inside, a top-level-form, or if there are no forms after
`mark-or-offset', the form preceding `mark-or-offset' is
returned. Otherwise, the form following `mark-or-offset' is
returned."
  (as-offsets ((offset mark-or-offset))
    (or (form-around syntax offset)
        (form-after syntax offset)
        (form-before syntax offset))))

(defun definition-at-mark (mark-or-offset syntax)
  "Return the top-level form at `mark-or-offset'. If `mark-or-offset' is just after,
or inside, a top-level-form, or if there are no forms after
`mark-or-offset', the top-level-form preceding `mark-or-offset'
is returned. Otherwise, the top-level-form following
`mark-or-offset' is returned."
  (form-toplevel (expression-at-mark mark-or-offset syntax) syntax))

(defun symbol-at-mark (mark-or-offset syntax)
  "Return a symbol token at `mark-or-offset'. This function will
  \"unwrap\" quote-forms in order to return the symbol token. If
  no symbol token can be found, NIL will be returned."
  (labels ((unwrap-form (form)
             (cond ((form-quoted-p form)
                    (unwrap-form (first-form (children form))))
                   ((form-token-p form)
                    form))))
    (unwrap-form (expression-at-mark mark-or-offset syntax))))

(defun fully-quoted-form (token)
  "Return the top token object for `token', return `token' or the
top quote-form that `token' is buried in. "
  (labels ((ascend (form)
             (cond ((form-quoted-p (parent form))
                    (ascend (parent form)))
                   (t form))))
    (ascend token)))

(defun fully-unquoted-form (token)
  "Return the bottom token object for `token', return `token' or
the form that `token' quotes, peeling away all quote forms."
  (labels ((descend (form)
             (cond ((form-quoted-p form)
                    (descend (first-form (children form))))
                   (t form))))
    (descend token)))

(defun this-form (mark-or-offset syntax)
  "Return a form at `mark-or-offset'. This function defines which
  forms the COM-FOO-this commands affect."
  (as-offsets ((offset mark-or-offset))
    (or (form-around syntax offset)
        (form-before syntax offset))))

(defun preceding-form (mark-or-offset syntax)
  "Return a form at `mark-or-offset'."
  (as-offsets ((offset mark-or-offset))
   (or (form-before syntax offset)
       (form-around syntax offset))))

(defun text-of-definition-at-mark (mark syntax)
  "Return the text of the definition at mark."
  (let ((definition (definition-at-mark mark syntax)))
    (buffer-substring (buffer mark)
                      (start-offset definition)           
                      (end-offset definition))))
                      
(defun text-of-expression-at-mark (mark-or-offset syntax)
  "Return the text of the expression at `mark-or-offset'."
  (let ((expression (expression-at-mark mark-or-offset syntax)))
    (token-string syntax expression)))

(defun symbol-name-at-mark (mark-or-offset syntax)
  "Return the text of the symbol at `mark-or-offset'."
  (let ((token (symbol-at-mark mark-or-offset syntax)))
    (when token (token-string syntax token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Querying forms for data

(defmacro define-form-predicate (name (&rest t-classes) &optional documentation)
  "Define a generic function named `name', taking a single
  argument. A default method that returns NIL will be defined,
  and methods returning T will be defined for all classes in
  `t-classes'."
  `(progn
     (defgeneric ,name (form)
       (:documentation ,(or documentation "Check `form' for something."))
       (:method (form) nil))
     ,@(loop for class in t-classes collecting
            `(defmethod ,name ((form ,class))
               t))))

(define-form-predicate formp (form))
(define-form-predicate form-list-p (complete-list-form incomplete-list-form))
(define-form-predicate form-incomplete-p (incomplete-form-mixin))
(define-form-predicate form-complete-p (complete-form-mixin))
(define-form-predicate form-token-p (token-mixin))
(define-form-predicate form-string-p (string-form))
(define-form-predicate form-quoted-p (quote-form backquote-form))

(define-form-predicate comment-p (comment))

(defgeneric form-at-top-level-p (form)
  (:documentation "Return NIL if `form' is not a top-level-form,
  T otherwise.")
  (:method ((form t))
    (or (typep (parent form) 'form*)
        (null (parent form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful functions for modifying forms based on the mark.

(defun replace-symbol-at-mark (mark syntax string)
  "Replace the symbol at `mark' with `string' and move `mark' to
after `string'."
  (let ((token (symbol-at-mark mark syntax)))
    (setf (offset mark) (start-offset token))
    (forward-kill-expression mark syntax)
    (insert-sequence mark string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defvar *white-space-start* nil)

(defvar *current-line* 0)

(defparameter *standard-faces*
  `((:error ,+red+ nil)
    (:string ,+rosy-brown+ ,(make-text-style nil :italic nil))
    (:keyword ,+orchid+ nil)
    (:macro ,+purple+ nil)
    (:special-form ,+purple+ nil)
    (:lambda-list-keyword ,+dark-green+ nil)
    (:comment ,+maroon+ nil)
    (:reader-conditional ,+gray50+ nil)))

(defparameter *reader-conditional-faces*
  `((:error ,+red+ nil)
    (:string ,+gray50+ ,(make-text-style nil :italic nil))
    (:keyword ,+gray50+ nil)
    (:macro ,+gray50+ nil)
    (:special-form ,+gray50+ nil)
    (:lambda-list-keyword ,+gray50+ nil)
    (:comment ,+gray50+ nil)
    (:reader-conditional ,+gray50+ nil)))

(defvar *current-faces* nil)

(defun face-colour (type)
  (first (cdr (assoc type *current-faces*))))

(defun face-style (type)
  (second (cdr (assoc type *current-faces*))))

(defmacro with-face ((face &optional (stream-symbol 'stream)) &body body)
  `(with-drawing-options (,stream-symbol :ink (face-colour ,face)
                                         :text-style (face-style ,face))
     ,@body))

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
        (tab-width (tab-width pane)))
    (with-sheet-medium (medium pane)
      (with-accessors ((cursor-positions cursor-positions)) (syntax buffer)
        (loop while (< start end)
           do (case (buffer-object buffer start)
                (#\Newline (record-line-vertical-offset pane (syntax buffer) (incf *current-line*))
                           (terpri pane)
                           (stream-increment-cursor-position
                            pane (first (aref cursor-positions 0)) 0))
                ((#\Page #\Return #\Space) (stream-increment-cursor-position
                                            pane space-width 0))
                (#\Tab (let ((x (stream-cursor-position pane)))
                         (stream-increment-cursor-position
                          pane (- tab-width (mod x tab-width)) 0))))
           (incf start))))))

(defgeneric display-parse-tree (parse-symbol stream drei syntax)
  (:documentation "Display the given parse-symbol on the supplied
  stream, assuming `drei' to be the relevant Drei instance and
  `syntax' being the syntax object responsible for the parse
  symbol."))

(defmethod display-parse-tree ((parse-symbol (eql nil)) stream (drei drei)
                               (syntax lisp-syntax))
  nil)

(defmethod display-parse-tree :around (parse-symbol stream (drei drei)
                                                    (syntax lisp-syntax))
  (with-slots (top bot) drei
     (when (and (start-offset parse-symbol) 
                (mark< (start-offset parse-symbol) bot)
                (mark> (end-offset parse-symbol) top))
       (call-next-method))))  

(defmethod display-parse-tree (parse-symbol stream (drei drei)
                               (syntax lisp-syntax))
  (with-slots (top bot) drei
    (loop for child in (children parse-symbol)
       when (and (start-offset child) 
                 (mark> (end-offset child) top))
         do (if (mark< (start-offset child) bot)
                (display-parse-tree child stream drei syntax)
                (return)))))

(defmethod display-parse-tree ((parse-symbol error-symbol) stream (drei drei)
                               (syntax lisp-syntax))
  (let ((children (children parse-symbol)))
    (loop until (or (null (cdr children))
		    (typep (parser-state (cadr children)) 'error-state))
	  do (display-parse-tree (pop children) stream drei syntax))
    (if (and (null (cdr children))
	     (not (typep (parser-state parse-symbol) 'error-state)))
	(display-parse-tree (car children) stream drei syntax)
	(with-face (:error)
	  (loop for child in children
		do (display-parse-tree child stream drei syntax))))))

(defmethod display-parse-tree ((parse-symbol error-lexeme) stream (drei drei) (syntax lisp-syntax))
  (with-face (:error)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol unmatched-right-parenthesis-lexeme)
			       stream (drei drei) (syntax lisp-syntax))
  (with-face (:error)
    (call-next-method)))

(define-presentation-type unknown-symbol () :inherit-from 'symbol
                          :description "unknown symbol")

(define-presentation-method presentation-typep (object (type unknown-symbol))
  (or (symbolp object) (stringp object)))

(defmethod display-parse-tree ((parse-symbol token-mixin) stream (drei drei) (syntax lisp-syntax))
  (if (> (the fixnum (end-offset parse-symbol)) (the fixnum (start-offset parse-symbol)))
      (let ((string (token-string syntax parse-symbol)))
        (multiple-value-bind (symbol status)
            (token-to-object syntax parse-symbol :no-error t)
          (with-output-as-presentation
              (stream (if status symbol string)
                      (if status 'symbol 'unknown-symbol)
                      :single-box :highlighting)
            (cond ((eql (buffer-object (buffer syntax) (start-offset parse-symbol)) #\:)
                   (with-face (:keyword)
                     (call-next-method)))
                  ((eql (buffer-object (buffer syntax) (start-offset parse-symbol)) #\&)
                   (with-face (:lambda-list-keyword)
                     (call-next-method)))
                  ((and (macro-function symbol)
                        (form-operator-p parse-symbol syntax))
                   (with-face (:macro)
                     (call-next-method)))
                  ((and (special-operator-p symbol)
                        (form-operator-p parse-symbol syntax))
                   (with-face (:special-form)
                     (call-next-method)))
                  (t (call-next-method))))))
      (call-next-method)))

(defmethod display-parse-tree ((parser-symbol literal-object-lexeme) stream (drei drei)
                               (syntax lisp-syntax))
  (updating-output
      (stream :unique-id (list drei parser-symbol)
              :id-test #'equal
              :cache-value parser-symbol
              :cache-test #'eql)
    (let ((object (token-to-object syntax parser-symbol)))
      (present object (presentation-type-of object) :stream stream))))

(defmethod display-parse-tree ((parser-symbol lisp-lexeme) stream (drei drei)
                               (syntax lisp-syntax))
  (flet ((cache-test (t1 t2)
           (and (eq t1 t2)
                (eq (slot-value t1 'ink)
                    (medium-ink (sheet-medium stream)))
                (eq (slot-value t1 'face)
                    (text-style-face (medium-text-style (sheet-medium stream)))))))
    (updating-output
        (stream :unique-id (list drei parser-symbol)
                :id-test #'equal
                :cache-value parser-symbol
                :cache-test #'cache-test)
      (with-slots (ink face) parser-symbol
        (setf ink (medium-ink (sheet-medium stream))
              face (text-style-face (medium-text-style (sheet-medium stream))))
        (let ((string (token-string syntax parser-symbol)))
          (present string 'string :stream stream))))))
          
(defmethod display-parse-tree :before ((parse-symbol lisp-lexeme) stream (drei drei)
                                       (syntax lisp-syntax))
  (handle-whitespace stream (buffer drei) *white-space-start* (start-offset parse-symbol))
  (setf *white-space-start* (end-offset parse-symbol)))

(define-presentation-type lisp-string () 
                          :description "lisp string")

(defmethod display-parse-tree ((parse-symbol complete-string-form) stream (drei drei) (syntax lisp-syntax))
  (let ((children (children parse-symbol)))
    (if (third children)
        (let ((string (buffer-substring (buffer syntax)
                                        (start-offset (second children))
                                        (end-offset (car (last children 2))))))
          (with-output-as-presentation (stream string 'lisp-string
                                               :single-box :highlighting)
            (with-face (:string)
              (display-parse-tree (pop children) stream drei syntax)
	      (loop until (null (cdr children))
                 do (display-parse-tree (pop children) stream drei syntax))
              (display-parse-tree (pop children) stream drei syntax))))
        (with-face (:string)
         (progn (display-parse-tree (pop children) stream drei syntax)
                (display-parse-tree (pop children) stream drei syntax))))))

(defmethod display-parse-tree ((parse-symbol incomplete-string-form) stream (drei drei) (syntax lisp-syntax))
  (let ((children (children parse-symbol)))
    (if (second children)
        (let ((string (buffer-substring (buffer syntax)
                                        (start-offset (second children))
                                        (end-offset (car (last children))))))
          (with-output-as-presentation (stream string 'lisp-string
                                               :single-box :highlighting)
            (with-face (:string)
              (display-parse-tree (pop children) stream drei syntax)
              (loop until (null children)
                 do (display-parse-tree (pop children) stream drei syntax)))))
        (with-face (:string)
         (display-parse-tree (pop children) stream drei syntax)))))

(defmethod display-parse-tree ((parse-symbol line-comment-form) stream (drei drei) (syntax lisp-syntax))
  (with-face (:comment)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol long-comment-form) stream (drei drei) (syntax lisp-syntax))
  (with-face (:comment)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol reader-conditional-positive-form)
			       stream (drei drei) (syntax lisp-syntax))
  (let ((conditional (second-noncomment (children parse-symbol))))
    (if (eval-feature-conditional conditional syntax)
	(call-next-method)
	(let ((*current-faces* *reader-conditional-faces*))
	  (with-face (:reader-conditional)
	    (call-next-method))))))

(defmethod display-parse-tree ((parse-symbol reader-conditional-negative-form)
				stream (drei drei) (syntax lisp-syntax))
  (let ((conditional (second-noncomment (children parse-symbol))))
    (if (eval-feature-conditional conditional syntax)
	(let ((*current-faces* *reader-conditional-faces*))
	  (with-face (:reader-conditional)
	    (call-next-method)))
	(call-next-method))))

(defgeneric eval-feature-conditional (conditional-form syntax))

(defmethod eval-feature-conditional (conditional-form (syntax lisp-syntax))
  nil)

;; Adapted from slime.el

(defconstant +keyword-package+ (find-package :keyword)
  "The KEYWORD package.")

(defmethod eval-feature-conditional ((conditional token-mixin) (syntax lisp-syntax))
  (let* ((string (token-string syntax conditional))
	 (symbol (parse-symbol string :package +keyword-package+)))
    (member symbol *features*)))

(defmethod eval-feature-conditional ((conditional list-form) (syntax lisp-syntax))
  (let ((children (children conditional)))
    (when (third-noncomment children)
      (flet ((eval-fc (conditional)
	       (funcall #'eval-feature-conditional conditional syntax)))
	(let* ((type (second-noncomment children))
	       (conditionals  (butlast
			       (nthcdr
				2
				(remove-if
				 #'comment-p
				 children))))
	       (type-string (token-string syntax type))
	       (type-symbol (parse-symbol type-string :package +keyword-package+)))
	  (case type-symbol
	    (:and (funcall #'every #'eval-fc conditionals))
	    (:or (funcall #'some #'eval-fc conditionals))
	    (:not (when conditionals
		    (funcall #'(lambda (f l) (not (apply f l)))
			     #'eval-fc conditionals)))))))))
	  
(defmethod display-parse-tree ((parse-symbol complete-list-form) stream (drei drei) (syntax lisp-syntax))
  (let* ((children (children parse-symbol))
         (point-offset (the fixnum (offset (point drei))))
         ;; The following is set to true if the location if the point
         ;; warrants highlighting of a set of matching parentheses.
         (should-highlight (and (active drei)
                                (or (= (the fixnum (end-offset parse-symbol)) point-offset)
                                    (= (the fixnum (start-offset parse-symbol)) point-offset)))))
    (if should-highlight
        (with-text-face (stream :bold)
          (display-parse-tree (car children) stream drei syntax))
        (display-parse-tree (car children) stream drei syntax))
    (loop for child-list on (cdr children)
       if (and should-highlight (null (cdr child-list))) do
       (with-text-face (stream :bold)
         (display-parse-tree (car child-list) stream drei syntax))
       else do
       (display-parse-tree (car child-list) stream drei syntax))))

(defmethod display-parse-tree ((parse-symbol incomplete-list-form) stream (drei drei) (syntax lisp-syntax))
  (let* ((children (children parse-symbol))
         (point-offset (the fixnum (offset (point drei))))
         ;; The following is set to true if the location if the point
         ;; warrants highlighting of the beginning parenthesis
         (should-highlight (and (active drei)
                                (= (the fixnum (start-offset parse-symbol)) point-offset))))
    (with-face (:error)
      (if should-highlight
          (with-text-face (stream :bold)
            (display-parse-tree (car children) stream drei syntax))
          (display-parse-tree (car children) stream drei syntax)))
    (loop for child in (cdr children) do
      (display-parse-tree child stream drei syntax))))

(defmethod display-drei-contents ((stream clim-stream-pane) (drei drei) (syntax lisp-syntax))
  (with-slots (top bot) drei
    (with-accessors ((cursor-positions cursor-positions)) syntax
      ;; There must always be room for at least one element of line
      ;; information.
      (setf cursor-positions (make-array (1+ (number-of-lines-in-region top bot))
                                         :initial-element nil)
            *current-line* 0
            (aref cursor-positions 0) (multiple-value-list
                                       (stream-cursor-position stream))))
    (setf *white-space-start* (offset top)))
  (let ((*current-faces* *standard-faces*))
    (with-slots (stack-top) syntax
      (display-parse-tree stack-top stream drei syntax))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse 

(defun form-before-in-children (children offset)
  (loop for (first . rest) on children
     if (formp first)
     do
       (cond ((< (start-offset first) offset (end-offset first))
              (return (if (null (children first))
                          nil
                          (form-before-in-children (children first) offset))))
             ((and (>= offset (end-offset first))
                   (or (null (first-form rest))
                       (<= offset (start-offset (first-form rest)))))
              (return (let ((potential-form
                             (cond ((form-list-p first)
                                    (form-before-in-children (children first) offset))
                                   ((and (form-quoted-p first)
                                         (not (form-incomplete-p first))
                                         (form-list-p (second (children first))))
                                    (form-before-in-children (children (second (children first))) offset)))))
                        (if (not (null potential-form))
                            (if (<= (end-offset first)
                                    (end-offset potential-form))
                                potential-form
                                first)
                            (when (formp first)
                              first)))))
             (t nil))))
		 
(defun form-before (syntax offset)
  (with-slots (stack-top) syntax
    (if (or (null (start-offset stack-top))
	    (<= offset (start-offset stack-top)))
	nil
	(form-before-in-children (children stack-top) offset))))

(defun form-after-in-children (children offset)
  (loop for child in children
     if (formp child)
     do (cond ((< (start-offset child) offset (end-offset child))
               (return (if (null (children child))
                           nil
                           (form-after-in-children (children child) offset))))
              ((<= offset (start-offset child))
               (return (let ((potential-form (form-after-in-children (children child) offset)))
                         (if (not (null potential-form))
                             (if (<= (start-offset child)
                                     (start-offset potential-form))
                                 child
                                 potential-form)
                             (when (formp child)
                               child)))))
              (t nil))))
		 
(defun form-after (syntax offset)
  (with-slots (stack-top) syntax
    (if (or (null (start-offset stack-top))
	    (>= offset (end-offset stack-top)))
	nil
	(form-after-in-children (children stack-top) offset))))
	     
(defun form-around-in-children (children offset)
  (loop for child in children
	if (formp child)
	do (cond ((or (<= (start-offset child) offset (end-offset child))
                      (= offset (end-offset child))
                      (= offset (start-offset child)))
		  (return (if (null (first-form (children child)))
			      (when (formp child)
				child)
			      (or (form-around-in-children (children child) offset)
                                  (when (formp child)
                                    child)))))
		 ((< offset (start-offset child))
		  (return nil))
		 (t nil))))

(defun form-around (syntax offset)
  (with-slots (stack-top) syntax
    (if (or (null (start-offset stack-top))
	    (> offset (end-offset stack-top))
	    (< offset (start-offset stack-top)))
	nil
	(form-around-in-children (children stack-top) offset))))

(defun find-list-parent-offset (form fn)
    "Find a list parent of `token' and return `fn' 
applied to this parent token. `Fn' should be a function 
that returns an offset when applied to a 
token (eg. `start-offset' or `end-offset'). If a list
parent cannot be found, return `fn' applied to `form'."
  (when (not (formp form))
    (let ((parent (parent form)))
      (typecase parent
        (form* (funcall fn form))
        (list-form (funcall fn form))
        (null (funcall fn form))
        (t (find-list-parent-offset parent fn))))))

(defun find-list-child-offset (form fn &optional (min-offset 0))
  "Find a list child of `token' with a minimum start 
offset of `min-offset' and return `fn' applied to this child token.
`Fn' should be a function that returns an offset when applied to a 
token (eg. `start-offset' or `end-offset'). If a list child cannot
be found, return nil."
  (labels ((has-list-child (form)
              (some #'(lambda (child)
                                   (if (and (form-list-p child)
                                            (>= (start-offset child)
                                                min-offset))
                                       child
                                       (has-list-child child)))
                               (children form))))
    (let ((list-child (has-list-child form)))
      (when (not (null list-child))
        (funcall fn list-child)))))

(defmethod backward-one-expression (mark (syntax lisp-syntax))
  (let ((potential-form (or (form-before syntax (offset mark))
			    (form-around syntax (offset mark)))))
    (when (and (not (null potential-form))
               (not (= (offset mark) (start-offset potential-form))))
	(setf (offset mark) (start-offset potential-form)))))

(defmethod forward-one-expression (mark (syntax lisp-syntax))
  (let ((potential-form (or (form-after syntax (offset mark))
			    (form-around syntax (offset mark)))))
    (when (and (not (null potential-form))
               (not (= (offset mark) (end-offset potential-form))))
	(setf (offset mark) (end-offset potential-form)))))

(defgeneric forward-one-list (mark syntax)
  (:documentation
   "Move `mark' forward by one list. 
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
     then (end-offset potential-form)
     for potential-form = (or (form-after syntax start)
                              (form-around syntax start))
     until (or (null potential-form)
               (and (= start
                       (end-offset potential-form))
                    (null (form-after syntax start))))
     when (form-list-p potential-form)
     do (setf (offset mark) (end-offset potential-form))
     (return t)))

(defgeneric backward-one-list (mark syntax)
  (:documentation
   "Move `mark' backward by one list.  Return T if successful, or
NIL if the buffer limit was reached."))

(defmethod backward-one-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
     then (start-offset potential-form)
     for potential-form = (or (form-before syntax start)
                              (form-around syntax start))
     until (or (null potential-form)
               (and (= start
                       (start-offset potential-form))
                    (null (form-before syntax start))))
     when (form-list-p potential-form)
     do (setf (offset mark) (start-offset potential-form))
     (return t)))

(drei-motion:define-motion-fns list)

(defun down-list-by-fn (mark syntax fn)
  (let* ((offset (offset mark))
         (potential-form (form-after syntax offset)))
    (let ((new-offset (typecase potential-form
                        (list-form (start-offset potential-form))
                        (null nil)
                        (t (find-list-child-offset
                            (parent potential-form) 
                            fn
                            offset)))))
      (when new-offset 
        (progn (setf (offset mark) (1+ new-offset)) t)))))

(defmethod forward-one-down (mark (syntax lisp-syntax))
  (down-list-by-fn mark syntax #'start-offset))

(defmethod backward-one-down (mark (syntax lisp-syntax))
  (down-list-by-fn mark syntax #'end-offset)
  (backward-object mark syntax))

(defun up-list-by-fn (mark syntax fn)
  (let ((form (or (form-before syntax (offset mark))
                  (form-after syntax (offset mark))
                  (form-around syntax (offset mark)))))
    (when form
        (let ((parent (parent form)))
          (when (not (null parent))
            (let ((new-offset (find-list-parent-offset parent fn)))
              (when new-offset
                (setf (offset mark) new-offset))))))))

(defmethod backward-one-up (mark (syntax lisp-syntax))
  (up-list-by-fn mark syntax #'start-offset))

(defmethod forward-one-up (mark (syntax lisp-syntax))
  (up-list-by-fn mark syntax #'end-offset))

(defmethod eval-defun (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
     (loop for form in (children stack-top)
	   when (and (mark<= (start-offset form) mark)
		     (mark<= mark (end-offset form)))
	     do (return (eval-form-for-drei
                         (get-usable-image syntax)
                         (token-to-object syntax form :read t))))))

(defmethod backward-one-definition (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
    (loop for form in (children stack-top)
	  with last-toplevel-list = nil
	  when (and (formp form)
		    (mark< mark (end-offset form)))
          do (if (mark< (start-offset form) mark)
		 (setf (offset mark) (start-offset form))
		 (when last-toplevel-list form
		       (setf (offset mark) (start-offset last-toplevel-list))))
	     (return t)
	  when (formp form)
	  do (setf last-toplevel-list form)
	  finally (when last-toplevel-list form
		       (setf (offset mark)
                             (start-offset last-toplevel-list))
                       (return t)))))

(defmethod forward-one-definition (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
    (loop for form in (children stack-top)
	  when (and (formp form)
		    (mark< mark (end-offset form)))
	  do (setf (offset mark) (end-offset form))
	     (loop-finish)
          finally (return t))))

(defun in-type-p-in-children (children offset type)
  (loop for child in children
	do (cond ((< (start-offset child) offset (end-offset child))
		  (return (if (typep child type)
			      child
			      (in-type-p-in-children (children child) offset type))))
		 ((<= offset (start-offset child))
		  (return nil))
		 (t nil))))

(defun in-type-p (mark syntax type)
  (let ((offset (offset mark)))
    (with-slots (stack-top) syntax
       (if (or (null (start-offset stack-top))
	       (>= offset (end-offset stack-top))
	       (<= offset (start-offset stack-top)))
	   nil)
       (in-type-p-in-children (children stack-top) offset type))))

(defun in-string-p (mark syntax)
  (in-type-p mark syntax 'string-form))

(defun in-comment-p (mark syntax)
  (in-type-p mark syntax 'comment))

;;; shamelessly replacing SWANK code
;; We first work through the string removing the characters and noting
;; which ones are escaped. We then replace each character with the
;; appropriate case version, according to the readtable.
;; Finally, we extract the package and symbol names.
;; Being in an editor, we are waaay more lenient than the reader.

(defun parse-escapes (string)
  "Return a string and a list of escaped character positions.
Uses part of the READ algorithm in CLTL2 22.1.1."
  (let ((length (length string))
	(index 0)
	irreplaceables chars)
    (tagbody
     step-8
       (unless (< index length) (go end))
       (cond 
	 ((char/= (char string index) #\\ #\|)
	  (push (char string index) chars)
	  (incf index)
	  (go step-8))
	 ((char= (char string index) #\\)
	  (push (length chars) irreplaceables)
	  (incf index)
	  (unless (< index length) (go end))
	  (push (char string index) chars)
	  (incf index)
	  (go step-8))
	 ((char= (char string index) #\|)
	  (incf index)
	  (go step-9)))
     step-9
       (unless (< index length) (go end))
       (cond 
	 ((char/= (char string index) #\\ #\|)
	  (push (length chars) irreplaceables)
	  (push (char string index) chars)
	  (incf index)
	  (go step-9))
	 ((char= (char string index) #\\)
	  (push (length chars) irreplaceables)
	  (incf index)
	  (unless (< index length) (go end))
	  (push (char string index) chars)
	  (incf index)
	  (go step-9))
	 ((char= (char string index) #\|)
	  (incf index)
	  (go step-8)))
     end
       (return-from parse-escapes
	 (values (coerce (nreverse chars) 'string)
		 (nreverse irreplaceables))))))

(defun invert-cases (string &optional (irreplaceables nil))
  "Returns two flags: unescaped upper-case and lower-case chars in STRING."
  (loop for index below (length string)
       with upper = nil
       with lower = nil
       when (not (member index irreplaceables))
        if (upper-case-p (char string index))
         do (setf upper t) end
        if (lower-case-p (char string index))
         do (setf lower t) end
     finally (return (values upper lower))))

(defun replace-case (string &optional (case (readtable-case *readtable*))
		                      (irreplaceables nil))
  "Convert string according to readtable-case."
  (multiple-value-bind (upper lower) (invert-cases string irreplaceables)
    (loop for index below (length string)
       as char = (char string index) then (char string index)
       if (member index irreplaceables)
         collect char into chars
       else
         collect (ecase case
		   (:preserve char)
		   (:upcase (char-upcase char))
		   (:downcase (char-downcase char))
		   (:invert (cond ((and lower upper) char)
				  (lower (char-upcase char))
				  (upper (char-downcase char))
				  (t char)))) into chars
       finally (return (coerce chars 'string)))))

(defun parse-token (string &optional (case (readtable-case *readtable*)))
  "Extracts the symbol-name and package name from STRING
and whether the symbol-name was separated from the package by a double colon."
  (multiple-value-bind (string irreplaceables) (parse-escapes string)
    (let ((string (replace-case string case irreplaceables))
	  package-name symbol-name internalp)
      (loop for index below (length string)
	   with symbol-start = 0
	   when (and (char= (char string index) #\:)
		     (not (member index irreplaceables)))
	        do (setf package-name (subseq string 0 index))
	           (if (and (< (incf index) (length string))
			    (char= (char string index) #\:)
			    (not (member index irreplaceables)))
		       (setf symbol-start (1+ index)
			     internalp t)
		       (setf symbol-start index))
	           (loop-finish)
	   finally (setf symbol-name (subseq string symbol-start)))
      (values symbol-name package-name internalp))))

#|
;;; Compare CLHS 23.1.2.1
 (defun test-parse-token ()
  (let ((*readtable* (copy-readtable nil)))
    (format t "READTABLE-CASE  Input         Symbol-name   Token-name~
             ~%------------------------------------------------------~
             ~%")
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (dolist (input '("ZEBRA" "Zebra" "zebra" "\\zebra" "\\Zebra" "z|ebr|a"
		       "|ZE\\bRA|" "ze\\|bra"))
	(format t "~&:~A~16T~A~30T~A~44T~A"
		(string-upcase readtable-case)
		input
		(progn (setf (readtable-case *readtable*) readtable-case)
		       (symbol-name (read-from-string input)))
		(parse-token input readtable-case))))))
|#

(defun token-string (syntax token)
  "Return the string that specifies `token' in the buffer of
  `syntax'."
  (buffer-substring (buffer syntax)
                    (start-offset token)
                    (end-offset token)))

(defun parse-symbol (string &key (package *package*) (case (readtable-case *readtable*)))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbol was
found in the package. Note that a symbol may be returned even if
it was not found in a package."
  (multiple-value-bind (symbol-name package-name)
      (parse-token string case)
    (let ((package (cond ((string= package-name "") +keyword-package+)
                         (package-name              (find-package package-name))
                         (t                         package))))
      (multiple-value-bind (symbol status)
          (when package
            (find-symbol symbol-name package))
        (if (or symbol status)
            (values symbol status)
            (values (make-symbol symbol-name) nil))))))

(defun token-to-symbol (syntax token &optional (case (readtable-case *readtable*)))
  "Return the symbol `token' represents. If the symbol cannot be
found in a package, an uninterned symbol will be returned."
  (token-to-object syntax token
                   :case case
                   :no-error t))

(defgeneric token-to-object (syntax token &key no-error package quote read &allow-other-keys)
  (:documentation "Return the Lisp object `token' would evaluate
to if read. An attempt will be made to construct objects from
incomplete tokens. This function may signal an error if
`no-error' is nil and `token' cannot be converted to a Lisp
object. Otherwise, nil will be returned.")
  (:method :around (syntax (token t) &rest args &key
                           package quote no-error &allow-other-keys)
           ;; Ensure that every symbol that is READ will be looked up
           ;; in the correct package. Also handle quoting.
           (flet ((act ()
                    (let ((*package* (or package
                                         (package-at-mark
                                     syntax (start-offset token)))))
                      (cond (quote
                             (setf (getf args :quote) nil)
                             `',(call-next-method))
                            (t
                             (call-next-method))))))
             (if no-error 
                 (ignore-errors (act))
                 (act))))
  (:method (syntax (token t) &key no-error &allow-other-keys)
           (declare (ignore no-error))
           ;; We ignore `no-error' as it is truly a bug in the syntax
           ;; module if no handler method is specialized on this form.
           (error "Cannot convert token to Lisp object: ~A"
                  token))
  (:method (syntax (token incomplete-form-mixin) &key no-error &allow-other-keys)
           (unless no-error
             (error "Cannot convert incomplete form to Lisp object: ~A"
                    token))))

(defmethod token-to-object (syntax (token complete-token-lexeme)
                            &key no-error read (case (readtable-case *readtable*))
                            &allow-other-keys)
  (declare (ignore no-error))
  (if read
      (read-from-string (token-string syntax token))
      (parse-symbol (token-string syntax token) :case case)))

(defmethod token-to-object (syntax (token complete-token-form)
                            &key no-error read (case (readtable-case *readtable*))
                            &allow-other-keys)
  (declare (ignore no-error))
  (if read
      (read-from-string (token-string syntax token))
      (parse-symbol (token-string syntax token) :case case)))

(defmethod token-to-object (syntax (token number-lexeme) &rest args)
  (declare (ignore args))
  (let ((*read-base* (base syntax)))
    (read-from-string (token-string syntax token))))

(defmethod token-to-object (syntax (token list-form) &rest args)
  (loop for child in (children token)
     if (typep child 'comma-at-form)
       append (apply #'token-to-object syntax child args)
     else if (formp child)
       collect (apply #'token-to-object syntax child args)))

(defmethod token-to-object (syntax (token simple-vector-form) &key &allow-other-keys)
  (apply #'vector (call-next-method)))

(defmethod token-to-object (syntax (token incomplete-string-form) &rest args)
  (declare (ignore args))
  (read-from-string (concatenate 'string
                                 (token-string syntax token)
                                 "\"")))

(defmethod token-to-object (syntax (token complete-string-form) &key no-error &allow-other-keys)
  (declare (ignore no-error))
  (read-from-string (token-string syntax token)))

(defmethod token-to-object (syntax (token complete-quote-form) &rest args)
  (apply #'token-to-object syntax (second (children token)) :quote t args))

(defmethod token-to-object (syntax (token incomplete-quote-form) &rest args)
  (declare (ignore args))
  ;; Utterly arbitrary, but reasonable in my opinion.
  '(quote))

;; I'm not sure backquotes are handled correctly, but they should be,
;; at least when :read t is specified.
(defmethod token-to-object (syntax (token backquote-form) &rest args)
  (let ((backquoted-form (first-form (children token))))
    (if (form-list-p backquoted-form)
        `'(,@(apply #'token-to-object syntax backquoted-form args))
        `',(apply #'token-to-object syntax backquoted-form args))))

(defmethod token-to-object (syntax (token comma-form) &rest args)
  (apply #'token-to-object syntax (first-form (children token)) args))

(defmethod token-to-object (syntax (token comma-at-form) &rest args)
  (apply #'token-to-object syntax (first-form (children token)) args))

(defmethod token-to-object (syntax (token function-form) &rest args)
  (list 'cl:function (apply #'token-to-object syntax (second (children token)) args)))

(defmethod token-to-object (syntax (token complete-character-lexeme) &key &allow-other-keys)
  (read-from-string (token-string syntax token)))

(defmethod token-to-object (syntax (token cons-cell-form) &key &allow-other-keys)
  (let ((components (remove-if #'(lambda (token)
                                   (not (formp token)))
                               (children token))))
    (if (<= (length components) 2)
        (cons (token-to-object syntax (first components))
              (token-to-object syntax (second components)))
        (loop for (head . tail) on components
           if (rest tail)
           collect (token-to-object syntax head)
           else if (not (null tail))
           append (cons (token-to-object syntax head)
                        (token-to-object syntax (first tail)))))))

;; Perhaps just returning no values for conditionals whose condition
;; evaluates to NIL isn't such a good idea?
(defmethod token-to-object (syntax (token reader-conditional-positive-form) &key &allow-other-keys)
  (let ((conditional (second-noncomment (children token))))
    (if (eval-feature-conditional conditional syntax)
        (token-to-object syntax (third-noncomment (children token)))
        (values))))

(defmethod token-to-object (syntax (token reader-conditional-negative-form) &key &allow-other-keys)
  (let ((conditional (second-noncomment (children token))))
    (when (not (eval-feature-conditional conditional syntax))
      (token-to-object syntax (third-noncomment (children token))))))

(defmethod token-to-object (syntax (token uninterned-symbol-form) &key &allow-other-keys)
  (read-from-string (token-string syntax token)))

(defmethod token-to-object (syntax (token undefined-reader-macro-form) &key read &allow-other-keys)
  ;; ???
  (when read
    (read-from-string (token-string syntax token))))

(defmethod token-to-object ((syntax lisp-syntax) (token literal-object-lexeme) &key &allow-other-keys)
  (object-after (start-mark token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Arglist fetching.
;;;
;;; Just the basics. The fun stuff is found in lisp-syntax-swine.lisp.

(defgeneric arglist-for-form (syntax operator &optional arguments)
  (:documentation
   "Return an arglist for `operator'")
  (:method (syntax operator &optional arguments)
    (declare (ignore arguments))
    (cleanup-arglist
     (arglist (get-usable-image syntax) operator))))

(defmethod arglist-for-form (syntax (operator list) &optional arguments)
  (declare (ignore arguments))
  (case (first operator)
    ('cl:lambda (cleanup-arglist (second operator)))))

;; HACK ALERT: SBCL, and some implementations I guess, provides us
;; with an arglist that is too simple, confusing the code
;; analysers. We fix that here.
(defmethod arglist-for-form (syntax (operator (eql 'clim-lisp:defclass)) &optional arguments)
  (declare (ignore arguments))
  '(name (&rest superclasses) (&rest slots) &rest options))

(defmethod arglist-for-form (syntax (operator (eql 'cl:defclass)) &optional arguments)
  (declare (ignore arguments))
  '(name (&rest superclasses) (&rest slots) &rest options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; indentation

(defgeneric indent-form (syntax tree path))

(defmethod indent-form ((syntax lisp-syntax) (tree form*) path)
  (cond ((or (null path)
	     (and (null (cdr path)) (zerop (car path))))
	 (values tree 0))
	((null (cdr path))
	 (values (elt-noncomment (children tree) (1- (car path))) 0))
	(t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

(defmethod indent-form ((syntax lisp-syntax) (tree string-form) path)
  (values (form-toplevel tree syntax) 0))

(defmethod indent-form ((syntax lisp-syntax) (tree reader-conditional-form) path)
  (cond ((or (null path)
	     (and (null (cdr path)) (zerop (car path))))
	 (values tree 0))
	((null (cdr path))
	 (values (first-form (children tree)) 0))))

(defmethod indent-form ((syntax lisp-syntax) (tree readtime-evaluation-form) path)
  (if (null (cdr path))
      (values tree 0)
      (indent-form syntax (elt-form (children tree) 0) (cdr path))))

(defmethod indent-form ((syntax lisp-syntax) (tree list-form) path)
  (if (= (car path) 1)
      ;; before first element
      (values tree 1)
      (let ((first-child (elt-noncomment (children tree) 1)))
	(cond ((and (form-token-p first-child)
		    (token-to-object syntax first-child))
	       (compute-list-indentation syntax (token-to-object syntax first-child) tree path))
	      ((null (cdr path))
	       ;; top level
	       (if (= (car path) 2)
		   ;; indent like first element
		   (values (elt-noncomment (children tree) 1) 0)
		   ;; indent like second element
		   (values (elt-noncomment (children tree) 2) 0)))
	      (t
	       ;; inside a subexpression
	       (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))))	    

(defmethod indent-form ((syntax lisp-syntax) (tree token-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree error-symbol) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree long-comment-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree quote-form) path)
  (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))

(defmethod indent-form ((syntax lisp-syntax) (tree backquote-form) path)
  (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))

(defmethod indent-form ((syntax lisp-syntax) (tree comma-form) path)
  (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))

(defmethod indent-form ((syntax lisp-syntax) (tree function-form) path)
  (if (null (cdr path))
      (values tree 0)
      (indent-form syntax (elt-form (children tree) 0) (cdr path))))

(defmethod indent-binding ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; top level
      (cond ((= (car path) 1)
	     ;; before variable, indent 1
	     (values tree 1))
	    ((= (car path) 2)
	     ;; between variable and value
	     (values (elt-noncomment (children tree) 1) 0))
	    (t
	     ;; after value
	     (values (elt-noncomment (children tree) 2) 0)))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-bindings ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; entire bind form
      (if (= (car path) 1)
	  ;; before first binding, indent 1
	  (values tree 1)
	  ;; after some bindings, align with first binding
	  (values (elt-noncomment (children tree) 1) 0))
      ;; inside a bind form
      (indent-binding syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation ((syntax lisp-syntax) symbol tree path)
  (if (null (cdr path))
      ;; top level
      (let* ((arglist (when (fboundp symbol)
                        (arglist-for-form syntax symbol)))
             (body-or-rest-pos (or (position '&body arglist)
                                   (position '&rest arglist))))
        (if (and (or (macro-function symbol)
                     (special-operator-p symbol))
                 (and (not (null body-or-rest-pos))
                      (plusp body-or-rest-pos)))
            ;; macro-form with "interesting" arguments.
            (if (>= (- (car path) 2) body-or-rest-pos)
                ;; &body arg.
                (values (elt-noncomment (children tree) 1) 1)
                ;; non-&body-arg.
                (values (elt-noncomment (children tree) 1) 1))
            ;; normal form.
            (if (= (car path) 2)
                ;; indent like first child
                (values (elt-noncomment (children tree) 1) 0)
                ;; indent like second child
                (values (elt-noncomment (children tree) 2) 0))))
      ;; inside a subexpression
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmacro define-list-indentor (name element-indentor)
  `(defun ,name (syntax tree path)
     (if (null (cdr path))
	 ;; top level
	 (if (= (car path) 1)
	     ;; indent one more than the list
	     (values tree 1)
	     ;; indent like the first element
	     (values (elt-noncomment (children tree) 1) 0))
	 ;; inside an element
	 (,element-indentor syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

;;; line up the elements vertically
(define-list-indentor indent-list indent-list)

;;; for now the same as indent-list, but try to do better with
;;; optional parameters with default values
(define-list-indentor indent-ordinary-lambda-list indent-list)
;;; again, can do better
(define-list-indentor indent-macro-lambda-list indent-list)
;;; FIXME: also BOA, DEFSETF, DEFTYPE, SPECIALIZED, GENERIC-FUNCTION,
;;; DESTRUCTURING, DEFINE-MODIFY-MACRO and
;;; DEFINE-METHOD-COMBINATION-ARGUMENTS

(defmacro define-simple-indentor (template)
  `(defmethod compute-list-indentation
       ((syntax lisp-syntax) (symbol (eql ',(car template))) tree path)
     (cond ((null (cdr path))
	    (values tree (if (<= (car path) ,(length template)) 4 2)))
	   ,@(loop for fun in (cdr template)
		  for i from 2
		  collect `((= (car path) ,i)
			    (,fun syntax (elt-noncomment (children tree) ,i) (cdr path))))
	   (t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(define-simple-indentor (progn))
(define-simple-indentor (prog1 indent-form))
(define-simple-indentor (prog2 indent-form indent-form))
(define-simple-indentor (locally))
(define-simple-indentor (let indent-bindings))
(define-simple-indentor (let* indent-bindings))
(define-simple-indentor (multiple-value-bind indent-list indent-form))
(define-simple-indentor (defun indent-list indent-ordinary-lambda-list))
(define-simple-indentor (defmacro indent-list indent-macro-lambda-list))
(define-simple-indentor (with-slots indent-bindings indent-form))
(define-simple-indentor (with-accessors indent-bindings indent-form))
(define-simple-indentor (when indent-form))
(define-simple-indentor (unless indent-form))
(define-simple-indentor (print-unreadable-object indent-list))
(define-simple-indentor (defvar indent-form))
(define-simple-indentor (defparameter indent-form))
(define-simple-indentor (defconstant indent-form))
(define-simple-indentor (lambda indent-ordinary-lambda-list))
(define-simple-indentor (pprint-logical-block indent-list))

;;; non-simple-cases: LOOP, MACROLET, FLET, LABELS

;;; do this better 
(define-list-indentor indent-slot-specs indent-list)

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defclass)) tree path)
  (if (null (cdr path))
      ;; top level
      (values tree (if (<= (car path) 3) 4 2))
      (case (car path)
	((2 3)
	 ;; in the class name or superclasses respectively
	 (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
	(4
	 ;; in the slot specs 
	 (indent-slot-specs syntax (elt-noncomment (children tree) 4) (cdr path)))
	(t
	 ;; this is an approximation, might want to do better
	 (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defgeneric)) tree path)
  (if (null (cdr path))
      ;; top level
      (values tree (if (<= (car path) 3) 4 2))
      (case (car path)
	(2
	 ;; in the function name
	 (indent-list syntax (elt-noncomment (children tree) 2) (cdr path)))
	(3
	 ;; in the lambda-list
	 (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) 3) (cdr path)))
	(t
	 ;; in the options or method specifications
	 (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defmethod)) tree path)
  (let ((lambda-list-pos (position-if #'form-list-p
				      (remove-if #'comment-p (children tree)))))
    (cond ((null (cdr path))
	   ;; top level
	   (values tree (if (or (null lambda-list-pos)
				(<= (car path) lambda-list-pos))
			    4
			    2)))
	  ((or (null lambda-list-pos)
	       (< (car path) lambda-list-pos))
	   (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
	  ((= (car path) lambda-list-pos)
	   (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
	  (t
	   (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defun indent-clause (syntax tree path)
  (if (null (cdr path))
      ;; top level
      (case (car path)
        (1 (values tree 1))
        (2 (values tree 1))
        (t (values (elt-noncomment (children tree) 2) 0)))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'cond)) tree path)
  (if (null (cdr path))
      ;; top level
      (if (= (car path) 2)
	  ;; after `cond' 
	  (values tree 2)
	  ;; indent like the first clause
	  (values (elt-noncomment (children tree) 2) 0))
      ;; inside a clause
      (indent-clause syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(macrolet ((def (symbol)
               `(defmethod compute-list-indentation
                 ((syntax lisp-syntax) (symbol (eql ',symbol)) tree path)
                 (if (null (cdr path))
                     (case (car path)
                       (2 (values tree 4))
                       (3 (values tree 2))
                       (t (values (elt-noncomment (children tree) 3) 0)))
                     (indent-clause syntax (elt-noncomment (children tree) (car path)) (cdr path))))))
  (def case)
  (def ccase)
  (def ecase)
  (def typecase)
  (def ctypecase)
  (def etypecase))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'tagbody)) tree path)
  (if (null (cdr path))
      ;; this TOKEN-MIXIN test is not quite right.  It should be a
      ;; test for symbolness of the token, but it shouldn't depend on
      ;; the symbol existing in the current image.  (Arguably, too,
      ;; this is a broken indentation form because it doesn't carry
      ;; over to the implicit tagbodies in macros such as DO.
      (if (form-token-p (elt-noncomment (children tree) (car path))) 
          (values tree 2)
          (values tree 4))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-local-function-definition ((syntax lisp-syntax) tree path)
  (cond ((null (cdr path))
	 ;; top level
	 (cond ((= (car path) 1)
		;; before name, indent 1
		(values tree 1))
	       ((= (car path) 2)
		;; between name and lambda list, indent 4
		(values (elt-noncomment (children tree) 1) 4))
	       (t
		;; after lambda list, indent 2
		(values (elt-noncomment (children tree) 1) 2))))
	((= (car path) 1)
	 ;; inside lambda list
	 (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) 1) (cdr path)))
	(t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

(define-list-indentor indent-local-function-definitions indent-local-function-definition)

(define-simple-indentor (flet indent-local-function-definitions))
(define-simple-indentor (labels indent-local-function-definitions))
(define-simple-indentor (with-open-file indent-list))

;;; CLIM indentation 

(define-simple-indentor (clim:with-output-as-presentation indent-list))
(define-simple-indentor (clim:vertically indent-list))
(define-simple-indentor (clim:horizontally indent-list))
(define-simple-indentor (clim:scrolling indent-list))
(define-simple-indentor (clim:with-drawing-options indent-list))
(define-simple-indentor (clim:define-command-table indent-list))
(define-simple-indentor (clim:define-command indent-list indent-list))
(define-simple-indentor (clim:define-application-frame indent-list indent-list))

(defun compute-path-in-trees (trees n offset)
  (cond ((or (null (first-noncomment trees))
	     (>= (start-offset (first-noncomment trees)) offset))    
	 (list n))
	((or (< (start-offset (first-noncomment trees)) offset (end-offset (first-noncomment trees)))
	     (typep (first-noncomment trees) 'incomplete-form-mixin))
	 (cons n (compute-path-in-tree (first-noncomment trees) offset)))
	(t (compute-path-in-trees (rest-noncomments trees) (1+ n) offset))))

(defun compute-path-in-tree (tree offset)
  (if (null (children tree))
      '()
      (compute-path-in-trees (children tree) 0 offset)))

(defun compute-path (syntax offset)
  (with-slots (stack-top) syntax
    (compute-path-in-tree stack-top offset)))

(defun real-column-number (mark tab-width)
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop with column = 0
	  until (mark= mark mark2)
	  do (if (eql (object-after mark2) #\Tab)
		 (loop do (incf column)
		       until (zerop (mod column tab-width)))
		 (incf column))
	  do (incf (offset mark2))
          finally (return column))))

(defmethod syntax-line-indentation (mark tab-width (syntax lisp-syntax))
  (setf mark (clone-mark mark))
  (beginning-of-line mark)
  (with-slots (stack-top) syntax
    (let ((path (compute-path syntax (offset mark))))
      (multiple-value-bind (tree offset)
	  (indent-form syntax stack-top path)
	(setf (offset mark) (start-offset tree))
	(+ (real-column-number mark tab-width)
	   offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commenting

(defmethod syntax-line-comment-string ((syntax lisp-syntax))
  ";;; ")

(defmethod comment-region ((syntax lisp-syntax) mark1 mark2)
  (line-comment-region syntax mark1 mark2))

(defmethod uncomment-region ((syntax lisp-syntax) mark1 mark2)
  (line-uncomment-region syntax mark1 mark2))
