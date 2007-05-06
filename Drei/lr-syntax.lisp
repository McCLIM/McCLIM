;; -*- Mode: Lisp; Package: DREI-LR-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2007 by
;;;           John Q Splittist (splittist@gmail.com)
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

;;; Base lexing and parsing functionality of 
;;; syntax modules for analysing languages

(in-package :drei-lr-syntax)

(defclass lr-syntax-mixin () 
     ((stack-top :initform nil)
      (potentially-valid-trees)
      (lookahead-lexeme :initform nil :accessor lookahead-lexeme)
      (current-state)
      (initial-state :initarg :initial-state)
      (current-start-mark)
      (current-size)))

(defmethod initialize-instance :after ((syntax lr-syntax-mixin) &rest args)
  (declare (ignore args))
  (with-slots (buffer scan) syntax
     (setf scan (clone-mark (low-mark buffer) :left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defgeneric skip-inter (syntax state scan)
  (:documentation "advance scan until the beginning of a new
    lexeme.  Return T if one can be found and NIL otherwise."))

(defgeneric lex (syntax state scan)
  (:documentation "Return the next lexeme starting at scan."))

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
    whitespace and should recognize ordinary lexemes of the language."))

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

(defmacro reduce-until-type (symbol type &optional end-of-buffer)
  `(let ((result (make-instance ',symbol
                  :children (pop-until-type syntax ',type))))
     (with-slots (start-mark size) result
       (when (null (children result))
         (with-slots (scan) syntax
           (setf start-mark (clone-mark scan :right)
                 size 0)))
       (when ,end-of-buffer
         (setf size (- (size (buffer syntax))
                       (start-offset result)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser step

(defgeneric parser-step (syntax))

(defmethod parser-step ((syntax lr-syntax-mixin))
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

(defmethod update-syntax-for-display (buffer (syntax lr-syntax-mixin) top bot)
  nil)

(defmethod update-syntax (buffer (syntax lr-syntax-mixin))
  (let* ((low-mark (low-mark buffer))
	 (high-mark (high-mark buffer)))
    (when (mark<= low-mark high-mark)
      (catch 'done
	(with-slots (current-state stack-top scan potentially-valid-trees
				   initial-state) syntax
          (setf potentially-valid-trees
                (if (null stack-top)
                    nil
                    (find-first-potentially-valid-lexeme (children stack-top)
                                                         (offset high-mark))))
          (setf stack-top (find-last-valid-lexeme stack-top (offset low-mark)))
          (setf (offset scan) (if (null stack-top) 0 (end-offset stack-top))
                current-state (if (null stack-top)
				  initial-state
                                  (new-state syntax
                                             (parser-state stack-top)
                                             stack-top)))
          (loop do (parse-patch syntax)))))))

