;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)

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

(in-package :clim-internals)

(defun get-environment-variable (string)
  #+excl (sys:getenv string)
  #+cmu (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (sys::getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #+openmcl (ccl::getenv string)
  #-(or excl cmu clisp sbcl openmcl) (error "GET-ENVIRONMENT-VARIABLE not implemented"))

(defun last1 (list)
  "Last element of LIST."
  (first (last list)))

(defun 2+ (x)
  (+ x 2))

(defun 2- (x)
  (- x 2))


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
       nconc (mapcar #'list vars vals) into temp-init-let-form
       nconc (copy-list store-vars) into temp-init-let-form
       nconc (copy-list old-value-names) into temp-init-let-form
       nconc `(,(valueify old-value-names) ,reader-form) into temp-save-old-values-setf-form
       nconc `(,(valueify store-vars) ,new-value) into temp-new-values-set-form
       nconc `(,(valueify store-vars) ,(valueify old-value-names)) into temp-old-values-set-form
       collect writer-form into temp-update-form
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

;;; XXX This is currently broken with respect to declarations

(defmacro letf* ((&rest forms) &body body)
  (if (null forms)
      `(locally
	 ,@body)
      `(letf (,(car forms))
	 (letf* (,(cdr forms))
	   ,@body))))

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
                (loop for i from 0 below (length sequence) by n do
                      (apply function (loop for j from 0 below n collect (aref sequence (+ i j))))))
               ((listp sequence)
                (let ((q sequence))
                  (loop until (null q) do
                        (apply function (loop for j from 0 below n collect (pop q))))))))
        (t
         ;; otherwise, we (for now) take the easy route of calling COERCE
         (coerce
          (cond ((vectorp sequence)
                 (loop for i from 0 below (length sequence) by n
                       nconc (multiple-value-list
                                 (apply function (loop for j from 0 below n collect (aref sequence (+ i j)))))))
                ((listp sequence)
                 (let ((q sequence))
                   (loop until (null q) nconc
                         (multiple-value-list
                             (apply function (loop for j from 0 below n collect (pop q))))))))
          result-type))))

;;; A different way of attacking iteration of sequences
(defmacro do-sequence ((vars sequence &optional result-form) &body body)
  "Iterate over SEQUENCE.  VARS is a list of symbols (or a single
symbol).  At each iteration the variables in VARS are bound to the
initial elements of the sequence.  The iteration is then \"stepped\"
by the number of variables in VARS."
  (flet ((list-accessor (n)
	   (case n
	     (0 'car)
	     (1 'cadr)
	     (2 'caddr)
	     (3 'cadddr)
	     (t `(lambda (list) (nth ,n list)))))
	 (list-stepper (n)
	   (case n
	     (1 'cdr)
	     (2 'cddr)
	     (3 'cdddr)
	     (4 'cddddr)
	     (t `(lambda (list) (nthcdr ,n list))))))
    (when (not (listp vars))
      (setq vars (list vars)))
    (let* ((body-fun (gensym "BODY-FUN"))
	   (var-length (length vars))
	   (seq-var (gensym "SEQ-VAR"))
	   (tail-var (gensym "TAIL-VAR"))
	   (i (gensym "I"))
	   (list-args (loop for j from 0 below var-length
			    collect `(,(list-accessor j) ,tail-var)))
	   (vector-args (loop for j from 0 below var-length
			      collect `(aref ,seq-var (+ ,i ,j)))))
      `(block nil
	 (flet ((,body-fun ,vars
		  (tagbody
		     ,@body)))
	   (let ((,seq-var ,sequence))
	     (etypecase ,seq-var
	       (list
		(loop for ,tail-var on ,seq-var by #',(list-stepper var-length)
		      do (,body-fun ,@list-args)))
	       (vector
		(loop for ,i from 0 below (length ,seq-var) by ,var-length
		      do (,body-fun ,@vector-args))))))
	 ,@(when result-form
	     `((let ,vars		;Bind variables to nil
		 (declare (ignorable ,vars))
		 ,result-form)))))))

(defun clamp (value min max)
  "Clamps the value 'value' into the range [min,max]."
  (max min (min max value)))
  
;;;;
;;;; Protocol Classes
;;;;

(defmacro define-protocol-class (name super-classes &optional slots &rest options)
  (let ((protocol-predicate
         (intern (concatenate 'string (symbol-name name) (if (find #\- (symbol-name name)) "-" "") "P")))
	(predicate-docstring
	 (concatenate 'string "Protocol predicate checking for class " (symbol-name name))))
    `(progn
       (defclass ,name ,super-classes ,slots ,@options)

       (let ((the-class (find-class ',name)))
	 (setf (documentation the-class 'type) "CLIM protocol class")
         (defmethod initialize-instance :after ((object ,name) &key &allow-other-keys)
           (when (eq (class-of object) the-class)
             (error "~S is a protocol class and thus can't be instantiated" ',name))))

       (defgeneric ,protocol-predicate (object)
	 (:method ((object t))
	   nil)
	 (:method ((object ,name))
	   t)
	 (:documentation ,predicate-docstring))

       ',name)))

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

;;; Convenience macros

(define-modify-macro maxf (&rest args) max)
(define-modify-macro minf (&rest args) min)
(define-modify-macro nconcf (&rest args) nconc)
(define-modify-macro orf (&rest args) or)


;;; Move this early so it can be used in presentations.lisp, which
;;; comes before commands.lisp.

(defmacro do-command-table-inheritance ((command-table-var command-table)
					&body body)
  `(apply-with-command-table-inheritance
    #'(lambda (,command-table-var)
	,@body)
    (find-command-table ,command-table)))

;;;

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (symbol) `(,symbol (gensym ,(symbol-name symbol))))
                 syms)
     ,@ body))

(defun parse-method (description)
  (loop
     for (qualifier-or-ll . body) on description
     until (listp qualifier-or-ll)
     collect qualifier-or-ll into qualifiers
     finally (return
               (values qualifiers
                       (clim-mop:extract-specializer-names qualifier-or-ll)
                       (clim-mop:extract-lambda-list qualifier-or-ll)
                       body))))

(defun decode-specializer (specializer-name)
  (if (atom specializer-name)
      (find-class specializer-name)
      (clim-mop:intern-eql-specializer (second specializer-name))))

(defmacro with-method ((name &rest description) &body body)
  "Executes BODY installing the specified method on the generic
  function named NAME."
  (multiple-value-bind (qualifiers specializers)
      (parse-method description)
    (with-gensyms (old-method decoded-specializers new-method)
      `(let* ((,decoded-specializers
               (mapcar #'decode-specializer ',specializers))
              (,old-method (find-method #',name
                                        ',qualifiers
                                        ,decoded-specializers
                                        nil))
              (,new-method
               (defmethod ,name ,@description)))
         (unwind-protect
              (locally ,@body)
           (remove-method #',name ,new-method)
           (when ,old-method (add-method #',name ,old-method)))))))

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

;;;
(declaim (inline maybe-funcall maybe-apply))

(defun maybe-funcall (function &rest args)
  "If FUNCTION is not NIL, funcall it."
  (when function (apply function args)))

(defun maybe-apply (function &rest args)
  "If FUNCTION is not NIL, apply it."
  (when function (apply #'apply function args)))

;;; Remove keyword pairs from an argument list, consing as little as possible

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
  "binds NEW-VAR (defaults to VAR) to VAR with the keyword arguments specified
in KEYWORDS removed."
  `(let ((,new-var (remove-keywords ,var ',keywords)))
     ,@body))


(defun symbol-concat (&rest symbols)
  "Actually this function raises the next question: what is *PACKAGE* supposed to be?
   The correct answer: listen to the elders and don't use this function or any variant
   of it -- Don't construct symbols, instead let the user specify them."
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))

(defun stream-designator-symbol (symbol)
  "Maps T to *standard-output*, barfs if argument does not look good.
   To be used in the various WITH-... macros."
  (cond ((eq symbol 't)
         '*standard-output*)
        ((symbolp symbol)
         symbol)
        (t
         (error "~S does not look too good as a symbol."))))
