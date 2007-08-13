;;; -*- Mode: Lisp; Package: ESA-UTILS -*-

;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Miscellaneous utilities used in Climacs.

(in-package :esa-utils)

;;; Cribbed from Paul Graham
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

;;; Cribbed from PCL by Seibel
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

(defun unlisted (obj &optional (fn #'first))
  (if (listp obj)
      (funcall fn obj)
      obj))

(defun fully-unlisted (obj &optional (fn #'first))
  (if (listp obj)
      (fully-unlisted (funcall fn obj))
      obj))

(defun listed (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun list-aref (list &rest subscripts)
  (if subscripts
      (apply #'list-aref (nth (first subscripts) list)
             (rest subscripts))
      list))

;;; Cribbed from McCLIM.
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

(defun invoke-with-dynamic-bindings-1 (bindings continuation)
  (let ((old-values (mapcar #'(lambda (elt)
                                (symbol-value (first elt)))
                            bindings)))
    (unwind-protect (progn
                      (mapcar #'(lambda (elt)
                                  (setf (symbol-value (first elt))
                                        (funcall (second elt))))
                              bindings)
                      (funcall continuation))
      (mapcar #'(lambda (elt value)
                  (setf (symbol-value (first elt))
                        value))
              bindings old-values))))

(defmacro invoke-with-dynamic-bindings ((&rest bindings) &body body)
  `(invoke-with-dynamic-bindings-1
    ,(loop for (symbol expression) in bindings
        collect (list `',symbol
                      `#'(lambda ()
                           ,expression)))
    #'(lambda ()
        ,@body)))

;;; XXX This is currently broken with respect to declarations

(defmacro letf* ((&rest forms) &body body)
  (if (null forms)
      `(locally
	 ,@body)
      `(letf (,(car forms))
	 (letf* (,(cdr forms))
	   ,@body))))

(defun display-string (string)
  (with-output-to-string (result)
    (loop for char across string
	  do (cond ((graphic-char-p char) (princ char result))
		((char= char #\Space) (princ char result))
		(t (prin1 char result))))))

(defun object-equal (x y)
  "Case insensitive equality that doesn't require characters"
  (if (characterp x)
      (and (characterp y) (char-equal x y))
      (eql x y)))

(defun object= (x y)
  "Case sensitive equality that doesn't require characters"
  (if (characterp x)
      (and (characterp y) (char= x y))
      (eql x y)))

(defun no-upper-p (string)
  "Does STRING contain no uppercase characters"
  (notany #'upper-case-p string))

(defun case-relevant-test (string)
  "Returns a test function based on the search-string STRING.
If STRING contains no uppercase characters the test is case-insensitive,
otherwise it is case-sensitive."
  (if (no-upper-p string)
      #'object-equal
      #'object=))

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

(defun maptree (fn x)
  "This auxiliary function is like MAPCAR but has two extra
purposes: (1) it handles dotted lists; (2) it tries to make the
result share with the argument x as much as possible."
  (if (atom x) 
      (funcall fn x) 
      (let ((a (funcall fn (car x))) 
            (d (maptree fn (cdr x)))) 
        (if (and (eql a (car x)) (eql d (cdr x))) 
            x 
            (cons a d)))))

(defun subtype-compatible-p (types)
  "Return true if an element of `types' is a subtype of every
other type specifier in `types'. `Types' must be a list of type
specifiers."
  (some (lambda (x)
          (subtypep x `(and ,@types))) types))
