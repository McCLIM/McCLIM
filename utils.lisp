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

(in-package :CLIM-INTERNALS)

(defun get-environment-variable (string)
  #+excl (sys:getenv string)
  #+cmu (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (sys::getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #-(or excl cmu clisp sbcl) (error "GET-ENVIRONMENT-VARIABLE not implemented"))

(defun last1 (list)
  (first (last list)))

(defun 2+ (x)
  (+ x 2))

(defun 2- (x)
  (- x 2))


(defun check-letf-form (form)
  (assert (and (listp form)
               (= 2 (length form)))))

(shadow 'letf)
(defmacro letf ((&rest forms) &body body &environment env)
  "LETF ({(Place Value)}*) Declaration* Form* During evaluation of the
Forms, SETF the Places to the result of evaluating the Value forms.
The places are SETF-ed in parallel after all of the Values are
evaluated."
  (mapc #'check-letf-form forms)
  (let* (init-let-form save-old-values-let-form
         new-values-set-form old-values-set-form
         update-form)
    (loop for (place new-value) in forms
          for (vars vals store-vars writer-form reader-form)
              = (multiple-value-list (get-setf-expansion place env))
          for (store-var) = store-vars
          for old-value-name = (gensym)
          nconc (mapcar #'list vars vals) into temp-init-let-form
	  collect store-var into temp-save-old-values-let-form
          collect (list old-value-name reader-form) into temp-save-old-values-let-form
          nconc (list store-var new-value) into temp-new-values-set-form
          nconc (list store-var old-value-name) into temp-old-values-set-form
          collect writer-form into temp-update-form
          finally (setq init-let-form temp-init-let-form
                        save-old-values-let-form temp-save-old-values-let-form
                        new-values-set-form temp-new-values-set-form
                        old-values-set-form temp-old-values-set-form
                        update-form (cons 'progn temp-update-form)))
    `(let* ,init-let-form
       (let ,save-old-values-let-form
         (unwind-protect
             (progn (setq ,@new-values-set-form)
                    ,update-form
                    (progn ,@body))
           (setq ,@old-values-set-form)
           ,update-form)))))

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

(defun clamp (value min max)
  "Clamps the value 'value' into the range [min,max]."
  (max min (min max value)))
  
;;;;
;;;; Protocol Classes
;;;;

(defmacro define-protocol-class (name super-classes &optional slots &rest options)
  (let ((protocol-predicate
         (intern (concatenate 'string (symbol-name name) (if (find #\- (symbol-name name)) "-" "") "P"))))
    `(progn
       (defclass ,name ,super-classes ,slots ,@options)

       (let ((the-class (find-class ',name)))
         (defmethod initialize-instance :after ((object ,name) &key &allow-other-keys)
           (when (eq (class-of object) the-class)
             (error "You are a fool; Since ~S is a protocol class, it is not instantiable."
                    ',name))))
     
       (defmethod ,protocol-predicate ((object t))
         nil)
     
       (defmethod ,protocol-predicate ((object ,name))
         t)

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

;;; Move this early so it can be used in presentations.lisp, which
;;; comes before commands.lisp.

(defmacro do-command-table-inheritance ((command-table-var command-table) &body body)
  `(apply-with-command-table-inheritance
    #'(lambda (,command-table-var)
	,@body)
    (find-command-table ,command-table)))
