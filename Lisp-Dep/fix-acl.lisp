;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

;;; Needed to keep ACL from issuing warnings about toplevel (shadow ...) forms
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

(defpackage :clim-mop
  (:use :clos)
  (:import-from :common-lisp
		#:class-name)
  (:export #:class-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (sym :clos)
	(export sym :clim-mop)))

#+NIL
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar clim-lisp-patch::*inline-functions* nil))

  (defmacro clim-lisp-patch:declaim (&rest args)
    (dolist (arg args)
      (cond ((and (consp arg) (eq (car arg) 'inline))
             (dolist (k (cdr arg))
               (pushnew k clim-lisp-patch::*inline-functions*)))))
    `(declaim ,@args) )

  (defmacro clim-lisp-patch:defun (fun args &body body)
    (cond ((member fun clim-lisp-patch::*inline-functions*)
           (cond ((and (consp fun) (eq (car fun) 'setf))
                  (let ((fnam (intern (concatenate 'string
                                        "(SETF " (symbol-name (cadr fun)) ")")
                                      (symbol-package (cadr fun)))))
                    `(progn
                       (defsetf ,(cadr fun) (&rest ap) (new-value)
                         (list* ',fnam new-value ap))
                       (defun ,fnam ,args .,body)
                       (define-compiler-macro ,fnam (&rest .args.)
                         (cons '(lambda ,args .,body)
                               .args.)))))
                 (t
                  `(progn
                     (defun ,fun ,args .,body)
                     (define-compiler-macro ,fun (&rest .args.)
                       (cons '(lambda ,args .,body)
                             .args.))))))
          (t
           `(defun ,fun ,args ,@body)))) )

