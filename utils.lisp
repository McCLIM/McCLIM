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
