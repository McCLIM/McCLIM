;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by
;;;           Tim Moore (moore@bricoworks.com)

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

(defun setf-name-p (name)
  (and (listp name) (eq (car name) 'setf)))

;;; Many implementations complain if a defsetf definition and a setf function
;;; exist for the same place. Time to stop fighting that...

(defun make-setf*-gfn-name (function-name)
  (let* ((name-sym (cadr function-name)))
    `(setf ,(intern (format nil ".~A-~A."
			    (symbol-name name-sym)
			    (symbol-name '#:star))
		    (symbol-package name-sym)))))

(defun make-arglist (args)
  "Convert the list ARGS to a list of arguments in the form (var
init-form supplied-p-parameter)."
  (mapcar (lambda (a) `(,a nil ,(gensym))) args))

(defmacro defgeneric* (fun-name lambda-list &body options)
  "Defines a SETF* generic function.  FUN-NAME is a SETF function
name.  The last required argument is the single argument to the function in a
SETF place form; the other arguments are values collected from the
SETF new value form."
  (unless (setf-name-p fun-name)
    (error "~S is not a valid name for a SETF* generic function." fun-name))
  (let ((setf-name (cadr fun-name))
        (gf (make-setf*-gfn-name fun-name)))
    (multiple-value-bind (required optional rest keys allow-other-keys aux keyp)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (when aux
        (error "The use of &aux is not allowed in generic function lambda list."))
      (let ((optional (make-arglist (mapcar #'car optional)))
            (keys (make-arglist (mapcar #'cadar keys))))
        `(progn
           (defsetf ,setf-name
               (,(car (last required))
                ,@(and optional `(&optional ,@optional))
                ,@(and rest `(&rest ,rest))
                ,@(and keyp `(&key ,@keys))
                ,@(and allow-other-keys `(&allow-other-keys)))
               ,(butlast required)
             `(funcall #',',gf ,,@required
                ,,@(mapcar (lambda (x) `(and ,(third x) ,(car x))) optional)
                ,@,@(mapcar (lambda (x) `(and ,(third x) `(,,(make-keyword (car x)) ,,(car x)))) keys)))
           (defgeneric ,gf ,lambda-list ,@options))))))

(defmacro defmethod* (name lambda-list &body body)
  "Defines a SETF* method.  NAME is a SETF function name.  Otherwise,
like DEFMETHOD except there must exist a corresponding DEFGENERIC* form."
  (unless (setf-name-p name)
    (error "~S is not a valid name for a SETF* generic function." name))
  `(defmethod ,(make-setf*-gfn-name name) ,lambda-list ,@body))

