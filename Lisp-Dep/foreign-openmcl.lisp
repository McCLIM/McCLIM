;;; -*- Mode: Lisp; Package: CLIM-FFI -*-

;;;  (c) copyright 2003 by Tim Moore (moore@bricoworks.com)
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

;;; Macros used to stack allocate memory, etc. when communicating
;;; with foreign code i.e., OpenGL.

(in-package :clim-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun map-tree (func tree)
  (let ((car-result (if (atom (car tree))
			(funcall func (car tree))
			(map-tree func (car tree))))
	(tail-result (if (null (cdr tree))
			 nil
			 (map-tree func (cdr tree)))))
    (cons car-result tail-result)))

(defun translate-typespec (typespec)
  (flet ((make-keyword (s)
	   (if (and s (symbolp s))
	       (intern (symbol-name s) :keyword)
	       s)))
    (if (atom typespec)
	(make-keyword typespec)
	(map-tree #'make-keyword typespec))))

)


(defmacro with-c-data ((bindings) &body body)
  (let ((rlet-bindings nil))
    (loop
       for (var typespec . initforms) in bindings
       for mcl-typespec = (translate-typespec typespec)
       collect `(var mcl-typespec ,@initforms) into new-bindings
       finally (setq rlet-bindings new-bindings))
    `(ccl:rlet ,rlet-bindings
       ,@body)))

(defmacro with-c-strings ((bindings) &body body)
  `(ccl:with-cstrs ,bindings ,@body))

(defmacro null-pointer ()
  `(ccl:%null-ptr))

(defmacro cref (pointer type &optional index)
  "Dereference the foreign POINTER of TYPE. Arrays can be accessed via INDEX."
  (let ((typespec (translate-typespec type)))
    (if (null index)
	`(ccl:pref ,pointer ,typespec)
	(let ((offset `(* ,index
			  ,(ccl::%foreign-type-or-record-size typespec :bits))))
	  (destructuring-bind (type-name &rest accessors)
	      (ccl::decompose-record-accessor typespec)
	    (ccl::%foreign-access-form pointer
				       (ccl::%foreign-type-or-record type-name)
				       offset
				       accessors))))))
