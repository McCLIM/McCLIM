;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; `inspector-view'
;;;
;;; Basically just something that is distinguishable from
;;; `textual-view'.

(defclass inspector-view (textual-view)
  ())

;;; Place presentations
;;;
;;; These visually represent the in which an inspected object
;;; resides. Also indicates whether the place can be changed (by
;;; assigning a different value or removing the value).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type place ()))

(define-presentation-method present :around ((object basic-place)
                                             (type   place)
                                             (stream t)
                                             (view   inspector-view)
                                             &key)
  ;; Present the place with the "changable" style if its value can be
  ;; changed or removed.
  (if (or (supportsp object 'setf) (supportsp object 'remove-value))
      (with-style (stream :changable) (call-next-method))
      (call-next-method)))

(macrolet
    ((define (place-class indicator)
       `(define-presentation-method present ((object ,place-class)
                                             (type   place)
                                             (stream t)
                                             (view   inspector-view)
                                             &key)
          (write-char ,indicator stream))))
  (define basic-place            #\→)
  (define sequence-element-place #\⁃)
  (define key-place              #\•)
  (define value-place            #\→))

;;; `inspected-object'

(defclass inspected-object ()
  ((%place :initarg  :place
           :reader   place)
   (%style :initarg  :style
           :accessor style
           :initform :collapsed)))

(defmethod object ((object inspected-object))
  (value (place object)))

(defmethod state-applicable-p ((state t) (object t) (place t))
  (eq (class-name (class-of state)) (object-state-class object place))) ; TODO compare the metaobjects?

(defmethod object-state-class ((object t) (place t))
  'inspected-object)

(defmethod make-object-state ((object t) (place t))
  (let ((class (object-state-class object place)))
    (make-instance class :place place)))
