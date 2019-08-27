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

;;; `slot-place'

(defclass slot-place (key-value-place)
  ())

(defclass immutable-slot-place (slot-place)
  ())

(defmethod supportsp ((place immutable-slot-place) (operator (eql 'setf)))
  nil)

(defclass mutable-slot-place (slot-place)
  ())

(defmethod supportsp ((place    mutable-slot-place)
                      (operator (eql 'remove-value)))
  t)

(flet ((slot-name (place)
         (c2mop:slot-definition-name (cell place))))

  (defmethod accepts-value-p ((place mutable-slot-place) (value t))
    (let* ((slot (cell place))
           (type (c2mop:slot-definition-type slot)))
      (typep value type)))

  (defmethod valuep ((place slot-place))
    (let ((container (container place))
          (name      (slot-name place)))
      (and (slot-exists-p container name)
           (slot-boundp container name))))

  (defmethod value ((place slot-place))
    (slot-value (container place) (slot-name place)))

  (defmethod (setf value) ((new-value t) (place mutable-slot-place))
    (setf (slot-value (container place) (slot-name place)) new-value))

  (defmethod remove-value ((place mutable-slot-place))
    (slot-makunbound (container place) (slot-name place))))

;;; `class-of-place'

(defclass class-of-place (pseudo-place)
  ())

(defmethod make-object-state ((object t)
                              (place  class-of-place))
  (make-instance (object-state-class object place) :place place
                                                   :style :name-only))

(defun inspect-class-as-name (class stream)
  ;; This presents the name of CLASS as a collapsed inspectable object
  ;; that expands into CLASS.
  (formatting-place (nil 'class-of-place class nil inspect)
    (inspect stream)))

;;; Object states

(defclass inspected-instance (inspected-identity-object-mixin
                              inspected-object)
  ((%slot-style :initarg  :slot-style
                :accessor slot-style
                :initform :by-class)))

(defmethod object-state-class ((object standard-object) (place t))
  'inspected-instance)

(defmethod object-state-class ((object structure-object) (place t))
  'inspected-instance)

(defmethod object-state-class ((object condition) (place t))
  'inspected-instance)

;;; Object inspection methods

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-instance)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream))

(defun inspect-slot (slot object stream &key (place-class 'mutable-slot-place))
  (formatting-place (object place-class slot present inspect :place-var place)
    (let ((name       (c2mop:slot-definition-name slot))
          (allocation (c2mop:slot-definition-allocation slot)))
      (formatting-row (stream)
        (formatting-cell (stream :align-y :center) ; TODO must be able to inspect slot
          (with-style (stream :slot-like)
            (write-string (symbol-name name) stream))
          (when (not (eq allocation :instance))
            (write-char #\Space stream)
            (badge stream "~A-allocated" allocation)))
        (formatting-cell (stream :align-x :center :align-y :center)
          (present stream))
        (formatting-cell (stream :align-y :center)
          (with-error-handling (stream "Error accessing slot")
            (inspect stream)))))))

(defmethod inspect-slots ((object t)
                          (style  (eql nil))
                          (stream t)))

(defmethod inspect-slots ((object t)
                          (style  (eql :flat))
                          (stream t))
  (let* ((class (class-of object))
         (slots (c2mop:class-slots class)))
    (with-placeholder-if-emtpy (stream)
      ((null slots)
       "no slots")
      (t
       (formatting-table (stream)
         (map nil (rcurry #'inspect-slot object stream) slots))))))

(defmethod inspect-slots ((object t)
                          (style  (eql :by-class))
                          (stream t))
  (let ((class (class-of object))
        (slots (make-hash-table :test #'eq)))
    (loop :for super :in (c2mop:class-precedence-list class)
          :for super-slots = (loop :for slot :in (c2mop:class-direct-slots super)
                                   :unless (gethash slot slots)
                                   :do (setf (gethash slot slots) t)
                                   :and :collect slot)
          :when super-slots
          :do (with-section (stream)
                  (with-drawing-options (stream :text-size :smaller)
                    (cond ((eq super class)
                           (write-string "Direct slots" stream))
                          (t
                           (write-string "Inherited from " stream)
                           (inspect-class-as-name super stream))))
                (formatting-table (stream)
                  (map nil (rcurry #'inspect-slot object stream) super-slots))))
    (when (zerop (hash-table-count slots))
      (with-style (stream :unbound)
        (write-string "no slots" stream)))))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-instance)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;; TODO first line of documentation string?
  (inspect-slots object (slot-style state) stream))

;;; Commands

(define-command (com-slot-style-flat :command-table inspector-command-table
                                     :name          "Slots Flat")
    ((object 'inspected-instance))
  (setf (slot-style object) :flat))

(define-presentation-to-command-translator inspected-instance->com-slot-style-flat
    (inspected-instance com-slot-style-flat inspector-command-table
     :tester ((object)
              (and (eq (style object) :expanded)
                   (not (eq (slot-style object) :flat))))
     :priority -1
     :documentation "Flat list of slots"
     :pointer-documentation ((object stream)
                             (with-print-error-handling (stream)
                               (with-safe-and-terse-printing (stream)
                                 (format stream "~@<Present slots of ~
                                                 ~A as a flat ~
                                                 list.~@:>"
                                         (object object))))))
    (object)
  (list object))

(define-command (com-slot-style-by-class :command-table inspector-command-table
                                         :name          "Slots by Class")
    ((object 'inspected-instance))
  (setf (slot-style object) :by-class))

(define-presentation-to-command-translator inspected-instance->com-slot-style-by-class
    (inspected-instance com-slot-style-by-class inspector-command-table
     :tester ((object)
              (and (eq (style object) :expanded)
                   (not (eq (slot-style object) :by-class))))
     :priority -1
     :documentation "Organize slots by class"
     :pointer-documentation ((object stream)
                             (with-print-error-handling (stream)
                               (with-safe-and-terse-printing (stream)
                                 (format stream "~@<Present slots of ~
                                                 ~A organized by ~
                                                 superclass.~@:>"
                                         (object object))))))
    (object)
  (list object))

(define-command (com-change-class :command-table inspector-command-table
                                  :name          "Change class")
    ((object    'inspected-instance)
     (new-class '(or symbol class inspected-class) :prompt "new class"))
  (with-command-error-handling
      ("Could not change class of ~A to ~A" object new-class)
      (let ((new-class (typecase new-class
                         (class  new-class)
                         (symbol (find-class new-class))
                         (t      (object new-class)))))
        (change-class (object object) new-class))))

(define-presentation-to-command-translator inspected-instance->com-change-class
    (inspected-instance com-change-class inspector-command-table
     :priority -1
     :documentation "Change the class of the object")
    (object)
  (list object (accept '(or symbol class inspected-class) :prompt "new class")))
