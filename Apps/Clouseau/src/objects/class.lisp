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

;;; TODO
;;; name = (setf (find-class ) nil) (setf (find-class new-name) …)
;;; direct methods
;;; add/remove slots

;;; Place classes

;;; `slot-definition-place'

(defclass slot-definition-place (read-only-place)
  ())

(defmethod supportsp ((place     slot-definition-place)
                      (operation (eql 'remove-value)))
  t)

(defmethod value ((place slot-definition-place))
  (cell place))

(defmethod remove-value ((place slot-definition-place))
  (error "not implemented"))

;;; `class-precedence-list-place'

(defclass class-precedence-list-place (read-only-place)
  ())

(defmethod value ((place class-precedence-list-place))
  (c2mop:class-precedence-list (container place)))

;;; Object states

(defclass inspected-slot-definition (inspected-instance)
  ()
  (:default-initargs
   :style :name-only))

(defmethod object-state-class ((object c2mop:slot-definition)
                               (place  slot-definition-place))
  'inspected-slot-definition)

(defclass inspected-class-precedence-list (inspected-proper-list)
  ())

(defmethod object-state-class ((object cons)
                               (place  class-precedence-list-place))
  'inspected-class-precedence-list)

(defclass inspected-class (inspected-instance)
  ((%collapsed-style :initarg  :collapsed-style
                     :accessor collapsed-style))
  (:default-initargs
   :slot-style nil))

(defmethod initialize-instance :after  ; TODO mixin for collapsed-style
    ((instance inspected-class)
     &key
     (collapsed-style nil collapsed-style-supplied-p))
  (declare (ignore collapsed-style))
  (unless collapsed-style-supplied-p
    (setf (collapsed-style instance) (style instance))))

(defmethod (setf style) :around ((new-value (eql :collapsed))
                                 (object    inspected-class))
  (let ((collapsed-style (collapsed-style object)))
    (if (eq new-value collapsed-style)
        (call-next-method)
        (setf (style object) collapsed-style))))

(defmethod object-state-class ((object class) (place t))
  'inspected-class)

;;; Object inspection methods

;;; `slot-definition'

(defmethod inspect-object-using-state ((object c2mop:effective-slot-definition)
                                       (state  inspected-slot-definition)
                                       (style  (eql :name-only))
                                       (stream t))
  (prin1 (c2mop:slot-definition-name object) stream))

(defmethod inspect-object-using-state ((object c2mop:effective-slot-definition)
                                       (state  inspected-slot-definition)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (call-next-method))

;;; Class precedence list

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-class-precedence-list)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (format-graph-from-roots
   object
   (lambda (class stream)
     (inspect-class-as-name class stream))
   #'c2mop:class-direct-superclasses
   :stream stream :orientation :vertical
   :graph-type :dag :merge-duplicates t :maximize-generations t))

;;; `class'

(defun safe-finalized-p (class)
  ;; This may be called, for example, on the prototype instance of
  ;; CLASS in which all slots are unbound.
  (ignore-errors (c2mop:class-finalized-p class)))

(defun anonymous-class-p (class)
  (let ((name (class-name class)))
    (values (not name) (not (eq (find-class name nil) class)))))

(defun print-class-name (object stream)
  (multiple-value-bind (no-name-p not-global-p) (anonymous-class-p object)
    (cond (no-name-p
           (badge stream "anonymous"))
          (t
           (prin1 (class-name object) stream)
           (when not-global-p
             (write-char #\Space stream)
             (badge stream "no global name"))))))

(defmethod inspect-object-using-state ((object class)
                                       (state  inspected-class)
                                       (style  (eql :name-only))
                                       (stream t))
  (print-class-name object stream))

(defmethod inspect-object-using-state ((object class)
                                       (state  inspected-class)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (call-next-method)

  (write-char #\Space stream)
  (print-class-name object stream))

(defmethod inspect-object-using-state ((object class)
                                       (state  inspected-class)
                                       (style  (eql :badges))
                                       (stream t))
  (let ((metaclass (class-of object)))
    (write-char #\Space stream)
    (badge stream "~:[not ~;~]finalized" (safe-finalized-p object))

    (when (not (eq metaclass
                   (load-time-value (find-class 'standard-class))))
      (write-char #\Space stream)
      (badge stream "non-default metaclass"))))

(defun inspect-initargs (initargs stream)
  (formatting-table (stream)
    (formatting-header (stream) "Name" "Initform" "Initfunction")
    (map nil (lambda (initarg)
               (destructuring-bind (name initform initfunction) initarg
                 (formatting-row (stream)
                   (formatting-cell (stream)
                     (formatting-place (nil 'pseudo-place name nil inspect)
                       (inspect stream)))
                   (formatting-cell (stream)
                     (formatting-place (nil 'pseudo-place initform nil inspect)
                       (inspect stream)))
                   (formatting-cell (stream)
                     (formatting-place (nil 'pseudo-place initfunction nil inspect)
                       (inspect stream))))))
         initargs)))

(defvar *hack-cache* (make-hash-table :test #'equal))

(defun inspect-effective-slot-list (object slots stream)
  (formatting-table (stream)
    (formatting-header (stream) "Name" "Allocation" "Type" "Initargs"
                                "Readers" "Writers" "Initform"
                                "Computed from direct slots")
    (map nil (lambda (slot)
               (let* ((name (c2mop:slot-definition-name slot))
                      (contributing (ensure-gethash
                                     (cons object name) *hack-cache*
                                     (loop :for super :in (c2mop:class-precedence-list object)
                                           :for super-slot = (find name (c2mop:class-direct-slots super)
                                                                   :key #'c2mop:slot-definition-name)
                                           :when super-slot :collect (cons super super-slot)))))
                 (formatting-row (stream)
                   (formatting-cell (stream)
                     (prin1 name stream)
                     (unless (alexandria:length= 1 contributing)
                       (write-char #\Space stream)
                       (badge stream "overwritten")))
                   (formatting-cell (stream)
                     (princ (c2mop:slot-definition-allocation slot) stream))
                   (formatting-cell (stream)
                     (princ (c2mop:slot-definition-type slot) stream))
                   (formatting-cell (stream)
                     (when-let ((initargs (c2mop:slot-definition-initargs slot)))
                       (prin1 initargs stream)))
                   (formatting-cell (stream)
                     (when-let ((readers (mappend (compose #'c2mop:slot-definition-readers
                                                      #'cdr)
                                             contributing)))
                       (princ readers stream)))
                   (formatting-cell (stream)
                     (when-let ((writers (mappend (compose #'c2mop:slot-definition-writers
                                                      #'cdr)
                                             contributing)))
                       (princ writers stream)))
                   (formatting-cell (stream)
                     (princ (c2mop:slot-definition-initform slot) stream))
                   (formatting-cell (stream)
                     (loop :for firstp = t :then nil
                           :for (class . slot) :in contributing
                           :unless firstp :do (write-string ", " stream)
                           :do (formatting-place (object 'pseudo-place slot nil nil
                                                  :place-var place)
                                 (clim:with-output-as-presentation (stream place 'place)
                                   (princ (c2mop:slot-definition-name slot) stream))) ; TODO how to print the symbol?
                               (write-string " in " stream)
                               (inspect-class-as-name class stream)))
                   (formatting-cell (stream)
                     (formatting-place (object 'slot-definition-place slot present inspect)
                       (present stream)
                       (inspect stream))))))
         slots)))

(defmethod inspect-object-using-state ((object class)
                                       (state  inspected-class)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (let ((finalizedp (safe-finalized-p object)))
    (with-preserved-cursor-x (stream)
      (formatting-table (stream)
        (formatting-row (stream)
          (format-place-cells stream object 'reader-place 'class-name :label "Name")
          (format-place-cells stream object 'reader-place 'class-of :label "Metaclass"))
        (formatting-row (stream)
          (format-place-cells stream object 'reader-place 'c2mop:class-direct-superclasses
                              :label "Superclasses")
          (format-place-cells stream object 'reader-place 'c2mop:class-direct-subclasses
                              :label "Subclasses"))
        (when finalizedp ; TODO else display placeholders
          (formatting-row (stream)
            (format-place-cells stream object 'class-precedence-list-place nil
                                :label "Precedence List")
            (format-place-cells stream object 'reader-place 'c2mop:class-prototype
                                :label "Prototype")))))

    (print-documentation object stream)

    (with-section (stream) "Initargs"
      (let (initargs)
        (with-placeholder-if-emtpy (stream)
          ((not finalizedp)
           "Not finalized - initargs not available~%")
          ((not (setf initargs (c2mop:class-default-initargs object)))
           "No initargs~%")
          (t
           (with-drawing-options (stream :text-size :smaller)
             (inspect-initargs initargs stream))))))

    (with-section (stream) "Effective slots"
      (let (slots)
        (with-placeholder-if-emtpy (stream)
          ((not finalizedp)
           "Not finalized - effective slots not available~%")
          ((not (setf slots (c2mop:class-slots object)))
           "No slots~%")
          (t
           (with-drawing-options (stream :text-size :smaller)
             (inspect-effective-slot-list object slots stream)))))))

  (with-section (stream) "Specializer usage"
    (let ((methods (c2mop:specializer-direct-methods object)))
      (with-placeholder-if-emtpy (stream)
        ((not methods)
         "Not used as a specializer~%")
        (t
         (with-drawing-options (stream :text-size :smaller)
           (inspect-method-list nil methods stream
                                :generic-function-name t))))))

  (call-next-method))

;;; Commands

(define-command (com-finalize :command-table inspector-command-table
                              :name          "Finalize Class")
    ((object 'inspected-class))
  (let ((object (object object)))
    (with-command-error-handling ("Could not finalize ~A" object)
        (c2mop:finalize-inheritance object))))

(define-presentation-to-command-translator inspected-class->com-finalize
    (inspected-class com-finalize inspector-command-table
     :tester ((object) (not (safe-finalized-p (object object))))
     :priority -1
     :documentation "Finalize class"
     :pointer-documentation ((object stream)
                             (format stream "~@<Finalize ~A~@:>"
                                     (object object))))
    (object)
  (list object))
