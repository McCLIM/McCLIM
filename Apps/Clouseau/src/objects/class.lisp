;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2022 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Places, inspection methods and commands for classes.
;;;

(cl:in-package #:clouseau)

;;; TODO
;;; name = (setf (find-class ) nil) (setf (find-class new-name) …)
;;; direct methods
;;; add/remove slots


;;;; Place classes

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

(defmethod make-object-state ((object t) (place slot-definition-place))
  (make-instance (object-state-class object place)
                 :place         place
                 :context-class (container place)
                 :style         :name-only))

;;; `class-list-place'
;;;
;;; Places of this kind contain a list of classes among which a
;;; certain relation such as "subclass" or "superclass" is of
;;; interest. As a result, such lists can be shown as a list or as the
;;; graph induced by the relation.

(defclass class-list-place (deep-read-only-place)
  ())

(macrolet
    ((def (name reader
           &key (relation reader)
                (default-class-list-style :graph))
       `(progn
          (defclass ,name (class-list-place)
            ((%relation                 :allocation :class
                                        :reader     relation
                                        :initform   #',relation)
             (%default-class-list-style :allocation :class
                                        :reader     default-class-list-style
                                        :initform   ',default-class-list-style)))

          (defmethod value ((place ,name))
            (,reader (container place))))))
  (def subclass-list-place         c2mop:class-direct-subclasses)
  (def superclass-list-place       c2mop:class-direct-superclasses)
  (def class-precedence-list-place c2mop:class-precedence-list
       :relation                 c2mop:class-direct-superclasses
       :default-class-list-style list))

(defmethod valuep ((place class-precedence-list-place))
  (c2mop:class-finalized-p (container place)))

;;; `prototype-place'

(defclass prototype-place (read-only-place)
  ())

(defmethod value ((place prototype-place))
  (c2mop:class-prototype (container place)))

(defmethod valuep ((place prototype-place))
  (c2mop:class-finalized-p (container place)))


;;;; Object states

;;; `inspected-slot-definition'

(defclass inspected-slot-definition (remembered-collapsed-style-mixin
                                     context-class-mixin
                                     inspected-instance)
  ())

(defmethod object-state-class ((object c2mop:slot-definition) (place t))
  'inspected-slot-definition)

;;; `inspected-class-list'

(defclass inspected-class-list (inspected-proper-list)
  ((%class-list-style :type     (member list :graph)
                      :accessor class-list-style
                      :initform :graph)))

(defmethod initialize-instance :after
    ((instance inspected-class-list)
     &key place
          (class-list-style (default-class-list-style place)))
  (setf (class-list-style instance) class-list-style))

;;; See comment about two methods for `package-nicknames-place'.
(defmethod object-state-class ((object null) (place class-list-place))
  'inspected-class-list)

(defmethod object-state-class ((object cons) (place class-list-place))
  'inspected-class-list)

;;; `inspected-class'

(defclass inspected-class (remembered-collapsed-style-mixin
                           context-package-mixin
                           inspected-instance)
  ()
  (:default-initargs
   :slot-style nil))

(defmethod object-state-class ((object class) (place t))
  'inspected-class)


;;;; Object inspection methods

;;; `inspected-slot-definition'

(defmethod inspect-object-using-state ((object c2mop:slot-definition)
                                       (state  inspected-slot-definition)
                                       (style  (eql :name-only))
                                       (stream t))
  (let ((slot-name (c2mop:slot-definition-name object)))
    (print-symbol-in-context slot-name (context-package state) stream)))

;;; `inspected-class-list'

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-class-list)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (case (class-list-style state)
    (list
     (call-next-method))
    (:graph
     (inspect-object-using-state object state :inheritance-graph stream))))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-class-list)
                                       (style  (eql :inheritance-graph))
                                       (stream t))
  ;; Present a graph that shows CLASS in relation to the classes in
  ;; the list OBJECT and other related classes.
  (let ((class (container (place state))))
    (format-graph-from-roots
     (list* class object)
     (lambda (other-class stream)
       (cond ((eq other-class class)
              (with-style (stream :header)
                (inspect-class-as-name other-class stream :context-object object)))
             ((find other-class object :test #'eq)
              (inspect-class-as-name other-class stream :context-object object))
             (t
              (with-style (stream :unbound)
                (inspect-class-as-name other-class stream :context-object object)))))
     (relation (place state))
     :stream stream :orientation :vertical
     :graph-type :dag :merge-duplicates t :maximize-generations t)))

;;; `class'

(defun safe-finalized-p (class)
  ;; This may be called, for example, on the prototype instance of
  ;; CLASS in which all slots are unbound.
  (ignore-errors (c2mop:class-finalized-p class)))

(defun anonymous-class-p (class)
  (let ((name (class-name class)))
    (values (not name) (not (eq (find-class name nil) class)))))

(defun print-class-name (object stream &key context-package)
  (multiple-value-bind (no-name-p not-global-p) (anonymous-class-p object)
    (cond (no-name-p
           (badge stream "anonymous"))
          (t
           (let ((name (class-name object)))
             (if context-package
                 (print-symbol-in-context name context-package stream)
                 (prin1 name stream)))
           (when not-global-p
             (write-char #\Space stream)
             (badge stream "no global name"))))))

(defmethod inspect-object-using-state ((object class)
                                       (state  inspected-class)
                                       (style  (eql :name-only))
                                       (stream t))
  (print-class-name object stream :context-package (context-package state)))

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

(defun inspect-effective-slot (class slot stream)
  (let* ((name (c2mop:slot-definition-name slot))
         (context-package (symbol-package (class-name class)))
         (contributing (ensure-gethash
                        (cons class name) *hack-cache*
                        (loop :for super :in (c2mop:class-precedence-list class)
                              :for super-slot = (find name (c2mop:class-direct-slots super)
                                                      :key #'c2mop:slot-definition-name)
                              :when super-slot :collect (cons super super-slot)))))
    (formatting-row (stream)
      (formatting-cell (stream)
        (print-symbol-in-context name context-package stream)
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
              :for (superclass . slot) :in contributing
              :unless firstp :do (write-string ", " stream)
              :do (formatting-place
                      (superclass 'slot-definition-place slot nil present-object)
                    (present-object stream))
                  (write-string " in " stream)
                  (inspect-class-as-name superclass stream :context-object class)))
      (formatting-cell (stream)
        (formatting-place
            (class 'slot-definition-place slot present-place present-object)
          (present-place stream)
          (present-object stream))))))

(defun inspect-effective-slot-list (object slots stream)
  (formatting-table (stream)
    (formatting-header (stream) "Name" "Allocation" "Type" "Initargs"
                                "Readers" "Writers" "Initform"
                                "Computed from direct slots")
    (map nil (lambda (slot)
               (inspect-effective-slot object slot stream))
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
          (format-place-cells stream object 'superclass-list-place nil
                              :label "Superclasses")
          (format-place-cells stream object 'subclass-list-place nil
                              :label "Subclasses"))
        (formatting-row (stream)
          (format-place-cells stream object 'class-precedence-list-place nil
                              :label "Precedence List")
          (format-place-cells stream object 'prototype-place nil
                              :label "Prototype"))))

    (print-documentation object stream)

    (with-section (stream) "Initargs"
      (let (initargs)
        (with-placeholder-if-empty (stream)
          ((not finalizedp)
           "Not finalized - initargs not available~%")
          ((not (setf initargs (c2mop:class-default-initargs object)))
           "No initargs~%")
          (t
           (with-drawing-options (stream :text-size :smaller)
             (inspect-initargs initargs stream))))))

    (with-section (stream) "Effective slots"
      (let (slots)
        (with-placeholder-if-empty (stream)
          ((not finalizedp)
           "Not finalized - effective slots not available~%")
          ((not (setf slots (c2mop:class-slots object)))
           "No slots~%")
          (t
           (without-noting-object-occurrences ()
             (with-drawing-options (stream :text-size :smaller)
               (inspect-effective-slot-list object slots stream))))))))

  (with-section (stream) "Specializer usage"
    (let ((methods (c2mop:specializer-direct-methods object)))
      (with-placeholder-if-empty (stream)
        ((not methods)
         "Not used as a specializer~%")
        (t
         (without-noting-object-occurrences ()
           (with-drawing-options (stream :text-size :smaller)
             (inspect-method-list nil methods stream
                                  :generic-function-name t)))))))

  (call-next-method))


;;;; Commands

;;; Class lists

(define-command (com-class-list-as-graph :command-table inspector-command-table
                                         :name          t)
    ((object 'inspected-class-list
             :gesture (:select
                       :priority -1
                       :tester   ((object)
                                  (not (eq (class-list-style object)
                                           :graph)))
                       :documentation "Show class list as graph")))
  (setf (class-list-style object) :graph))

(define-command (com-class-list-as-list :command-table inspector-command-table
                                        :name          t)
    ((object 'inspected-class-list
             :gesture (:select
                       :priority -1
                       :tester   ((object)
                                  (not (eq (class-list-style object)
                                           'list)))
                       :documentation "Show class list as list")))
  (setf (class-list-style object) 'list))

;;; Finalization

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
                             (with-print-error-handling (stream)
                               (with-safe-and-terse-printing (stream)
                                 (format stream "~@<Finalize ~A~@:>"
                                         (object object))))))
    (object)
  (list object))
