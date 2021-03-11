;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Places, inspection methods and commands for symbols.

(cl:in-package #:clouseau)

;;; Utilities

(declaim (inline symbol-state))
(defun symbol-state (symbol package)
  (nth-value 1 (find-symbol (symbol-name symbol) package)))

(declaim (inline package-locked-p))
(defun package-locked-p (package)
  #+sbcl (sb-ext:package-locked-p package)
  #-sbcl nil)

;;; Places

;;; `symbol-slot-place'

(defclass symbol-slot-place (basic-place)
  ())

#+sbcl
(defmethod supportsp :around ((place symbol-slot-place) (operation t))
  (and (if-let ((package (symbol-package (container place))))
         (not (package-locked-p package))
         t)
       (call-next-method)))

(defmethod supportsp ((place symbol-slot-place) (operation (eql 'remove-value)))
  t)

;;; `symbol-value-place'
;;;
;;; TODO show type
;;; TODO check value against type

(defclass symbol-value-place (symbol-slot-place)
  ())

(defmethod supportsp ((place symbol-value-place) (operation t))
  (and (not (constantp (container place)))
       (call-next-method)))

(defmethod valuep ((place symbol-value-place))
  (boundp (container place)))

(defmethod value ((place symbol-value-place))
  (symbol-value (container place)))

(defmethod (setf value) ((new-value t) (place symbol-value-place))
  (setf (symbol-value (container place)) new-value))

(defmethod remove-value ((place symbol-value-place))
  (makunbound (container place)))

;;; `symbol-function-place'

(defclass symbol-function-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-function-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-function-place) (value function))
  t)

(defmethod valuep ((place symbol-function-place))
  (fboundp (container place)))

(defmethod value ((place symbol-function-place))
  (fdefinition (container place)))

(defmethod (setf value) ((new-value function) (place symbol-function-place))
  (setf (fdefinition (container place)) new-value))

(defmethod remove-value ((place symbol-function-place))
  (fmakunbound (container place)))

;;; `symbol-type-place'

(defclass symbol-type-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-type-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-type-place) (value class))
  t)

(defmethod valuep ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod value ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod (setf value) ((new-value class) (place symbol-type-place))
  (setf (find-class (container place) nil) new-value))

(defmethod remove-value ((place symbol-type-place))
  (setf (find-class (container place)) nil))

(defmethod make-object-state ((object class)
                              (place  symbol-type-place))
  (make-instance (object-state-class object place) :place place
                                                   :style :name-only))

;;; Object inspection methods

(defmethod inspect-object-using-state :after ((object symbol)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (write-char #\Space stream)
  (if-let ((package (symbol-package object)))
    (badge stream "~(~A~)" (symbol-state object package))
    (badge stream "uninterned"))

  #+sbcl (when-let ((kind (sb-cltl2:variable-information object)))
           (write-char #\Space stream)
           (badge stream "~(~A~)" kind)))

(defmethod inspect-object-using-state ((object symbol)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (formatting-row (stream)
      (format-place-cells stream object 'reader-place 'symbol-name
                          :label "Name")
      (format-place-cells stream object 'reader-place 'symbol-package ; TODO should be mutable
                          :label "Package"))
    (formatting-row (stream)
      (format-place-cells stream object 'symbol-value-place nil
                          :label "Value")
      (format-place-cells stream object 'symbol-function-place nil
                          :label "Function"))
    (formatting-row (stream)
      (format-place-cells stream object 'symbol-type-place nil
                          :label "Type"))))
