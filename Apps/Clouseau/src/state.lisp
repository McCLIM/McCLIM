;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; State for one inspector instance (Basically the model in a model
;;; view controller architecture).
;;;

(cl:in-package #:clouseau)

;;; `inspector-state'
;;;
;;; The state is just the tree of inspected places and objects,
;;; starting at the root place.

(defvar *old-root-place* nil)

(defclass inspector-state ()
  ((%root-place    :accessor %root-place)
   ;; Change hook
   (%change-hook   :initarg  :change-hook
                   :type     list #|of function|#
                   :accessor change-hook
                   :initform '())
   ;; Mainly for debugging
   (%handle-errors :initarg  :handle-errors
                   :type     boolean
                   :accessor handle-errors
                   :initform t)))

(defmethod initialize-instance :after ((instance inspector-state)
                                       &key
                                       (root-object nil root-object-supplied-p))
  (declare (ignore root-object))
  (unless root-object-supplied-p
    (setf (root-place instance) (make-instance 'root-place :container instance))))

(defmethod shared-initialize :after ((instance   inspector-state)
                                     (slot-names t)
                                     &key
                                     (root-object nil root-object-supplied-p))
  (when root-object-supplied-p
    (setf (root-place instance)
          (make-instance 'root-place :container instance
                                     :cell      root-object))))

(defmethod run-hook ((inspector-state inspector-state)
                     (old-root-place  t)
                     (new-root-place  t))
  (when-let ((change-hook (change-hook inspector-state)))
    (map nil (rcurry #'funcall old-root-place new-root-place) change-hook)))

(defmethod root-place ((inspector-state inspector-state) &key run-hook-p)
  (declare (ignore run-hook-p))
  (%root-place inspector-state))

(defmethod (setf root-place) ((new-value t) (inspector-state inspector-state)
                              &key run-hook-p)
  (if run-hook-p
      (let ((*old-root-place*  (%root-place inspector-state)))
        (setf (%root-place inspector-state) new-value)
        (note-changed inspector-state))
      (setf (%root-place inspector-state) new-value)))

(defmethod root-object ((inspector-state inspector-state) &key run-hook-p)
  (declare (ignore run-hook-p))
  (let ((place (root-place inspector-state)))
    (if (valuep place)
        (values (value place) t)
        (values nil           nil))))

(defmethod (setf root-object) ((new-value t) (inspector-state inspector-state)
                               &key run-hook-p)
  (let* ((place         (root-place inspector-state))
         (same-object-p (and (valuep place)
                             (eq new-value (value place))))
         (new-place     (if same-object-p
                            place
                            (make-instance 'root-place
                                           :container inspector-state
                                           :cell      new-value)))
         (run-hook-p    (case run-hook-p
                          (:if-changed (not same-object-p))
                          (t           run-hook-p))))
    (setf (root-place inspector-state :run-hook-p run-hook-p) new-place)
    new-value))

(defmethod note-changed ((place inspector-state))
  (let ((root-place (root-place place)))
    (run-hook place (or *old-root-place* root-place) root-place)))
