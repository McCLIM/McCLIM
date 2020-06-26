;;; ---------------------------------------------------------------------------
;;;     Title: Presentation histories
;;;   Created: 2020-06-26 15:00
;;;    Author: Daniel Kochmański <daniel@turtleware.eu>
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementatino of the presentation history. The concept is not
;;; well-specified in CLIM II specification, but it seems that they
;;; are mostly useful in ACCEPT.
;;;

(in-package #:clim-internals)

;;; This allocates a hash table even if the frame doesn't support
;;; histories!  I'm over it already. -- moore
(defclass presentation-history-mixin ()
  ((presentation-history :accessor frame-presentation-history
                         :initform (make-hash-table :test #'eq)))
  (:documentation "Mixin class for frames that implements presentation
type histories"))

(define-presentation-generic-function %presentation-type-history
    presentation-type-history
  (type-key parameters type))

;;; This function should exist for convenience even if not mentioned in the
;;; spec.

(defun presentation-type-history (type)
  (funcall-presentation-generic-function presentation-type-history type))

(defclass presentation-history ()
  ((stack :accessor presentation-history-array
          :initform (make-array 1 :fill-pointer 0
                                  :adjustable t)
          :documentation "The history, with the newest objects at
the end of the array. Should contain conses with the car being
the object and the cdr being the type.")
   (pointer :accessor presentation-history-pointer
            :initform nil
            :documentation "The index of the \"current\" object,
used when navigating the history. If NIL, means that no
navigation has yet been performed."))
  (:documentation "Class for objects that contain the history for
a specific type."))

(define-default-presentation-method presentation-type-history (type)
  (if (and *application-frame*
           (frame-maintain-presentation-histories *application-frame*))
      (with-presentation-type-decoded (name)
        type
        (let* ((ptype (get-ptype-metaclass name))
               (history (history ptype)))
          (case history
            ((t)
             (let* ((history-table (frame-presentation-history
                                    *application-frame*))
                    (history-object (gethash name history-table)))
               (unless history-object
                 (setf history-object
                       (make-instance 'presentation-history)
                       (gethash name history-table)
                       history-object))
               history-object))
            ((nil)
             nil)
            (otherwise
             (funcall-presentation-generic-function presentation-type-history
                                                    type)))))))

;;; Not in the spec, but I think this is necessary (or at any rate, the easiest
;;; way) to control whether or not to use histories in a given context.
(define-presentation-generic-function %presentation-type-history-for-stream
    presentation-type-history-for-stream
  (type-key parameters type stream)
  (:documentation "Returns a type history or nil for a presentation TYPE and
STREAM. This is used by McCLIM to decide whether or not to use histories in a
given situation. A primary method specialized on just the type should
call-next-method to get the \"real\" answer based on the stream type."))

(define-default-presentation-method presentation-type-history-for-stream
    (type stream)
  (declare (ignore stream))
  nil)

;;; method for clim-stream-pane in panes.lisp

(define-presentation-method presentation-type-history-for-stream
    ((type t) (stream input-editing-stream))
  ;; What is the purpose of this? Makes stuff harder to do, so
  ;; commented out...
  ;;(if (not (stream-rescanning-p stream))
  ;;       (funcall-presentation-generic-function presentation-type-history type)
  ;;       nil)
  (funcall-presentation-generic-function presentation-type-history type))

(defun presentation-history-insert (history object ptype)
  "Unconditionally insert `object' as an input of presentation
type `type' at the top of the presentation history `history', as
the most recently added object."
  (vector-push-extend (cons object ptype)
                      (presentation-history-array history)))

(defun presentation-history-top (history ptype)
  "Find the topmost (most recently added object) of `history'
that is of the presentation type `ptype' or a subtype. Two values
will be returned, the object and the presentation type of the
object. If no applicable object can be found, these values will
both be NIL."
  (loop
     with array = (presentation-history-array history)
     for index from (1- (fill-pointer array)) downto 0
     for (object . object-ptype) = (aref array index)
     do
     (when (presentation-subtypep object-ptype ptype)
       (return (aref array index)))
     finally (return (values nil nil))))

(defun presentation-history-reset-pointer (history)
  "Set the pointer to point at the object most recently added
object."
  (setf (presentation-history-pointer history) nil))

(defun presentation-history-next (history ptype)
  "Go to the next input (forward in time) in `history' that is a
presentation-subtype of `ptype', respective to the pointer in
`history'. Returns two values: the found object and its
presentation type, both of which will be NIL if no applicable
object can be found."
  (with-accessors ((pointer presentation-history-pointer)
                   (array presentation-history-array)) history
    ;; If no navigation has been performed, we have no object to go
    ;; forwards to.
    (if (or (null pointer) (>= (1+ pointer) (length array)))
        (values nil nil)
        (progn
          (incf pointer)
          (destructuring-bind (object . object-ptype)
              (aref array pointer)
            (if object-ptype
                (if (presentation-subtypep object-ptype ptype)
                    (values object object-ptype)
                    (presentation-history-next history ptype))
                (values nil nil)))))))

(defun presentation-history-previous (history ptype)
  "Go to the previous input (backward in time) in `history' that
is a presentation-subtype of `ptype', respective to the pointer
in `history'. Returns two values: the found object and its
presentation type, both of which will be NIL if no applicable
object can be found."
  (with-accessors ((pointer presentation-history-pointer)
                   (array presentation-history-array)) history
    (if (and (numberp pointer) (zerop pointer))
        (values nil nil)
        (progn
          (cond ((and (numberp pointer) (plusp pointer))
                 (decf pointer))
                ((plusp (length array))
                 (setf pointer (1- (fill-pointer array)))))
          (if (and (numberp pointer) (array-in-bounds-p array pointer))
              (destructuring-bind (object . object-ptype)
                  (aref array pointer)
                (if object-ptype
                    (if (presentation-subtypep object-ptype ptype)
                        (values object object-ptype)
                        (progn (presentation-history-previous history ptype)))
                    (values nil nil)))
              (values nil nil))))))

(defmacro with-object-on-history ((history object ptype) &body body)
  "Evaluate `body' with `object' as `ptype' as the head (most
recently added object) on `history', and remove it again after
`body' has run. If `body' as `ptype' is already the head, the
history will be unchanged."
  (with-gensyms (added)
    `(let ((,added (presentation-history-add ,history ,object ,ptype)))
       (unwind-protect (progn ,@body)
         (when ,added
           (decf (fill-pointer (presentation-history-array ,history))))))))

(defun presentation-history-add (history object ptype)
  "Add OBJECT and PTYPE to the HISTORY unless they are already at the head of
 HISTORY"
  (multiple-value-bind (top-object top-ptype)
      (presentation-history-top history ptype)
    (unless (and top-ptype (eql object top-object) (equal ptype top-ptype))
      (presentation-history-insert history object ptype))))
