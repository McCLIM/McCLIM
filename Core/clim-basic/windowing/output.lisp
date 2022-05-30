;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

(defclass standard-sheet-output-mixin ()
  ())

(defclass sheet-mute-output-mixin ()
  ())

(defclass sheet-with-medium-mixin ()
  ((medium :initform nil
           :reader sheet-medium
           :writer (setf %sheet-medium))))

(macrolet ((frob (fn &rest args)
             `(defmethod ,fn ,(substitute '(medium sheet-with-medium-mixin)
                                          'medium
                                          args)
                ;; medium arg is really a sheet
                (let ((medium (sheet-medium medium)))
                  ,(if (symbolp fn)
                       `(,fn ,@args)
                       `(funcall #',fn ,@args))))))
  (frob medium-foreground medium)
  (frob medium-background medium)
  (frob (setf medium-foreground) design medium)
  (frob (setf medium-background) design medium)
  (frob medium-ink medium)
  (frob (setf medium-ink) design medium)
  (frob medium-transformation medium)
  (frob (setf medium-transformation) transformation medium)
  (frob medium-clipping-region medium)
  (frob (setf medium-clipping-region) region medium)
  (frob medium-line-style medium)
  (frob (setf medium-line-style) line-style medium)
  (frob medium-default-text-style medium)
  (frob (setf medium-default-text-style) text-style medium)
  (frob medium-text-style medium)
  (frob (setf medium-text-style) text-style medium)
  (frob medium-current-text-style medium)
  (frob medium-beep medium))

(defclass temporary-medium-sheet-output-mixin (sheet-with-medium-mixin)
  ())

(defclass permanent-medium-sheet-output-mixin (sheet-with-medium-mixin)
  ())

(defmethod initialize-instance :after
    ((sheet permanent-medium-sheet-output-mixin) &key port)
  ;; hmm,
  (setf (%sheet-medium sheet) (make-medium port sheet))
  ;; hmm...
  (engraft-medium (sheet-medium sheet) (port sheet) sheet))

(defmacro with-sheet-medium ((medium sheet) &body body)
  (check-type medium symbol)
  (let ((fn (gensym)))
    `(labels ((,fn (,medium)
                ,(declare-ignorable-form* medium)
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-sheet-medium-bound #',fn nil ,sheet))))

(defmacro with-sheet-medium-bound ((sheet medium) &body body)
  (check-type medium symbol)
  (let ((fn (gensym)))
    `(labels ((,fn  (,medium)
                ,(declare-ignorable-form* medium)
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-sheet-medium-bound #',fn ,medium ,sheet))))

(defgeneric invoke-with-sheet-medium-bound (continuation medium sheet)
  (:argument-precedence-order sheet medium continuation)
  (:method (continuation medium sheet)
    (declare (ignore sheet))
    (funcall continuation medium))
  (:method (continuation medium (sheet permanent-medium-sheet-output-mixin))
    (declare (ignore medium))
    (funcall continuation (sheet-medium sheet)))
  (:method (continuation medium (sheet temporary-medium-sheet-output-mixin))
    (if-let ((sheet-medium (sheet-medium sheet)))
      (funcall continuation sheet-medium)
      (let ((port (port sheet)))
        (if (null medium)
            (let ((new-medium (allocate-medium port sheet)))
              (unwind-protect
                   (progn
                     (engraft-medium new-medium port sheet)
                     (setf (%sheet-medium sheet) new-medium)
                     (funcall continuation new-medium))
                (setf (%sheet-medium sheet) nil)
                (degraft-medium new-medium port sheet)
                (deallocate-medium port new-medium)))
            (unwind-protect
                 (progn
                   (engraft-medium medium port sheet)
                   (setf (%sheet-medium sheet) medium)
                   (funcall continuation medium))
              (setf (%sheet-medium sheet) nil)
              (degraft-medium medium port sheet)))))))
