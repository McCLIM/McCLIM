;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

(defclass standard-sheet-output-mixin ()
  (
   ))

(defclass sheet-mute-output-mixin ()
  (
   ))

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

(defmethod initialize-instance :after ((sheet permanent-medium-sheet-output-mixin) &rest args)
  (declare (ignore args))
  ;; hmm, 
  (setf (%sheet-medium sheet) (make-medium (port sheet) sheet))
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

(defmethod invoke-with-sheet-medium-bound (continuation (medium null) (sheet permanent-medium-sheet-output-mixin))
  (funcall continuation (sheet-medium sheet)))

; BTS added this. CHECKME
(defmethod invoke-with-sheet-medium-bound (continuation (medium null) (sheet mirrored-pixmap))
  (funcall continuation (pixmap-medium sheet)))

(defmethod invoke-with-sheet-medium-bound (continuation (medium null) (sheet temporary-medium-sheet-output-mixin))
  (let ((old-medium (sheet-medium sheet))
        (new-medium (allocate-medium (port sheet) sheet)))
    (unwind-protect
         (progn
           (engraft-medium new-medium (port sheet) sheet)
           (setf (%sheet-medium sheet) new-medium)
           (funcall continuation new-medium))
      (setf (%sheet-medium sheet) old-medium)
      (degraft-medium new-medium (port sheet) sheet)
      (deallocate-medium (port sheet) new-medium))))

;; The description of WITH-SHEET-MEDIUM-BOUND in the spec, seems to be
;; extremly bogus, what is its purpose?

(defmethod invoke-with-sheet-medium-bound (continuation
                                           (medium basic-medium)
                                           (sheet permanent-medium-sheet-output-mixin))
  ;; this seems to be extremly bogus to me.
  (funcall continuation medium))

(defmethod invoke-with-sheet-medium-bound (continuation
                                           (medium basic-medium)
                                           (sheet temporary-medium-sheet-output-mixin))
  (cond ((not (null (sheet-medium sheet)))
         (funcall continuation medium))
        (t
         (let ((old-medium (sheet-medium sheet))
               (new-medium medium))
           (unwind-protect
                (progn
                  (engraft-medium new-medium (port sheet) sheet)
                  (setf (%sheet-medium sheet) new-medium)
                  (funcall continuation new-medium))
             (setf (%sheet-medium sheet) old-medium)
             (degraft-medium new-medium (port sheet) sheet) )))))

(defmethod invoke-with-special-choices (continuation (sheet sheet-with-medium-mixin))
  (with-sheet-medium (medium sheet)
    (with-special-choices (medium)
      (funcall continuation sheet))))
