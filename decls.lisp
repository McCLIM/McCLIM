;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: DEFGENERICs and stuff
;;;   Created: 2001-08-12
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

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

(in-package :CLIM-INTERNALS)

;;;; Changes

;;;  When        Who    What
;;; --------------------------------------------------------------------------------------
;;;  2001-08-12  GB     created
;;;

;; This is just an ad hoc list. Would it be a good idea to include all
;; (exported) generic functions here? --GB

(defgeneric point-x (point))
(defgeneric point-y (point))

(defgeneric transform-region (transformation region))

;;;

(defmacro with-special-choices ((sheet) &body body)
  "Macro for optimizing drawing with graphical system dependant mechanisms."
  (let ((fn (gensym "FN.")))
    `(labels ((,fn (,sheet)
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-special-choices #',fn ,sheet))))

(defgeneric invoke-with-special-choices (continuation sheet))

;; fall back, where to put this?

(defmethod invoke-with-special-choices (continuation (sheet T))
  (funcall continuation sheet))



