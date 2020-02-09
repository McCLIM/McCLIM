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

;;; Backward compatibility with the previous inspector clouseau
;;; implementation.

(defgeneric inspect-object-briefly (object stream))

(defun inspector (object &key (new-process nil))
  (inspect object :new-process new-process))

#+sbcl (declaim (sb-ext:deprecated
                 :early ("Clouseau" "2.0.0")
                 (function inspect-object-briefly :replacement inspect-object-using-state)
                 (function inspector :replacement inspect)))
