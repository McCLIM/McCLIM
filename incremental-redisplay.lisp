;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)

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

(defmethod invoke-updating-output (stream continuation record-type unique-id id-test
				   cache-value cache-test &key all-new parent-cache)
  (funcall continuation stream))

(defmacro updating-output
                 ((stream &rest args &key unique-id (id-test #'eql ) cache-value (cache-test
                 #'eql ) fixed-position all-new parent-cache record-type) &body body)
  (declare (ignore fixed-position))
  `(flet ((func (,stream)
	    ,@body))
     (invoke-updating-output ,stream #'func ,record-type ,unique-id ,id-test ,cache-value ,cache-test :all-new ,all-new :parent-cache ,parent-cache)))