;;; -*- Mode: Lisp; Package: INSPECTOR -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)

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

;;; CLIM inspector application

(defpackage :clouseau
  (:use :clim-lisp :clim)
  (:export #:inspector
	   #:inspect-object
	   #:inspect-object-briefly
	   #:define-inspector-command
	   #:inspector-table
	   #:inspector-table-row))

(asdf:defsystem clouseau
    :serial t
    :components
    ((:file "disassembly")
     (:file "inspector")))