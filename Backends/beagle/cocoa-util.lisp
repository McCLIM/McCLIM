;;; -*- Mode: Lisp; Package: CCL; -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2003, 2004 by
;;;           Duncan Rose (duncan@robotcat.demon.co.uk)

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

(in-package :beagle)

(declaim (inline cg-floatify))
(defun cg-floatify (cg-float-value)
  (float cg-float-value ns:+cgfloat-zero+))

(defun make-ns-rect (x y width height)
  "Make a Cocoa NSRect structure with the origin at (x, y) and with the
width and height specified. The memory for any structure created with
this method must be released by the user (using (#_free))."
  (ccl:make-record :<NSR>ect
		   :origin.x    (cg-floatify x)
		   :origin.y    (cg-floatify y)
		   :size.width  (cg-floatify width)
		   :size.height (cg-floatify height)))

(defun make-ns-point (x y)
  "Make a Cocoa NSPoint structure populated with x and y provided.
The memory for any structure created with this method must be released
by the user (using (#_free))."
  (ccl:make-record :<NSP>oint :x (cg-floatify x) :y (cg-floatify y)))

;; Stolen from Bosco "main.lisp".
(defun description (c)
  (ccl::with-autorelease-pool
    (ccl::lisp-string-from-nsstring
     (ccl::send c 'description))))

(defun nslog (c)
  (let* ((rep (format nil "~a" c)))
    (with-cstrs ((str rep))
      (with-nsstr (nsstr str (length rep))
	(#_NSLog #@"Logging: %@" :address nsstr)))))


