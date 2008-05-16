;;; -*- Mode: Lisp; Package: beagle; -*-

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

(defclass lisp-bezier-path (ns:ns-bezier-path)
  ((colour :foreign-type :id
	   :accessor path-colour)
   (fill   :foreign-type :<bool>
	   :initform #$NO
	   :accessor path-fill))
  (:metaclass ns:+ns-object))

(define-objc-method ((:void :set-colour colour) lisp-bezier-path)
  (setf (path-colour self) colour))

(define-objc-method ((:void :set-fill (:<bool> fill)) lisp-bezier-path)
  (setf (path-fill self) fill))

(define-objc-method ((:void draw) lisp-bezier-path)

  ;; Check we know what colour we're supposed to be; if we don't we
  ;; probably haven't been initialized properly.
  (if (not (eql (%null-ptr) (path-colour self)))
      (progn
;;;	(format *debug-io* "lisp-bezier-path:draw invoked when colour is non null. Continuing~%")

	;; Set the drawing colour; not sure how this will interact with patterns
	;; or bitmaps.
	(send (the ns:ns-color (path-colour self)) 'set)

;;;	(format *debug-io* "lisp-bezier-path:draw invoked with fill = ~A~%" (path-fill self))
	
	;; Draw ourselves.
	(if (= (path-fill self) 1)
	    (send self 'fill)
	  (send self 'stroke)))
    (format *debug-io* "lisp-bezier-path:draw invoked when colour is null! Skipping~%")))
