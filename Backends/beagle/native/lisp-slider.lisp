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

;; Does this want to deal with NSSmallControlSize? It doesn't, currently.

(defclass lisp-slider (ns:ns-slider)
  ((lispslider :initform nil
	       :accessor view-lisp-slider)
   (eventmask :initform (%null-ptr) 
	      :foreign-type :int
	      :accessor view-event-mask))
  (:metaclass ns:+ns-object))


;;; This method is the 'recipient' of any actions sent by the slider
;;; (we set the slider up as its own action 'target'). It just calls
;;; back into Lisp [BEAGLE-SLIDER-PANE] to handle things.
(define-objc-method ((:void :take-slider-action (:id sender)) lisp-slider)
  (slider-action-handler (view-lisp-slider self) sender))

