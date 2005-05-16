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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create a subclass of NSWindow that will behave as we want for CLIM.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define an obj-c class for the window we use as a frame for clim

(defclass lisp-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Event handling methods.
;;; Add the event they're invoked with to the "event queue" we define
;;; in the events.lisp file.
;;;

;;; Think this is not correct; we want to pass the pointer motion event to the
;;; specific view rather than handle them here. Or do we?
(define-objc-method ((:void :mouse-moved event) lisp-window)
  ;; Find the NSView under the event position - and send the event to that sheet.
  (let* ((contentview (send self 'content-view))
         (targetview  (send contentview :hit-test (send event 'location-in-window))))
;;;    (format *debug-io* "mouse-moved: got targetview of ~S~%" targetview)
;;;    ;; Moved event wasn't over an NSView - pass it on up the responder chain. - doesn't work. Look at later.
;;; ::FIXME::
;;;    (if (eql targetview (%null-ptr))
;;;        (send-super :mouse-moved event)
;;;      (add-event-to-queue targetview event))))
    (unless (eql targetview (%null-ptr))
      (add-event-to-queue targetview event))))

(define-objc-method ((:void :flags-changed event) lisp-window)
  (add-event-to-queue self event))

(define-objc-method ((:void :key-down event) lisp-window)
;;;  (nslog (format nil "LISP-WINDOW: Received KEY DOWN event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :key-up event) lisp-window)
;;;  (nslog (format nil "LISP-WINDOW: Received KEY UP event: ~S" (description event)))
  (add-event-to-queue self event))


