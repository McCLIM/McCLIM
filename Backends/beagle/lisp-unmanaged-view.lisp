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
;;; Create a subclass of NSView that will behave as we want for CLIM.
;;; Note that all the OpenMCL objective c stuff is currently in package
;;; ccl, so we need to make this stuff look a little ugly for it to
;;; work.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define an obj-c class for the view we use as a mirror for clim
;(ccl::def-objc-class lisp-unmanaged-view lisp-view
;  )

(defclass lisp-unmanaged-view (lisp-view)
  ()
  (:metaclass ns:+ns-object))



(define-objc-method ((:void :set-bounds (:<NSR>ect bounds)) lisp-unmanaged-view)
  (send-super :set-bounds bounds))

(define-objc-method ((:void :set-bounds-origin (:<NSP>oint point)) lisp-unmanaged-view)
  (send-super :set-bounds-origin point))

(define-objc-method ((:void :set-bounds-rotation (:float angle)) lisp-unmanaged-view)
  (send-super :set-bounds-rotation angle))

(define-objc-method ((:void :set-bounds-size (:<NSS>ize size)) lisp-unmanaged-view)
  (send-super :set-bounds-size size))

(define-objc-method ((:void :translate-origin-to-point (:<NSP>oint point)) lisp-unmanaged-view)
  (send-super :translate-origin-to-point point))

(define-objc-method ((:void :scale-unit-square-to-size (:<NSS>ize size)) lisp-unmanaged-view)
  (send-super :scale-unit-square-to-size size))

(define-objc-method ((:void :rotate-by-angle (:float angle)) lisp-unmanaged-view)
  (send-super :rotate-by-angle angle))

(define-objc-method ((:void :set-frame (:<NSR>ect frame)) lisp-unmanaged-view)
  (send-super :set-frame frame))

(define-objc-method ((:void :set-frame-origin (:<NSP>oint point)) lisp-unmanaged-view)
  (send-super :set-frame-origin point))

(define-objc-method ((:void :set-frame-rotation (:float angle)) lisp-unmanaged-view)
  (send-super :set-frame-rotation angle))

(define-objc-method ((:void :set-frame-size (:<NSS>ize size)) lisp-unmanaged-view)
  (send-super :set-frame-size size))

;;; ----------------------------------------------------------------------------

;;; Event handling methods.

;;; Add the event they're invoked with to the "event queue" we define
;;; in the events.lisp file.
;;;
;;; Cocoa docs say if you don't want to handle the event, you should
;;; pass it on to your superclass. So that's what we do.

;;; ----------------------------------------------------------------------------

(define-objc-method ((:void :mouse-moved event) lisp-unmanaged-view)
  (nslog (format nil "LISP-UNMANAGED-VIEW: Received MOUSE MOVED event"))
  (add-event-to-queue self event))

(define-objc-method ((:void :mouse-down event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received MOUSE DOWN event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :mouse-dragged event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received MOUSE DRAGGED event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :mouse-up event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received MOUSE UP event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :mouse-entered event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received MOUSE ENTERED event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :mouse-exited event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received MOUSE EXITED event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :right-mouse-down event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received RIGHT MOUSE DOWN event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :right-mouse-dragged event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received RIGHT MOUSE DRAGGED event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :right-mouse-up event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received RIGHT MOUSE UP event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :other-mouse-down event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received OTHER MOUSE DOWN event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :other-mouse-dragged event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received OTHER MOUSE DRAGGED event: ~S" (description event)))
  (add-event-to-queue self event))

(define-objc-method ((:void :other-mouse-up event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received OTHER MOUSE UP event: ~S" (description event)))
  (add-event-to-queue self event))

;;; ----------------------------------------------------------------------------

;;; Events after this point are not handled.

(define-objc-method ((:void :scroll-wheel event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received SCROLL WHEEL event: ~S" (description event)))
  (send-super :scroll-wheel event))

(define-objc-method ((:void :flags-changed event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received FLAGS CHANGED event: ~S" (description event)))
  (send-super :flags-changed event))

(define-objc-method ((:void :help-requested event) lisp-unmanaged-view)
;;;  (nslog (format nil "LISP-UNMANAGED-VIEW: Received HELP REQUESTED event: ~S" (description event)))
  (send-super :help-requested event))

