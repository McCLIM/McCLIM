;;; -*- Mode: Lisp; Package: BEAGLE; -*-

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
;;; Create an objective c class that will be a delegate for NSWindow.
;;; Note that all the OpenMCL objective c stuff is currently in package
;;; ccl, so we need to make this stuff look a little ugly for it to
;;; work.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define an obj-c class for the delegate we will use

(defclass lisp-window-delegate (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Event handling methods.
;;; Add the event they're invoked with to the "event queue" we define
;;; in the events.lisp file.
;;;


(define-objc-method ((:void :window-did-resize notification) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidResize event"))
;;;  (nslog (format nil "Notification name is: ~A" (description (send notification 'name))))
  ;; Window extents should be non-nil in this case...
  (slet ((rect (send (send (send notification 'object) 'content-view) 'frame)))
    (add-notification-to-queue (send notification 'object) notification
			       (pref rect :<NSR>ect.origin.x)
			       (pref rect :<NSR>ect.origin.y)
			       (pref rect :<NSR>ect.size.width)
			       (pref rect :<NSR>ect.size.height))))

(define-objc-method ((:void :window-did-become-key notification) lisp-window-delegate)
  (add-notification-to-queue (send notification 'object) notification))

;;; No notifications below this point that we're interested in.
#||

;;; ... although this one might be useful. Especially wrt native cut&paste.

(define-objc-method ((:void :window-will-close notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowWillClose event"))
  (nslog (format nil "Notification name is: ~A" (description (send notification 'name))))
  ;; Window extents *should* be nil in this case...
  (add-notification-to-queue (send notification 'object) notification))

;;;(define-objc-method ((:void :window-did-become-main notification) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidBecomeMain event")))
;;;  (send-super :window-did-become-main notification)))

||#


#||
(define-objc-method ((:void :window-did-change-screen notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidChangeScreen event")))
;;;  (send-super :window-did-change-screen notification))

(define-objc-method ((:void :window-did-deminiaturize notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidDeminiaturize event")))
;;;  (send-super :window-did-deminiaturize notification))

(define-objc-method ((:void :window-did-end-sheet notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidEndSheet event")))
;;;  (send-super :window-did-end-sheet notification))

(define-objc-method ((:void :window-did-move notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidMove event ~A" (description notification))))
;;;  (send-super :window-did-move notification))

(define-objc-method ((:void :window-did-resign-key notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidResignKey event")))
;;;  (send-super :window-did-resign-key notification))

(define-objc-method ((:void :window-did-resign-main notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidResignMain event")))
;;;  (send-super :window-did-resign-main notification))

(define-objc-method ((:void :window-did-update notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidUpdate event")))
;;;  (send-super :window-did-update notification))

;;;(define-objc-method ((:<BOOL> :window-should-close id-sender) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowShouldClose event"))
;;;  ;; We don't want to stop the window closing for any reason (at least, not at the
;;;  ;; moment. Maybe in future we'll want to add this functionality)
;;;  #$YES)
;;;  (send-super :window-should-close id-sender))

;;;(define-objc-method ((:<BOOL> :window-should-zoom ns-window :to-frame ns-rect) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidBecomeMain event"))
;;;  #$YES)
;;;  (send-super :window-should-zoom ns-window :to-frame ns-rect))

(define-objc-method ((:void :window-will-begin-sheet notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowWillBeginSheet event")))
;;;  (send-super :window-will-begin-sheet notification))

(define-objc-method ((:void :window-will-miniaturize notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowWillMiniaturize event")))
;;;  (send-super :window-will-miniaturize notification))

(define-objc-method ((:void :window-will-move notification) lisp-window-delegate)
  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowWillMove event ~S" (description notification))))
;;;  (send-super :window-will-move notification))

;;;(define-objc-method ((:<NSS>ize :window-will-resize ns-window
;;;                                     :to-size (:<NSS>ize size)) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidWillResize event"))
;;;  size)
;;;  (send-super :window-will-resize ns-window :to-size size))



;;;(define-objc-method ((:void :window-did-expose notification) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidExpose event"))
;;;  (nslog (format nil "Notification name is: ~A" (description (send notification 'name))))
;;;  ;; Window extents should be non-nil in this case... We can get an NSExposedRect out of the userInfo
;;;  ;; dictionary in the notification and pass this back to CLIM. Should speed things up a little...
;;;  #+nyi (slet ((rect (send (send notification 'object) 'frame)))
;;;    (add-notification-to-queue (send notification 'object) notification
;;;                               (pref rect :<NSR>ect.origin.x)
;;;                               (pref rect :<NSR>ect.origin.y)
;;;                               (pref rect :<NSR>ect.size.width)
;;;                               (pref rect :<NSR>ect.size.height))))



;;;(define-objc-method ((:id :window-will-return-field-editor window-sender
;;;                               :to-object id-obj) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowWillReturnFieldEditor event")))
;;;  (send-super :window-will-return-field-editor window-sender :to-object id-obj))

;;;(define-objc-method ((:<NSU>ndo<M>anager :window-will-return-undo-manager window-sndr) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidBecomeMain event")))
;;;  (send-super :window-will-return-undo-manager window-sndr))

;;;(define-objc-method ((:<NSR>ect :window-will-use-standard-frame window-sndr
;;;                                     :default-frame (:<NSR>ect frame-rect)) lisp-window-delegate)
;;;  (nslog (format nil "LISP-WINDOW-DELEGATE: received windowDidBecomeMain event")))
;;;  (send-super :window-will-use-standard-frame window-sndr :default-frame frame-rect))

||#
