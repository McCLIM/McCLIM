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

;(in-package :clim-cocoa)

(in-package :ccl)
;(require "OBJC-SUPPORT")

;; Should be using "with-autorelease-pool" somewhere... but for now, take it out because
;; something isn't working...

;; Make an NSColor object that is the desired colour provided as a parameter, and with
;; the opacity provided in the key argument (defaults to 1.0 (opaque)).
;;;(defun make-ns-color (desired-color &key (alpha 1.0))
;;;  (cl-user::debug-log 1 "cocoa-util.lisp: -> MAKE-NS-COLOR() - TYPE-OF (desired-color): ~A~%"
;;;          (type-of desired-color))
;;;  (cl-user::debug-log 1 "cocoa-util: Entered MAKE-NS-COLOR, desired colour = ~S~%" desired-color)
;;;  (multiple-value-bind (r g b)
;;;      (clim:color-rgb desired-color)
;;;      (send (@class ns-color) :color-with-calibrated-red r
;;;                              :green g
;;;                              :blue b
;;;                              :alpha alpha)))


;; Given a CLIM event-mask, generate a Cocoa event-mask
;;;(defun clim-event-mask->cocoa-event-mask (event-mask)
;;;  (cl-user::debug-log 1 "cocoa-util: Entered CLIM-EVENT-MASK->COCOA-EVENT-MASK (stubbed)~%")
;;;  event-mask)

;; Tell an NSWindow what events to respond to
;;;(defun set-ns-window-event-mask (window event-mask)
;;;  (cl-user::debug-log 1 "cocoa-util: Entered SET-NS-WINDOW-EVENT-MASK~%")
;;;    (send window :next-event-matching-mask event-mask))

;; Make an NSRect structure with the origin at (x, y) and with the width and height
;; specified.
(defun make-ns-rect (x y width height)
  (make-record :<NSR>ect :origin.x    (+ (coerce x      'short-float) 0.5)
                         :origin.y    (+ (coerce y      'short-float) 0.5)
			 :size.width     (coerce width  'short-float)
			 :size.height    (coerce height 'short-float)))

(defun make-ns-point (x y)
  (make-record :<NSP>oint :x (+ (coerce x 'short-float) 0.5)
	                  :y (+ (coerce y 'short-float) 0.5)))

;; Get the *NSApp* reference
;;;(defun get-ns-app ()
;;;  *NSApp*)

;; Send the NSWindow provided a setFrame: message
;;;(defun window-set-frame (window rect &key (display t))
;;;    (send window :set-frame rect :display display))

;; Stolen from Bosco "main.lisp"
(defun description (c)
  (with-autorelease-pool
   (lisp-string-from-nsstring
	(send c 'description))))

(defun nslog (c)
  (let* ((rep (format nil "~a" c)))
    (with-cstrs ((str rep))
      (with-nsstr (nsstr str (length rep))
	(#_NSLog #@"Logging: %@" :address nsstr)))))


