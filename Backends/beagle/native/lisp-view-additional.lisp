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

;;; --->8--- start cut for kludgey work-around of OpenMCL bug --->8---

;;; These have been moved here from lisp-view.lisp because they invoke
;;; methods (#'fill-bounds and #'establish-tracking-rect) that are
;;; defined in that file but an issue with OpenMCL (currently under
;;; investigation) prevents them being referenced in the same compilation
;;; unit as they are defined. This is a (hopefully temporary) work-around
;;; until that's resolved.

(define-objc-method ((:void :stroke-path path :in-colour colour) lisp-view)
  (when (send self 'lock-focus-if-can-draw)
    (send (the lisp-bezier-path path) :set-colour colour)
    (send path :set-fill #$NO)
    (send path 'draw)
;;    (send path 'release)
    (send self 'unlock-focus)))

(define-objc-method ((:void :fill-path path :in-colour colour) lisp-view)
  (when (send self 'lock-focus-if-can-draw)
    (send (the lisp-bezier-path path) :set-colour colour)
    (send path :set-fill #$YES)
    (send path 'draw)
;;    (send path 'release)
    (send self 'unlock-focus)))

(define-objc-method ((:void :set-bounds (:<NSR>ect bounds)) lisp-view)
  (send-super :set-bounds bounds)
;;;  (send self 'fill-bounds)  
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :set-bounds-origin (:<NSP>oint point)) lisp-view)
  (send-super :set-bounds-origin point)
;;;  (send self 'fill-bounds)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :set-bounds-rotation (:float angle)) lisp-view)
  (send-super :set-bounds-rotation angle)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :set-bounds-size (:<NSS>ize size)) lisp-view)
  (send-super :set-bounds-size size)
;;;  (send self 'fill-bounds)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :translate-origin-to-point (:<NSP>oint point)) lisp-view)
  (send-super :translate-origin-to-point point)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :scale-unit-square-to-size (:<NSS>ize size)) lisp-view)
  (send-super :scale-unit-square-to-size size)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :rotate-by-angle (:float angle)) lisp-view)
  (send-super :rotate-by-angle angle)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :set-frame (:<NSR>ect frame)) lisp-view)
  (send-super :set-frame frame)
;;;  (send self 'fill-bounds)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :set-frame-origin (:<NSP>oint point)) lisp-view)
  (send-super :set-frame-origin point)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :set-frame-rotation (:float angle)) lisp-view)
  (send-super :set-frame-rotation angle)
  (send self 'establish-tracking-rect))

(define-objc-method ((:void :set-frame-size (:<NSS>ize size)) lisp-view)
  (send-super :set-frame-size size)
;;;  (send self 'fill-bounds)
  (send self 'establish-tracking-rect))

;;; --->8--- end cut for kludgey work-around of OpenMCL bug --->8---
