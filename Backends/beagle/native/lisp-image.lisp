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
;;; Create a subclass of NSImage that will behave as we want for CLIM.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lisp-image (ns:ns-image)
  ()
  (:metaclass ns:+ns-object))

(define-objc-method ((:void :draw-string string
				 :at-point (:<NSP>oint point)
				 :with-attributes attr
				 :in-colour colour
				 :with-width (:float width)
				 :with-cap-style (:int cap)
				 :with-join-style (:int join)) lisp-image)
;;;  (format *debug-io* "draw-string: isValid = ~S~%" (send self 'is-valid))
  ;; Note: lockFocus in NSImage is defined: "- (void)lockFocus". There's
  ;; *no* T/NIL returned like for NSView, so we can't test that we were
  ;; able to lock the focus!
  ;; See "http://www.stone.com/porting/dict_walk_and_lockFocus.html"
  (when (send self 'is-valid) ;;lock-focus) ;;-if-can-draw)
    (send self 'lock-focus)
    (send (the ns-color colour) 'set)
    (send (@class ns-bezier-path) :set-default-line-width width)
    (send (@class ns-bezier-path) :set-default-line-cap-style cap)
    (send (@class ns-bezier-path) :set-default-line-join-style join)
    (send string :draw-at-point point :with-attributes attr)
    (send (send self 'window) 'flush-window)
    (send self 'unlock-focus)))

(define-objc-method ((:void :stroke-path path :in-colour colour) lisp-image)
;;;  (format *debug-io* "stroke-path: isValid = ~S~%" (send self 'is-valid))
  ;; Note: lockFocus in NSImage is defined: "- (void)lockFocus". There's
  ;; *no* T/NIL returned like for NSView, so we can't test that we were
  ;; able to lock the focus!
  ;; See "http://www.stone.com/porting/dict_walk_and_lockFocus.html"
  (when (send self 'is-valid) ;;lock-focus) ;;-if-can-draw)
    (send self 'lock-focus)
    (send (the ns-color colour) 'set)      ; colour for current graphics context
    (send path 'stroke)
    (send (send self 'window) 'flush-window)
    (send self 'unlock-focus)))

(define-objc-method ((:void :fill-path path :in-colour colour) lisp-image)
;;;  (format *debug-io* "fill-path: isValid = ~S~%" (send self 'is-valid))
  ;; Note: lockFocus in NSImage is defined: "- (void)lockFocus". There's
  ;; *no* T/NIL returned like for NSView, so we can't test that we were
  ;; able to lock the focus!
  ;; See "http://www.stone.com/porting/dict_walk_and_lockFocus.html"
  (when (send self 'is-valid) ;;lock-focus) ;;-if-can-draw)
    (send self 'lock-focus)
    (send (the ns-color colour) 'set)      ; colour for current graphics context
    (send path 'fill)
    (send (send self 'window) 'flush-window)
    (send self 'unlock-focus)))

(define-objc-method ((:id :copy-bitmap-from-region (:<NSR>ect rect)) lisp-image)
  (if (send self 'is-valid) ;;lock-focus) ;;-if-can-draw)
      (progn
	(send self 'lock-focus)
	(let ((bitmap (send (send (@class ns-bitmap-image-rep) 'alloc) :init-with-focused-view-rect rect)))
	  (send bitmap 'retain)
	  (send self 'unlock-focus)
	  bitmap))
    (progn
;;;      (format *debug-io* "(copy-bitmap...) - FAILED TO LOCK FOCUS ON VIEW (NOT VALID) ~S!!!~%" self)
      nil)))

#||
copy-area (medium pixmap)
from = LISP-VIEW 0x418fc990
to = LISP-IMAGE 0x41b50e10 size={583, 382} Reps=()
   -> NS-BITMAP-IMAGE-REP 0x41b52100 size={582, 382}

paste-bitmap
best-representation-for-device: NS-CACHED-IMAGE-REP 0x429770 size={583, 382}
sending self (LispImage 0x41b50e10 size={583, 382} Reps=(NSCachedImageRep 0x428770)

   -> fails to lock focus.

This is because lockFocus in NSImage is defined: "- (void)lockFocus". There's
*no* T/NIL returned like for NSView, so we can't test that we were
able to lock the focus!
See "http://www.stone.com/porting/dict_walk_and_lockFocus.html"

||#

(define-objc-method ((:void :paste-bitmap bitmap :to-point (:<NSP>oint point)) lisp-image)
  ;; "fraction" defines the opacity of the bitmap.
  
;  NSImage image = [[NSImage alloc] initWithData:[bitmap TIFFRepresentation]];
;  // We can also use "composite" methods in NSImage to do flipping (probably).
;  [image dissolveToPoint:point fraction:1.0];
;  [image release];

;;;  (format *debug-io* "paste-bitmap: isValid = ~S~%" (send self 'is-valid))
;;;  (format *debug-io* "best-representation-for-device: ~S~%" (send self :best-representation-for-device nil))
;;;  (format *debug-io* "sending self (~S) 'lock-focus~%" (description self))
  (if (send self 'is-valid) ;;lock-focus) ;;-if-can-draw)
      (progn
	(send self 'lock-focus)
	(let ((image (send (send (@class ns-image) 'alloc) :init-with-data (send bitmap "TIFFRepresentation"))))
	  (send image :dissolve-to-point point :fraction #.(cg-floatify 1.0)))
;;;	(send (send self 'window) 'flush-window)
	(send self 'unlock-focus))))
;;;    (format *debug-io* "(paste-bitmap...) - FAILED TO LOCK FOCUS ON VIEW (NOT VALID) ~S!!!~%" self)))



