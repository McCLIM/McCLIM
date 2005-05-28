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


;;; event mask should be all *except* pointer-motion.
;;; Very similar to CLX defun of same name... a few differences.
(defun realize-mirror-aux (port sheet &key view (map nil) (event-mask nil))
  (declare (ignore map))
  ;; with this in, the Listener fails attempting to create a mirror for a
  ;; SCROLLER-PANE, which has no medium (not sheet-with-medium-mixin).
;;;  (check-type sheet sheet-with-medium-mixin)
  (when (null (port-lookup-mirror port sheet))
    (update-mirror-geometry sheet)  ; Copy CLX
    ;; because not every sheet coming through this method even has a medium,
    ;; we have to use the nasty hack for background colour. This needs
    ;; fixing.
    (let* ((desired-color ;(medium-background sheet))
	    (typecase sheet
			    (sheet-with-medium-mixin (medium-background sheet))
			    (basic-pane
			     (let ((background (pane-background sheet)))
			       (if (typep background 'color)
				   background
				 +white+)))
			    (t
			     +white+)))
	   ;; Is sheet-mirror-transformation *different* to sheet-native-transformation? If
	   ;; so, in what way do they differ? %sheet-mirror-transformation is the accessor
	   ;; for the mirror-transformation slot in the mirrored-sheet-mixin.
	   ;; This is "our idea of the current mirror transformation".
	   ;; native-transformation is a slot in BASIC-SHEET - I'm not sure what the difference
	   ;; is between these two. Suspect they are not both needed...
	   ;; I *suspect* that one converts into the mirror region (i.e. takes into account
	   ;; its physical limitation on the screen) whereas the other doesn't (allows
	   ;; coords outside the mirror's physical screen size to be used).
	   ;; x,y = 0,0 unless there's a mirror-transformation in play
	   (x (if (%sheet-mirror-transformation sheet)
		  (nth-value 0 (transform-position (%sheet-mirror-transformation sheet) 0 0))
		0))
	   (y (if (%sheet-mirror-transformation sheet)
		  (nth-value 1 (transform-position (%sheet-mirror-transformation sheet) 0 0))
		0))
	   (q (compose-space sheet))
	   ;; Take the width / height from the mirror-region if there's one set, otherwise from the
	   ;; space requirement.
	   (width (if (%sheet-mirror-region sheet)
		      (bounding-rectangle-width (%sheet-mirror-region sheet))
		    (space-requirement-width q)))
	   (height (if (%sheet-mirror-region sheet)
		       (bounding-rectangle-height (%sheet-mirror-region sheet))
		     (space-requirement-height q)))
	   (rect (ccl::make-ns-rect (pixel-center x) (pixel-center y)
				    (pixel-count width) (pixel-count height)))
	   (mirror (make-instance view :with-frame rect)))
      (#_free rect)
      (send mirror 'retain)
      (send mirror 'establish-tracking-rect)
      (setf (view-background-colour mirror) (%beagle-pixel port desired-color))
      (unless event-mask (setf event-mask (logior #$NSKeyDownMask
						  #$NSKeyUpMask
						  #$NSLeftMouseDownMask
						  #$NSRightMouseDownMask
						  #$NSOtherMouseDownMask
						  #$NSLeftMouseUpMask
						  #$NSRightMouseUpMask
						  #$NSOtherMouseUpMask
						  #$NSMouseEnteredMask
						  #$NSMouseExitedMask
						  #$NSLeftMouseDraggedMask
						  #$NSRightMouseDraggedMask
						  #$NSOtherMouseDraggedMask
						  #$NSScrollWheelMask)))
      (setf (view-event-mask mirror) event-mask)
      (port-register-mirror (port sheet) sheet mirror)
      ;; Also record the view against the (McCLIM) sheet - used to look the sheet up when we get
      ;; events which identify the view. Don't rely on 'standard' cache since it relies on 'eq
      ;; test and we need an 'eql test.
      (let ((vtable (slot-value port 'view-table)))
	(setf (gethash mirror vtable) sheet))))
  (port-lookup-mirror port sheet))

;; All mirrored-sheets (apart from the top-level pane) are view objects in Cocoa
;; From CLX/port.lisp
(defmethod realize-mirror ((port beagle-port) (sheet mirrored-sheet-mixin))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
	              (realize-mirror-aux port sheet :view 'lisp-view)))

(defmethod realize-mirror ((port beagle-port) (sheet border-pane))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
		      (realize-mirror-aux port sheet :view 'lisp-view
					  ;; CLX uses (:exposure :structure-notify) event-mask, but
					  ;; this is a NOTIFICATION in Beagle and all notifications
					  ;; are provided as CLIM events to the target sheet (well -
					  ;; all notifications that Beagle takes any notice of ;-)
					  :event-mask 0)))

;;; For now, realize frame top-level sheets as Windows on the display, and everything
;;; else as a view. From CLX/port.lisp line ~380.
;;; This one's a bit of a departure - since we don't use realize-mirror-aux. Maybe there's
;;; a way to squeeze it in (could use additional key parameters maybe...)

;;; Also note that this is "obsolete" in the CLX back end

(defmethod realize-mirror ((port beagle-port) (sheet top-level-sheet-pane))

  ;; Steps:
  ;;
  ;; 1.  Get the frame-manager associated with this "top-level" sheet since this will
  ;;     tell us if we're creating a frame (NSWindow) or if we're being embedded within
  ;;     an already-existing frame (in which case we want to be an NSView).
  ;;     Correction - we *always* want to be a view. but if there's no frame for the
  ;;     application, we need to make that too. - this one would make sense in the
  ;;     frame-manager source.
  ;;
  ;; 2.  Generate all the information we need to tell the NSWindow how big it needs
  ;;     to be, the size of the border (this can probably be defaulted - especially in
  ;;     the short-term), "override-redirect" (whatever that is), whether the window
  ;;     should be mapped (must be T since we're in the realize- method?), whether
  ;;     there's a backing store (for blitting?) and the event mask we want the window
  ;;     to respond to.
  ;;
  ;; 3.  I don't think we need to worry about bit-gravity and all that... again, at
  ;;     least not in the short-term.
  ;;
  ;; 4.  Register the mirror with the port (do we need to do anything with NSApp at
  ;;     this point to make the frame visible? - ** no - DR **)
  ;;
  ;; 5.  Return the mirror that's created.
  ;;
  ;;

  ;; We create an NSWindow that will hold the view. Then we create a view, set
  ;; this (view) as the mirror and set the mirror as the NSWindow's content
  ;; view. Then all our mirrors are instances of NSView.
  ;; *Might* (i.e. probably will!) need to hook the NSWindow into the frame
  ;; manager object otherwise we'll be creating new windows every time the
  ;; mirror hierarchy gets realized...
  
  (when (null (port-lookup-mirror port sheet)) ; Don't create a new object if one already exists
    (update-mirror-geometry sheet)             ; ?
    (let* ((top-level-frame (make-instance 'lisp-window))
	   (desired-color (typecase sheet
			    (sheet-with-medium-mixin
			     (medium-background sheet))
			    (basic-pane
			     (let ((background (pane-background sheet)))
			       (if (typep background 'color)
				   background
				 +white+)))
			    (t
			     +white+)))
	   (frame (pane-frame sheet))
	   (q (compose-space sheet))
	   (x (if (%sheet-mirror-transformation sheet)
		  (nth-value 0 (transform-position (%sheet-mirror-transformation sheet) 0 0))
		0))
	   (y (if (%sheet-mirror-transformation sheet)
		  (nth-value 1 (transform-position (%sheet-mirror-transformation sheet) 0 0))
		0))
	   (width (if (%sheet-mirror-region sheet)
		      (bounding-rectangle-width (%sheet-mirror-region sheet))
		    (space-requirement-width q)))
	   (height (if (%sheet-mirror-region sheet)
		       (bounding-rectangle-height (%sheet-mirror-region sheet))
		     (space-requirement-height q)))
	   (rect (ccl::make-ns-rect (pixel-center x) (pixel-center y)
				    (pixel-count width) (pixel-count height)))
	   (style-mask (logior #$NSTitledWindowMask
			       #$NSClosableWindowMask
			       #$NSMiniaturizableWindowMask
			       #$NSResizableWindowMask)))
      (send top-level-frame 'retain)
      ;; Should we move the window somewhere more central after it's been put up on screen?
      (send top-level-frame :init-with-content-rect rect :style-mask style-mask
	    ;; only get exposed notifications for nonretained windows
	    ;;		                 :backing #$NSBackingStoreNonretained :defer nil
	    :backing #$NSBackingStoreBuffered :defer nil
	    :screen (beagle-port-screen port))
      (send top-level-frame :set-title (%make-nsstring (frame-pretty-name frame)))
      (send top-level-frame :set-accepts-mouse-moved-events #$YES) ; Have to explicitly do this for Cocoa
      (let ((delegate (make-instance 'lisp-window-delegate)))      ; Create delegate instance...
	(send top-level-frame :set-delegate delegate))             ; ...and assign it to the window

      (let ((clim-mirror (make-instance 'lisp-view :with-frame rect)))
	(send clim-mirror 'retain)
	(send clim-mirror 'establish-tracking-rect)
	(setf (view-background-colour clim-mirror) (%beagle-pixel port desired-color))
	(setf (view-event-mask clim-mirror) 0)
	(send top-level-frame :set-content-view clim-mirror)
	(port-register-mirror (port sheet) sheet clim-mirror)
	;; Record the cocoa view against the sheet.
	(let ((vtable (slot-value port 'view-table)))
	  (setf (gethash clim-mirror vtable) sheet))
	;; Things don't work if we don't do this... hopefully it will help. Maybe it won't.
	(send top-level-frame :make-key-and-order-front nil)
	(#_free rect)))))

;;; The parent of this sheet is the NSScreen... how'd that happen? Very strange. Well, that
;;; means we can't add this sheet to its parent; so what's this sheet used for, and how
;;; is it handled? It's unmanaged but it must be attached to the hierarchy somewhere...

;;; I think we need to make THIS generate the menu frame; then the menu buttons themselves
;;; can be made a child of this unmanaged-top-level-sheet-pane. Seems a rather retarded
;;; way of doing it, but hey.
(defmethod realize-mirror ((port beagle-port) (sheet unmanaged-top-level-sheet-pane))
  (when (null (port-lookup-mirror port sheet)) ; Don't create a new object if one already exists
    (update-mirror-geometry sheet)
    (let* ((menu-frame (make-instance 'lisp-window))
	   (desired-color (typecase sheet
			    (sheet-with-medium-mixin
			     (medium-background sheet))
			    (basic-pane
			     (let ((background (pane-background sheet)))
			       (if (typep background 'color)
				   background
				 +white+)))
			    (t
			     +white+)))
;;;	   (frame (pane-frame sheet))
	   (q (compose-space sheet))
	   (x (if (%sheet-mirror-transformation sheet)
		  (nth-value 0 (transform-position (%sheet-mirror-transformation sheet) 0 0))
		0))
	   (y (if (%sheet-mirror-transformation sheet)
		  (nth-value 1 (transform-position (%sheet-mirror-transformation sheet) 0 0))
		0))
	   (width (if (%sheet-mirror-region sheet)
		      (bounding-rectangle-width (%sheet-mirror-region sheet))
		    (space-requirement-width q)))
	   (height (if (%sheet-mirror-region sheet)
		       (bounding-rectangle-height (%sheet-mirror-region sheet))
		     (space-requirement-height q)))
	   (rect (ccl::make-ns-rect (pixel-center x) (pixel-center y)
				    (pixel-count width) (pixel-count height)))
	   ;;; For a "popup" menu, we get rid of all decoration - allow the windowing system
	   ;;; (McCLIM) get rid of the menu when it's no longer needed.
	   (style-mask #$NSBorderlessWindowMask))
      ;;; How do we work out what level to display this window? Don't bother displaying relative
      ;;; for now, just use the default menu-level from Cocoa.
      (send menu-frame 'retain)
;;;      (send menu-frame 'enable-cursor-rects)  ; Otherwise we don't get them; not sure we need them for menus anyway.
;;;      (format *debug-io* "Made window for menu-frame (menu) of: ~S~%" menu-frame)
      ;; Should we move the window somewhere more central after it's been put up on screen?
      (send menu-frame :init-with-content-rect rect :style-mask style-mask
	    ;; only get exposed notifications for nonretained windows
	    ;;		                 :backing #$NSBackingStoreNonretained :defer nil
	    ;; Suspect (for popup menus) we want a non-retained window. Fiddle with later.
	    :backing #$NSBackingStoreBuffered :defer nil
	    :screen (beagle-port-screen port))
      (send menu-frame :set-accepts-mouse-moved-events #$YES) ; Have to explicitly do this for Cocoa
      (let ((delegate (make-instance 'lisp-window-delegate)))      ; Create delegate instance...
	(send menu-frame :set-delegate delegate))             ; ...and assign it to the window

      (let ((clim-mirror (make-instance 'lisp-unmanaged-view :with-frame rect)))
	(send clim-mirror 'retain)
	(send clim-mirror 'establish-tracking-rect)
	(setf (view-background-colour clim-mirror) (%beagle-pixel port desired-color))
	(setf (view-event-mask clim-mirror) 0)  ; (logior #$NSMouseEnteredMask
						;         #$NSMouseExitedMask))  ; CLX uses (:structure-notify)
	(send menu-frame :set-content-view clim-mirror)
	(port-register-mirror (port sheet) sheet clim-mirror)
	;; Record the cocoa view against the sheet.
	(let ((vtable (slot-value port 'view-table)))
	  (setf (gethash clim-mirror vtable) sheet))
;;;	(send menu-frame :set-level (ccl::%get-ptr (ccl::foreign-symbol-address "_NSPopUpMenuWindowLevel")))
	(#_free rect)
	;; Things don't work if we don't do this... hopefully it will help. Maybe it won't.
	(send menu-frame :make-key-and-order-front nil)))))

;;; menu-button-pane is actually a gadget, and this method isn't invoked when it is
;;; constructed; presumably realize-mirror isn't used for gadgets.
(defmethod realize-mirror ((port beagle-port) (sheet menu-button-pane)) ; was -> (sheet command-menu-pane))  ; CLX -> menu-button-pane
;;;  (format *debug-io* "mirror.lisp -> realize-mirror ~S with parent ~S~%" sheet (sheet-parent sheet))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
	(realize-mirror-aux port sheet :view 'lisp-view
			               :map (sheet-enabled-p sheet)
				       ;; CLX passes (:exposure :key-press :key-release
				       ;;             :button-press :button-release
				       ;;             :enter-window :leave-window
				       ;;             :structure-notify :button-motion
				       ;;             :owner-grab-button)
				       :event-mask (logior #$NSKeyDownMask
							   #$NSKeyUpMask
							   #$NSLeftMouseDownMask
							   #$NSRightMouseDownMask
							   #$NSOtherMouseDownMask
							   #$NSLeftMouseUpMask
							   #$NSRightMouseUpMask
							   #$NSOtherMouseUpMask
							   #$NSMouseEnteredMask
							   #$NSMouseExitedMask
							   #$NSScrollWheelMask))))


(defmethod realize-mirror ((port beagle-port) (sheet clim-stream-pane))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
		(realize-mirror-aux port sheet :view 'lisp-view
					       ;; CLX uses (:exposure :key-press :key-release
					       ;;           :button-press :button-release
					       ;;           :enter-window :leave-window
					       ;;           :structure-notify
					       ;;           :pointer-motion :pointer-motion-hint
					       ;;           :button-motion :owner-grab-button)
					       :event-mask (logior #$NSKeyDownMask
								   #$NSKeyUpMask
								   #$NSLeftMouseDownMask
								   #$NSRightMouseDownMask
								   #$NSOtherMouseDownMask
								   #$NSLeftMouseUpMask
								   #$NSRightMouseUpMask
								   #$NSOtherMouseUpMask
								   #$NSMouseEnteredMask
								   #$NSMouseExitedMask
								   #$NSMouseMovedMask
								   #$NSLeftMouseDraggedMask
								   #$NSRightMouseDraggedMask
								   #$NSOtherMouseDraggedMask
								   #$NSScrollWheelMask))))

(defmethod realize-mirror ((port beagle-port) (pixmap pixmap))
  (when (null (port-lookup-mirror port pixmap))
    ;; Ignore direct mirror of (pixmap-sheet pixmap) - appears to be CLX specific...
    ;; -> [[NSImage alloc] initWithSize: <NSS>ize]
;;;    (rlet ((size :<NSS>ize :width  (coerce (pixmap-width pixmap) 'short-float)
;;;		           :height (coerce (pixmap-height pixmap) 'short-float)))
;;;      (let ((pix (send (make-instance 'ns::ns-image :init-with-size size) 'retain)))
;;;	(port-register-mirror port pixmap pix)))
;;;    (values)))

    (let* (;;(desired-color +white+)
	   ;; Take the width / height from the mirror-region if there's one set, otherwise from the
	   ;; space requirement.
	   (width (coerce (pixmap-width pixmap) 'short-float))
	   (height (coerce (pixmap-height pixmap) 'short-float))
	   (mirror (make-instance 'lisp-image))) ;; :with-frame rect)))
      (send mirror 'retain)
      (slet ((size (ccl::ns-make-size width height)))
        (send mirror :set-size size))
      ;; Don't need a tracking rect since pixmaps are never 'on screen' and therefore the
      ;; mouse can't move into / out of them.
      (port-register-mirror port pixmap mirror)))
    (port-lookup-mirror port pixmap))

;; Need to fix this... remove the mirror from the sheet -> mirror hashtable when the
;; mirror is destroyed. Also need to release the main frame window when the sheet
;; being released is a top-level-sheet-pane.
(defmethod destroy-mirror ((port beagle-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (port-lookup-mirror port sheet)))
	(when mirror
	  (port-unregister-mirror port sheet (sheet-mirror sheet))
	  (when (typep sheet 'command-menu-pane)
	    (send (send mirror 'window) 'close)  ;Make sure :set-released-when-closed has been set to #$YES.
	    (send mirror 'release)
	    (return-from destroy-mirror))
	  (when (typep sheet 'top-level-sheet-pane)
	    (send (send mirror 'window) 'close))
	  (send mirror 'release))))

(defmethod destroy-mirror ((port beagle-port) (pixmap pixmap))
  (let ((mirror (port-lookup-mirror port pixmap)))
    (when mirror
      (port-unregister-mirror port pixmap mirror)  ;;(sheet-mirror pixmap))
      (send mirror 'release))))

;; The transformation and region stuff has come from CLX/port.lisp - it seemed to make sense to me
;; that it should be here instead.

;;; A note about transformations; Cocoa supports transformations natively, so it might be an idea
;;; to just set the transformation in the NSViews used throughout this backend, and then we may
;;; well be able to ignore transformations going on at the backend level. Not sure though.

;; From CLX/port.lisp - I have *no* idea if this is right 8-)

;; This method isn't described in the specification; I'm not quite sure what the transformation
;; we're creating is used for - and therefore I can't tell if it's right or not! It's a direct
;; copy of the one from CLX/port.lisp

;; COCOA NOTE: It's not actually an exact copy! We (were) taking the width + height, whereas the
;; CLX version used the x and y origin of the window (note that for CLX, that's the TOP-LEFT. For
;; Cocoa, it's the BOTTOM-LEFT. Gah. However, this provides a translation from coordinates in
;; a 0,0 origin plane (like a sheet) into coordinates in a NON-0,0 origin plane (like the mirror).
;; However, all our mirrors have their origin at 0,0 anyway so I don't think this needs to return
;; anything else. Could be wrong...

;; Not in the spec.; where's it from?
(defmethod mirror-transformation ((port beagle-port) mirror)
  (declare (ignore port))
  (slet ((frame (send mirror 'frame)))
    (make-translation-transformation (pref frame :<NSR>ect.origin.x)
				     (+ (pref frame :<NSR>ect.origin.y)        ; take account of flipped coords...
					(pref frame :<NSR>ect.size.height)))))

;; Was invoked from 'sheets.lisp', no longer referenced. ::FIXME:: (remove)
(defmethod port-set-sheet-region ((port beagle-port) (graft graft) region)
  (declare (ignore region))
  (error "port-set-sheet-region (graft) - implement me"))

;; Included in 'decls.lisp', but not used in the code. ::FIXME:: (remove)
(defmethod port-set-sheet-transformation ((port beagle-port) (graft graft) transformation)
  (declare (ignore transformation))
  (error "port-set-sheet-transformation (graft) - implement me"))

;; Is *this* the missing piece of the puzzle? - apparently not :-(
;; WAS invoked from 'sheets.lisp' but no longer.
#+nil
(defmethod port-set-sheet-region ((port beagle-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region))
  (format *debug-io* "port-set-sheet-region entered~%")
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (tr rg) (%invent-sheet-mirror-transformation-and-region sheet)
      (declare (ignore tr))
      (multiple-value-bind (x1 y1 x2 y2) (if (eql rg +nowhere+)
					     (values 0 0 0 0)
					   (bounding-rectangle* rg))
	(declare (ignore x1 y1))
	(cond ((or (<= x2 0) (<= y2 0))
	       ;; ?
	       ))

	;; Might need to dick around with the frame size depending on the sheet
	;; type; might not. Dunno.
	(slet ((bound-rect (send mirror 'bounds)))
	  (rlet ((new-rect :<NSR>ect :origin.x (pref bound-rect :<NSR>ect.origin.x)
			             :origin.y (pref bound-rect :<NSR>ect.origin.y)
				     :size.width (coerce x2 'short-float)
				     :size.height (coerce y2 'short-float)))
	    (send mirror :set-bounds new-rect)))
	y2))))


;;; I think port-set-mirror-region + port-set-mirror-transformation are indeed the key
;;; to scrolling; we can probably confirm this by getting the CLX back end running
;;; properly. However, in Cocoa, just changing these makes no difference. We also
;;; need to explicitly redraw the appropriate windows (whichever they are).

;;; Also, these methods get invoked (all the time) for mirrors whose regions +
;;; transformations ARE NOT CHANGING which seems rather wasteful. Need to catch
;;; these situations.

;;; Can probably do better than this and copy an area that's already on-screen, and
;;; just expose the bit that needs redrawing; then we can (hopefully) get better
;;; speed out of it.
(defmethod port-set-mirror-region ((port beagle-port) mirror mirror-region)

  ;; When we're asked to resize the mirror corresponding to the top-level-sheet-pane, ALSO resize the
  ;; frame (NSWindow) in which it's situated.

  ;; Handle top-level-sheet-pane case
  (when (typep (%beagle-port-lookup-sheet-for-view port mirror) 'top-level-sheet-pane)
    (slet ((frame-rect (send mirror 'frame)))
      (rlet ((rect :<NSR>ect :origin.x    (pref frame-rect :<NSR>ect.origin.x)
		   :origin.y    (pref frame-rect :<NSR>ect.origin.y)
		   :size.width  (coerce (floor (bounding-rectangle-width mirror-region)) 'short-float)
		   :size.height (coerce (floor (bounding-rectangle-height mirror-region)) 'short-float)))
	    (send (send mirror 'window) :set-frame
		  (send (send mirror 'window)
			:frame-rect-for-content-rect rect
			:style-mask (logior #$NSTitledWindowMask
					    #$NSClosableWindowMask
					    #$NSMiniaturizableWindowMask
					    #$NSResizableWindowMask))
		  :display T))))
  ;; Handle command-menu-pane case; I'd like to combine this and the previous (when ...) - note that
  ;; the two cases only differ on the style-mask.

  ;; It's not a command-menu-pane case; it's a menu-button-pane case. Move to this, then we can scrap
  ;; the 'lisp-unmanaged-view' view type. Hopefully mouse motion events would work then...
  (when	(typep mirror 'lisp-unmanaged-view)
    (slet ((frame-rect (send mirror 'frame)))
      (rlet ((rect :<NSR>ect :origin.x    (pref frame-rect :<NSR>ect.origin.x)
		   :origin.y    (pref frame-rect :<NSR>ect.origin.y)
		   :size.width  (coerce (floor (bounding-rectangle-width mirror-region)) 'short-float)
		   :size.height (coerce (floor (bounding-rectangle-height mirror-region)) 'short-float)))
	    (send (send mirror 'window) :set-frame
		  (send (send mirror 'window)
			:frame-rect-for-content-rect rect
			:style-mask #$NSBorderlessWindowMask)
		  :display T))))
  ;; We've handled the frame (if necessary) - now resize the mirror itself.
  (slet ((frame-size (send mirror 'frame)))
    (rlet ((size :<NSS>ize :width  (coerce (floor (bounding-rectangle-width mirror-region)) 'short-float)
		 :height (coerce (floor (bounding-rectangle-height mirror-region)) 'short-float)))
      (when (and (equal (pref frame-size :<NSR>ect.size.width) (pref size :<NSS>ize.width))
		 (equal (pref frame-size :<NSR>ect.size.height) (pref size :<NSS>ize.height)))
	;; No change to transformation; don't even try doing any repainting.
	(return-from port-set-mirror-region (floor (bounding-rectangle-max-y mirror-region))))
      (send mirror :set-frame-size size)))
  (floor (bounding-rectangle-max-y mirror-region)))

;;; Cocoa doesn't automatically repaint in this case (unlike CLX it seems)
;;; Note that this should already be done by update-mirror-geometry, but appears not
;;; to be... strange. Just do the repaint in port-set-mirror-transformation since
;;; both methods are invoked sequentially and in the order -region -transformation.

        ;;; Should perhaps be an :after method on update-mirror-geometry?
;;;	(climi::dispatch-repaint (port-lookup-sheet-for-view port mirror) mirror-region)))) ; Use proper region asap

(defmethod port-set-mirror-transformation ((port beagle-port) (mirror lisp-unmanaged-view)
					   mirror-transformation)
  (multiple-value-bind (i1 i2 i3 i4 x y)
      (get-transformation mirror-transformation)
      (declare (ignore i1 i2 i3 i4))
    ;; This is why menus aren't being shown in the right place; because they are frames
    ;; (borderless, admittedly) the coords provided for them aren't flipped (parent =
    ;; screen). Hence, they're at the bottom instead of the top. Can fix now I've realized
    ;; what the problem is ;-)
    (send (send mirror 'window) :set-frame-top-left-point
	  (ns-make-point (coerce x 'short-float) (coerce y 'short-float)))
    (send (send mirror 'window) :make-key-and-order-front nil))
  (floor (nth-value 1 (transform-position mirror-transformation 0 0))))
		
;;; From CLX/port.lisp
(defmethod port-set-mirror-transformation ((port beagle-port) mirror mirror-transformation)
  (slet ((mirror-bounds (send mirror 'bounds))
	 (frame-origin (send mirror 'frame)))  ;position + size _in parent_
    (rlet ((point :<NSP>oint
		  :x (coerce (floor (nth-value 0 (transform-position mirror-transformation 0 0))) 'short-float)
		  :y (coerce (floor (nth-value 1 (transform-position mirror-transformation 0 0))) 'short-float)))
      (when (and (equal (pref frame-origin :<NSR>ect.origin.x) (pref point :<NSP>oint.x))
		 (equal (pref frame-origin :<NSR>ect.origin.y) (pref point :<NSP>oint.y)))
	;; No change to transformation; do even try doing any repainting.
	(return-from port-set-mirror-transformation (floor (nth-value
							    1
							    (transform-position mirror-transformation 0 0)))))
      ;; From Cocoa NSView documentation:-
      ;; Sets the origin of the receiver's frame rectangle to newOrigin, effectively repositioning it within its
      ;; superview. This method neither redisplays the receiver nor marks it as needing display. You must do this
      ;; yourself with display or setNeedsDisplay:.
      (send mirror :set-frame-origin point)
      ;; Should perhaps be an :after method on update-mirror-geometry? This *should* be done automatically
      ;; by 'care-for-new-native-transformation' (see climsource:sheets.lisp), but that method never
      ;; seems to be invoked...
      (climi::dispatch-repaint (%beagle-port-lookup-sheet-for-view port mirror)
;;;			       (untransform-region (sheet-device-transformation (port-lookup-sheet-for-view
;;;										 port mirror))
;;;						   (%beagle-region-from-ns-rect mirror-bounds)))
			       (untransform-region mirror-transformation
						   (%beagle-region-from-ns-rect mirror-bounds)))

      ))
  (floor (nth-value 1 (transform-position mirror-transformation 0 0))))

(defun %beagle-region-from-ns-rect (rect)
  (make-bounding-rectangle (pref rect :<NSR>ect.origin.x)
			   (pref rect :<NSR>ect.origin.y)
			   (+ (pref rect :<NSR>ect.origin.x) (pref rect :<NSR>ect.size.width))
			   (+ (pref rect :<NSR>ect.origin.y) (pref rect :<NSR>ect.size.height))))

;;; Nabbed from CLX backend port.lisp - however, I think it's wrong. This (and the CLX) method
;;; actually attempt to put the sheet up on screen. I suspect it only needs to set a flag, and
;;; invoke "notify-sheet-enabled". Maybe not :-)  In any case, I think this gets called for every
;;; sheet in the hierarchy, and we certainly don't want to push the window onto screen for all of
;;; them!
;;; A few comments about how the current clim implementation works would be nice.

;;; Not in spec.; where's it from? Invoked from 'sheets.lisp'. No concept of 'enabling'
;;; a sheet in the spec....
(defmethod port-enable-sheet ((port beagle-port) (sheet mirrored-sheet-mixin))
  (when (null (port-lookup-mirror port sheet))
	(error "port-enable-sheet: can't enable sheet with no mirror"))
  (let ((window (send (port-lookup-mirror port sheet) 'window)))
	(unless (send window 'is-key-window)
	  (send window :make-key-and-order-front nil))))

;; Not in spec. Possibly invoked from sheets.lisp. ::FIXME::
(defmethod port-disable-sheet ((port beagle-port) (mirror mirrored-sheet-mixin))
  (error "port-disable-sheet: implement me"))

;;; From CLX/port.lisp - hrm. What the heck is this doing exactly?
;;; I suspect (though can't be sure) that a proper implementation of grafts might
;;; make all this much, much easier.
(defun %invent-sheet-mirror-transformation-and-region (sheet)
  (let* ((r (sheet-region sheet))                                 ; sheet region (origin, width, height) in "imaginary" units
	 (r* (transform-region  
	      (sheet-native-transformation (sheet-parent sheet))  ; native == user transformation. Of the *parent*
	      (transform-region (sheet-transformation sheet) r))) ; transfrms sheet coords -> parnt coords retrning a region
	 ;; Now r = sheet's region, r* = same region in user coord system of the sheet's *parent*
	 (mirror-transformation
	  (if (region-equal r* +nowhere+)
	      (make-translation-transformation 0 0)               ; if r* isn't a valid region, make the mirror
	    (make-translation-transformation                      ;           transformation a noop translation
	     (bounding-rectangle-min-x r*)                        ; otherwise make it a translation to min-x, min-y.
	     (bounding-rectangle-min-y r*))))
	 ;; Mirror transformation is always a translation only since r* is already in the parent's native coord system
	 (mirror-region
	  (untransform-region mirror-transformation r*)))         ; Now we have the mirror transformation, untransform r* to get the required region. This would give us... um. ::FIXME::
    (values
     mirror-transformation
     mirror-region)))
