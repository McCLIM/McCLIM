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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; REALIZE-MIRROR-AUX is invoked by the different REALIZE-MIRROR methods.
;;; These are invoked by the core when a sheet is GRAFTED (specifically
;;; from the sheet's NOTE-SHEET-GRAFTED method).

;;; Sheets are GRAFTED after they are adopted by a sheet that is itself
;;; grafted; see SHEET-ADOPT-CHILD :AFTER method in SHEETS.LISP

;;; It appears that when the GRAFT (automatically grafted) ADOPTS a
;;; sheet, a MIRROR is created via the NOTE-SHEET-GRAFTED method.

;;; So we can say at this point that sheets will definately be grafted,
;;; but not necessarily have a MEDIUM associated (which seems very bad
;;; to me). [aside: some sheets in McCLIM are mirrored, and NEVER have
;;; a medium associated. This violates the spec, if nothing else.]

;;; Sheets can't possibly have a mirror transformation at this point, since
;;; we're in the process of creating the mirror... if any mirror
;;; transformation exists, it must surely be +IDENTITY-TRANSFORMATION+.
;;; UPDATE: at mirror realization time, (%sheet-mirror-transformation) is
;;; ALWAYS +identity-transformation+.

;;; _SUSPECT_ we can say the same about the sheet-mirror-region; this
;;; is presumably set to some default. (%sheet-mirror-region) ALWAYS
;;; returns NIL at the point of mirror realization (which seems to not
;;; be unreasonable).

;;; When a mirror is realized, the NSView native object created is added
;;; as a child of the mirror (NSView) via the :add-subview message. This
;;; means that any PARENTS of the current sheet MUST be MIRRORED (and
;;; therefore grafted) BEFORE this sheet. This doesn't seem too
;;; unreasonable.

;;; QUERY: must a sheet be grafted in order to be mirrored?

;;; The exceptions to this are TOP-LEVEL-SHEET-PANE, for which we create
;;; a 'FRAME' (a native window). Suspect this should happen at a
;;; different part of the process, but... it doesn't. Yet. ::FIXME::

;;; Note that when the TOP-LEVEL-SHEET-PANE is realized, the sheet DOES
;;; have a FRAME associated with it (PANE-FRAME sheet). We *should*
;;; create the NSWindow native object when this frame is associated.

;;; The same 'rules' regarding mirror-transformation and mirror-region as
;;; mentioned above still apply for TOP-LEVEL-SHEET-PANEs. Note also that
;;; in CLX the realize method for these is marked as 'obsolete' so there
;;; may be a better way of mirroring them.

;;; Personally I don't think we should be invoking 'update-mirror-
;;; geometry' in the realization process. This should be implicit from the
;;; bounds of the sheet, wrt the size of the graft (screen).

;;; Realization methods (even for top level sheets) do NOT put the window
;;; up on screen; this is done elsewhere.

;;; The UNMANAGED-TOP-LEVEL-SHEET-PANE *also* requires its own window.
;;; These panes are used for popup and command menus (although I suspect
;;; this *isn't* what the spec. intends when talking about COMMAND-MENU-
;;; PANE types.

;;; For UNMANAGED-TOP-LEVEL-SHEET-PANEs, the transformation is initially
;;; +identity-transformation+, but the region is 0,0 x 100,100 (which is
;;; a default; appears not to take into account any actual size
;;; requirement)

;;; Also, for COMMAND-MENU panes, the position they are drawn is incorrect.
;;; This is (I think) because they are WINDOWS and therefore need to be
;;; positioned relative to the GRAFT; currently they are being treated as
;;; sheets (NSViews), and positioned relative to the origin of the window
;;; in which they appear. This is wrong. ::FIXME::

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Event masks for the different pane types we realize in Beagle. Defined
;;; here for easy comparison (and it makes the code a *little* clearer I
;;; think).


;;; Group Cocoa events together to give 'event groupings'.
(defconstant +key-events+ (logior #$NSKeyDownMask #$NSKeyUpMask))

(defconstant +mouse-motion-events+ (logior #$NSMouseMovedMask
					   #$NSLeftMouseDraggedMask
					   #$NSRightMouseDraggedMask
					   #$NSOtherMouseDraggedMask))

(defconstant +mouse-button-events+ (logior #$NSLeftMouseDownMask
					   #$NSRightMouseDownMask
					   #$NSOtherMouseDownMask
					   #$NSLeftMouseUpMask
					   #$NSRightMouseUpMask
					   #$NSOtherMouseUpMask))

(defconstant +enter-exit-events+ (logior #$NSMouseEnteredMask #$NSMouseExitedMask))

(defconstant +scroll-wheel-events+ #$NSScrollWheelMask)

(defconstant +ignores-events+ 0)


;;; Define which 'event groups' specific sheet types are interested in.
;;; ::TODO:: define more of these; prevent as many sheets as possible
;;;          from responding to events (particularly layout panes, like
;;; vrack-pane etc.)
(defconstant +border-pane-event-mask+ +ignores-events+)

(defconstant +menu-button-pane-event-mask+ (logior +key-events+
						   +mouse-button-events+
						   +enter-exit-events+
						   +mouse-motion-events+
						   +scroll-wheel-events+))

(defconstant +clim-stream-pane-event-mask+ (logior +key-events+
						   +mouse-button-events+
						   +enter-exit-events+
						   +mouse-motion-events+
						   +scroll-wheel-events+))

(defconstant +mirrored-sheet-mixin-event-mask+ (logior +key-events+
						       +mouse-button-events+
						       +enter-exit-events+
						       +mouse-motion-events+
						       +scroll-wheel-events+))

(defconstant +top-level-sheet-pane-event-mask+ +ignores-events+)

(defconstant +unmanaged-top-level-sheet-pane-event-mask+ +ignores-events+)


;;; Define window style masks for the two types of window used currently
;;; in Beagle.
(defconstant +decorated-window-style-mask+ (logior #$NSTitledWindowMask
						   #$NSClosableWindowMask
						   #$NSMiniaturizableWindowMask
						   #$NSResizableWindowMask))

(defconstant +plain-window-style-mask+ #$NSBorderlessWindowMask)


(defun %beagle-make-window (screen rect &key (name nil) (decorated t))
"Creates a native window (NSWindow) on the screen indicated by 'screen'
with the internal (content rectangle) bounds 'rect'. If the window is
'decorated' (i.e. has maximise, minimise and close buttons) set the
window name to 'name' (if 'name' is non-NIL).

'rect' specifies both the internal (bounds) and external (frame) size
for windows that are not decorated."
  (let* ((window (make-instance 'lisp-window))
	 (style-mask (if decorated
			 +decorated-window-style-mask+
		       +plain-window-style-mask+))
	 (delegate (make-instance 'lisp-window-delegate)))
    (send window 'retain)
    ;; Could use the following in the init form below:-
    ;; :backing #$NSBackingStoreNonretained :defer nil
    ;; Suspect (for popup menus) we might want a non-retained window.
    (send window :init-with-content-rect rect :style-mask style-mask
	  :backing #$NSBackingStoreBuffered :defer nil
	  :screen screen)
    (when (and name decorated)
      (send window :set-title name))
    (send window :set-accepts-mouse-moved-events #$YES)
    (send window :set-delegate delegate)
    (send window :set-released-when-closed #$YES)
    window))

 
;;; This is a nasty hack; should pick the sheet background colour up from
;;; the sheet medium, but since not all mirrored sheets have mediums (!!)
;;; that's not possible.
;;; Would prefer to use (medium-background sheet) instead of the following.
(defun %beagle-sheet-background-colour (sheet)
  (typecase sheet
    (sheet-with-medium-mixin (medium-background sheet))
    (basic-pane
     (let ((background (pane-background sheet)))
       (if (typep background 'color)
	   background
	 +white+)))
    (t +white+)))
  

(defun %beagle-mirror->sheet-assoc (port mirror sheet)
  ;; Record the view against the (CLIM) sheet - used to look the sheet up when we get
  ;; events which identify the view. Don't rely on 'standard' cache since it relies on 'eq
  ;; test and we need an 'eql test.
  (let ((vtable (slot-value port 'view-table)))
    (setf (gethash mirror vtable) sheet)))


;;; For now, realize frame top-level sheets as Windows on the display, and everything
;;; else as a view. From CLX/port.lisp line ~380.

;;; Note that this is "obsolete" in the CLX back end
(defmethod realize-mirror ((port beagle-port) (sheet top-level-sheet-pane))

  ;; We create an NSWindow that will hold the view. Then we create a view, set
  ;; this (view) as the mirror and set the mirror as the NSWindow's content
  ;; view. Then all our mirrors are instances of NSView.

  (when (null (port-lookup-mirror port sheet))
    (update-mirror-geometry sheet)
    (let* ((desired-color   (%beagle-sheet-background-colour sheet))
	   (frame           (pane-frame sheet))
	   (q               (compose-space sheet))
	   (x               0)
	   (y               0)
	   (width           (space-requirement-width q))
	   (height          (space-requirement-height q))
	   (rect            (make-ns-rect (pixel-center x) (pixel-center y)
					  (pixel-count width) (pixel-count height)))
	   (name            (%make-nsstring (frame-pretty-name frame)))
	   (top-level-frame (%beagle-make-window (beagle-port-screen port)
						 rect
						 :name name
						 :decorated t))
	   (clim-mirror     (make-instance 'lisp-view :with-frame rect)))
      (send clim-mirror 'retain)
      (send clim-mirror 'establish-tracking-rect)
      (setf (view-background-colour clim-mirror) (%beagle-pixel port desired-color))
      (setf (view-event-mask clim-mirror) +top-level-sheet-pane-event-mask+)
      (send top-level-frame :set-content-view clim-mirror)
      (port-register-mirror (port sheet) sheet clim-mirror)
      (%beagle-mirror->sheet-assoc port clim-mirror sheet)
      (#_free rect)
      clim-mirror)))  ; <- (port-lookup-mirror port sheet)?


;;; This generates the 'menu frame'; then the menu buttons themselves
;;; can be made a child of this unmanaged-top-level-sheet-pane. Seems a rather retarded
;;; way of doing it, but hey.
(defmethod realize-mirror ((port beagle-port) (sheet unmanaged-top-level-sheet-pane))
  (when (null (port-lookup-mirror port sheet)) ; Don't create a new object if one already exists
    (update-mirror-geometry sheet)
    (let* ((desired-color (%beagle-sheet-background-colour sheet))
	   (q             (compose-space sheet))
	   (x             0)
	   (y             0)
	   (width         (space-requirement-width q))
	   (height        (space-requirement-height q))
	   (rect          (make-ns-rect (pixel-center x) (pixel-center y)
					(pixel-count width) (pixel-count height)))
	   (menu-frame    (%beagle-make-window (beagle-port-screen port) rect :decorated nil))
	   (clim-mirror   (make-instance 'lisp-view :with-frame rect)))
	(send clim-mirror 'retain)
	(send clim-mirror 'establish-tracking-rect)
	(setf (view-background-colour clim-mirror) (%beagle-pixel port desired-color))
	(setf (view-event-mask clim-mirror) +unmanaged-top-level-sheet-pane-event-mask+)
	(send menu-frame :set-content-view clim-mirror)
	(port-register-mirror (port sheet) sheet clim-mirror)
	(%beagle-mirror->sheet-assoc port clim-mirror sheet)
	(#_free rect)
	clim-mirror)))


(defun realize-mirror-aux (port sheet &key (view 'lisp-view) (event-mask +ignores-events+))
  ;; Current all realized views are instances of LISP-VIEW. It's conceivable
  ;; that in the future different native types will be used for different
  ;; views (as was the case in the past) but this seems unlikely. Is there
  ;; then any value to retaining the :view keyword?
  (when (null (port-lookup-mirror port sheet))
    (update-mirror-geometry sheet)
    (let* ((desired-color (%beagle-sheet-background-colour sheet))
	   (x             0)
	   (y             0)
	   (q             (compose-space sheet))
	   (width         (space-requirement-width q))
	   (height        (space-requirement-height q))
	   (rect          (make-ns-rect (pixel-center x) (pixel-center y)
					(pixel-count width) (pixel-count height)))
	   (mirror        (make-instance view :with-frame rect)))
      (#_free rect)
      (send mirror 'retain)
      (send mirror 'establish-tracking-rect)
      (setf (view-background-colour mirror) (%beagle-pixel port desired-color))
      (setf (view-event-mask mirror) event-mask)
      (port-register-mirror (port sheet) sheet mirror)
      (%beagle-mirror->sheet-assoc port mirror sheet)))
  (port-lookup-mirror port sheet))


;; All mirrored-sheets (apart from the top-level pane) are view objects in Cocoa
(defmethod realize-mirror ((port beagle-port) (sheet mirrored-sheet-mixin))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
	              (realize-mirror-aux port sheet
					  :event-mask +mirrored-sheet-mixin-event-mask+)))


(defmethod realize-mirror ((port beagle-port) (sheet border-pane))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
		      (realize-mirror-aux port sheet :event-mask +border-pane-event-mask+)))


(defmethod realize-mirror ((port beagle-port) (sheet menu-button-pane))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
	(realize-mirror-aux port sheet :event-mask +menu-button-pane-event-mask+)))


(defmethod realize-mirror ((port beagle-port) (sheet clim-stream-pane))
  (send (sheet-mirror (sheet-parent sheet)) :add-subview
		(realize-mirror-aux port sheet :event-mask +clim-stream-pane-event-mask+)))


(defmethod realize-mirror ((port beagle-port) (pixmap pixmap))
  (when (null (port-lookup-mirror port pixmap))
    (let* ((width  (cg-floatify (pixmap-width pixmap)))
	   (height (cg-floatify (pixmap-height pixmap)))
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
	    ;; Memory for NSWindow is released if :set-released-when-closed = #$YES.
	    (send (send mirror 'window) 'close)
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
  (slet ((frame (send mirror 'frame)))  ; location of NSView in its *parent*
    (make-translation-transformation (pref frame :<NSR>ect.origin.x)
				     (+ (pref frame :<NSR>ect.origin.y)   ; consider flipped coords...
					(pref frame :<NSR>ect.size.height)))))

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
  ;; frame (NSWindow) in which it's situated. Presumably this is done automatically in CLX?

  (let ((sheet (%beagle-port-lookup-sheet-for-view port mirror)))
    ;; Handle unmanaged/top-level-sheet-pane cases
    ;; nb: UNMANAGED-TOP-LEVEL-SHEET-PANEs *ARE* instances of TOP-LEVEL-SHEET-PANEs, so we
    ;;     don't need to test for both...
    (when (typep sheet 'top-level-sheet-pane)
      (%beagle-set-frame-region sheet mirror mirror-region))

    ;; We've handled the frame (if necessary) - now resize the mirror itself.
    (slet ((frame-size (send mirror 'frame)))
      (rlet ((size :<NSS>ize :width  (cg-floatify (floor (bounding-rectangle-width mirror-region)))
		   :height (cg-floatify (floor (bounding-rectangle-height mirror-region)))))
	;; ignore this (for now)
	#+nil
        (when (and (equal (pref frame-size :<NSR>ect.size.width) (pref size :<NSS>ize.width))
		   (equal (pref frame-size :<NSR>ect.size.height) (pref size :<NSS>ize.height)))
	  ;; No change to transformation; don't even try doing any repainting.
	  (return-from port-set-mirror-region (floor (bounding-rectangle-max-y mirror-region))))
	(send mirror :set-frame-size size)))))


(defun %beagle-set-frame-region (sheet mirror mirror-region)
  (slet ((frame-rect (send mirror 'frame)))
    (rlet ((rect :<NSR>ect :origin.x    (pref frame-rect :<NSR>ect.origin.x)
		 :origin.y    (pref frame-rect :<NSR>ect.origin.y)
		 :size.width  (cg-floatify (floor (bounding-rectangle-width mirror-region)))
		 :size.height (cg-floatify (floor (bounding-rectangle-height mirror-region)))))
      (send (send mirror 'window) :set-frame
	    (send (send mirror 'window)
		  :frame-rect-for-content-rect rect
		  :style-mask (%beagle-style-mask-for-frame sheet))
	    :display t))))


(defun %beagle-style-mask-for-frame (sheet)
"Returns the appropriate native 'style mask' for the frame containing
'sheet', which must be a TOP-LEVEL-SHEET-PANE instance.
If invoked on any other kind of sheet, returns NIL."
  ;; Since UNMANAGED-top-level-sheet-pane objects are also top-level-sheet-pane objects,
  ;; but not the other way around, test for the most specific pane type here. Otherwise
  ;; the wrong style-mask is returned, with peculiar results (well. Not that peculiar!)
  (when (typep sheet 'top-level-sheet-pane)
    (if (typep sheet 'unmanaged-top-level-sheet-pane)
	+plain-window-style-mask+
      +decorated-window-style-mask+)))       ; Must be 'top-level-sheet-pane


;;; ::FIXME::

;;; Menu-frames from a right-click are displayed in the correct place; set to the
;;; click location in the graft (I guess - check). However, 'drop down' menus are
;;; not drawn in the right place; an example would be the transform (1 0 0 1 76 29).
;;; This is relative to the APPLICATION-FRAME that contains the menu. How do we
;;; distinguish which is which? Even if we can distinguish, how do we work out the
;;; appropriate location? How does CLX work out the differences?

;;; Would like to get rid of the next method, but things are even worse when we do so...
;;; It's likely it can be sorted out.

;;; Parent of unmanaged-top-level-sheet-pane = graft (as you'd expect).
(defun %beagle-port-move-mirror-window (port mirror mirror-transformation)

  ;; It *looks* like we need to check the contents of the frame; just output
  ;; them (for now) [want pane hierarchy, and also the frames of those panes].

  ;; From reading PANES.LISP it looks like the 'drop-down' menu will contain
  ;; a 'command-menu' whose frame is the *application-frame* rather than the
  ;; *menu-frame*. Sigh.

  ;; Having checked this with debug (below), *every* pane is a child of the
  ;; LISTENER frame (in this instance; I was running the Listener ;-). So
  ;; even though a MENU-FRAME is created, its contents appear not to be owned
  ;; by it.
  
  ;; "Couriouser and couriouser!" cried Alice.
  
  ;; Debug only; remove later
  #+nil
  (let ((sheet (%beagle-port-lookup-sheet-for-view port mirror)))
    (%beagle-debug-assist-dump-sheet-hierarchy sheet))
;;;    (return-from %beagle-port-move-mirror-window))

  (multiple-value-bind (i1 i2 i3 i4 x y)
      (get-transformation mirror-transformation)
      (declare (ignore i1 i2 i3 i4))

      (let ((sheet (%beagle-port-lookup-sheet-for-view port mirror)))
      
	(cond ((and (typep sheet 'top-level-sheet-pane)
		    (not (typep sheet 'unmanaged-top-level-sheet-pane)))
	       ;; Dealing with application-frame top level sheet. Flip y by height of graft, and
	       ;; stick it there.
	       (setf y (- (graft-height (graft sheet)) y)))
	  
	      ;; Otherwise, we're dealing with a menu-frame top level sheet (unmanaged-top-level-
	      ;; -sheet-pane). Move relative to the APPLICATION-frame associated with this pane.
      
	      ;; Just using x and y directly for 'context' menus works, but not for 'drop down' menus.
	      ;; Go figure. At least, it *appears* to work. I actually suspect that it may not, hence
	      ;; the poor pointer motion tracking on these windows. Or perhaps it works, but for the
	      ;; wrong reasons.

	      ;; For other menus (i.e. those that *do not* contain a COMMAND-MENU-PANE in the
	      ;; sheet hierarchy), we need to do the massaging. This is the only way I've found to
	      ;; distinguish between the two cases (which sucks donkey balls btw).

	      ((not (%beagle-sheet-hierarchy-contains-command-menu-pane sheet))
	       ;; Horrific. Deal with 'drop down' menu case (i.e. hierarchy doesn't contain
	       ;; a 'command-menu-pane' instance)

	       ;; This doesn't work; will have to try plan B instead :-(
;;;	       (multiple-value-bind (w h frame-x frame-y) (climi::frame-geometry* (pane-frame sheet))
;;;		 (declare (ignore w h))
	       
	       ;; Plan B is to find the mirror for the top-level-sheet of the application-frame,
	       ;; and get its (x, y) in GRAFT coords. Then ADD the 'transformation x' to the graft
	       ;; x, and SUBTRACT the 'transformation y' from the graft y, giving the (x, y) we
	       ;; want to use. Then we need to add in the height of the window to find the
	       ;; top-left point of the 'drop down' menu frame.

	       (let* ((app-tls (frame-top-level-sheet (pane-frame sheet)))
		      (tls-mirror (port-lookup-mirror port app-tls))
		      (tls-window (send tls-mirror 'window))
		      (origin-pt (make-ns-point 0.0 0.0)))
		 (slet ((frame-pt (send tls-window :convert-base-to-screen origin-pt))
			(tls-bounds (send tls-mirror 'bounds)))
		   (#_free origin-pt)
		   (let ((frame-x (pref frame-pt :<NSP>oint.x))
			 (frame-y (pref frame-pt :<NSP>oint.y))
			 (tls-height (pref tls-bounds :<NSR>ect.size.height)))
		     ;; x, y relative to frame; increment x by frame x -> location in graft
		     (setf x (+ x frame-x))
		     ;; Increment y by frame y -> location in graft (flipped)
;;;		     (setf y (+ y frame-y))
		     (setf y (- (+ frame-y tls-height) y)))))))
	
	(let ((point (make-ns-point x y)))
	  (send (send mirror 'window) :set-frame-top-left-point point)
	  (#_free point)))))


;;; debug only. remove later
(defun %beagle-debug-assist-dump-sheet-hierarchy (sheet)
  (format *trace-output* "~%Got sheet ~a with frame ~a~%" sheet (pane-frame sheet))
  (loop for child in (sheet-children sheet)
	do (%beagle-debug-assist-dump-sheet-hierarchy child)))


(defun %beagle-sheet-hierarchy-contains-command-menu-pane (sheet)
  ;; This is rather ugly; must be a way to do this better...
  (if (typep sheet 'command-menu-pane)
      t
    (let ((children (sheet-children sheet)))
      (dolist (child children)
	(when (%beagle-sheet-hierarchy-contains-command-menu-pane child)
	    (return-from %beagle-sheet-hierarchy-contains-command-menu-pane t)))
      nil)))


(defmethod port-set-mirror-transformation ((port beagle-port) mirror mirror-transformation)

  ;; Check if we're changing the transformation of a top level sheet pane; if we are,
  ;; move the frame rather than shifting the transformation. Otherwise, shift the
  ;; NSView and redraw for the changed origin.

  ;; NB. when we do this the other way around ('mirror-transformation', above) we furtle
  ;;     with the origin and flip it by the size of the window; do we need to do something
  ;; similar when we're SETTING it too? It might make sense to do so...

  ;; NB(2). mirror transformations for unmanaged top level sheet panes are strange things;
  ;; those displayed for 'drop down' menus appear to be specified relative to the window
  ;; over which they appear (for example [1 0 0 1 2 29]). Those for 'pop up' menus appear
  ;; to be displayed relative to the graft (for example [1 0 0 1 608 425]).

  ;; In order to 'have all this work' I think we need more information than we actually
  ;; have available. Boo.

  (if (typep (%beagle-port-lookup-sheet-for-view port mirror) 'top-level-sheet-pane)
      (%beagle-port-move-mirror-window port mirror mirror-transformation)
    (slet ((mirror-bounds (send mirror 'bounds))
	   (frame-origin (send mirror 'frame)))  ;position + size _in parent_
      (let* ((x (floor (nth-value 0 (transform-position mirror-transformation 0 0))))
	     (y (floor (nth-value 1 (transform-position mirror-transformation 0 0))))
	     (point (make-ns-point x y)))
	;; Skip this (for now...)
	#+nil
	(when (and (equal (pref frame-origin :<NSR>ect.origin.x) x)
		   (equal (pref frame-origin :<NSR>ect.origin.y) y))
	  ;; No change to transformation; don't even try doing any repainting.
	  (#_free point)
	  (return-from port-set-mirror-transformation nil))
      
	;; From Cocoa NSView documentation:-
	;; Sets the origin of the receiver's frame rectangle to newOrigin, effectively repositioning
	;; it within its superview. This method neither redisplays the receiver nor marks it as
	;; needing display. You must do this yourself with display or setNeedsDisplay:.
	(send mirror :set-frame-origin point)
	(#_free point)
      
	;; Should perhaps be an :after method on update-mirror-geometry? This *should* be done
	;; automatically by 'care-for-new-native-transformation' (see climsource:sheets.lisp), but
	;; that method never seems to be invoked... UPDATE: it *is* invoked, but only when the
	;; mirror grows to be more than 2^16. This is an X limitation, apparently.
	(climi::dispatch-repaint (%beagle-port-lookup-sheet-for-view port mirror)
				 (%beagle-repaint-region port
							 mirror
							 mirror-transformation
							 mirror-bounds))))))


(defun %beagle-repaint-region (port mirror mirror-transformation mirror-bounds)
  ;; Should use the device transformation, but since this is invoked on MIRRORED sheets
  ;; that don't have a medium, we lose there :-(  Depending on which form we end up
  ;; using, different parameters will be needed. Sort out once it's stable.
  (declare (ignorable port mirror mirror-transformation mirror-bounds))
  #+nil
  (untransform-region (sheet-device-transformation (port-lookup-sheet-for-view port mirror))
		      (%beagle-region-from-ns-rect mirror-bounds))

  (untransform-region mirror-transformation (%beagle-region-from-ns-rect mirror-bounds)))


(defun %beagle-region-from-ns-rect (rect)
  (make-bounding-rectangle (pref rect :<NSR>ect.origin.x)
			   (pref rect :<NSR>ect.origin.y)
			   (+ (pref rect :<NSR>ect.origin.x) (pref rect :<NSR>ect.size.width))
			   (+ (pref rect :<NSR>ect.origin.y) (pref rect :<NSR>ect.size.height))))

;;; Unused by Beagle; possibly unnecessary altogether.
(defmethod port-enable-sheet ((port beagle-port) (sheet mirrored-sheet-mixin))
  t)


;; Not in spec. Possibly invoked from sheets.lisp. ::FIXME::
(defmethod port-disable-sheet ((port beagle-port) (mirror mirrored-sheet-mixin))
  (error "port-disable-sheet: implement me"))


