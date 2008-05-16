;;; -*- Mode: List; Package: BEAGLE -*-

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

(defclass beagle-medium (basic-medium)
  ((bezier-path :initform nil :accessor medium-bezier-path)
   (native-font :initform nil :accessor medium-native-font)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-drawable medium                            [Generic function]

;;; Returns an implementation-dependent object that corresponds to the
;;; actual host window that will be drawn on when the medium medium is
;;; drawn on. If medium is not grafted to a sheet or the medium's sheet
;;; is not currently mirrored on a display server, medium-drawable
;;; returns nil.

;;; Programmers can use this function to get a host window system object
;;; that can be manipulated using the functions of the host window
;;; system. This might be done in order to explicitly trade of
;;; performance against portability.

(defmethod medium-drawable ((medium beagle-medium))
  (and (graft medium)
       (port-lookup-mirror (port medium) (medium-sheet medium))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; make-medium port sheet [Generic function] 

;;; Creates a new medium for the port port. The new medium will have its default
;;; characteristics determined by sheet.

;;; I guess this isn't quite right; should we be grafting the sheet at this
;;; point in the process? Seems rather too early to me...

;;; Also, mediums should be held as resources... rather than rebuilding them
;;; each time. At least then they can be reused. See how bezier path types
;;; are handled in 'with-beagle-medium'. (actually, don't. Bezier paths are
;;; not currently stored as resources; I think there are problems in the
;;; resource code, but need to run some tests to be sure.)
(defmethod make-medium ((port beagle-port) sheet)
  (let ((medium (make-instance 'beagle-medium 
			       :sheet sheet)))
    (setf (medium-bezier-path medium) (make-instance 'lisp-bezier-path))
    (send (medium-bezier-path medium) 'retain)
    (setf (medium-native-font medium) (%text-style->beagle-font (or (medium-text-style medium)
								    *default-text-style*)))
    medium))

(defmethod deallocate-medium ((port beagle-port) (medium beagle-medium))
  (send (medium-bezier-path medium) 'release))

(defmethod (setf medium-text-style) :before (text-style (medium beagle-medium))
  (unless (equal (medium-text-style medium) text-style)
    (setf (medium-native-font medium) (%text-style->beagle-font (or text-style
								    *default-text-style*)))))

(defmethod (setf medium-line-style) :before (line-style (medium beagle-medium))
  (unless (equal (medium-line-style medium) line-style)
    (let ((width (cg-floatify (line-style-thickness line-style)))
	  (cap (%translate-cap-shape (line-style-cap-shape line-style)))
	  (dashes (line-style-dashes line-style))
	  (join (%translate-joint-shape (line-style-joint-shape line-style))))
      (send (medium-bezier-path medium) :set-line-cap-style cap)
      (send (medium-bezier-path medium) :set-line-join-style join)
      (send (medium-bezier-path medium) :set-line-width width)

      (when dashes
	(when (eq dashes t)
	  ;; Provide default dash pattern... no idea why, but when I use
	  ;; #(5.0 5.0) as the dafault dash, it gets displayed as a solid
	  ;; line (no dashing). So the default is larger than it needs to
	  ;; be. Weird, but at least this works.
	  (setf dashes #(5.0 5.0 5.0 5.0)))

	(when (and dashes
		   (or (listp dashes)
		       (vectorp dashes)))
	  (assert (evenp (length dashes)))

	  ;; CLIM specifies that the pattern starts with an UNPAINTED segment and finishes
	  ;; with a PAINTED segment, Cocoa is the other way around, so add an 'empty'
	  ;; segment to the front and end of the line; prefix and prepend '0.0' onto 'dashes'.

	  (let ((size (* (+ 2 (length dashes)) 4)))
	    (ccl::%stack-block ((beagle-dash size))
              (setf (ccl::%get-single-float beagle-dash 0) 0.0)
	      (dotimes (i (length dashes))
		(setf (ccl::%get-single-float beagle-dash (* (1+ i) 4)) (elt dashes i)))
	      (setf (ccl::%get-single-float beagle-dash (- 4 size)) 0.0)

	      (send (medium-bezier-path medium) :set-line-dash beagle-dash
		    :count (+ 2 (length dashes))
		    :phase 0.0)))))

    )))


;; I don't believe this will solve the problem with the device transform...
(defmethod (setf medium-clipping-region) :after (region (medium beagle-medium))
  (declare (ignore region))
  
  ;; This sucks, but if we DON'T specifically ask for the medium-device-region,
  ;; it never gets calculated; and then things get stuffed up with the
  ;; transformations etc.

  ;; I think this is a bug in McCLIM...
  
  (let ((clipping-region (medium-device-region medium)))
    (if (region-equal clipping-region +nowhere+)
	#()
      (%clipping-region->rect-seq clipping-region)))
  ;; Should actually do something with the clipping region at some point!

  ;; In CLX, this (medium-device-region ...) is also invoked from
  ;; (medium-gc ...) when the ink is a COLOR (but not otherwise...)
  )

;; Shamelessly stolen from CLX, as usual...
(defun %clipping-region->rect-seq (clipping-region)
  (loop
     for region in (nreverse (mapcan
			      (lambda (v) (unless (eq v +nowhere+) (list v)))
			      (region-set-regions clipping-region
						  :normalize :y-banding)))
     as rectangle = (bounding-rectangle region)
     for clip-x = (rectangle-min-x rectangle)
     for clip-y = (rectangle-min-y rectangle)
     nconcing (list clip-x
		    clip-y
		    (- (rectangle-max-x rectangle) clip-x)
		    (- (rectangle-max-y rectangle) clip-y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given a medium (and a body) set up some bindings and do some drawing
;;; initialisation prior to actually performing the drawing.

;;; Bindings provided: port, mirror, ink, bezier-path, colour

(defun make-foreign-colour-from-design (medium design)
  "Given a medium and a design, return a native NSColor object. This
object is created for uniform colours, transparent colours, patterns
and stencils."
  ;; Deal with uniform designs first (e.g. colours + opacities)

  (let* ((clim-colour (if (or (typep design 'climi::indirect-ink)
			      (typep design 'climi::standard-flipping-ink)
			      (typep design 'climi::standard-color))
			  (%clim-colour-from-design medium design)
			nil))
	 (opacity     (if (null clim-colour)
			  nil
			(%clim-opacity-from-design medium design))))

    ;; if a colour was found, return it for use. Need to deal with opacity
    ;; in the design here too...

    (unless (null clim-colour)
      (return-from make-foreign-colour-from-design (%beagle-pixel (port medium)
								  clim-colour
								  :alpha opacity))))

  ;; Not a uniform design; must be a pattern, stencil or image. Need to deal
  ;; with these by converting to an NSImage and creating an NSColor from
  ;; this.

  ;; Three (?) cases here... we either have a 'climi::transformed-design',
  ;; a 'climi::rectangular-tile' or a 'climi::indexed-pattern'.

  ;; A rectangular tile is a repeating pattern that conceptually fills the
  ;; entire plain (region = +everywhere+).
  ;; A transformed design is one with a transformation; this affects the
  ;; pattern, not any of the designs drawn in the pattern. CLX only supports
  ;; translation transformations, so we'll be doing as well as CLX if we do
  ;; the same.

  ;; An indexed pattern seems to just define the pattern; presumably it would
  ;; be a non-repeating pattern drawn at (0, 0) if it were used directly as
  ;; a design in its own right for drawing purposes.

  (let ((pattern (typecase design
		   (climi::indexed-pattern design)
		   (climi::rectangular-tile (slot-value design 'climi::design))
		   (climi::transformed-design (climi::transformed-design-design design)))))
    (let ((image (%cocoa-image-from-pattern medium pattern)))

      ;; For climi::rectangular-tile we want to create an NSColor that uses this tile as
      ;; the ink.

      ;; For the other two, we need to return the NSImage that we want to draw and deal
      ;; with the exact location of that image in the drawing functions themselves (not
      ;; nice :-< ). Will try to use NSGraphicsContext setPatternPhase to do transform.

      ;; When we're NOT dealing with a (typep design rectangular-tile) at this
      ;; point, just return the image. This will be put on screen directly
      ;; by the drawing methods. This may not be quite the right thing, but it
      ;; will (hopefully) look better AND be in the right place. ::FIXME::
      (unless (typep design 'climi::rectangular-tile)
	(return-from make-foreign-colour-from-design image))

      ;; Otherwise continue, and set the pattern as the colour (will tile).
      (let ((colour (send (@class ns-color) :color-with-pattern-image image)))
	colour))))


;;; :around method; check if there's already an NSImage in the cache. Might be better to cache
;;; the actual NSImage instead of the bitmap image rep; this should be better than nothing
;;; though.
(defmethod %cocoa-image-from-pattern :around ((medium beagle-medium) (design climi::indexed-pattern))

  ;; ::FIXME:: Should have some means of preventing these from being cached; something
  ;;           user-configurable.
  
  (let ((design-cache (slot-value (port medium) 'design-cache)))
    (let ((cached (gethash design design-cache)))
      (or cached
          (setf (gethash design design-cache)
                (call-next-method))))))

;;; v need one of these for transformed design too for the listener to work... the
;;;   basics are working though, we can generate bmps and get them displayed.

(defmethod %cocoa-image-from-pattern ((medium beagle-medium) (design climi::indexed-pattern))
  "Given a CLIM pattern #2() + list of designs, return a native NSImage that
represents the pattern. This can be used as an ink (NSColor) in Cocoa.

Returns a MACPTR containing the NSImage which can be used either in
NSColor (colorFromImage) for tiled designs, or directly with drawInRect
for non-tiled designs."

  ;; Our assumption here is that each design in the pattern is a uniform
  ;; design (i.e. a colour (with or without an opacity) for patterns, or
  ;; an opacity (with no colour) for stencils). I suspect in the generalised
  ;; drawing model of CLIM this is a bad assumption, but for real-world
  ;; applications it feels like it should be ok.

  ;; Construct data by iterating over the pixels in the pattern, and
  ;; creating a new matrix where each value is a 16- or 32-bit colour
  ;; value. We then construct an NSImage and pass this data into the
  ;; image object (setting width, height and depth for the bitmap)
  ;; which is returned for use.

  ;; ::FIXME:: the current implementation is wasteful, assuming full 32-bit
  ;;           values for each pixel. It's likely this could be reduced
  ;; (especially for simple patterns and stencils) but for now at least it
  ;; should work; more on efficiency later.

  (let* ((array (slot-value design 'climi::array))
	 (inks  (slot-value design 'climi::designs))
	 (height (pattern-height design))
	 (width (pattern-width design))
	 (bmp-image (make-instance 'ns:ns-bitmap-image-rep
				   :init-with-bitmap-data-planes (%null-ptr)     ; Cocoa will allocate
				   :pixels-wide width                    ; int-width
				   :pixels-high height                   ; int-height
				   :bits-per-sample 8                    ; int-bps
				   :samples-per-pixel 4                  ; RGB + opacity | int-spp
				   :has-alpha #$YES                      ; bool-alpha
				   :is-planar #$NO                       ; bool-planar
				   :color-space-name #@"NSCalibratedRGBColorSpace"
				   :bytes-per-row 0                      ; int-bpr - Cocoa works out from
					                                 ; width, bps, spp with no padding.
				   :bits-per-pixel 0))                   ; int-bits - Cocoa interprets to
					                                 ; be expected value, without any
					                                 ; meaningless bits
	 (planes (send bmp-image 'bitmap-data)))

    (send bmp-image 'autorelease)

    ;; populate 'planes' with bytes for the image. Note that RGB components must be
    ;; pre-multiplied with the 'coverage' (alpha) component. We get this for free
    ;; if we use ns-color to calculate the R, G and B components (I hope :-)

    ;; This code basically ripped off from that in CLX;medium.lisp (design-gcontext)

    (dotimes (y height)
      (dotimes (x width)
	(let* ((ink (elt inks (aref array y x)))
	       ;; Generate a colour for this design (will include an opacity - or it would
	       ;; if we had a way to get an opacity from a design that wasn't itself an
	       ;; opacity, but rather made use of an opacity. %m-t-n-c defaults opacity to
	       ;; 1.0).
	       (colour (if (eq ink +transparent-ink+)
			   (send (@class ns-color) 'clear-color)  ; need to do better...
			 (%beagle-pixel (port medium) (%clim-colour-from-design medium ink))))
	       ;; Then extract the R,G,B + coverage from this colour...
	       (red     (if (eq ink +transparent-ink+)
			    (coerce 0 '(unsigned-byte 8))
			  (coerce (round (* (send (the ns-color colour) 'red-component) 255))
				  '(unsigned-byte 8))))
	       (green   (if (eq ink +transparent-ink+)
			    (coerce 0 '(unsigned-byte 8))
			  (coerce (round (* (send (the ns-color colour) 'green-component) 255))
				  '(unsigned-byte 8))))
	       (blue    (if (eq ink +transparent-ink+)
			    (coerce 0 '(unsigned-byte 8))
			  (coerce (round (* (send (the ns-color colour) 'blue-component) 255))
				  '(unsigned-byte 8))))
	       (opacity (coerce (round (* (send (the ns-color colour) 'alpha-component) 255))
				'(unsigned-byte 8))))
	  
	  ;; and set it in the (char *) named planes. rgb,o {0..255}

	  (setf (ccl::%get-unsigned-byte planes (+ (* y width 4) (* x 4) 0)) red)
	  (setf (ccl::%get-unsigned-byte planes (+ (* y width 4) (* x 4) 1)) green)
	  (setf (ccl::%get-unsigned-byte planes (+ (* y width 4) (* x 4) 2)) blue)
	  (setf (ccl::%get-unsigned-byte planes (+ (* y width 4) (* x 4) 3)) opacity))))

    (let ((pasteboard (send bmp-image 'tiff-representation)))
      (let ((image (make-instance 'ns:ns-image :init-with-data pasteboard)))
	(send image 'retain)
	image))))


(defmethod %clim-opacity-from-design ((medium beagle-medium) design)
  (declare (ignore medium design))
  ;; Just a stub for now. ::FIXME:: Need to ask on the list about this...
  #.(cg-floatify 1.0))


(defmethod %clim-colour-from-design ((medium beagle-medium) (design climi::indirect-ink))
  (if (eql design +foreground-ink+)
      (medium-foreground medium)
    (medium-background medium)))


(defmethod %clim-colour-from-design ((medium beagle-medium) (design climi::standard-flipping-ink))
  ;; Currently it's not clear how to deal with flipping ink. When I've upgraded to Tiger (10.4)
  ;; then can use to XOR compositing mode on the NSBezierPath. But even then this will be
  ;; specific to the path, and not really a colour.
  +red+)


(defmethod %clim-colour-from-design ((medium beagle-medium) (design climi::standard-color))
  (declare (ignore medium))
  design)

       
;;; In the cocoa drawing model, it is only possible to draw on a window
;;; after successfully 'locking focus' on it. Whilst multiple windows
;;; can be focus-locked, only to most recently focus locked view can
;;; be drawn on. So view drawing is mandated to be a single-threaded
;;; operation (although setting up the bezier path need not be).

;;; Think we need to handle colour differently; we either want an
;;; NSColor that is a solid (possibly transparent) colour (merge
;;; ink + opacity) OR a general design (pattern, stencil or 'image'
;;; (just a complex pattern)) in which case we use 'colorWithPatternImage:'
;;; in NSColor to give us a 'colour'which can be set. The only real
;;; complication with this is how (for images) we work out the phase
;;; offset to set in the NSGraphicsContext.
(defmacro with-beagle-graphics ((medium) &body body)
  `(let* ((port   (port ,medium))
		  (mirror (port-lookup-mirror port (medium-sheet ,medium))))
     (when mirror
       (let* ((ink          (medium-ink ,medium))
	      (colour       (make-foreign-colour-from-design ,medium ink))
	      (path         (medium-bezier-path medium)))
	 ;; There's a good chance that the regions / transformations have
	 ;; been invalidated (seems to happen all the time) so make sure
	 ;; they get recalculated.
	 (medium-device-region medium)
	 (unless (send path 'is-empty)
	   (send path 'remove-all-points))
	 ,@body))))
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-finish-output medium                       [Generic function]

;;; Ensures that all the output sent to medium has reached its destination,
;;; and only then return false. This is used by finish-output.

;;; COCOA NOTE: each drawing operation flushes the window already, so
;;; we never get into the position of having unmirrored drawing ops
;;; in the off-screen buffer.

(defmethod medium-finish-output ((medium beagle-medium))
  (send (send (port-lookup-mirror *beagle-port* (medium-sheet medium)) 'window)
	'flush-window-if-needed)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-force-output medium                        [Generic function]

;;; Like medium-finish-output, except that it may return false without
;;; waiting for the output to complete.
;;; This is used by force-output.

;;; COCOA NOTE: each drawing operation flushes the window already, so
;;; we never get into the position of having unmirrored drawing ops
;;; in the off-screen buffer.

(defmethod medium-force-output ((medium beagle-medium))
  (send (send (port-lookup-mirror *beagle-port* (medium-sheet medium)) 'window)
	'flush-window)
    nil)


(defmethod medium-beep ((medium beagle-medium))
  (#_NSBeep))

;;; Translate from a CLIM cap-shape style to a Cocoa cap shape.

(defconstant +beagle-cap-shape-map+ (list `(:butt . ,#$NSButtLineCapStyle)
					  `(:square . ,#$NSSquareLineCapStyle)
					  `(:round . ,#$NSRoundLineCapStyle)
					  `(:no-end-point . ,#$NSRoundLineCapStyle)))

(defun %translate-cap-shape (clim-shape)
  (let ((beagle-shape (cdr (assoc clim-shape +beagle-cap-shape-map+))))
    (if beagle-shape
        beagle-shape
      #$NSButtLineCapStyle)))

(defconstant +beagle-line-joint-map+ (list `(:miter . ,#$NSMiterLineJoinStyle)
				           `(:round . ,#$NSRoundLineJoinStyle)
					   `(:bevel . ,#$NSBevelLineJoinStyle)
					   `(:none . ,#$NSBevelLineJoinStyle)))

(defun %translate-joint-shape (clim-joint)
  (let ((beagle-shape (cdr (assoc clim-joint +beagle-line-joint-map+))))
    (if beagle-shape
        beagle-shape
      #$NSMiterLineJoinStyle)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-copy-area from-drawable from-x from-y      [Generic function]
;;;                  width height to-drawable
;;;                  to-x to-y

;;; Copies the pixels from the source drawable from-drawable at the
;;; position (from-x, from-y) to the destination drawable to-drawable at
;;; the position (to-x, to-y).  A rectangle whose width and height is
;;; specified by width and height is copied. from-x, from-y, to-x, and
;;; to-y are specified in user coordinates. The x and y are transformed
;;; by the user transformation. 

;;; This is intended to specialize on both the from-drawable and
;;; to-drawable arguments. from-drawable and to-drawable may be either
;;; mediums or pixmaps.

(defun medium-copy-area-aux (from from-x from-y width height to to-x to-y)
  "Helper method for copying areas. 'from' and 'to' must both be 'mirror'
objects. From and To coordinates must already be transformed as appropriate."
  (let* ((source-region (make-ns-rect from-x from-y width height))
	 (target-point  (make-ns-point to-x to-y))
	 (bitmap-image  (send from :copy-bitmap-from-region source-region)))
    (when (eql bitmap-image (%null-ptr))
      (warn "medium.lisp -> medium-copy-area: failed to copy specified region (null bitmap)~%")
      (return-from medium-copy-area-aux nil))
    (send to :paste-bitmap bitmap-image :to-point target-point)
    (#_free source-region)
    (#_free target-point)
    (send bitmap-image 'release)))


(defmethod medium-copy-area ((from-drawable beagle-medium) from-x from-y width height
                             (to-drawable beagle-medium) to-x to-y)
  ;; width + height *are* a width + a height. from-x, from-y and to-x, to-y specify
  ;; the UPPER-LEFT of the region; for us they need to specify the LOWER-LEFT. Remember
  ;; that these will be flipped in the NSView, but I'm not convinced this is quite
  ;; correct! ::FIXME::

  ;; We appear to COPY the area correctly, but then PASTE it in the wrong place which
  ;; is a little weird. Output looks like:
  ;;
  ;;                m
  ;; Help (with) com ands
  ;;
  ;; with the 2nd 'm' being copied but put in the wrong place. Need to move it down
  ;; some (i.e. increase y in the mcclim coord system). Hence to "to-y" massaging.
  ;; Additionally, the paste is a couple of pixels off (slightly too low, and slightly
  ;; too far to the right).
  ;; Probably caused by the way we do rounding (note the cursor rectangle is also off
  ;; slightly when it's moved).
  ;; Probably want to "round" everything before adding the 0.5 offset.
  
  ;; Apparently there's no need to do this for the "from" coordinates... strange.
;;;  (setf from-y (+ from-y height))
  (setf to-y   (+ to-y   height))

  ;; Slight problem here in that the region we copy is actually slightly *smaller* than
  ;; the cursor, so the bounding rectangle shows through. I guess (but don't know) that
  ;; this will go away when flipping ink is implemented otherwise we'll need to furtle
  ;; with this by a pixel or so. ::FIXME::

  ;; Could *probably* fix this by changing the text-size + text-height methods to *not*
  ;; make use of Cocoa's built-in text-height function since that appends padding, and
  ;; then McCLIM uses the ascent + descent and appends its own padding and I think these
  ;; two don't match up...

  ;; Might be better to do this anyway so that Cocoa back end is closer to CLX back end
  ;; in all this.
  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
                              from-x from-y)
    (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
                                to-x to-y)
	(medium-copy-area-aux (sheet-direct-mirror (medium-sheet from-drawable)) from-x from-y
			      width height
			      (sheet-direct-mirror (medium-sheet to-drawable)) to-x to-y))))


(defmethod medium-copy-area ((from-drawable beagle-medium) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
			      from-x from-y)
    (medium-copy-area-aux (sheet-direct-mirror (medium-sheet from-drawable)) from-x from-y
			  width height
			  (pixmap-mirror to-drawable) to-x to-y)))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable beagle-medium) to-x to-y)
  (setf to-y (+ to-y height))
  (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
			      to-x to-y)
    (medium-copy-area-aux (pixmap-mirror from-drawable) from-x from-y
			  width height
			  (sheet-direct-mirror (medium-sheet to-drawable)) to-x to-y)))


(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (medium-copy-area-aux (pixmap-mirror from-drawable) from-x from-y
			width height
			(pixmap-mirror to-drawable) to-x to-y))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-rectangles* medium coord-seq          [Generic function] 

;;; Draws a set of rectangles on the medium medium. coord-seq is a
;;; sequence of coordinate pairs, which are real numbers.  It is an
;;; error if coord-seq does not contain an even number of elements.
;;; Each successive pair of coordinate pairs is taken as the upper-left
;;; and lower-right corner of the rectangle.

(defmethod medium-draw-rectangles* ((medium beagle-medium) coord-seq filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-positions (tr coord-seq)
      (with-beagle-graphics (medium)
        (do-sequence ((left top right bottom) coord-seq)
	  (when (< right left) (rotatef left right))
	  (when (< top bottom) (rotatef top bottom))
	  (let ((rect (make-ns-rect (pixel-center left)
				    (pixel-center bottom)
				    (pixel-count (- right left))
				    (pixel-count (- top bottom)))))
	    (send path :append-bezier-path-with-rect rect)
	    (#_free rect)))
	(if filled
	    (send mirror :fill-path path :in-colour colour)
	  (send mirror :stroke-path path :in-colour colour))))))

;; ::FIXME:: Move these from here!
(defun pixel-center (pt)
"Ensure any ordinate provided sits on the center of a pixel. This
prevents Cocoa from 'antialiasing' lines, making them thicker and a
shade of grey. Ensures the return value is an appropriate float type."
  (cg-floatify (+ (round-coordinate pt) 0.5)))


(defun pixel-count (sz)
"Ensures any value provided is rounded to the nearest unit, and
returned as an appropriate float type."
  (cg-floatify (round-coordinate sz)))


;;; Nabbed from CLX backend medium.lisp
(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates:

We use \"mercantile rounding\", instead of the CL round to nearest
even number, when in doubt.

Reason: As the CLIM drawing model is specified, you quite often
want to operate with coordinates, which are multiples of 1/2. 
Using CL:ROUND gives \"random\" results. Using \"mercantile
rounding\" gives consistent results."
  (floor (+ x .5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-ellipse* medium center-x center-y     [Generic function]
;;;                      radius-1-dx radius-1-dy
;;;                      radius-2-dx radius-2-dy
;;;                      start-angle end-angle

;;; Draws an ellipse or elliptical arc on the medium medium. The center
;;; of the ellipse is at (x,y), and the radii are specified by the two
;;; vectors (radius-1-dx, radius-1-dy) and (radius-2-dx, radius-2-dy). 

;;; start-angle and end-angle are real numbers that specify an arc rather
;;; than a complete ellipse. Note that the medium and device
;;; transformations must be applied to the angles as well. 

;;; Method is similar in execution (complete with axis-alignment
;;; limitation) to CLX back end. We add another limitation in that we
;;; ignore the start and end angles, but at least we made a start ;-)

(defmethod medium-draw-ellipse* ((medium beagle-medium) center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (declare (ignore start-angle end-angle))
  ;;; Suspect we should be transforming the radii as well as the centre...
  (unless (or (= radius-2-dx radius-1-dy 0) (= radius-1-dx radius-2-dy 0))
    (error "medium-draw-ellipse* not implemented for non axis-aligned ellipses."))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
			      center-x
			      center-y)
    (with-beagle-graphics (medium)
      (let* ((radius-dx (abs (+ radius-1-dx radius-2-dx)))
	     (radius-dy (abs (+ radius-1-dy radius-2-dy)))
	     (origin-x (- center-x radius-dx))
	     (origin-y (- center-y radius-dy))
	     (width (* 2 radius-dx))
	     (height (* 2 radius-dy))
	     (rect (make-ns-rect (pixel-center origin-x)
				 (pixel-center origin-y)
				 (pixel-count width)
				 (pixel-count height))))
	(send path :append-bezier-path-with-oval-in-rect rect)
	(#_free rect)
	(if filled
	    (send mirror :fill-path path :in-colour colour)
	  (send mirror :stroke-path path :in-colour colour))))))


(defmethod medium-draw-circle* ((medium beagle-medium) center-x center-y
				radius start-angle end-angle filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-beagle-graphics (medium)
	(with-transformed-position (tr center-x center-y)
	  (slet ((point (ns-make-point (pixel-center center-x)
				       (pixel-center center-y))))
	    (send path :append-bezier-path-with-arc-with-center point
		       :radius (pixel-count radius)
		       :start-angle (cg-floatify (/ start-angle (/ pi 180)))
		       :end-angle (cg-floatify (/ end-angle (/ pi 180)))
		       :clockwise NIL)))
	(if filled
	    (send mirror :fill-path path :in-colour colour)
	  (send mirror :stroke-path path :in-colour colour)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-point* medium x y                     [Generic function] 

;;; Draws a point on the medium 'medium'.

(defmethod medium-draw-point* ((medium beagle-medium) x y)
  (let ((width (cg-floatify (line-style-thickness (medium-line-style medium)))))
    (medium-draw-circle* medium x y (/ width 2) 0 (* 2 pi) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-points* medium coord-seq              [Generic function] 

;;; Draws a set of points on the medium medium. coord-seq is a sequence
;;; of coordinate pairs, which are real numbers. It is an error if
;;; coord-seq does not contain an even number of elements. 

(defmethod medium-draw-points* ((medium beagle-medium) coord-seq)
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium)) coord-seq)
    (let ((width (cg-floatify (line-style-thickness (medium-line-style medium)))))
      (do-sequence ((x y) coord-seq)
        (medium-draw-circle* medium x y (/ width 2) 0 (* 2 pi) t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-line* medium x1 y1 x2 y2              [Generic function] 

;;; Draws a line on the medium medium. The line is drawn from (x1,y1)
;;; to (x2,y2).

(defmethod medium-draw-line* ((medium beagle-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-beagle-graphics (medium)
      (with-transformed-position (tr x1 y1)
	(with-transformed-position (tr x2 y2)
	  (slet ((p1 (ns-make-point (pixel-center x1)
				    (pixel-center y1)))
		 (p2 (ns-make-point (pixel-center x2)
				    (pixel-center y2))))
	    (send path :move-to-point p1)
	    (send path :line-to-point p2)
	    (send mirror :stroke-path path :in-colour colour)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-lines* stream position-seq            [Generic function] 

;;; Draws a set of disconnected lines on the medium medium. coord-seq
;;; is a sequence of coordinate pairs, which are real numbers. Each
;;; successive pair of coordinate pairs is taken as the start and end
;;; position of each line. It is an error if coord-seq does not contain
;;; an even number of elements. 

(defmethod medium-draw-lines* ((medium beagle-medium) coord-seq)
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium)) coord-seq)
    (with-beagle-graphics (medium)
      (do-sequence ((x1 y1 x2 y2) coord-seq)
        (slet ((p1 (ns-make-point (pixel-center x1)
				  (pixel-center y1)))
	       (p2 (ns-make-point (pixel-center x2)
				  (pixel-center y2))))
	      (send path :move-to-point p1)
	      (send path :line-to-point p2)))
      (send mirror :stroke-path path :in-colour colour))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-rectangle* medium x1 y1 x2 y2         [Generic function] 

;;; Draws a rectangle on the medium medium. The corners of the rectangle
;;; are at (x1,y1) and (x2,y2).

(defmethod medium-draw-rectangle* ((medium beagle-medium) left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left bottom)
      (with-transformed-position (tr right top)
        (with-beagle-graphics (medium)
	  (when (< right left) (rotatef left right))
	  (when (< top bottom) (rotatef top bottom))
	  (when (and filled (or (typep ink 'climi::transformed-design)
				(typep ink 'climi::indexed-pattern)))
	    (send mirror :draw-image colour :at-point (ns-make-point (pixel-center left)
								     (pixel-center top)))
	    (return-from medium-draw-rectangle* (values)))
	  (let ((rect (make-ns-rect (pixel-center left)
				    (pixel-center bottom)
				    (pixel-count (- right left))
				    (pixel-count (- top bottom)))))
	    (send path :append-bezier-path-with-rect rect)
	    (#_free rect)
	    (if filled
		(send mirror :fill-path path :in-colour colour)
	      (send mirror :stroke-path path :in-colour colour))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-polygon* medium coord-seq closed      [Generic function] 

;;; Draws a polygon or polyline on the medium medium. coord-seq is a
;;; sequence of coordinate pairs, which are real numbers.  It is an error
;;; if coord-seq does not contain an even number of elements. Each
;;; successive coordinate pair is taken as the position of one vertex of
;;; the polygon. 

(defmethod medium-draw-polygon* ((medium beagle-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium)) coord-seq)
    (with-beagle-graphics (medium)
      (send path :move-to-point (ns-make-point (pixel-center (elt coord-seq 0))
					       (pixel-center (elt coord-seq 1))))
      (do ((count 2 (+ count 2)))
	  ((> count (1- (length coord-seq))))
	(slet ((pt (ns-make-point (pixel-center (elt coord-seq count))
				  (pixel-center (elt coord-seq (1+ count))))))
	  (send path :line-to-point pt)))
      ;; ensure polyline joins up if appropriate. This needs to be done after
      ;; all points have been set in the bezier path.
      (when closed
	(send path 'close-path))

      (if filled
	  (send mirror :fill-path path :in-colour colour)
	(send mirror :stroke-path path :in-colour colour)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; medium-draw-text* medium text x y (start 0) end   [Generic function]
;;;                                   (align-x :left)
;;;                                   (align-y :baseline)
;;;                                   toward-x toward-y transform-glyphs

;;; Draws a character or a string on the medium medium. The text is drawn
;;; starting at (x,y), and towards (toward-x, toward-y). In some
;;; implementations of CLIM, medium-draw-text* may call either
;;; medium-draw-string* or medium-draw-character* in order to draw the text. 

(defmethod medium-draw-text* ((medium beagle-medium) string x y
			      start end
			      align-x align-y
			      toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium)) x y)
    (with-beagle-graphics (medium)
      (let ((font (medium-native-font medium)))
	(when (characterp string)
	  (setq string (make-string 1 :initial-element string)))
	(when (null end) (setq end (length string)))
	(multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
	    (text-size medium string :start start :end end)
	  (declare (ignore x-cursor y-cursor))
	  (setf x (- x (ecase align-x
			 (:left 0)
			 (:center (round text-width 2))
			 (:right text-width))))
	  (setf y (ecase align-y
;;;		    (:top (- y text-height))
		    (:top y)
		    (:center (- y (floor text-height 2)))
		    (:baseline (- y baseline))
;;;		    (:bottom y)))
		    (:bottom (- y text-height))))
	  (slet ((point (ns-make-point (cg-floatify x) (cg-floatify y))))
	    (let ((objc-string (%make-nsstring (subseq string start end))))
	      ;; NB: draw-string-at-point uses upper-left as origin in a flipped
	      ;; view.
	      (send mirror :draw-string objc-string
		    :at-point point
		    :with-attributes (reuse-attribute-dictionary medium font :colour colour)
		    :in-colour colour
;		    :with-width width
;		    :with-cap-style cap
;		    :with-join-style join
		    )
	      (send objc-string 'release))))))))

;;; Can't see these making any difference, but it brings us a little closer to equality with
;;; the CLX back end.
(defmethod medium-buffering-output-p ((medium beagle-medium))
  t)

;;; This doesn't look right to me; all our windows are buffered and there's no way (currently)
;;; to prevent that in the back end (windows are buffered, but output isn't currently since
;;; we flush after every operation?).
;;; Need to look this up - it might mean that CLIM is buffering (recording output) maybe?
(defmethod (setf medium-buffering-output-p) (buffer-p (medium beagle-medium))
  buffer-p)

(defmethod invoke-with-special-choices (continuation (medium beagle-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

