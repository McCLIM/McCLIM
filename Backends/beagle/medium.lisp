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

#||

Occasionally see lock-ups from Cocoa attempting to "unlock topmost reader".

+--------+           +-----------------------------+
| MEDIUM |           | TRANSFORM-COORDINATES-MIXIN |    * There's a comment in "medium.lisp" in the main source
+--------+           +-----------------------------+      that this may not be correct - keep an eye on it.
^                                ^
|                                |
+----------------+---------------+
|
             +------------------+
             |   BASIC-MEDIUM   |
             +------------------+
             |foreground        |
             |background        |
             |ink               |
             |transformation    |
             |clipping-region   |
             |line-style        |
             |text-style        |
             |default-text-style|
             |sheet             |
             +------------------+
                       ^
                       |
+---------+-------------+
|                       |
+----------------+      +------------------+
|  BEAGLE-MEDIUM  |      | UNGRAFTED-MEDIUM |
+----------------+      +------------------+
|fontset         |
+----------------+

I guess the job that BEAGLE-MEDIUM has to do is override all the
BASIC-MEDIUM methods so that everything is done against an appropriate
Cocoa graphics context.

Note3: The spec says (sec 8.3.4) "Before a sheet may be used for output,
       it MUST BE ASSOCIATED WITH A MEDIUM" (emphasis mine). The sheet
*also* needs to be associated with a mirror... I guess this means that
every mirror we ever try to draw on has a medium (graphics context)
associated with it. That's not necessarily a problem, in our drawing
functions we need to ensure that this is the case though. Then when we
draw into an NSView (which is the *MIRROR*, we push the graphics context
that is the *MEDIUM* first). If we do this I think everything will just
work.

Note4: For drawing text, we ignore "transform-glyphs" at the current time.

||#

(defclass beagle-medium (basic-medium)
  ())
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-drawable medium [Generic function]

;;; Returns an implementation-dependent object that corresponds to the actual host window that will be
;;; drawn on when the medium medium is drawn on. If medium is not grafted to a sheet or the medium's
;;; sheet is not currently mirrored on a display server, medium-drawable returns nil.

;;; Programmers can use this function to get a host window system object that can be manipulated using
;;; the functions of the host window system. This might be done in order to explicitly trade of
;;; performance against portability.

(defmethod medium-drawable ((medium beagle-medium))
  (debug-log 2 "medium.lisp -> medium-drawable~%")
  (and (medium-graft medium) (port-lookup-mirror (port medium) medium)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; make-medium port sheet [Generic function] 

;;; Creates a new medium for the port port. The new medium will have its default
;;; characteristics determined by sheet.

(defmethod make-medium ((port beagle-port) sheet)
  (debug-log 2 "medium.lisp -> make-medium~%")
  (make-instance 'beagle-medium 
		 :port  port 
		 :graft (find-graft :port port) 
		 :sheet sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given a medium (and a body) set up some bindings and do some drawing
;;; initialisation prior to actually performing the drawing.

;;; Bindings provided: port, mirror, ink, bezier-path, colour

(defgeneric %map-to-named-colour (medium ink))

(defmethod %map-to-named-colour ((medium beagle-medium) (ink (eql +foreground-ink+)))
  (debug-log 2 "medium.lisp -> %map-to-named-colour (+foreground-ink+)~%")
  (medium-foreground medium))

(defmethod %map-to-named-colour ((medium beagle-medium) (ink (eql +background-ink+)))
  (debug-log 2 "medium.lisp -> %map-to-named-colour (+background-ink+)~%")
  (medium-background medium))

(defmethod %map-to-named-colour ((medium beagle-medium) (ink (eql +flipping-ink+)))
  (debug-log 2 "medium.lisp -> %map-to-named-colour (+flipping-ink+)~%")
  +red+)

(defmethod %map-to-named-colour ((medium beagle-medium) (ink t))
  (debug-log 2 "medium.lisp -> %map-to-named-colour (ink t)~%")
  ink)

;;; See OpenMCL mailing [19th Feb 2004 - Gary Byers - Re: mcl-doubles problem] and follow-up
;;; [20th Feb 2004 - Bill Schottstaedt - Re: mcl-doubles problem].
;;;
;;; Not actually used at the moment, but should be useful going forward...
(defmacro with-foreign-double-float-array ((ptr-var lisp-array) &body body)
  (let* ((length (gensym)) (size (gensym)))
    `(let* ((,length (length ,lisp-array))
	   (,size (* ,length 8)))
       (%stack-block ((,ptr-var ,size))
         (dotimes (i ,length)
	   (setf (%get-double-float ,ptr-var (* i 8)) (aref ,lisp-array i)))
	 ,@body))))

;;; Do we *really* need to do this prior to every drawing request? We must be able to capture
;;; these as a default and then we could just monitor changes to the drawing options, and handle
;;; them as they happen. Maybe this isn't so easy, but it certainly feels like it should be
;;; faster.

(defmacro %with-beagle-graphics ((medium) &body body)
  `(let* ((port   (port ,medium))
		  (mirror (port-lookup-mirror port (medium-sheet ,medium))))
     (when mirror
       (let* ((ink          (medium-ink ,medium))
	      (line-style   (medium-line-style ,medium))
	      ;; CONVERT: if (eql ink +background-ink+) -> (medium-background medium)
	      ;;          if (eql ink +foreground-ink+) -> (medium-foreground medium)
	      ;;          if (eql ink +flipping-ink+)   -> ( ??? )
	      (colour       (if (eql ink +flipping-ink+)
				(%beagle-pixel port (%map-to-named-colour ,medium ink) :alpha 0.4)
			      (%beagle-pixel port (%map-to-named-colour ,medium ink))))
	      (font         (%text-style->beagle-font (or (medium-text-style ,medium) *default-text-style*)))
	      (width        (coerce (line-style-thickness line-style) 'short-float))
	      (cap          (%translate-cap-shape (line-style-cap-shape line-style)))
	      (join         (%translate-joint-shape (line-style-joint-shape line-style))))
	 (unwind-protect
	     ;;            #+nil
	     ;;	    (ccl::send bezier-path :set-line-dash (%translate-line-dash-pattern (line-dash-style line-style)) :count (%translate-line-dash-count (line-dash-style line-style)) :phase (%translate-line-dash-phase (line-dash-style line-style)))
	     ,@body)))))

(defmacro with-beagle-graphics ((medium) &body body)
  `(let* ((port   (port ,medium))
		  (mirror (port-lookup-mirror port (medium-sheet ,medium))))
     (when mirror
       (let* ((ink          (medium-ink ,medium))
	      (line-style   (medium-line-style ,medium))
	      ;; CONVERT: if (eql ink +background-ink+) -> (medium-background medium)
	      ;;          if (eql ink +foreground-ink+) -> (medium-foreground medium)
	      ;;          if (eql ink +flipping-ink+)   -> ( ??? )
	      (colour       (if (eql ink +flipping-ink+)
				(%beagle-pixel port (%map-to-named-colour ,medium ink) :alpha 0.4)
			      (%beagle-pixel port (%map-to-named-colour ,medium ink))))
	      (font         (%text-style->beagle-font (or (medium-text-style ,medium) *default-text-style*)))
	      (width        (coerce (line-style-thickness line-style) 'short-float))
	      (cap          (%translate-cap-shape (line-style-cap-shape line-style)))
	      (join         (%translate-joint-shape (line-style-joint-shape line-style)))
	      (path         (send (@class ns-bezier-path) 'bezier-path)))
	     ;;            #+nil
	     ;;	    (ccl::send bezier-path :set-line-dash (%translate-line-dash-pattern (line-dash-style line-style)) :count (%translate-line-dash-count (line-dash-style line-style)) :phase (%translate-line-dash-phase (line-dash-style line-style)))
	 (send path :set-line-cap-style cap)
	 (send path :set-line-join-style join)
	 (send path :set-line-width width)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: we extract the dash pattern from the line-style via "line-dash-style", but the spec says we should
;; use the "line-style-dashes" generic function to get this information. Possible specification violation?

(defun %translate-line-dash-pattern (line-dash-in)
  "CLIM defines line dash patterns as a vector (usually) of alternating
solid and empty segments. This is the same way that Cocoa defines line
dash patterns, so we just need to convert between a Lisp vector and an
Objective C array-of-floats."
  (debug-log 2 "medium.lisp -> %translate-line-dash-pattern~%")
  line-dash-in)

(defun %translate-line-dash-phase (line-dash-in)
  "Indicates (in view coordinate units) how far into the pattern to start.
We always start at the beginning of the pattern, so this is hard-coded
to 0.0"
  (declare (ignore line-dash-in))
  (debug-log 2 "medium.lisp -> %translate-line-dash-phase~%")
  0.0)

(defun %translate-line-dash-count (line-dash-in)
  "Indicates the number of solid / empty segments that make up the pattern.
This is equal to the length of the provided line-dash pattern."
  (debug-log 2 "medium.lisp -> %translate-line-dash-count~%")
  (length line-dash-in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-finish-output medium [Generic function]

;;; Ensures that all the output sent to medium has reached its destination, and only then return false.
;;; This is used by finish-output.

;;; COCOA NOTE: each drawing operation flushes the window already, so
;;; we never get into the position of having unmirrored drawing ops
;;; in the off-screen buffer.
#+nil
(defmethod medium-finish-output ((medium beagle-medium))
  (debug-log 2 "medium.lisp -> medium-finish-output~%")
  (progn
    (send (send (port-lookup-mirror *beagle-port* (medium-sheet medium)) 'window) 'flush-window-if-needed)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-force-output medium [Generic function]

;;; Like medium-finish-output, except that it may return false without waiting for the output to complete.
;;; This is used by force-output.

;;; COCOA NOTE: each drawing operation flushes the window already, so
;;; we never get into the position of having unmirrored drawing ops
;;; in the off-screen buffer.
#+nil
(defmethod medium-force-output ((medium beagle-medium))
  (debug-log 2 "medium.lisp -> medium-force-output~%")
  (progn
    (send (send (port-lookup-mirror *beagle-port* (medium-sheet medium)) 'window) 'flush-window)
    nil))

;;; ::FIXME:: How does this get sent? Weird. Not sure this is actually correct...
(defmethod medium-beep ((medium beagle-medium))
  (#_NSBeep))

;;; Translate from a CLIM cap-shape style to a Cocoa cap shape.

(defconstant +beagle-cap-shape-map+ `((:butt . ,#$NSButtLineCapStyle)
				     (:square . ,#$NSSquareLineCapStyle)
				     (:round . ,#$NSRoundLineCapStyle)
				     (:no-end-point . ,#$NSRoundLineCapStyle)))

(defun %translate-cap-shape (clim-shape)
  (debug-log 2 "medium.lisp -> %translate-cap-shape~%")
  (let ((beagle-shape (cdr (assoc clim-shape +beagle-cap-shape-map+))))
    (if beagle-shape
        beagle-shape
      (progn
	(warn "Unknown cap style ~S, using :butt" clim-shape)
	#$NSButtLineCapStyle)))) ; Default is :butt, see spec 10.3.1 (why does CLX default to :round?)

(defconstant +beagle-line-joint-map+ `((:miter . ,#$NSMiterLineJoinStyle)
				      (:round . ,#$NSRoundLineJoinStyle)
				      (:bevel . ,#$NSBevelLineJoinStyle)
				      (:none . ,#$NSBevelLineJoinStyle))) ; Looks like :none = :bevel from spec 10.3.1

(defun %translate-joint-shape (clim-joint)
  (debug-log 2 "medium.lisp -> %translate-joint-shape~%")
  (let ((beagle-shape (cdr (assoc clim-joint +beagle-line-joint-map+))))
    (if beagle-shape
        beagle-shape
      (progn
	(warn "Unknown joint shape ~S, using :miter" clim-joint)
	#$NSMiterLineJoinStyle)))) ; default join style = :miter from spec 10.3.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-copy-area from-drawable from-x from-y width height to-drawable to-x to-y [Generic function] 

;;; Copies the pixels from the source drawable from-drawable at the position (from-x, from-y) to the destination
;;; drawable to-drawable at the position (to-x, to-y).  A rectangle whose width and height is specified by width
;;; and height is copied. from-x, from-y, to-x, and to-y are specified in user coordinates. The x and y are
;;; transformed by the user transformation. 

;;; This is intended to specialize on both the from-drawable and to-drawable arguments. from-drawable and
;;; to-drawable may be either mediums or pixmaps.

(defmethod medium-copy-area ((from-drawable beagle-medium) from-x from-y width height
                             (to-drawable beagle-medium) to-x to-y)
  (debug-log 2 "medium.lisp -> medium-copy-area (drawable drawable)~%")
  (debug-log 3 "               fromx=~A fromy=~A width=~A height=~A tox=~A toy=~A~%" from-x from-y width height to-x to-y)
  ;; width + height *are* a width + a height. from-x, from-y and to-x, to-y specify the UPPER-LEFT of the region;
  ;; for us they need to specify the LOWER-LEFT. Remember that these will be flipped in the NSView, but I'm not
  ;; convinced this is quite correct! ::FIXME::

  ;; Apparently there's no need to do this for the "from" coordinates... strange.
;;;  (setf from-y (+ from-y height))
  (setf to-y   (+ to-y   height))

  ;; Slight problem here in that the region we copy is actually slightly *smaller* than the cursor, so the
  ;; bounding rectangle shows through. I guess (but don't know) that this will go away when flipping ink
  ;; is implemented "properly" otherwise we'll need to furtle with this by a pixel or so. ::FIXME::

  ;; Could *probably* fix this by changing the text-size + text-height methods to *not* make use of
  ;; Cocoa's built-in text-height function since that appends padding, and then McCLIM uses the ascent +
  ;; descent and appends its own padding and I think these two don't match up...

  ;; Might be better to do this anyway so that Cocoa back end is closer to CLX back end in all this.
  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
                              from-x from-y)
    (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
                                to-x to-y)
	;; We appear to COPY the area correctly, but then PASTE it in the wrong place which is a little
	;; weird. Output looks like:
	;;
        ;;                m
	;; Help (with) com ands
	;;
        ;; with the 2nd 'm' being copied but put in the wrong place. Need to move it down some (i.e. increase y in the
	;; mcclim coord system). Hence to "to-y" massaging above.
	;; Additionally, the paste is a couple of pixels off (slightly too low, and slightly too far to the right). Probably
	;; caused by the way we do rounding (note the cursor rectangle is also off slightly when it's moved).
	;; Probably want to "round" everything before adding the 0.5 offset.
        (let* ((source-region (ccl::make-ns-rect (+ (round-coordinate from-x) 0.5)
						 (+ (round-coordinate from-y) 0.5)
						 (round-coordinate width)
						 (round-coordinate height)))
	       (target-point  (ccl::make-ns-point (+ (round-coordinate to-x) 0.5)
						  (+ (round-coordinate to-y) 0.5)))
	       (bitmap-image  (send (sheet-direct-mirror (medium-sheet from-drawable)) :copy-bitmap-from-region source-region)))
	  (when (eql bitmap-image (%null-ptr))
	    (warn "medium.lisp -> medium-copy-area: failed to copy specified region (null bitmap)~%")
	    nil)
	  (debug-log 3 "medium.lisp: got bitmap-image ~S~%" (ccl::description bitmap-image))
	  (debug-log 3 "pasting bitmap image to: ~A ~A~%" to-x to-y)
	  (send (sheet-direct-mirror (medium-sheet to-drawable)) :paste-bitmap bitmap-image :to-point target-point)
	  (send bitmap-image 'release)))))
  
(defmethod medium-copy-area ((from-drawable beagle-medium) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (declare (ignore from-drawable from-x from-y
		   width height
		   to-drawable to-x to-y))
  (debug-log 2 "medium.lisp -> medium-copy-area (drawable pixmap)~%")
  (error "medium-copy-area (drawable -> pixmap) not implemented"))

;;; Don't forget to round-coordinate!

;;;  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
;;;                              from-x from-y)
;;;    (xlib:copy-area (sheet-direct-mirror (medium-sheet from-drawable))
;;;                    (medium-gcontext from-drawable +background-ink+)
;;;                    (round from-x) (round from-y) (round width) (round height)
;;;                    (pixmap-mirror to-drawable)
;;;                    (round to-x) (round to-y))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable beagle-medium) to-x to-y)
  (declare (ignore from-drawable from-x from-y
		   width height
		   to-drawable to-x to-y))
  (debug-log 2 "medium.lisp -> medium-copy-area (pixmap drawable)~%")
  (error "medium-copy-area (pixmap -> drawable) not implemented"))

;;;  (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
;;;                              to-x to-y)
;;;    (xlib:copy-area (pixmap-mirror from-drawable)
;;;                    (medium-gcontext to-drawable +background-ink+)
;;;                    (round from-x) (round from-y) (round width) (round height)
;;;                    (sheet-direct-mirror (medium-sheet to-drawable))
;;;                    (round to-x) (round to-y))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (declare (ignore from-drawable from-x from-y
		   width height
		   to-drawable to-x to-y))
  (debug-log 2 "medium.lisp -> medium-copy-area (pixmap pixmap)~%")
  (error "medium-copy-area (pixmap -> pixmap) not implemented"))

;;;  (xlib:copy-area (pixmap-mirror from-drawable)
;;;                  (medium-gcontext from-drawable +background-ink+)
;;;                  (round from-x) (round from-y) (round width) (round height)
;;;                  (pixmap-mirror to-drawable)
;;;                  (round to-x) (round to-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-rectangles* medium coord-seq                           [Generic function] 

;;; Draws a set of rectangles on the medium medium. coord-seq is a sequence of coordinate pairs,
;;; which are real numbers.  It is an error if coord-seq does not contain an even number of elements.
;;; Each successive pair of coordinate pairs is taken as the upper-left and lower-right corner of
;;; the rectangle.

(defmethod medium-draw-rectangles* ((medium beagle-medium) coord-seq filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-positions (tr coord-seq)
      (with-beagle-graphics (medium)
        (do-sequence ((left top right bottom) coord-seq)
	  (when (< right left) (rotatef left right))
	  (when (< top bottom) (rotatef top bottom))
	  (let ((left   (round-coordinate left))
		(top    (round-coordinate top))
		(right  (round-coordinate right))
		(bottom (round-coordinate bottom)))
	    (send path :append-bezier-path-with-rect (ccl::make-ns-rect left bottom (- right left) (- top bottom)))))
	(if filled
	    (send mirror :fill-path path :in-colour colour)
	  (send mirror :stroke-path path :in-colour colour))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-ellipse* medium center-x center-y radius-1-dx radius-1-dy    [Generic function]
;;;                                               radius-2-dx radius-2-dy
;;;                                               start-angle end-angle

;;; Draws an ellipse or elliptical arc on the medium medium. The center of the ellipse is at (x,y),
;;; and the radii are specified by the two vectors (radius-1-dx, radius-1-dy) and (radius-2-dx,
;;; radius-2-dy). 

;;; start-angle and end-angle are real numbers that specify an arc rather than a complete ellipse.
;;; Note that the medium and device transformations must be applied to the angles as well. 

;;; AGAIN, WE HAVE THE NON-STANDARD "filled" PARAMETER. Method is similar in execution (complete with
;;; axis-alignment limitation) to CLX back end. We add another limitation in that we ignore the start
;;; and end angles, but at least we made a start ;-)

(defmethod medium-draw-ellipse* ((medium beagle-medium) center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy start-angle end-angle filled)
  (declare (ignore start-angle end-angle))
  (debug-log 2 "medium.lisp -> medium-draw-ellipse*~%")
  ;;; Suspect we should be transforming the radii as well as the centre...
  (unless (or (= radius-2-dx radius-1-dy 0) (= radius-1-dx radius-2-dy 0))
    (error "medium-draw-ellipse* not implemented for non axis-aligned ellipses."))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium)) center-x center-y)
    (with-beagle-graphics (medium)
      (let* ((radius-dx (abs (+ radius-1-dx radius-2-dx)))
	     (radius-dy (abs (+ radius-1-dy radius-2-dy)))
	     (origin-x (- center-x radius-dx))
	     (origin-y (- center-y radius-dy))
	     (width (* 2 radius-dx))
	     (height (* 2 radius-dy)))
	(send path :append-bezier-path-with-oval-in-rect (ccl::make-ns-rect origin-x origin-y width height))
	(if filled
	    (send mirror :fill-path path :in-colour colour)
	  (send mirror :stroke-path path :in-colour colour))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NOT IN SPEC...

(defmethod medium-draw-circle* ((medium beagle-medium) center-x center-y radius start-angle end-angle filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-beagle-graphics (medium)
	(with-transformed-position (tr center-x center-y)
	  (slet ((point (ns-make-point (coerce center-x 'short-float) (coerce center-y 'short-float))))
	    (progn
              ;; Looks to me like this will give part of a circle with a flattened edge
	      ;; when (not (eq start-angle end-angle)). Maybe that's what we want...

	      ;; Cocoa measures angles in DEGREES (not radians) from the x-axis. Need
	      ;; to ensure this is what CLIM is doing too!
	      (send path :append-bezier-path-with-arc-with-center point
		    :radius radius
		    :start-angle (/ start-angle (/ pi 180))
		    :end-angle (/ end-angle (/ pi 180))
		    :clockwise NIL)
	      (if filled
		  (send mirror :fill-path path :in-colour colour)
		(send mirror :stroke-path path :in-colour colour))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-point* medium x y [Generic function] 

;;; Draws a point on the medium medium.

(defmethod medium-draw-point* ((medium beagle-medium) x y)
  (debug-log 2 "medium.lisp -> medium-draw-point*~%")
  (let ((width (coerce (line-style-thickness (medium-line-style medium)) 'short-float)))
    (cond ((< width 2)
	   (medium-draw-line* medium x y (1+ x) y))
	  (t
	   (medium-draw-circle* medium x y (/ width 2) 0 (* 2 pi) T)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-points* medium coord-seq                      [Generic function] 

;;; Draws a set of points on the medium medium. coord-seq is a sequence of coordinate pairs, which
;;; are real numbers. It is an error if coord-seq does not contain an even number of elements. 

(defmethod medium-draw-points* ((medium beagle-medium) coord-seq)
  (debug-log 2 "medium.lisp -> medium-draw-points*~%")
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium)) coord-seq)
    (let ((width (coerce (line-style-thickness (medium-line-style medium)) 'short-float)))
      (cond ((< width 2)
	     (progn
	       (do-sequence ((x y) coord-seq)
	         (medium-draw-line* medium x y (1+ x) y))))
	    (t
	     (progn
	       (do-sequence ((x y) coord-seq)
	         (medium-draw-circle* medium x y (/ width 2) 0 (* 2 pi) T))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-line* medium x1 y1 x2 y2                      [Generic function] 

;;; Draws a line on the medium medium. The line is drawn from (x1,y1) to (x2,y2).

(defmethod medium-draw-line* ((medium beagle-medium) x1 y1 x2 y2)
  (debug-log 2 "medium.lisp -> medium-draw-line*~%")
  ;; Explanation: Drawing primitives in CLIM are specified in sheet coordinate system. We
  ;; need to convert these to native coordinates (i.e. coordinates of the mirror) prior to
  ;; drawing. Sheets can have arbitrary units (pretty much) and arbitrary extent.
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-beagle-graphics (medium)
      (with-transformed-position (tr x1 y1)
	(with-transformed-position (tr x2 y2)
	  (let ((x1 (round-coordinate x1))
		(y1 (round-coordinate y1))
		(x2 (round-coordinate x2))
		(y2 (round-coordinate y2)))
	    (cond ((and (<= #x-8000 x1 #x7FFF) (<= #x-8000 y1 #x7FFF)
			(<= #x-8000 x2 #x7FFF) (<= #x-8000 y2 #x7FFF))
		   (slet ((p1 (ns-make-point (+ (coerce x1 'short-float) 0.5)
					     (+ (coerce y1 'short-float) 0.5)))
			  (p2 (ns-make-point (+ (coerce x2 'short-float) 0.5)
					     (+ (coerce y2 'short-float) 0.5))))
		     (progn
		       (send path :move-to-point p1)
		       (send path :line-to-point p2)
		       (send mirror :stroke-path path :in-colour colour))))
		  (t
		   (let ((line (region-intersection (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)
						    (make-line* x1 y1 x2 y2))))
		     (when (linep line)
		       (multiple-value-bind (x1 y1) (line-start-point* line)
			 (multiple-value-bind (x2 y2) (line-end-point* line)
			   (slet ((p1 (ns-make-point (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate x1))) 'short-float) 0.5)
						     (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate y1))) 'short-float) 0.5)))
				  (p2 (ns-make-point (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate x2))) 'short-float) 0.5)
						     (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate y2))) 'short-float) 0.5))))
			     (progn
			       (send path :move-to-point p1)
			       (send path :line-to-point p2)
			       (send mirror :stroke-path path :in-colour colour)))))))))))))))
				      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-lines* stream position-seq [Generic function] 

;;; Draws a set of disconnected lines on the medium medium. coord-seq is a sequence of coordinate
;;; pairs, which are real numbers. Each successive pair of coordinate pairs is taken as the start
;;; and end position of each line. It is an error if coord-seq does not contain an even number of
;;; elements. 

(defmethod medium-draw-lines* ((medium beagle-medium) coord-seq)
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium)) coord-seq)
    (with-beagle-graphics (medium)
      (do-sequence ((x1 y1 x2 y2) coord-seq)
        (let ((x1 (round-coordinate x1))
	      (y1 (round-coordinate y1))
	      (x2 (round-coordinate x2))
	      (y2 (round-coordinate y2)))
	  (cond ((and (<= #x-8000 x1 #x7FFF) (<= #x-8000 y1 #x7FFF)
		      (<= #x-8000 x2 #x7FFF) (<= #x-8000 y2 #x7FFF))
		 (slet ((p1 (ns-make-point (+ (coerce x1 'short-float) 0.5)
					   (+ (coerce y1 'short-float) 0.5)))
			(p2 (ns-make-point (+ (coerce x2 'short-float) 0.5)
					   (+ (coerce y2 'short-float) 0.5))))
		   (progn
		     (send path :move-to-point p1)
		     (send path :line-to-point p2))))
		(t
		 (let ((line (region-intersection (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)
						  (make-line* x1 y1 x2 y2))))
		   (when (linep line)
		     (multiple-value-bind (x1 y1) (line-start-point* line)
		       (multiple-value-bind (x2 y2) (line-end-point* line)
		         (slet ((p1 (ns-make-point (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate x1))) 'short-float) 0.5)
						   (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate y1))) 'short-float) 0.5)))
				(p2 (ns-make-point (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate x2))) 'short-float) 0.5)
						   (+ (coerce (min #x7FFF (max #x-8000 (round-coordinate y2))) 'short-float) 0.5))))
			   (progn
			     (send path :move-to-point p1)
			     (send path :line-to-point p2)))))))))))
      (send mirror :stroke-path path :in-colour colour))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-rectangle* medium x1 y1 x2 y2 [Generic function] 

;;; Draws a rectangle on the medium medium. The corners of the rectangle are at (x1,y1) and (x2,y2).

(defmethod medium-draw-rectangle* ((medium beagle-medium) left top right bottom filled)
  (debug-log 2 "medium.lisp -> medium-draw-rectangle*~%")
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left bottom)
      (with-transformed-position (tr right top)
        (with-beagle-graphics (medium)
	  ;; Cocoa rect specified with x,y,width + height.
	  ;; x = left, y = bottom, width = right - left, height = top - bottom.
	  (when (< right left) (rotatef left right))
	  (when (< top bottom) (rotatef top bottom))
	  ;; When the following have 0.5 added, things go a little wrong.
	  (let ((left   (round-coordinate left))
		(top    (round-coordinate top))
		(right  (round-coordinate right))
		(bottom (round-coordinate bottom)))
	    ;; append-bezier-path-with-rect automatically closes the path if needed.
	    (send path :append-bezier-path-with-rect (ccl::make-ns-rect left bottom (- right left) (- top bottom)))
	    (if filled
		(send mirror :fill-path path :in-colour colour)
	      (send mirror :stroke-path path :in-colour colour))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; medium-draw-polygon* medium coord-seq closed [Generic function] 

;;; Draws a polygon or polyline on the medium medium. coord-seq is a sequence of coordinate pairs,
;;; which are real numbers.  It is an error if coord-seq does not contain an even number of elements.
;;; Each successive coordinate pair is taken as the position of one vertex of the polygon. 

(defmethod medium-draw-polygon* ((medium beagle-medium) coord-seq closed filled)
  ;; There must be an even number of coordinates - this should be checked at a higher level.
  (assert (evenp (length coord-seq)))
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium)) coord-seq)
    (with-beagle-graphics (medium)
      ;; Move to the start of the polyline
      (send path :move-to-point (ns-make-point (coerce (elt coord-seq 0) 'short-float) (coerce (elt coord-seq 1) 'short-float)))
      (do ((count 2 (+ count 2)))
	  ((> count (1- (length coord-seq))))
	;; Note: not offsetting ordinates by +0.5; will have to see if this is useful.
	(slet ((pt (ns-make-point (coerce (elt coord-seq count) 'short-float)
				  (coerce (elt coord-seq (1+ count)) 'short-float))))
	  (send path :line-to-point pt)))
      ;; ensure polyline joins up if appropriate. This needs to be done after all points have been
      ;; set in the bezier path.
      (when closed
	(send path 'close-path))

      (if filled
	  (send mirror :fill-path path :in-colour colour)
	(send mirror :stroke-path path :in-colour colour)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; medium-draw-text* medium text x y (start 0) end (align-x :left )             [Generic function]
;;;                                   (align-y :baseline ) toward-x toward-y
;;;                                   transform-glyphs

;;; Draws a character or a string on the medium medium. The text is drawn starting at (x,y), and
;;; towards (toward-x, toward-y). In some implementations of CLIM, medium-draw-text* may call either
;;; medium-draw-string* or medium-draw-character* in order to draw the text. 

(defmethod medium-draw-text* ((medium beagle-medium) string x y
			      start end
			      align-x align-y
			      toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (debug-log 2 "medium.lisp -> medium-draw-text*~%")
  (debug-log 3 "               x=~A y=~A string=~A~%" x y string)
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium)) x y)
    (%with-beagle-graphics (medium)
      (when (characterp string)
	(setq string (make-string 1 :initial-element string)))
      (when (null end) (setq end (length string)))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
	  (text-size medium string :start start :end end)
	(declare (ignore x-cursor y-cursor))
	(debug-log 3 "draw-text: mark 1~%")
	(setq x (- x (ecase align-x
		       (:left 0)
		       (:center (round text-width 2))
		       (:right text-width))))
	(debug-log 3 "draw-text: mark 2~%")
	(setq y (ecase align-y
		  (:top (- y text-height))
		  (:center (- y (floor text-height 2)))
		  (:baseline (- y baseline))
		  (:bottom y))))
      (let ((x (+ (round-coordinate x) 0.5))
	    (y (+ (round-coordinate y) 0.5)))
	(when (and (<= #x-8000 x #x7FFF)
		   (<= #x-8000 y #x7FFF))
	  (debug-log 3 "draw-text: mark 3~%")
	  (slet ((point (ns-make-point x y)))
		(let ((objc-string (%make-nsstring (subseq string start end))))
		  (debug-log 3 "draw-text: mark 4~%")
		  (send mirror :draw-string objc-string
			:at-point point
			:with-attributes (reuse-attribute-dictionary medium font :colour colour)
			:in-colour colour
			:with-width width
			:with-cap-style cap
			:with-join-style join)
		  (send objc-string 'release))))))));)

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

;;; This is currently the same as the one defined in the CLX backend; we need to set
;;; this in Cocoa too I guess (I think it's configurable in Cocoa) ::FIXME::
(defmethod medium-miter-limit ((medium beagle-medium))
  #.(* pi (/ 11 180)))
