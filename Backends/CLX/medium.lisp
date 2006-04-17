;;; -*- Mode: Lisp; Package: CLIM-CLX -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)

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

(in-package :clim-clx)

;;; Needed changes:

;; The gc slot in clx-medium must be either thread local, or
;; [preferred] we should have a unified drawing options -> gcontext
;; cache.
;; --GB

;;; CLX-MEDIUM class

(defclass clx-medium (basic-medium)
  ((gc :initform nil)
   (picture :initform nil)
   (buffer :initform nil :accessor medium-buffer)))

#+CLX-EXT-RENDER
(defun clx-medium-picture (clx-medium)
  (with-slots (picture) clx-medium
    (or picture
        (setf picture
              (xlib:render-create-picture (port-lookup-mirror (port clx-medium) (medium-sheet clx-medium)))))))


;;; secondary methods for changing text styles and line styles

(defmethod (setf medium-text-style) :before (text-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-text-style (medium-text-style medium)))
	(unless (eq text-style old-text-style)
	  (setf (xlib:gcontext-font gc)
		(text-style-to-X-font (port medium) (medium-text-style medium))))))))

;;; Translate from CLIM styles to CLX styles.
(defconstant +cap-shape-map+ '((:butt . :butt)
			       (:square . :projecting)
			       (:round . :round)
			       (:no-end-point . :not-last)))

(defun translate-cap-shape (clim-shape)
  (let ((clx-shape (cdr (assoc clim-shape +cap-shape-map+))))
    (if clx-shape
	clx-shape
	(progn
	  (warn "Unknown cap style ~S, using :round" clim-shape)
	  :round))))

(defmethod (setf medium-line-style) :before (line-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-line-style (medium-line-style medium)))
	(unless (eql (line-style-thickness line-style)
		     (line-style-thickness old-line-style))
	  ;; this is kind of false, since the :unit should be taken
	  ;; into account -RS 2001-08-24
	  (setf (xlib:gcontext-line-width gc)
		(round (line-style-thickness line-style))))
	(unless (eq (line-style-cap-shape line-style)
		    (line-style-cap-shape old-line-style))
	  (setf (xlib:gcontext-cap-style gc)
		(translate-cap-shape (line-style-cap-shape line-style))))
	(unless (eq (line-style-joint-shape line-style)
		    (line-style-joint-shape old-line-style))
	  (setf (xlib:gcontext-join-style gc)
		(line-style-joint-shape line-style)))
	;; we could do better here by comparing elements of the vector
	;; -RS 2001-08-24
	(unless (eq (line-style-dashes line-style)
		    (line-style-dashes old-line-style))
	  (setf (xlib:gcontext-line-style gc)
		(if (line-style-dashes line-style) :dash :solid)
		(xlib:gcontext-dashes gc)
		(case (line-style-dashes line-style)
		  ((t nil) 3)
		  (otherwise (line-style-dashes line-style)))))))))

(defmethod (setf medium-clipping-region) :after (region (medium clx-medium))
  (declare (ignore region))
  (with-slots (gc) medium
    (when gc
      (let ((clipping-region (medium-device-region medium)))
        (if (region-equal clipping-region +nowhere+)
	    (setf (xlib:gcontext-clip-mask gc) #())
	    (let ((rect-seq (clipping-region->rect-seq clipping-region)))
	      (when rect-seq
		#+nil
		;; ok, what McCLIM is generating is not :yx-banded...
		;; (currently at least)
		(setf (xlib:gcontext-clip-mask gc :yx-banded) rect-seq)
		#-nil
		;; the region code doesn't support yx-banding...
		;; or does it? what does y-banding mean in this implementation?
		;; well, apparantly it doesn't mean what y-sorted means
		;; to clx :] we stick with :unsorted until that can be sorted out
		(setf (xlib:gcontext-clip-mask gc :unsorted) rect-seq))))))))
  

(defgeneric medium-gcontext (medium ink))

(defmethod medium-gcontext :before ((medium clx-medium) ink)
  (let* ((port (port medium))
	 (mirror (port-lookup-mirror port (medium-sheet medium))))
    (with-slots (gc) medium
      (unless gc
        (setq gc (xlib:create-gcontext :drawable mirror))
        (and gc
             (setf (xlib:gcontext-fill-style gc) :solid))))))

(defmethod medium-gcontext ((medium clx-medium) (ink color))
  (let* ((port (port medium))
	 (mirror (port-lookup-mirror port (medium-sheet medium)))
	 (line-style (medium-line-style medium)))
    (with-slots (gc) medium
      (unless gc
	(setq gc (xlib:create-gcontext :drawable mirror))
	;; this is kind of false, since the :unit should be taken
	;; into account -RS 2001-08-24
	(setf (xlib:gcontext-line-width gc) (line-style-thickness line-style)
	      (xlib:gcontext-cap-style gc) (translate-cap-shape
					    (line-style-cap-shape line-style))
	      (xlib:gcontext-join-style gc) (line-style-joint-shape line-style))
	(let ((dashes (line-style-dashes line-style)))
	  (unless (null dashes)
	    (setf (xlib:gcontext-line-style gc) :dash
		  (xlib:gcontext-dashes gc) (if (eq dashes t) 3
						dashes)))))
      (setf (xlib:gcontext-function gc) boole-1)
      (setf (xlib:gcontext-font gc) (text-style-to-X-font port (medium-text-style medium)))
      (setf (xlib:gcontext-foreground gc) (X-pixel port ink)
	    (xlib:gcontext-background gc) (X-pixel port (medium-background medium)))
      ;; Here is a bug with regard to clipping ... ;-( --GB )
      #-nil ; being fixed at the moment, a bit twitchy though -- BTS
      (let ((clipping-region (medium-device-region medium)))
        (if (region-equal clipping-region +nowhere+)
	    (setf (xlib:gcontext-clip-mask gc) #())
	    (let ((rect-seq (clipping-region->rect-seq clipping-region)))
	      (when rect-seq
		#+nil
		;; ok, what McCLIM is generating is not :yx-banded...
		;; (currently at least)
		(setf (xlib:gcontext-clip-mask gc :yx-banded) rect-seq)
		#-nil
		;; the region code doesn't support yx-banding...
		;; or does it? what does y-banding mean in this implementation?
		;; well, apparantly it doesn't mean what y-sorted means
		;; to clx :] we stick with :unsorted until that can be sorted out
		(setf (xlib:gcontext-clip-mask gc :unsorted) rect-seq)))))
      gc)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +foreground-ink+)))
  (medium-gcontext medium (medium-foreground medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +background-ink+)))
  (medium-gcontext medium (medium-background medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +flipping-ink+)))
  (let* ((gc (medium-gcontext medium (medium-background medium)))
	 (port (port medium))
	 (flipper (logxor (X-pixel port (medium-foreground medium))
			  (X-pixel port (medium-background medium)))))
    ;; Now, (logxor flipper foreground) => background
    ;; (logxor flipper background) => foreground
    (setf (xlib:gcontext-function gc) boole-xor)
    (setf (xlib:gcontext-foreground gc) flipper)
    (setf (xlib:gcontext-background gc) flipper)
    gc))

;;; From Tagore Smith <tagore@tagoresmith.com>

(defmethod medium-gcontext ((medium clx-medium) 
			    (ink climi::standard-flipping-ink))
  (let* ((gc (medium-gcontext medium (medium-background medium)))
	 (port (port medium))
	 (color1 (slot-value ink 'climi::design1))
	 (color2 (slot-value ink 'climi::design2))
	 (flipper (logxor (X-pixel port color1)
			  (X-pixel port color2))))
    (setf (xlib:gcontext-function gc) boole-xor)
    (setf (xlib:gcontext-foreground gc) flipper)
    (setf (xlib:gcontext-background gc) flipper)
    gc))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::indexed-pattern))
  (design-gcontext medium ink))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::rectangular-tile))
  (design-gcontext medium ink))

;;;;

(defmethod design-gcontext :around ((medium clx-medium) (ink climi::indexed-pattern))
  (let ((design-cache (slot-value (port medium) 'design-cache)))
    (let ((cached (gethash ink design-cache)))
      (or cached
          (setf (gethash ink design-cache)
                (call-next-method))))))

(defmethod design-gcontext ((medium clx-medium) (ink climi::indexed-pattern))
  (let* ((array (slot-value ink 'climi::array))
         (inks  (slot-value ink 'climi::designs))
         (w     (array-dimension array 1))
         (h     (array-dimension array 0)))
    (let* ((pm   (allocate-pixmap (first (port-grafts (port medium))) w h))
           (mask (xlib:create-pixmap :drawable (port-lookup-mirror
                                                (port medium)
                                                (first (port-grafts (port medium))))
                                     :depth 1
                                     :width w
                                     :height h))
           (mask-gc (xlib:create-gcontext :drawable mask :foreground 1)))
      (xlib:draw-rectangle mask mask-gc 0 0 w h t)
      (setf (xlib:gcontext-foreground mask-gc) 0)
      (dotimes (y h)
        (dotimes (x w)
          (let ((ink (elt inks (aref array y x))))
            (cond ((eq ink +transparent-ink+)
                   (xlib:draw-point mask mask-gc x y))
                  (t
                   (draw-point* pm x y :ink ink))))))
      (xlib:free-gcontext mask-gc)
      (let ((gc (xlib:create-gcontext :drawable (port-lookup-mirror (port medium) (medium-sheet medium)))))
        (setf (xlib:gcontext-fill-style gc) :tiled
              (xlib:gcontext-tile gc) (port-lookup-mirror (port pm) pm)
              (xlib:gcontext-clip-x gc) 0
              (xlib:gcontext-clip-y gc) 0
              (xlib:gcontext-ts-x gc) 0
              (xlib:gcontext-ts-y gc) 0
              (xlib:gcontext-clip-mask gc) mask)
        gc))))

(defmethod design-gcontext ((medium clx-medium) (ink climi::rectangular-tile))
  (let* ((design (slot-value ink 'climi::design))
         (w      (slot-value ink 'climi::width))
         (h      (slot-value ink 'climi::height)))
    (let ((pm (allocate-pixmap (first (port-grafts (port medium))) w h))) ;dito
      (draw-rectangle* pm 0 0 w h :ink design)
      (let ((gc (xlib:create-gcontext :drawable (port-lookup-mirror (port medium) (medium-sheet medium)))))
        (setf (xlib:gcontext-fill-style gc) :tiled
              (xlib:gcontext-tile gc) (port-lookup-mirror (port pm) pm)
              (xlib:gcontext-clip-x gc) 0
              (xlib:gcontext-clip-y gc) 0
              (xlib:gcontext-ts-x gc) 0
              (xlib:gcontext-ts-y gc) 0)
        gc))))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::transformed-design))
  (let ((transformation (climi::transformed-design-transformation ink))
        (design (climi::transformed-design-design ink)))
    (unless (translation-transformation-p transformation)
      (error "Sorry, not yet implemented."))
    ;; Bah!
    (typecase design
      ((or climi::indexed-pattern climi::rectangular-tile)
       (multiple-value-bind (tx ty)
	   (transform-position transformation 0 0)
	 (let ((gc-x (round-coordinate tx))
	       (gc-y (round-coordinate ty))
	       (gc (design-gcontext medium design)))
	   (setf (xlib:gcontext-ts-x gc) gc-x
               (xlib:gcontext-ts-y gc) gc-y
               (xlib:gcontext-clip-x gc) gc-x
               (xlib:gcontext-clip-y gc) gc-y)
	   gc)))
      (t
       (error "You lost, we not yet implemented transforming an ~S."
              (type-of ink))))))

;;;;

#+nil
(defun clipping-region->rect-seq (clipping-region)
  (loop for region in (nreverse (region-set-regions clipping-region
                                                    :normalize :x-banding))
        as rectangle = (bounding-rectangle region)
        nconcing (list (round (rectangle-min-x rectangle))
                       (round (rectangle-min-y rectangle))
                       (round (rectangle-width rectangle))
                       (round (rectangle-height rectangle)))))

; this seems to work, but find out why all of these +nowhere+s are coming from
; and kill them at the source...
#-nil
(defun clipping-region->rect-seq (clipping-region)
  (loop
     for region in (nreverse (mapcan
			      (lambda (v) (unless (eq v +nowhere+) (list v)))
			      (region-set-regions clipping-region
						  :normalize :y-banding)))
     as rectangle = (bounding-rectangle region)
     for clip-x = (round-coordinate (rectangle-min-x rectangle))
     for clip-y = (round-coordinate (rectangle-min-y rectangle))
     nconcing (list clip-x
		    clip-y
		    (- (round-coordinate (rectangle-max-x rectangle)) clip-x)
		    (- (round-coordinate (rectangle-max-y rectangle)) clip-y))))

(defmacro with-clx-graphics ((medium) &body body)
  `(let* ((port (port ,medium))
	  (mirror (or (medium-buffer medium) (port-lookup-mirror port (medium-sheet ,medium)))))
    (when mirror
      (let* ((line-style (medium-line-style ,medium))
	     (ink        (medium-ink ,medium))
	     (gc         (medium-gcontext ,medium ink)))
	line-style ink
	(unwind-protect
	     (progn ,@body)
	  #+ignore(xlib:free-gcontext gc))))))


;;; Pixmaps
;;; width and height arguments should be integers, but we'll leave the calls
;;; to round in for now.

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
                              from-x from-y)
    (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
                                to-x to-y)
      (multiple-value-bind (width height) (transform-distance (medium-transformation from-drawable)
                                                              width height)
        (xlib:copy-area (sheet-direct-mirror (medium-sheet from-drawable))
                        (medium-gcontext from-drawable +background-ink+)
                        (round-coordinate from-x) (round-coordinate from-y)
                        (round width) (round height)
                        (or (medium-buffer to-drawable) (sheet-direct-mirror (medium-sheet to-drawable)))
                        (round-coordinate to-x) (round-coordinate to-y))))))

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
                              from-x from-y)
    (xlib:copy-area (sheet-direct-mirror (medium-sheet from-drawable))
                    (medium-gcontext from-drawable +background-ink+)
                    (round-coordinate from-x) (round-coordinate from-y)
		    (round width) (round height)
                    (pixmap-mirror to-drawable)
                    (round-coordinate to-x) (round-coordinate to-y))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
                              to-x to-y)
    (xlib:copy-area (pixmap-mirror from-drawable)
                    (medium-gcontext to-drawable +background-ink+)
                    (round-coordinate from-x) (round-coordinate from-y)
		    (round width) (round height)
                    (or (medium-buffer to-drawable) (sheet-direct-mirror (medium-sheet to-drawable)))
                    (round-coordinate to-x) (round-coordinate to-y))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (xlib:copy-area (pixmap-mirror from-drawable)
                  (medium-gcontext from-drawable +background-ink+)
                  (round-coordinate from-x) (round-coordinate from-y)
		  (round width) (round height)
                  (pixmap-mirror to-drawable)
                  (round-coordinate to-x) (round-coordinate to-y)))


;;; Medium-specific Drawing Functions

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-transformed-position ((sheet-native-transformation
                               (medium-sheet medium))
                              x y)
    (with-clx-graphics (medium)
      (cond ((< (line-style-thickness line-style) 2)
             (let ((x (round-coordinate x))
                   (y (round-coordinate y)))
               (when (and (typep x '(signed-byte 16))
                          (typep y '(signed-byte 16)))
                 (xlib:draw-point mirror gc x y))))
            (t
             (let* ((radius (/ (line-style-thickness line-style) 2))
		    (min-x (round-coordinate (- x radius)))
		    (min-y (round-coordinate (- y radius)))
		    (max-x (round-coordinate (+ x radius)))
		    (max-y (round-coordinate (+ y radius))))
	       (when (and (typep min-x '(signed-byte 16))
			  (typep min-y '(signed-byte 16)))
                   (xlib:draw-arc mirror gc min-x min-y
				  (- max-x min-x) (- max-y min-y)
				  0 (* 2 pi) t))))))))


(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (with-transformed-positions ((sheet-native-transformation
                                (medium-sheet medium))
                               coord-seq)
    (with-clx-graphics (medium)
      (cond ((< (line-style-thickness line-style) 2)
             (do-sequence ((x y) coord-seq)
               (let ((x (round-coordinate x))
                     (y (round-coordinate y)))
                 (when (and (typep x '(signed-byte 16))
                            (typep y '(signed-byte 16)))
                   (xlib:draw-point mirror gc x y)))))
            (t
             (let ((radius (/ (line-style-thickness line-style) 2)))
               (do-sequence ((x y) coord-seq)
                 (let ((min-x (round-coordinate (- x radius)))
		       (min-y (round-coordinate (- y radius)))
		       (max-x (round-coordinate (+ x radius)))
		       (max-y (round-coordinate (+ y radius))))
                   (when (and (typep min-x '(signed-byte 16))
                              (typep min-y '(signed-byte 16)))
                     (xlib:draw-arc mirror gc min-x min-y
				    (- max-x min-x) (- max-y min-y)
				    0 (* 2 pi) t))))))))))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (with-clx-graphics (medium)
          (let ((x1 (round-coordinate x1))
                (y1 (round-coordinate y1))
                (x2 (round-coordinate x2))
                (y2 (round-coordinate y2)))
            (cond ((and (<= #x-8000 x1 #x7FFF) (<= #x-8000 y1 #x7FFF)
                        (<= #x-8000 x2 #x7FFF) (<= #x-8000 y2 #x7FFF))
                   (xlib:draw-line mirror gc x1 y1 x2 y2))
                  (t
                   (let ((line (region-intersection (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)
                                                    (make-line* x1 y1 x2 y2))))
                     (when (linep line)
                       (multiple-value-bind (x1 y1) (line-start-point* line)
                         (multiple-value-bind (x2 y2) (line-end-point* line)
                           (xlib:draw-line mirror gc
                                           (min #x7FFF (max #x-8000 (round-coordinate x1)))
                                           (min #x7FFF (max #x-8000 (round-coordinate y1)))
                                           (min #x7FFF (max #x-8000 (round-coordinate x2)))
                                           (min #x7FFF (max #x-8000 (round-coordinate y2))))))))))))))))

;; Invert the transformation and apply it here, as the :around methods on
;; transform-coordinates-mixin will cause it to be applied twice, and we
;; need to undo one of those. The transform-coordinates-mixin stuff needs
;; to be eliminated.
(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (do-sequence ((x1 y1 x2 y2) coord-seq)
        (medium-draw-line* medium x1 y1 x2 y2)))))

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq closed filled)
  ;; TODO:
  ;; . cons less
  ;; . clip
  (assert (evenp (length coord-seq)))
  (with-transformed-positions ((sheet-native-transformation
                                (medium-sheet medium))
                               coord-seq)
    (setq coord-seq (map 'vector #'round-coordinate coord-seq))
    (with-clx-graphics (medium)
      (xlib:draw-lines mirror gc
                       (if closed
                           (concatenate 'vector
                                        coord-seq
                                        (vector (elt coord-seq 0)
                                                (elt coord-seq 1)))
                           coord-seq)
                       :fill-p filled))))

(defmethod medium-draw-rectangle* ((medium clx-medium) left top right bottom filled)
  (medium-draw-rectangle-using-ink* medium (medium-ink medium)
                                    left top right bottom filled))

(defmethod medium-draw-rectangle-using-ink* ((medium clx-medium) (ink t) left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
        (with-clx-graphics (medium)
          (if (< right left)
              (rotatef left right))
          (if (< bottom top)
              (rotatef top bottom))
          (let ((left   (round-coordinate left))
                (top    (round-coordinate top))
                (right  (round-coordinate right))
                (bottom (round-coordinate bottom)))
            ;; To clip rectangles, we just need to clamp the
	    ;; coordinates
            (xlib:draw-rectangle mirror gc
                                 (max #x-8000 (min #x7FFF left))
                                 (max #x-8000 (min #x7FFF top))
                                 (max 0 (min #xFFFF (- right left)))
                                 (max 0 (min #xFFFF (- bottom top)))
                                 filled)))))))

#+CLX-EXT-RENDER
(defmethod medium-draw-rectangle-using-ink* ((medium clx-medium) (ink climi::uniform-compositum)
                                             x1 y1 x2 y2 filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium)))
        (port (port medium)))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (multiple-value-bind (r g b) (color-rgb (slot-value ink 'climi::ink))
            (let ((a (opacity-value (slot-value ink 'climi::mask))))
              ;; Hmm, XRender uses pre-multiplied alpha, how useful!
              (setf r (min #xffff (max 0 (round (* #xffff a r))))
                    g (min #xffff (max 0 (round (* #xffff a g))))
                    b (min #xffff (max 0 (round (* #xffff a b))))
                    a (min #xffff (max 0 (round (* #xffff a)))))
              (let ((picture (clx-medium-picture medium)))
                (xlib:render-fill-rectangle picture :over (list r g b a)
                                            (max #x-8000 (min #x7FFF x1))
                                            (max #x-8000 (min #x7FFF y1))
                                            (max 0 (min #xFFFF (- x2 x1)))
                                            (max 0 (min #xFFFF (- y2 y1))))))))))))


(defmethod medium-draw-rectangles* ((medium clx-medium) position-seq filled)
  (assert (evenp (length position-seq)))
  (with-transformed-positions ((sheet-native-transformation
				(medium-sheet medium))
                               position-seq)
    (with-clx-graphics (medium)
      (loop
	 for (left top right bottom) on position-seq by #'cddddr
	 for min-x = (round-coordinate left)
	 for max-x = (round-coordinate right)
	 for min-y = (round-coordinate top)
	 for max-y = (round-coordinate bottom)
	 nconcing (list min-x min-y (- max-x min-x) (- min-y max-y)) into points
	 finally (xlib:draw-rectangles mirror gc points filled)))))

;;; Round the parameters of the ellipse so that it occupies the expected pixels
(defmethod medium-draw-ellipse* ((medium clx-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (unless (or (= radius-2-dx radius-1-dy 0) (= radius-1-dx radius-2-dy 0))
    (error "MEDIUM-DRAW-ELLIPSE* not yet implemented for non axis-aligned ellipses."))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              center-x center-y)
    (let* ((arc-angle (- end-angle start-angle))
           (arc-angle (if (< arc-angle 0)
                          (+ (* pi 2) arc-angle)
                          arc-angle)))
      (with-clx-graphics (medium)
        (let* ((radius-dx (abs (+ radius-1-dx radius-2-dx)))
	       (radius-dy (abs (+ radius-1-dy radius-2-dy)))
	       (min-x (round-coordinate (- center-x radius-dx)))
	       (min-y (round-coordinate (- center-y radius-dy)))
	       (max-x (round-coordinate (+ center-x radius-dx)))
	       (max-y (round-coordinate (+ center-y radius-dy))))
	   #+nil (when (typep mirror 'xlib:pixmap)
		  (break))
          (xlib:draw-arc mirror gc
                         min-x min-y (- max-x min-x) (- max-y min-y)
                         (mod start-angle (* 2 pi)) arc-angle
                         filled))))))

(defmethod medium-draw-circle* ((medium clx-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (with-transformed-position ((sheet-native-transformation (medium-sheet
							    medium))
                              center-x center-y)
    (let* ((arc-angle (- end-angle start-angle))
           (arc-angle (if (< arc-angle 0)
                          (+ (* pi 2) arc-angle)
                          arc-angle))
	   (min-x (round-coordinate (- center-x radius)))
	   (min-y (round-coordinate (- center-y radius)))
	   (max-x (round-coordinate (+ center-x radius)))
	   (max-y (round-coordinate (+ center-y radius))))
      (with-clx-graphics (medium)
        (xlib:draw-arc mirror gc
                       min-x min-y
                       (- max-x min-x) (- min-y max-y)
                       start-angle arc-angle
                       filled)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for text styles

(defmethod text-style-ascent (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (xlib:font-ascent font)))

(defmethod text-style-descent (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (xlib:font-descent font)))

(defmethod text-style-height (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (+ (xlib:font-ascent font) (xlib:font-descent font))))

(defmethod text-style-character-width (text-style (medium clx-medium) char)
  (xlib:char-width (text-style-to-X-font (port medium) text-style) (char-code char)))

(defmethod text-style-width (text-style (medium clx-medium))
  (text-style-character-width text-style medium #\m))

(eval-when (:compile-toplevel :execute)
  ;; ASCII / CHAR-CODE compatibility checking
  (unless (equal (mapcar #'char-code '(#\Backspace #\Tab #\Linefeed
                                       #\Page #\Return #\Rubout))
                 '(8 9 10 12 13 127))
    (error "~S not ASCII-compatible for semi-standard characters: ~
           implement a CLX translate function for this implementation."
           'code-char))
  (let ((standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
    (dotimes (i 95)
      (unless (eql (char standard-chars i) (code-char (+ i 32)))
        (error "~S not ASCII-compatible for standard character ~S: ~
                implement a CLX translate function for this implementation."
               'code-char (code-char (+ i 32)))))))

;;; The default CLX translation function is defined to work only for
;;; ASCII characters; quoting from the documentation,
;;;
;;;   The default :translate function handles all characters that
;;;   satisfy graphic-char-p by converting each character into its
;;;   ASCII code.
;;;
;;; We provide our own translation function which is essentially the
;;; same as that of CLX, but with the ASCII restriction relaxed.  This
;;; is by no means a proper solution to the problem of
;;; internationalization, because fonts tend not to have a complete
;;; coverage of the entirety of the Unicode space, even assuming that
;;; the underlying lisp supports it (as of 2006-02-06, only the case
;;; for SBCL and CLISP); instead, the translation function is meant to
;;; handle font sets by requesting the X server change fonts in the
;;; middle of rendering strings.  However, the below stands a chance
;;; of working when using ISO-8859-1-encoded fonts, and will tend to
;;; lose in other cases.
(defun translate (src src-start src-end afont dst dst-start)
  (declare (type sequence src)
	   (type xlib:array-index src-start src-end dst-start)
	   (type (or null xlib:font) afont)
	   (type vector dst))
  ;; FIXME: what if AFONT is null?
  (let ((min-char-index (xlib:font-min-char afont))
	(max-char-index (xlib:font-max-char afont)))
    (if (stringp src)
	(do ((i src-start (xlib::index+ i 1))
	     (j dst-start (xlib::index+ j 1))
	     (char))
	    ((xlib::index>= i src-end)
	     i)
          (declare (type xlib:array-index i j))
          (setq char (char-code (char src i)))
          (if (or (< char min-char-index) (> char max-char-index))
              (progn
                (warn "Character ~S not representable in font ~S" 
                      (char src i) afont)
                (return i))
              (setf (aref dst j) char)))
        (do ((i src-start (xlib::index+ i 1))
	     (j dst-start (xlib::index+ j 1))
	     (elt))
	    ((xlib::index>= i src-end)
	     i)
          (declare (type xlib:array-index i j))
          (setq elt (elt src i))
          (when (characterp elt) 
            (setq elt (char-code elt)))
          (if (or (not (integerp elt)) 
                  (< elt min-char-index)
                  (> elt max-char-index))
              (progn
                (warn "Thing ~S not representable in font ~S"
                      (elt src i) afont)
                (return i))
              (setf (aref dst j) elt))))))

(defmethod text-size ((medium clx-medium) string &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-X-font (port medium) text-style)))
    (cond ((= start end)
           (values 0 0 0 0 0))
          (t
           (let ((position-newline (position #\newline string :start start :end end)))
             (cond ((not (null position-newline))
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (xlib:text-extents xfont string
                                           :start start :end position-newline
                                           :translate #'translate)
                      (declare (ignorable left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (multiple-value-bind (w h x y baseline)
                          (text-size medium string :text-style text-style
                                     :start (1+ position-newline) :end end)
                        (values (max w width) (+ ascent descent h)
                                x (+ ascent descent y) (+ ascent descent baseline)))))
                   (t
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (xlib:text-extents xfont string
                                   :start start :end end
                                   :translate #'translate)
                      (declare (ignorable left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (values width (+ ascent descent) width 0 ascent)) )))))) )

(defmethod climi::text-bounding-rectangle*
    ((medium clx-medium) string &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-X-font (port medium) text-style)))
    (cond ((= start end)
           (values 0 0 0 0))
          (t
           (let ((position-newline (position #\newline string :start start :end end)))
             (cond ((not (null position-newline))
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (xlib:text-extents xfont string
                                           :start start :end position-newline
                                           :translate #'translate)
                      (declare (ignorable left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (multiple-value-bind (minx miny maxx maxy)
                          (climi::text-bounding-rectangle*
                           medium string :text-style text-style
                           :start (1+ position-newline) :end end)
                        (values (min minx left) (- ascent)
                                (max maxx right) (+ descent maxy)))))
                   (t
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (xlib:text-extents xfont string
                                   :start start :end end
                                   :translate #'translate)
                      (declare (ignore width direction first-not-done))
                      ;; FIXME: Potential style points:
                      ;; * (min 0 left), (max width right)
                      ;; * font-ascent / ascent
                      (values left (- font-ascent) right font-descent)))))))))



(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-clx-graphics (medium)
      (when (characterp string)
        (setq string (make-string 1 :initial-element string)))
      (when (null end) (setq end (length string)))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline) 
          (text-size medium string :start start :end end)
        (declare (ignore x-cursor y-cursor))
        (unless (and (eq align-x :left) (eq align-y :baseline))	    
          (setq x (- x (ecase align-x
                         (:left 0)
                         (:center (round text-width 2))
                         (:right text-width))))
          (setq y (ecase align-y
                    (:top (+ y baseline))
                    (:center (+ y baseline (- (floor text-height 2))))
                    (:baseline y)
                    (:bottom (+ y baseline (- text-height)))))))
      (let ((x (round-coordinate x))
            (y (round-coordinate y)))
        (when (and (<= #x-8000 x #x7FFF)
                   (<= #x-8000 y #x7FFF))
          (multiple-value-bind (halt width)
              (xlib:draw-glyphs mirror gc x y string
                                :start start :end end
                                :translate #'translate)))))))

(defmethod medium-buffering-output-p ((medium clx-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium clx-medium))
  buffer-p)

(defmethod medium-draw-glyph ((medium clx-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs align-x align-y))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-clx-graphics (medium)
      (xlib:draw-glyph mirror gc (round-coordinate x) (round-coordinate y)
		       element
                       :size 16
                       :translate #'translate))))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium clx-medium))
  (xlib:display-finish-output (clx-port-display (port medium))))

(defmethod medium-force-output ((medium clx-medium))
  (xlib:display-force-output (clx-port-display (port medium))))

(defmethod medium-clear-area ((medium clx-medium) left top right bottom)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
	(let ((min-x (round-coordinate (min left right)))
	      (min-y (round-coordinate (min top bottom)))
	      (max-x (round-coordinate (max left right)))
	      (max-y (round-coordinate (max top bottom))))
	  (xlib:draw-rectangle (or (medium-buffer medium)
				   (port-lookup-mirror (port medium)
						       (medium-sheet medium)))
			       (medium-gcontext medium (medium-background medium))
			       (max #x-8000 (min #x7fff min-x))
			       (max #x-8000 (min #x7fff min-y))
			       (max 0 (min #xffff (- max-x min-x)))
			       (max 0 (min #xffff (- max-y min-y)))
			       t))))))
  
(defmethod medium-beep ((medium clx-medium))
  (xlib:bell (clx-port-display (port medium))))

;;;;

; With-double-buffering is broken, so I remove it for now - BTS
#+nil
(defmethod invoke-with-special-choices (continuation (medium clx-medium))
  (let ((sheet (medium-sheet medium)))
    (with-double-buffering (sheet)
      (funcall continuation (sheet-medium sheet)))))

(defmethod invoke-with-special-choices (continuation (medium clx-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

;;;;

(defmethod medium-miter-limit ((medium clx-medium))
  #.(* pi (/ 11 180)))

(defmethod climi::medium-invoke-with-possible-double-buffering (frame pane (medium clx-medium) continuation)
  (if (climi::pane-double-buffering pane)
      (let* ((mirror (sheet-direct-mirror pane))
	     (width (xlib:drawable-width mirror))
	     (height (xlib:drawable-height mirror))
	     (depth (xlib:drawable-depth mirror))
	     (pixmap (xlib:create-pixmap :width width :height height :depth depth :drawable mirror)))
	(setf (medium-buffer medium) pixmap)
	(unwind-protect (funcall continuation)
	  (xlib:copy-area pixmap (medium-gcontext medium (medium-foreground medium)) 0 0 width height mirror 0 0)
	  (xlib:free-pixmap pixmap)
	  (setf (medium-buffer medium) nil)))
      (funcall continuation)))

