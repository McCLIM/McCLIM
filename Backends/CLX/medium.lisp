;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

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

(in-package :CLIM-INTERNALS)

;;; CLX-MEDIUM class

(defclass clx-medium (basic-medium)
  ((gc :initform nil)
   ) )


;;; secondary methods for changing text styles and line styles

(defmethod (setf medium-text-style) :before (text-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-text-style (medium-text-style medium)))
	(unless (eq text-style old-text-style)
	  (setf (xlib:gcontext-font gc)
		(text-style-to-X-font (port medium) (medium-text-style medium))))))))

(defmethod (setf medium-line-style) :before (line-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-line-style (medium-line-style medium)))
	(unless (eql (line-style-thickness line-style)
		     (line-style-thickness old-line-style))
	  ;; this is kind of false, since the :unit should be taken
	  ;; into account -RS 2001-08-24
	  (setf (xlib:gcontext-line-width gc)
		(line-style-thickness line-style)))
	(unless (eq (line-style-cap-shape line-style)
		    (line-style-cap-shape old-line-style))
	  (setf (xlib:gcontext-cap-style gc)
		(line-style-cap-shape line-style)))
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
  

(defgeneric medium-gcontext (medium ink))

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
	      (xlib:gcontext-cap-style gc) (line-style-cap-shape line-style)
	      (xlib:gcontext-join-style gc) (line-style-joint-shape line-style))
	(let ((dashes (line-style-dashes line-style)))
	  (unless (null dashes)
	    (setf (xlib:gcontext-line-style gc) :dash
		  (xlib:gcontext-dashes gc) (if (eq dashes t) 3
						dashes)))))
      (setf (xlib:gcontext-function gc) boole-1)
      (setf (xlib:gcontext-font gc) (text-style-to-X-font port (medium-text-style medium))
	    (xlib:gcontext-foreground gc) (X-pixel port ink)
	    (xlib:gcontext-background gc) (X-pixel port (medium-background medium)))
      (let ((clipping-region (medium-device-region medium)))
        (unless (region-equal clipping-region +nowhere+)
          (setf (xlib:gcontext-clip-mask gc :yx-banded)
                (clipping-region->rect-seq clipping-region))))
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

(defun clipping-region->rect-seq (clipping-region)
  (loop for region in (nreverse (region-set-regions clipping-region
                                                    :normalize :x-banding))
        as rectangle = (bounding-rectangle region)
        nconcing (list (round (rectangle-min-x rectangle))
                       (round (rectangle-min-y rectangle))
                       (round (rectangle-width rectangle))
                       (round (rectangle-height rectangle)))))

(defmacro with-CLX-graphics ((medium) &body body)
  `(let* ((port (port ,medium))
	  (mirror (port-lookup-mirror port (medium-sheet ,medium)))
	  (line-style (medium-line-style ,medium))
	  (ink (medium-ink ,medium))
	  (gc (medium-gcontext ,medium ink)))
     line-style ink
     (unwind-protect
	 (progn ,@body)
       #+ignore(xlib:free-gcontext gc))))


;;; Pixmaps

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
                              from-x from-y)
    (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
                                to-x to-y)
      (xlib:copy-area (sheet-direct-mirror (medium-sheet from-drawable))
                      (medium-gcontext from-drawable +background-ink+)
                      (round from-x) (round from-y) (round width) (round height)
                      (sheet-direct-mirror (medium-sheet to-drawable))
                      (round to-x) (round to-y)))))

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet from-drawable))
                              from-x from-y)
    (xlib:copy-area (sheet-direct-mirror (medium-sheet from-drawable))
                    (medium-gcontext from-drawable +background-ink+)
                    (round from-x) (round from-y) (round width) (round height)
                    (pixmap-mirror to-drawable)
                    (round to-x) (round to-y))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
                              to-x to-y)
    (xlib:copy-area (pixmap-mirror from-drawable)
                    (medium-gcontext to-drawable +background-ink+)
                    (round from-x) (round from-y) (round width) (round height)
                    (sheet-direct-mirror (medium-sheet to-drawable))
                    (round to-x) (round to-y))))

(defmethod medium-copy-area ((from-drawable pixmap) from-x from-y width height
                             (to-drawable pixmap) to-x to-y)
  (xlib:copy-area (pixmap-mirror from-drawable)
                  (medium-gcontext from-drawable +background-ink+)
                  (round from-x) (round from-y) (round width) (round height)
                  (pixmap-mirror to-drawable)
                  (round to-x) (round to-y)))


;;; Medium-specific Drawing Functions

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-CLX-graphics (medium)
      (if (< (line-style-thickness line-style) 2)
          (xlib:draw-point mirror gc (round x) (round y))
          (let* ((radius (round (line-style-thickness line-style) 2))
                 (diameter (* radius 2)))
            (xlib:draw-arc mirror gc
                           (round (- x radius)) (round (- y radius))
                           diameter diameter
                           0 (* 2 pi)
                           t))))))

(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium))
                              coord-seq)
    (setq coord-seq (mapcar #'round coord-seq))
    (with-CLX-graphics (medium)
      (if (< (line-style-thickness line-style) 2)
          (xlib:draw-points mirror gc coord-seq)
          (loop with radius = (round (line-style-thickness line-style) 2)
                with diameter = (* radius 2)
                for (x y) on coord-seq by #'cddr
                nconcing (list (round (- x radius)) (round (- y radius))
                               diameter diameter
                               0 (* 2 pi)) into arcs
                finally (xlib:draw-arcs mirror gc arcs t))))))

(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2. 
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  (floor (+ x 1/2)))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (with-CLX-graphics (medium)
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

(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (map-repeated-sequence nil 4
                         (lambda (x1 y1 x2 y2)
                           (medium-draw-line* medium x1 y1 x2 y2))
                         coord-seq))

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium))
                               coord-seq)
    (setq coord-seq (mapcar #'round-coordinate coord-seq))
    (with-CLX-graphics (medium)
      (xlib:draw-lines mirror gc
                       (if closed
                           (append coord-seq (list (first coord-seq)
                                                   (second coord-seq)))
                           coord-seq)
                       :fill-p filled))))

(defmethod medium-draw-rectangle* ((medium clx-medium) left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
        (with-CLX-graphics (medium)
          (if (< right left)
              (rotatef left right))
          (if (< bottom top)
              (rotatef top bottom))
          (let ((left   (round-coordinate left))
                (top    (round-coordinate top))
                (right  (round-coordinate right))
                (bottom (round-coordinate bottom)))
            ;; To clip rectangles, we just need to clamp the cooridnates
            (xlib:draw-rectangle mirror gc
                                        (max #x-8000 (min #x7FFF left))
                                        (max #x-8000 (min #x7FFF top))
                                        (max 0 (min #xFFFF (- right left)))
                                        (max 0 (min #xFFFF (- bottom top)))
                                        filled) ))))))

(defmethod medium-draw-rectangles* ((medium clx-medium) position-seq filled)
  (assert (evenp (length position-seq)))
  (with-transformed-positions ((sheet-native-transformation (medium-sheet medium))
                               position-seq)
    (with-CLX-graphics (medium)
      (loop for (left top right bottom) on position-seq by #'cddddr
            nconcing (list (round left) (round top)
                           (round (- right left)) (round (- bottom top))) into points
                           finally (xlib:draw-rectangles mirror gc points filled)))))

(defmethod medium-draw-ellipse* ((medium clx-medium) center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (unless (or (= radius-2-dx radius-1-dy 0) (= radius-1-dx radius-2-dy 0))
    (error "MEDIUM-DRAW-ELLIPSE* not yet implemented for non axis-aligned ellipses."))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              center-x center-y)
    (with-CLX-graphics (medium)
      (let ((radius-dx (abs (+ radius-1-dx radius-2-dx)))
            (radius-dy (abs (+ radius-1-dy radius-2-dy))))
        (xlib:draw-arc mirror gc
                       (round (- center-x radius-dx)) (round (- center-y radius-dy))
                       (round (* radius-dx 2)) (round (* radius-dy 2))
                       start-angle (- end-angle start-angle)
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

(defun translate (src src-start src-end afont dst dst-start)
  ;; This is for replacing the clx-translate-default-function
  ;; who does'nt know about accentated characters because
  ;; of a call to cl:graphic-char-p that return nil with accentated characters.
  ;; For further informations, on a clx-translate-function, see the clx-man.
  (declare (type sequence src)
	   (type xlib:array-index src-start src-end dst-start)
	   (type (or null xlib:font) afont)
	   (type vector dst))
  #+cmucl(declare (xlib::clx-values integer
				    (or null integer xlib:font)
				    (or null integer)))
  (let ((min-char-index (xlib:font-min-char afont))
	(max-char-index (xlib:font-max-char afont)))
    afont
    (if (stringp src)
	(do ((i src-start (xlib::index+ i 1))
	     (j dst-start (xlib::index+ j 1))
	     (char))
	    ((xlib::index>= i src-end)
	     i)
	    (declare (type xlib:array-index i j))
	    (setq char (xlib:char->card8 (char src i)))
	    (if (or (< char min-char-index) (> char max-char-index))
		(return i)
	        (setf (aref dst j) char)))
        (do ((i src-start (xlib::index+ i 1))
	     (j dst-start (xlib::index+ j 1))
	     (elt))
	    ((xlib::index>= i src-end)
	     i)
	    (declare (type xlib:array-index i j))
	    (setq elt (elt src i))
	    (when (characterp elt) (setq elt (xlib:char->card8 elt)))
	    (if (or (not (integerp elt)) 
		    (< elt min-char-index)
		    (> elt max-char-index))
		(return i)
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
           (let ((position-newline (position #\newline string :start start)))
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
                                   :start start :end (length string)
                                   :translate #'translate)
                      (declare (ignorable left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (values width (+ ascent descent) width 0 ascent)) )))))) )

(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-CLX-graphics (medium)
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
      (xlib:draw-glyphs mirror gc (round-coordinate x) (round-coordinate y) string
                        :start start :end end
                        :translate #'translate))))

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
    (with-CLX-graphics (medium)
      (xlib:draw-glyph mirror gc (round x) (round y) element
                       :translate #'translate))))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium clx-medium))
  (xlib:display-finish-output (clx-port-display (port medium))))

(defmethod medium-force-output ((medium clx-medium))
  (xlib:display-force-output (clx-port-display (port medium))))

(defmethod medium-clear-area ((medium clx-medium) left top right bottom)
  (xlib:clear-area (port-lookup-mirror (port medium) (medium-sheet medium))
                   :x (round left) :y (round top)
                   :width (round (- right left)) :height (round (- bottom top))))

(defmethod medium-beep ((medium clx-medium))
  (xlib:bell (clx-port-display (port medium))))

;;;;

(defmethod invoke-with-special-choices (continuation (sheet clx-medium))
  ;; CLX-MEDIUM right here? --GB
  (with-double-buffering (sheet)
    (funcall continuation sheet)))

;;;;

