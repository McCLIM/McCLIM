;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
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

(defclass clx-medium (medium)
  ((gc :initform nil)
   )
  )

(defgeneric medium-gcontext (medium ink))

(defmethod medium-gcontext ((medium clx-medium) (ink color))
  (let* ((port (port medium))
	 (mirror (port-lookup-mirror port (medium-sheet medium)))
	 (line-style (medium-line-style medium)))
    (with-slots (gc) medium
      (if (null gc)
	  (setq gc (xlib:create-gcontext :drawable mirror)))
      (setf (xlib:gcontext-font gc) (text-style-to-X-font port (medium-text-style medium))
	    (xlib:gcontext-foreground gc) (X-pixel port ink)
	    (xlib:gcontext-background gc) (X-pixel port (medium-background medium))
            (xlib:gcontext-line-width gc) (line-style-thickness line-style)
            (xlib:gcontext-cap-style gc) (line-style-cap-shape line-style)
            (xlib:gcontext-join-style gc) (line-style-joint-shape line-style))
      (let ((dashes (line-style-dashes line-style)))
        (unless (null dashes)
          (setf (xlib:gcontext-line-style gc) :dash
                (xlib:gcontext-dashes gc) (if (eq dashes t) 3 dashes))))
    ; the right method to use is the medium-device-region, but
    ; as the device-transformation, in the case of the back-end CLX, is 
    ; the identity transformation, a short path is used for optimizing
    ; the drawing operations.
      (let ((clipping-region (medium-clipping-region medium))) ;(medium-device-region medium)))
	(unless (eq clipping-region +everywhere+)
          (setf (xlib:gcontext-clip-mask gc :yx-banded)
                (clipping-region->rect-seq medium clipping-region))))
      gc)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +foreground-ink+)))
  (medium-gcontext medium (medium-foreground medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +background-ink+)))
  (medium-gcontext medium (medium-background medium)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +flipping-ink+)))
  (let ((gc (medium-gcontext medium (medium-background medium))))
    (setf (xlib:gcontext-background gc)
	  (X-pixel (port medium) (medium-foreground medium)))
    gc))

(defun clipping-region->rect-seq (medium clipping-region)
  (loop for region in (nreverse (region-set-regions clipping-region
                                                    :normalize :x-banding))
        as rectangle = (bounding-rectangle (transform-region (medium-transformation medium) region))
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

(defun medium-transform-position (medium x y)
  (multiple-value-bind (xr yr) (bounding-rectangle* 
				(sheet-region (medium-sheet medium)))
    ; the right method to use is the medium-device-transformation, but
    ; as the device-transformation, in the case of the back-end CLX, is 
    ; the identity transformation, a short path is used for optimizing
    ; the drawing operations.
    (transform-position (medium-transformation medium) ;(medium-device-transformation medium)
			(- x xr) (- y yr))))

(defun medium-transform-distance (medium dx dy)
  ; the right method to use is the medium-device-transformation, but
  ; as the device-transformation, in the case of the back-end CLX, is 
  ; the identity transformation, a short path is used for optimizing
  ; the drawing operations.
  (transform-distance (medium-transformation medium) ;(medium-device-transformation medium)
                      dx dy))

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-CLX-graphics (medium)
    (multiple-value-bind (tx ty)
	(medium-transform-position medium x y)
      (if (< (line-style-thickness line-style) 2)
	  (xlib:draw-point mirror gc (round tx) (round ty))
	(let* ((radius (round (line-style-thickness line-style) 2))
               (diameter (* radius 2)))
	  (xlib:draw-arc mirror gc
			 (round (- tx radius)) (round (- ty radius))
			 diameter diameter
			 0 (* 2 pi)
			 t))))))

(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (loop for (x y) on coord-seq by #'cddr
	do (medium-draw-point* medium x y)))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (with-CLX-graphics (medium)
    (multiple-value-bind (tx1 ty1)
	(medium-transform-position medium x1 y1)
      (multiple-value-bind (tx2 ty2)
	  (medium-transform-position medium x2 y2)
	(xlib:draw-line mirror gc (round tx1) (round ty1) (round tx2) (round ty2))))))

(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (with-CLX-graphics (medium)
    (loop with points = (apply #'vector coord-seq)
	  for i below (length coord-seq) by 2
	  do (multiple-value-bind (tx ty)
		 (medium-transform-position medium (aref points i) (aref points (1+ i)))
	       (setf (aref points i) (round tx)
		     (aref points (1+ i)) (round ty)))
	  finally (xlib:draw-segments mirror gc points))))

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (with-CLX-graphics (medium)
    (loop for (x y) on coord-seq by #'cddr
          as (tx ty) = (mapcar #'round (multiple-value-list
                                        (medium-transform-position medium x y)))
          nconcing (list tx ty) into points
          finally (when closed
                    (setf points (nconc points
                                        (list (first points) (second points)))))
                  (xlib:draw-lines mirror gc points :fill-p filled))))

(defmethod medium-draw-rectangle* ((medium clx-medium) left top right bottom filled)
  (with-CLX-graphics (medium)
    (multiple-value-bind (x1 y1)
	(medium-transform-position medium left top)
      (multiple-value-bind (x2 y2)
	  (medium-transform-position medium right bottom)
        (if (rectilinear-transformation-p (medium-transformation medium))
            (progn
              (if (< x2 x1)
                  (rotatef x1 x2))
              (if (< y2 y1)
                  (rotatef y1 y2))
              (xlib:draw-rectangle mirror gc
                                   (round x1) (round y1)
                                   (round (- x2 x1)) (round (- y2 y1))
                                   filled))
            (multiple-value-bind (x1* y1*)
                (medium-transform-position medium right top)
              (multiple-value-bind (x2* y2*)
                  (medium-transform-position medium left bottom)
                (xlib:draw-lines mirror gc
                                 (list (round x1) (round y1)
                                       (round x1*) (round y1*)
                                       (round x2) (round y2)
                                       (round x2*) (round y2*)
                                       (round x1) (round y1))
                                 :fill-p filled))))))))

(defmethod medium-draw-ellipse* ((medium clx-medium) center-x center-y
				 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (unless (or (= radius-2-dx radius-1-dy 0) (= radius-1-dx radius-2-dy 0))
    (error "MEDIUM-DRAW-ELLIPSE* not yet implemented for non axis-aligned ellipses."))
  (with-CLX-graphics (medium)
    (multiple-value-bind (x y)
	(medium-transform-position medium center-x center-y)
      (multiple-value-bind (dx1 dy1)
          (medium-transform-distance medium radius-1-dx radius-1-dy)
        (multiple-value-bind (dx2 dy2)
            (medium-transform-distance medium radius-2-dx radius-2-dy)
          (let ((dx (abs (+ dx1 dx2)))
                (dy (abs (+ dy1 dy2))))
            (xlib:draw-arc mirror gc
                           (round (- x dx)) (round (- y dy))
                           (round (* dx 2)) (round (* dy 2))
                           start-angle (- end-angle start-angle)
                           filled)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for text styles

(defun translate (src src-start src-end afont dst dst-start)
  ;; This is for replacing the clx-translate-default-function
  ;; who does'nt know about accentated characters because
  ;; of a call to cl:graphic-char-p that return nil with accentated characters.
  ;; For further informations, on a clx-translate-function, see the clx-man.
  (declare (type sequence src)
	   (type xlib:array-index src-start src-end dst-start)
	   (type (or null xlib:font) afont)
	   (type vector dst))
  (declare (xlib::clx-values integer
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
  (if (= start end)
      (values 0 0 0 0 0)
      (let ((gctxt (medium-gcontext medium (medium-ink medium)))
            (position-newline (position #\newline string :start start)))
        (if position-newline
            (multiple-value-bind (width ascent descent left right
                                        font-ascent font-descent direction
                                        first-not-done)
                (xlib:text-extents gctxt string
                                   :start start :end position-newline
                                   :translate #'translate)
              (declare (ignorable left right
				  font-ascent font-descent
				  direction first-not-done))
              (multiple-value-bind (w h x y baseline)
                  (text-size medium string :text-style text-style
                             :start (1+ position-newline) :end end)
                (values (max w width) (+ ascent descent h)
                        x (+ ascent descent y) (+ ascent descent baseline))))
            (multiple-value-bind (width ascent descent left right
                                        font-ascent font-descent direction
                                        first-not-done)
                (xlib:text-extents gctxt string
                                   :start start :end position-newline
                                   :translate #'translate)
              (declare (ignorable left right
				  font-ascent font-descent
				  direction first-not-done))
              (values width (+ ascent descent) width 0 ascent))))))

(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-CLX-graphics (medium)
    (when (characterp string)
      (setq string (make-string 1 :initial-element string)))
    (when (null end) (setq end (length string)))
    (multiple-value-bind (tx ty)
	(medium-transform-position medium x y)
      (multiple-value-bind (text-width text-height x y baseline) 
	  (text-size medium string :start start :end end)
	(declare (ignore x y))
	(unless (and (eq align-x :left) (eq align-y :baseline))	    
	  (setq tx (- tx (ecase align-x
			   (:left 0)
			   (:center (round text-width 2))
			   (:right text-width))))
	  (setq ty (ecase align-y
		     (:top (+ ty baseline))
		     (:center (+ ty baseline (- (floor text-height 2))))
		     (:baseline ty)
		     (:bottom (+ ty baseline (- text-height)))))))
      (xlib:draw-glyphs mirror gc (round tx) (round ty) string
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
  (with-CLX-graphics (medium)
    (multiple-value-bind (tx ty)
	(medium-transform-position medium x y)
      (xlib:draw-glyph mirror gc (round tx) (round ty) element
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
