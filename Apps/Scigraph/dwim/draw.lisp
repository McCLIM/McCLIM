;;; -*- Syntax: Common-lisp; Package: DWIM -*-

#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :dwim)

;;;
;;; Drawing Operations
;;;

;;; Common alu choices.  These ought to be constants, but in CLIM they point to
;;; objects, and that seems to cause some compilers to barf.
(defvar %flip
	#+clim-0.9 :flipping-ink
	#+clim-1.0 clim:+flipping-ink+
	#+clim-2 clim:+flipping-ink+
	#-clim :flip)
(defvar %draw
	#+clim-0.9 :foreground
	#+clim-1.0 clim:+foreground+
	#+clim-2 clim:+foreground-ink+
	#-clim :draw)
(defvar %erase
	#+clim-0.9 :background
	#+clim-1.0 clim:+background+
	#+clim-2 clim:+background-ink+
	#-clim :erase)
(defvar %alu %draw)

(defun make-color-rgb (red green blue)
  #-clim (declare (special color:color-screen))
  #FEATURE-CASE
  (((not :clim)
    (when (color:color-exists-p)
      (scl:send color:color-screen :compute-rgb-alu tv:alu-seta red green blue)))
   (:clim-2
    (clim:make-rgb-color red green blue))
   ((or :clim-1.0 :clim-0.9)
    (clim:make-color-rgb red green blue))))

(defun color-exists-p ()
  #-clim (color:color-exists-p)
  #+clim t)

(defun color-stream-p (stream)
  "Is STREAM capable of rendering color?"
  #FEATURE-CASE
  (((not :clim)
    (and (color:color-exists-p)
	 ;; error, for example if stream = cold-load-stream
	 (ignore-errors (color:color-stream-p stream))))
   (:clim-2
    (and (clim:extended-output-stream-p stream)
	 (or (and
	      (type-specifier-p #-mcclim 'postscript-clim::postscript-stream
				#+mcclim 'clim-postscript::postscript-stream)
	      (typep stream
		     #-mcclim 'postscript-clim::postscript-stream
		     #+mcclim 'clim-postscript::postscript-stream))
	     #+ignore		; disallowed
	     (clim:palette-color-p 
	      (clim:medium-palette (clim:sheet-medium stream)))
	     #+ignore		; currently broken & undocumented
	     (clim:color-stream-p stream)
	     #-mcclim		; "official" answer.
	     (clim:palette-color-p
	      (let ((frame (clim:pane-frame stream)))
		(if frame
		    (clim:frame-palette frame)
		  (clim:port-default-palette
		   (clim:port (clim:sheet-medium stream))))))
	     #+mcclim t)))
   (:clim-0.9
    (if (typep stream 'ci::encapsulating-stream-mixin)
	(color-stream-p (slot-value stream 'ci::stream))
      (let ((port (clim:port stream)))
	(cond #+Genera
	      ((typep port 'on-genera::genera-port)
	       (dolist (inf (tv:sheet-inferiors (on-genera::genera-screen port)))
		 (ignore-errors
		  (return (color:color-stream-p inf)))))
	      #+XLIB
	      ((typep port 'on-x::x-port)
	       (> (slot-value port 'on-x::depth) 1))
	      (t nil)))))
   ((and :clim-1.0 :clx)
    (if (typep stream 'clim::clx-window)
	(slot-value stream 'clim::color-p)
      t))
   ((and :clim-1.0 (not mcl))
    (if (typep stream 'clim::sheet-window-stream)
	(slot-value stream 'clim::color-p)
      t))
   ((and :clim-1.0 mcl)
    (ccl::screen-color-p ccl::*screen-gdevice*))))

(defmacro with-underlining ((stream &key underline-whitespace) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(dw:with-underlining (,stream :underline-whitespace ,underline-whitespace) ,@body))
   (:clim
    `(multiple-value-bind (x y) (stream-cursor-position* ,stream)
       ;; This doesn't work right if output involves multiple lines.
       ,underline-whitespace
       (unwind-protect (progn ,@body)
	 (multiple-value-bind (x2 y2) (stream-cursor-position* ,stream)
	   (draw-line x y x2 y :stream ,stream)))))))

(defmacro surrounding-output-with-border
    ((&optional (stream '*standard-output*)
      &key
      (alu %draw)
      (margin 1)
      (thickness 1)
      (vsp #-clim 4 #+clim 2)
      )
     &body body)
  ;; This one is better done in terms of other DWIM operations.
  (let ((continuation (gensym)) (x (gensym)) (y (gensym))
	(x1 (gensym)) (y1 (gensym))
	(width (gensym)) (height (gensym)))
    `(let ((,continuation #'(lambda (,stream) ,@body)))
       (multiple-value-bind (,x ,y) (stream-cursor-position* ,stream)
	 (stream-set-cursor-position* ,stream (+ ,x ,margin) (+ ,y ,margin))
	 (multiple-value-bind (,width ,height)
	     (continuation-output-size ,continuation ,stream)
	   (funcall ,continuation ,stream)
	   (let ((,x1 (+ ,x ,width ,margin ,margin))
		 (,y1 (+ ,y ,height ,margin ,margin 
			 (- ,vsp))))
	     (stream-set-cursor-position* ,stream ,x1 ,y1)
	     (draw-rectangle ,x ,x1 ,y ,y1
			     :thickness ,thickness
			     :stream ,stream :filled nil :alu ,alu)))))))

;;; Declaring the drawing functions to be inline gets rid of a funcall,
;;; and does the keyword processing at compile time (if possible).
#-(and MCL (not openmcl))
(eval-when (compile load eval)
  (proclaim '(inline
	      draw-point
	      draw-line
	      draw-string
	      draw-string-image
	      draw-polygon
	      draw-triangle
	      draw-rectangle
	      draw-circle
	      draw-ellipse)))

(defun DRAW-POINT (x y &key (stream *standard-output*) (alu %alu) &allow-other-keys)
  #+clim (clim:draw-point* stream x y :ink alu)
  #-clim (graphics:draw-point x y :stream stream :alu alu))

(defun DRAW-LINE (u1 v1 u2 v2 &key
		  (stream *standard-output*)
		  (thickness #+clim 1 #-clim nil)
		  (alu %alu)
		  (line-end-shape :butt)
		  (pattern nil)
		  #+clim (line-dashes nil) ;; add parameter, res 3/2/93
		  &allow-other-keys)
  #+clim (declare (ignore pattern))
  #+clim (clim:draw-line* stream u1 v1 u2 v2 :ink alu
			  :line-thickness thickness
			  :line-cap-shape line-end-shape
			  :line-dashes line-dashes)
  #-clim (graphics:draw-line u1 v1 u2 v2 :stream stream :alu alu
			     :thickness thickness
			     :line-end-shape line-end-shape
			     :pattern pattern))

(defun draw-string (string u v &key
		    (stream *standard-output*)
		    (alu %alu)
		    (attachment-x :left)
		    (attachment-y :baseline)
		    character-style
		    &allow-other-keys)
  #+clim (clim:draw-text* stream string u v :ink alu
			  :align-x attachment-x :align-y attachment-y
			  :text-style character-style)
  #-clim (graphics:draw-string string u v :stream stream
			       :attachment-x attachment-x
			       :attachment-y attachment-y
			       :character-style character-style :alu alu))

(defun draw-vertical-text (text stream u v &key (rotation (/ pi 2))
			   style (alu %draw))
  "Draw ordinary characters on a rotated baseline."
  ;; U,V defined to be vertex of rotation.  Positive rotation 
  ;; is considered to be CLOCKWISE, rather than the usual
  ;; counterclockwise, so that 90 degree rotation leaves the 
  ;; first character at the TOP.  90 degree rotation is the
  ;; most common case, so it should look correct.
  ;;
  ;; OMIT vertical spacing (VSP) between letters for aesthetic 
  ;; reasons.  Looks a little squashed in some cases, but otherwise 
  ;; lettering looks too sparse.
  (let* ((vsp 2)
	 (height (- (stream-line-height stream) vsp)))
    (dotimes (i (length text))
      (let ((char (elt text i)))
	(incf u (round (* height (cos rotation))))
	(incf v (round (* height (sin rotation))))
	(stream-set-cursor-position* stream u v)
	(draw-string (string char) u v
		     :stream stream :character-style style :alu alu)))))

(defun draw-string-image (string u v &key (rotation 0)
			  (stream *standard-output*) (alu %alu)
			  character-style (attachment-y :baseline)
			  &allow-other-keys)
  (cond ((zerop rotation)
	 (draw-string string u v :alu alu :stream stream
		      :attachment-y attachment-y
		      :character-style character-style))
	(t
	 ;; Try to handle rotation.
	 ;; Ideally, we would want to draw on a bitmap here and rotate the bitmap.
	 ;; In CLIM, however, you can't do that.  And in Dynamic Windows, that is
	 ;; an extremely expensive operation.  So forget it.
	 (draw-vertical-text string stream u v
			     :rotation rotation
			     :alu alu :style character-style))))

(defun draw-polygon (points &key (stream *standard-output*) (alu %alu) (filled nil)
		     &allow-other-keys)
  #-clim (graphics:draw-polygon points :stream stream :alu alu :filled filled)
  #+clim (clim:draw-polygon* stream points :ink alu :filled filled))

(defun draw-triangle (u1 v1 u2 v2 u3 v3 &key (stream *standard-output*) (alu %alu)
		      (filled nil) &allow-other-keys)
  #-clim (graphics:draw-triangle u1 v1 u2 v2 u3 v3 :stream stream :filled filled :alu alu)
  #+clim (let ((points (list u1 v1 u2 v2 u3 v3)))
	   ;; No stack allocation, please, redisplay needs the list around
	   ;; permanently. 
	   (clim:draw-polygon* stream points :ink alu :filled filled)))

(defconstant 2pi (* 2 pi))

(defun DRAW-CIRCLE (u v radius &key (filled nil) (stream *standard-output*)
		    (alu %alu) (start-angle 0 start-p) (end-angle 2pi end-p)
		    (thickness 0) &allow-other-keys)
  ;; 30 Sep 91.  CLIM 1.0 bug was detected for start-angle and end-angle.
  ;; Hence won't pass those keywords along unless supplied.
  #+clim (if (or start-p end-p)
	     (clim:draw-circle* stream u v radius :ink alu :filled filled
				:start-angle start-angle :end-angle end-angle
				:line-thickness thickness )
	     (clim:draw-circle* stream u v radius :ink alu :filled filled
				:line-thickness thickness))
  #-clim (graphics:draw-circle u v radius :stream stream :alu alu
			       :filled filled :thickness thickness
			       :start-angle start-angle :end-angle end-angle))

(defun DRAW-RECTANGLE (left right bottom top &key (stream *standard-output*) (alu %alu)
		       (filled nil) (thickness nil) &allow-other-keys)
  #+clim (clim:draw-rectangle* stream left top (1+ right) (1+ bottom)
			       :ink alu :filled filled :line-thickness thickness)
  #-clim (graphics:draw-rectangle left top right bottom :stream stream :alu alu
				  :filled filled :thickness thickness))

(defun draw-ellipse (x-center y-center x-radius y-radius &key
			      (stream *standard-output*)
			      (filled nil)
			      (alu %alu)
			      (start-angle 0)
			      (end-angle 2pi)
			      thickness)
  #-clim
  (graphics:draw-ellipse x-center y-center x-radius y-radius
			 :start-angle start-angle
			 :end-angle end-angle
			 :alu alu
			 :filled filled
			 :stream stream)
  #+clim
  (clim:draw-ellipse* stream x-center y-center x-radius 0 0 y-radius
		      :start-angle start-angle
		      :end-angle end-angle
		      :ink alu
		      :line-thickness thickness
		      :filled filled))


