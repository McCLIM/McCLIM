;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: ico.lisp,v 1.22 1993/12/07 05:33:55 colin Exp $

;;;
;;; Copyright (c) 1989, 1990 by Xerox Corporation.  All rights reserved.
;;; Portions (c) 1992 Franz Inc.
;;; Algorithm pretty much lifted from X11 demo program.
;;;

(in-package :clim-demo)

;;;
;;; Entry
;;;

(defvar *ico-mode* :clim)

;;;
;;; ICO Frames
;;;

(defparameter *background-color* +black+)
(defparameter *line-color* +white+)
(defparameter *face1-color* +red+)
(defparameter *face2-color* +green+)

(define-application-frame ico-frame ()
    ((ico-time-p :initform t)
     (ico-line-style :initform :thin)
     (ico-buffers :initform 1)
     (ico-color-set2 :initform nil)
     (ico-color-set3 :initform nil)
     (inks0 :initform (vector *background-color*))
     (inks1 :initform (vector *line-color*))
     (inks2 :initform (vector *face1-color*))
     (inks3 :initform (vector *face2-color*))
     (draw-edges :initform t)
     (draw-faces :initform nil)
     (ico-process :initform nil :accessor ico-process))
  (:command-table (ico-frame :inherit-from (accept-values-pane)))
  (:panes
    (target :application :width 400 :height 400)
    (options :accept-values
	     :scroll-bars nil
	     :min-width :compute
	     :width :compute :height :compute
	     :max-height :compute
	     :display-function `(accept-values-pane-displayer
				  :displayer display-options-pane)))
  (:layouts (:default (vertically () target options))))

(defmethod frame-target ((fr ico-frame))
  (get-frame-pane fr 'target))

(defmethod display-options-pane ((frame ico-frame) pane &key max-width max-height)
  (declare (ignore max-width max-height))
  (formatting-item-list (pane :n-rows 1)
    (with-slots (ico-time-p ico-buffers ico-line-style draw-edges draw-faces) frame
      (formatting-cell (pane)
	(setf ico-time-p (accept 'boolean
				 :default ico-time-p
				 :stream pane
				 :prompt "Time")))
      (formatting-cell (pane)
	(let ((x (append (and draw-faces '(:faces))
			 (and draw-edges '(:edges)))))
	  (setf x (accept '(subset :faces :edges) :default x :stream pane :prompt "Choose"))
	  (setf draw-edges (and (member :edges x) t)
		draw-faces (and (member :faces x) t))))
      (formatting-cell (pane)
	(setf ico-line-style (accept '(member :thin :thick)
				     :default ico-line-style
				     :stream pane
				     :prompt "Line style")))
      (when (palette-dynamic-p (frame-palette frame))
	(formatting-cell (pane)
	  (multiple-value-bind (buffering ptype changed)
	      (accept '(member :single :double :triple)
		      :default (svref #(:single :double :triple) (1- ico-buffers))
		      :stream pane
		      :prompt "Buffering")
	    (declare (ignore ptype))
	    (when changed
	      (with-slots (ico-color-set2 ico-color-set3
			   inks0 inks1 inks2 inks3) frame
		(case buffering
		  (:single
		    (setf ico-buffers 1
			  inks0 (vector *background-color*)
			  inks1 (vector *line-color*)
			  inks2 (vector *face1-color*)
			  inks3 (vector *face2-color*)))
		  (:double
		   (let ((g (or ico-color-set2
				(let ((color-set (make-layered-color-set 4 4)))
				  (add-colors-to-palette (frame-palette frame)
							 color-set)
				  (setf ico-color-set2 color-set)))))
		     (setf ico-buffers 2
			   inks0 (vector (layered-color g 0 nil)
					 (layered-color g nil 0))
			   inks1 (vector (layered-color g 1 nil)
					 (layered-color g nil 1))
			   inks2 (vector (layered-color g 2 nil)
					 (layered-color g nil 2))
			   inks3 (vector (layered-color g 3 nil)
					 (layered-color g nil 3)))))
		  (:triple
		   (let ((g (or ico-color-set3
				(let ((color-set (make-layered-color-set 4 4)))
				  (add-colors-to-palette (frame-palette frame)
							 color-set)
				  (setf ico-color-set3 color-set)))))
		     (setf ico-buffers 3
			   inks0 (vector (layered-color g 0 nil nil)
					 (layered-color g nil 0 nil)
					 (layered-color g nil nil 0))
			   inks1 (vector (layered-color g 1 nil nil)
					 (layered-color g nil 1 nil)
					 (layered-color g nil nil 1))
			   inks2 (vector (layered-color g 2 nil nil)
					 (layered-color g nil 2 nil)
					 (layered-color g nil nil 2))
			   inks3 (vector (layered-color g 3 nil nil)
					 (layered-color g nil 3 nil)
					 (layered-color g nil nil 3))))))))))))))


(define-ico-frame-command (com-ico-throw-ball :menu "Throw ball" :keystroke #\t) ()
  (with-application-frame (frame)
    (throw-ball frame)))

(define-ico-frame-command (com-ico-catch-ball :menu "Catch ball" :keystroke #\c) ()
  (with-application-frame (frame)
    (catch-ball frame)))

(define-ico-frame-command (com-ico-quit :menu "Quit" :keystroke #\q) ()
  (with-application-frame (frame)
    (with-slots (ico-color-set2 ico-color-set3) frame
      (let ((palette (frame-palette frame)))
	(catch-ball frame)
	(when ico-color-set2
	  (remove-colors-from-palette palette ico-color-set2)
	  (setf ico-color-set2 nil))
	(when ico-color-set3
	  (remove-colors-from-palette palette ico-color-set3)
	  (setf ico-color-set3 nil))
	(frame-exit frame)))))

(defmethod disable-frame :after ((frame ico-frame))
  (catch-ball frame))

(defmethod throw-ball ((frame ico-frame))
  (unless (ico-process frame)
    (setf (ico-process frame)
	  (clim-sys:make-process #'(lambda ()
				     (ignore-errors
				      (if (slot-value frame 'ico-time-p)
					  (time (run-ico frame))
					(run-ico frame))
				      (setf (ico-process frame) nil)))
				 :name "ICO ball process"))))

(defmethod catch-ball ((frame ico-frame))
  (when (ico-process frame)
    (clim-sys:destroy-process (ico-process frame))
    (setf (ico-process frame) nil)))

(defmethod repaint-frame ((frame ico-frame) pane region)
  (declare (ignore pane region))
  (throw-ball frame))


;;;
;;; Ico, the stuff
;;;

(defun make-ico-point (x y)
  (#+allegro excl::fast #-allegro progn
    (let ((z (make-array 2 :element-type 'single-float)))
      (declare (type (simple-array single-float (2)) z))
      (setf (aref z 0) (float x 0s0)
	    (aref z 1) (float y 0s0))
      z)))

(defmacro ico-point-x (z)
  `(the single-float (aref (the (simple-array single-float (2)) ,z) 0)))
(defmacro ico-point-y (z)
  `(the single-float (aref (the (simple-array single-float (2)) ,z) 1)))

(defconstant ico-w   150)
(defconstant ico-h   150)
(defconstant ico-w/2 (floor ico-w 2))
(defconstant ico-h/2 (floor ico-h 2))

(defun run-ico (frame &optional (count 1000000))
  (let* ((pane  (frame-target frame))
	 (medium (sheet-medium pane))
	 (win-w (bounding-rectangle-width pane))
	 (win-h (bounding-rectangle-height pane))
	 (ico-x (floor (* (- win-w ico-w) 128) 256))
	 (ico-y (floor (* (- win-h ico-h) 128) 256))
	 (ico-dx 13)
	 (ico-dy 9)
	 (xtop (- win-w ico-w))
	 (ytop (- win-h ico-h))
	 (prev-x 0)
	 (prev-y 0)
	 edges
	 face-list
	 display black white drawable gcontext)
    (declare (integer win-w win-h)
	     (integer ico-x ico-y ico-dx ico-dy xtop ytop prev-x prev-y)
	     (special prev-x prev-y)
	     (ignore gcontext white black display
		     #-(or xlib-ignore genera) drawable))

    (with-slots (draw-faces draw-edges ico-buffers inks0 inks1 inks2 inks3) frame
      (ecase *ico-mode*
	#+abc
	(:abc (clear-region medium +everywhere+))
	#+clim
	(:clim)
	(:dont)
	#+xlib-ignore
	(:clx
	  (setq display (on-x::x-display (port medium)))
	  (setq black (xlib:screen-black-pixel (on-x::x-screen (port medium))))
	  (setq white (xlib:screen-white-pixel (on-x::x-screen (port medium))))
	  (setq drawable (medium-drawable medium))
	  (setq gcontext (slot-value medium 'on-x::gcontext))
	  (setf (xlib:gcontext-fill-style gcontext) :solid))
	#+genera
	(:genera
	  (setq drawable (medium-drawable medium))))

      (window-clear pane)
      (dotimes (i count)
	(let* ((ico-buffers ico-buffers)
	       (inks0 inks0)
	       (inks1 inks1)
	       (inks2 inks2)
	       (inks3 inks3)
	       (buffer (mod i ico-buffers)))
	  (multiple-value-setq (edges face-list)
	    (calculate-ico ico-x ico-y draw-edges draw-faces edges face-list))
	  ;; Draw ICO
	  (ecase *ico-mode*
	    #+clim
	    (:clim
	      (draw-rectangle* medium 0 0 win-w win-h :ink (svref inks0 buffer) :filled t)
	      (when draw-edges
		(draw-lines* medium edges :ink (svref inks1 buffer)
			     :line-thickness
			     (ecase (slot-value frame 'ico-line-style)
			       (:thick 5)
			       (:thin nil))))
	      (when draw-faces
		(do ((f face-list (cdr (cdddr (cdddr f)))))
		    ((null f))
		  (draw-polygon* medium (subseq f 1 7)
				 :closed t
				 :filled t
				 :ink (if (oddp (car f))
					  (svref inks2 buffer)
					  (svref inks3 buffer)))))
	      (when (> ico-buffers 1)
		(with-delayed-recoloring
		  (setf (layered-color-color (svref inks0 buffer)) *background-color*
			(layered-color-color (svref inks1 buffer)) *line-color*
			(layered-color-color (svref inks2 buffer)) *face1-color*
			(layered-color-color (svref inks3 buffer)) *face2-color*)))
	      (medium-force-output medium))

	    #+xlib-ignore
	    (:clx
	      (setf (xlib:gcontext-foreground gcontext) white)
	      (xlib:draw-rectangle drawable gcontext prev-x prev-y
				   (1+ ico-w) (1+ ico-h) t)
	      (setf (xlib:gcontext-foreground gcontext) black)
	      (xlib:draw-segments drawable gcontext
				  (mapcan
				    #'(lambda (point)
					(list (round (ico-point-x point))
					      (round (ico-point-y point))))
				    edges))
	      (xlib:display-force-output display))

	    #+genera
	    (:genera
	      (scl:send drawable :draw-rectangle
			(1+ ico-w) (1+ ico-h) prev-x prev-y
			:erase)
	      (apply #'scl:send drawable :draw-lines :draw
				(mapcan
				  #'(lambda (point)
				      (list (round (ico-point-x point))
					    (round (ico-point-y point))))
				  edges)))
	    (:dont))

	  (setq prev-x ico-x
		prev-y ico-y)
	  (incf ico-x ico-dx)
	  (when (or (< ico-x 0) (> ico-x xtop))
	    (decf ico-x (* ico-dx 2))
	    (setq ico-dx (- ico-dx)))
	  (incf ico-y ico-dy)
	  (when (or (< ico-y 0) (> ico-y ytop))
	    (decf ico-y (* ico-dy 2))
	    (setq ico-dy (- ico-dy))))))))


;;;
;;; DRAW ICO
;;;

(defconstant nv 12)
(defconstant nf 20)

(defparameter v3-seq
	      (let ((x
		      '(;; Initial Position
			(0.0        0.0        -0.9510565)
			(0.0        0.8506508  -0.42532536)
			(0.809017   0.26286557 -0.42532536)
			(0.5       -0.68819094 -0.42532536)
			(-0.5      -0.68819094 -0.42532536)
			(-0.809017  0.26286557 -0.42532536)
			(0.5        0.68819094  0.42532536)
			(0.809017  -0.26286557  0.42532536)
			(0.0       -0.8506508   0.42532536)
			(-0.809017 -0.26286557  0.42532536)
			(-0.5       0.68819094  0.42532536)
			(0.0        0.0         0.9510565))))
		(make-array (list (length x) 3)
			    :element-type 'single-float
			    :initial-contents x)))

(defparameter faces '((0 2 1)  (0 3 2)  (0 4 3)  (0 5 4)
		      (0 1 5)  (1 6 10) (1 2 6)  (2 7 6)
		      (2 3 7)  (3 8 7)  (3 4 8)  (4 9 8)
		      (4 5 9)  (5 10 9) (5 1 10) (10 6 11)
		      (6 7 11) (7 8 11) (8 9 11) (9 10 11)))

(defparameter xform nil) ; Initialized Below

(defmacro v2-aref (vertex-number field)
  (case field
    ((xy) `(aref v2 ,vertex-number 0))
    ((z) `(aref v2 ,vertex-number 1))
    (t (error "Bad array reference on v2"))))

(defparameter v2 (make-array
		   (list nv 2)
		   :initial-contents
		     (clim-utils:with-collection
		       (dotimes (i nv)
			 (clim-utils:collect (list (make-ico-point 0 0) 0))))))
(defparameter v2-fill (make-array (* nv 2) :displaced-to v2))

(defparameter drawn      (make-array (list nv nv) :initial-element nil))
(defparameter drawn-fill (make-array (* nv nv) :displaced-to drawn))

(defmacro with-non-consing-collection ((list) &body body)
  `(let* (($with-collection-result$ (or ,list (list :hohoho-and-b-of-rum)))
	  ($with-collection-tail$ $with-collection-result$)
	  $with-collection-rest$)
     (macrolet
       ((collect (form)
	  ;;  The FORM is evaluated first so that COLLECT nests
	  ;; properly, i.e., The test to determine if this is
	  ;; the first value collected should be done after the
	  ;; value itself is generated in case it does
	  ;; collection as well.
	  `(let (($collectable$ ,form))
	     (if $with-collection-tail$
		 (progn
		   (setf (car $with-collection-tail$) $collectable$)
		   (setq $with-collection-tail$
			 (cdr $with-collection-tail$)))
		 (push $collectable$ $with-collection-rest$))
	     $collectable$)))
       ,@body
       (unless (eq (car $with-collection-result$) :hohoho-and-b-of-rum)
	 (if $with-collection-rest$
	     (nconc $with-collection-result$ (nreverse $with-collection-rest$))
	     $with-collection-result$)))))

(defun calculate-ico (ico-x ico-y do-edges do-faces edge-point-list face-point-list)
  (declare (integer ico-x ico-y)
	   (optimize (safety 0) (speed 3)))
  (let (#+allegro (v2-fill (excl::array-base v2))
	(v3-seq v3-seq)
	(v2 v2)
	(drawn drawn)
	(fp 0)
	p0 p1 p2)
    (declare
      (type (simple-array single-float (12 3)) v3-seq)
      (type (simple-array t (12 2)) v2)
      (type (simple-array t (12 12)) drawn)
      #+allegro (simple-vector v2-fill))

    ;; Clear the drawn array
    #+allegro (fill (excl::array-base drawn-fill) nil)
    #-allegro (fill drawn-fill nil)

    ;; Rotate vertices
    (partial-nonhom-transformation xform v3-seq)

    ;; Convert 3d coordinates to 2D positions
    (dotimes (i (array-dimension v3-seq 0))
      (let ((z (aref v2-fill fp))
	    (x (+ ico-x (* ico-w/2 (+ (aref v3-seq i 0) 1.0))))
	    (y (+ ico-y (* ico-h/2 (+ (aref v3-seq i 1) 1.0)))))
	(declare (single-float x y))
	(if z
	    (setf (ico-point-x z) x (ico-point-y z) y)
	    (setf (aref v2-fill fp)
		  (make-ico-point x y))))
      (incf fp)
      ;; Save the z for hidden line removal
      (setf (aref v2-fill fp) (aref v3-seq i 2))
      (incf fp))

    ;; Accumulate edges, w/o duplicates
    (values
      (when do-edges
	(with-non-consing-collection (edge-point-list)
	  (macrolet ((collect-point (p)
		       (let ((pp (gensym)))
			 `(let ((,pp ,p))
			    (collect (ico-point-x ,pp))
			    (collect (ico-point-y ,pp))))))
	    (dolist (face faces)
	      (setq p0 (first face))
	      (setq p1 (second face))
	      (setq p2 (third face))
	      ;; Unless hidden for some reason if the sum of the
	      ;; z-coordinates of the faces is < 0 then its hidden???
	      (unless (< (+ (v2-aref p0 z)
			    (v2-aref p1 z)
			    (v2-aref p2 z))
			 0.0)
		;; At this point if we are drawing faces then we have
		;; something to do

		;; Mark for Draw
		(unless (aref drawn p0 p1)
		  (setf (aref drawn p0 p1) t)
		  (setf (aref drawn p1 p0) t)
		  (collect-point (v2-aref p0 xy))
		  (collect-point (v2-aref p1 xy)))

		(unless (aref drawn p1 p2)
		  (setf (aref drawn p1 p2) t)
		  (setf (aref drawn p2 p1) t)
		  (collect-point (v2-aref p1 xy))
		  (collect-point (v2-aref p2 xy)))

		(unless (aref drawn p2 p0)
		  (setf (aref drawn p2 p0) t)
		  (setf (aref drawn p0 p2) t)
		  (collect-point (v2-aref p2 xy))
		  (collect-point (v2-aref p0 xy))))))))

      (when do-faces
	(with-non-consing-collection (face-point-list)
	  (macrolet ((collect-point (p)
		       (let ((pp (gensym)))
			 `(let ((,pp ,p))
			    (collect (ico-point-x ,pp))
			    (collect (ico-point-y ,pp))))))
	    (let ((i 0))
	      (dolist (face faces)
		(setq p0 (first face))
		(setq p1 (second face))
		(setq p2 (third face))
		;; Unless hidden for some reason if the sum of the
		;; z-coordinates of the faces is < 0 then its hidden???
		(unless (< (+ (v2-aref p0 z)
			      (v2-aref p1 z)
			      (v2-aref p2 z))
			   0.0)
		  (collect (incf i))
		  (collect-point (v2-aref p0 xy))
		  (collect-point (v2-aref p1 xy))
		  (collect-point (v2-aref p2 xy)))))))))))


;;;
;;; Matrix Operations
;;;

(deftype transformation-3d nil '(simple-array single-float (4 4)))

(defun concat-mat (l r m)
  (declare (type transformation-3d l r m))
  (dolist (i '(0 1 2 3))
    (dolist (j '(0 1 2 3))
      (setf (aref m i j)
	    (+ (* (aref l i 0)
		  (aref r 0 j))
	       (* (aref l i 1)
		  (aref r 1 j))
	       (* (aref l i 2)
		  (aref r 2 j))
	       (* (aref l i 3)
		  (aref r 3 j))))))
  m)

(defun format-rotate-mat (axis angle m)
  (declare (character axis)
	   (single-float angle)
	   (type transformation-3d m))
  (ident-mat m)
  (let ((s (sin angle))
	(c (cos angle)))
    (declare (single-float s c))
    (case axis
      (x (setf (aref m 1 1) c)
	 (setf (aref m 2 2) c)
	 (setf (aref m 1 2) s)
	 (setf (aref m 2 1) (- s)))
      (y (setf (aref m 0 0) c)
	 (setf (aref m 2 2) c)
	 (setf (aref m 2 0) s)
	 (setf (aref m 0 2) (- s)))
      (z (setf (aref m 0 0) c)
	 (setf (aref m 1 1) c)
	 (setf (aref m 0 1) s)
	 (setf (aref m 1 0) (- s))))))

(defun ident-mat (m)
  (declare (type transformation-3d m))
  (dolist (i '(0 1 2 3))
    (dolist (j '(0 1 2 3))
      (setf (aref m i j) 0.0))
    (setf (aref m i i) 1.0))
  m)

(defun partial-nonhom-transformation (m v3-seq)
  (declare (type transformation-3d m)
	   (type (simple-array single-float (12 3)) v3-seq)
	   (optimize (safety 0) (speed 3)))
  (let ((m00 (aref m 0 0))
	(m10 (aref m 1 0))
	(m20 (aref m 2 0))
	(m01 (aref m 0 1))
	(m11 (aref m 1 1))
	(m21 (aref m 2 1))
	(m02 (aref m 0 2))
	(m12 (aref m 1 2))
	(m22 (aref m 2 2)))
    (dotimes (i (array-dimension v3-seq 0))
      (let* ((in-x (aref v3-seq i 0))
	     (in-y (aref v3-seq i 1))
	     (in-z (aref v3-seq i 2)))
	(setf (aref v3-seq i 0) (+ (* in-x m00)
				   (* in-y m10)
				   (* in-z m20))
	      (aref v3-seq i 1) (+ (* in-x m01)
				   (* in-y m11)
				   (* in-z m21))
	      (aref v3-seq i 2) (+ (* in-x m02)
				   (* in-y m12)
				   (* in-z m22)))))))

(defun create-xform nil
  (let ((r1 (create-transformation-3d))
	(r2 (create-transformation-3d))
	(r3 (create-transformation-3d)))
    (format-rotate-mat 'x (/ (* 5 (coerce pi 'single-float)) 180.0) r1)
    (format-rotate-mat 'y (/ (* 5 (coerce pi 'single-float)) 180.0) r2)
    (concat-mat r1 r2 r3)
    r3))

(defun create-transformation-3d nil
  (make-array '(4 4) :element-type 'single-float))

(setq xform (create-xform))



(define-demo "Ico Demo" ico-frame :left 200 :top 50)
