;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graphics-demos.lisp,v 1.14 1993/07/27 01:45:28 colin Exp $

(in-package :clim-demo)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

(define-application-frame graphics-demo ()
    ()
  (:panes 
    (demo :application
	  :min-width 200 :min-height 100
	  :width 800 :height 600)
    (explanation :application
		 :height 100))
  (:layouts 
    (:default
      (vertically () demo explanation))))

(define-graphics-demo-command (com-exit-graphics-demo :menu "Exit")
    ()
  (frame-exit *application-frame*))

(defmacro define-gdemo (name explanation (window) &body body)
  `(define-graphics-demo-command 
       (,(intern (format nil "~A-~A-~A" 'com name 'graphics-demo))
	:menu ,(nstring-capitalize (substitute #\space #\- (string name)))) 
       ()
     (explain ,explanation)
     (let ((,window (get-frame-pane *application-frame* 'demo)))
       (window-clear ,window)
       ,@body)))

(define-gdemo spin "A simple example of the use of affine transforms.
Take a simple function that draws a picture and invoke it
repeatedly under various rotations."
	      (stream)
  (multiple-value-bind (w h) (window-inside-size stream)
    (with-translation (stream (round w 2) (round h 2))
      (with-scaling (stream (/ (min w h) 500))
	(flet ((draw (stream)
		 (draw-rectangle* stream 0 0 50 50 :ink +blue+)
		 (draw-triangle* stream 50 50 50 75 75 50 :ink +cyan+)
		 #+Ignore
		 (draw-circle* stream 70 30 20 :ink +cyan+)))
	  (dotimes (i 8)
	    (let ((angle (* pi (/ i 4))))
	      (with-rotation (stream angle)
		(with-translation (stream 100 0)
		  (draw stream))))))))))

(define-gdemo big-spin "A more complex example using both
rotation and scaling."
	      (stream)
  (multiple-value-bind (w h) (window-inside-size stream)
    (with-translation (stream (round w 2) (round h 2))
      (with-scaling (stream (/ (min w h) 500))
	(with-scaling (stream 1.7)
	  (with-translation (stream 0 -25)
	    (do ((angle 0 (+ angle (/ pi 4)))
		 (scale 1 (* scale 7/8)))
		((< scale .07) nil)
	      ;; ((> angle (* 2 pi)) nil)
	      (with-rotation (stream angle)
		(with-scaling (stream scale)
		  (with-translation (stream 100 0)
		    (dotimes (i 4)
		      (with-translation (stream (* i 18) 0)
			(with-scaling (stream (/ (- 5 i) 5))
			  (draw-rectangle* stream 0 10 10 80)
			  (draw-rectangle* stream 0 70 80 80)
			  (draw-triangle* stream 10 0 10 10 0 10)
			  (draw-triangle* stream 80 70 90 70 80 80)
			  ;; (draw-triangle* stream 0 0 0 10 10 10)
			  ;; (draw-triangle* stream 80 70 80 80 91 80)
			  )))))))))))))

(defun draw-crosshairs-on-window (ws &optional (scale-p nil) (x nil) (y nil) (size nil) (ink +foreground-ink+))
  (multiple-value-bind (width height)
      (window-inside-size ws)
    (unless size
      (setq size (max width height)))
    (unless (and x y)
      (setf x (/ width 4))
      (setf y (/ height 4)))
    (draw-line* ws x (- y size) x (+ y size) :ink ink)
    (draw-line* ws (- x size) y (+ x size) y :ink ink)
    (when scale-p
      (do ((x1 x (- x1 scale-p))
	   (x2 x (+ x2 scale-p))
	   (y1 y (- y1 scale-p))
	   (y2 y (+ y2 scale-p)))
	  ((and (>= x2 size) (>= y2 size)) nil)
	(let ((x3 (- x (/ scale-p 2)))
	      (x4 (+ x (/ scale-p 2)))
	      (y3 (- y (/ scale-p 2)))
	      (y4 (+ y (/ scale-p 2))))
	  (draw-line* ws x3 y1 x4 y1 :ink ink)
	  (draw-line* ws x3 y2 x4 y2 :ink ink)
	  (draw-line* ws x1 y3 x1 y4 :ink ink)
	  (draw-line* ws x2 y3 x2 y4 :ink ink))))
    ))

(define-gdemo cbs-logo ""
	      (stream)
  (multiple-value-bind (w h) (window-inside-size stream)
    (with-translation (stream (round w 2) (round h 2))
      (with-scaling (stream (/ (min w h) 500))
	(let ((ink (make-rgb-color 0 .5 1)))
	  (draw-circle* stream 0 0 200 :ink ink)
	  (draw-ellipse* stream 0 0 200 0 0 100 :ink +background-ink+)
	  (draw-circle* stream 0 0 100 :ink ink)
	  (draw-crosshairs-on-window stream 25 0 0 200 +background-ink+))))))

(defun demo-sleep (ws secs)
  (finish-output ws)
  (let ((end-time (+ (get-internal-real-time) (* internal-time-units-per-second secs))))
    (loop
      (let ((time-to-go (- end-time (get-internal-real-time))))
	(unless (plusp time-to-go) (return nil))
	(multiple-value-bind (gesture type)
	    (read-gesture :stream ws :timeout (/ time-to-go internal-time-units-per-second))
	  (case type
	    ((:timeout) (return nil))
	    ((nil)
	     (if (characterp gesture)
		 (return t)
		 (frame-exit *application-frame*)))))))))

(defun compute-regular-polygon (x1 y1 x2 y2 n)
  (let ((theta (* pi (1- (/ 2.0 n))))
	(coords (make-list (* 2 n))))
    (let ((temp coords))
      (macrolet ((addit (x)
		   `(progn
		      (setf (car temp) (float ,x 0s0))
		      (setf temp (cdr temp)))))
	(addit x1)
	(addit y1)
	(addit x2)
	(addit y2)
	(do ((i 2 (1+ i))
	     (sin-theta (sin theta))
	     (cos-theta (cos theta))
	     (x3) (y3))
	    ((not (< i n)))
	  (setq x3 (+ (- (- (* x1 cos-theta)
			    (* y1 sin-theta))
			 (* x2 (1- cos-theta)))
		      (* y2 sin-theta))
		y3 (- (- (+ (* x1 sin-theta)
			    (* y1 cos-theta))
			 (* x2 sin-theta))
		      (* y2 (1- cos-theta))))
	  (addit x3)
	  (addit y3)
	  (setq x1 x2 y1 y2 x2 x3 y2 y3))))
    coords))

(defvar *polygons* (make-array 10))
(do ((i 3 (1+ i)))
    ((= i 10))
  (setf (aref *polygons* i) (compute-regular-polygon 0 1 0 -1 i)))

(define-gdemo polygons ""
	      (stream)
  (multiple-value-bind (w h) (window-inside-size stream)
    (with-translation (stream (- (round w 2) 200) (round h 2))
      (with-scaling (stream (/ (min w h) 500))
	(dolist (number-of-sides '(3 #+Ignore 4 5 #+Ignore 6 #+Ignore 7 8))
	  (window-clear stream)
	  (do ((i 100 (- i 5)))
	      ((< i 10) nil)
	    ;;--- assumption about size of viewport and current transform
	    (with-scaling (stream i)
	      (draw-polygon* stream (aref *polygons* number-of-sides) :filled t
			     :ink (if (oddp i) +background-ink+ +foreground-ink+))))
	  (demo-sleep stream 2))))))

(defconstant *random-ink-list*
	     (list +red+ +green+ +blue+
		   +cyan+ +magenta+ +yellow+ +black+))

(defun random-ink ()
  (nth (random (length *random-ink-list*)) *random-ink-list*))

(define-gdemo circles "A lot of circles in a variety of colors.
On a monochrome display, stipples are used to simulate the colors."
	      (stream)
  (let* ((radius 20)
	 (separation (+ 2 (* 2 radius))))
    (multiple-value-bind (wid hei)
	(window-inside-size stream)
      (do ((y separation (+ y separation)))
	  ((> y (- hei separation)) nil)
	(do ((x separation (+ x separation)))
	    ((> x (- wid separation)) nil)
	  (draw-circle* stream x y radius :filled nil :ink (random-ink)))))))

(define-gdemo maze "This simple maze drawer uses the graphics
scaling feature to adjust the maze size
to the window in which it is displayed."
	      (stream)
  (multiple-value-bind (w h) (window-inside-size stream)
    ;; --- seems to be designed for 700x600 window, so scale appropriately
    (let ((xs (/ w 700)) (ys (/ h 600)))
      (with-scaling (stream xs ys)
	(draw-polygon* stream '(30   40 670  40 670 560  30 560  30  80  70  80
				 70  520 630 520 630  80 590  80 590 480 110 480
				 110 120 510 120 510 400 190 400 190 160)
		       :closed nil :filled nil :line-thickness 3)
	(draw-polygon* stream '(110  80 550  80 550 440 150 440 150 160 470 160
				 470 360 230 360 230 200 430 200 430 320 270 320
				 270 240 390 240 390 280)
		       :closed nil :filled nil :line-thickness 3)
	;;draw start
	(draw-circle* stream 25 60 5)
	;; draw finish
	(draw-circle* stream 330 280 5)
  
	(demo-sleep stream 3)

	;; draw a solution path
	(draw-polygon* stream '(30 60 570  60 570 460 130 460 130 140 490 140
			       490 380 210 380 210 180 450 180 450 340 250 340
			       250 220 410 220 410 300 330 280)
		       :ink +green+
		       :closed nil :filled nil)
	))))

;;; The EXPLAINs should probably be in some def-graphics-demo form rather
;;; than scattered in the code...
(defun explain (text)
  (let ((window (get-frame-pane *application-frame* 'explanation)))
    (when window
      (window-clear window)
      (with-text-style (window '(:sans-serif :roman :large))
	(write-string text window)))))



(define-demo "Graphics Demos" graphics-demo)
