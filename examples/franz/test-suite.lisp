;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: test-suite.lisp,v 1.77 1995/10/17 05:02:53 colin Exp $

(in-package :clim-user)

#|
To do:

Benchmarks of how long it takes to bring up an application frame.
AND and OR types?  SEQUENCE?  What others?
More correct search for the sensitive presentation (Move command on NOR gate).
Non-unique unique-ids in redisplay.
DRAGGING-OUTPUT should replay the records that get clobbered as you drag.
Dialog-style input of partial commands.
Mixed-mode commands that contains "unreadable" objects in them.
See if to-command translators work when the command has no command-line name.
Presentation histories and kill rings?
c-? in completion when there are no completions.
MENU-CHOOSE when there are no items.
Text style accessors.
What about environment issue?
  Compile-time diagnostics
  Accurate argument lists
  Proper set of exported symbols

|#


(defmacro repeat (n &body body)
  (let ((i '#:i))
    `(dotimes (,i ,n)
       #-(or minima genera allegro) (declare (ignore i))
       ,@body)))

(defmacro with-display-pane ((stream) &body body)
  `(let ((,stream (get-frame-pane *application-frame* 'display-pane)))
     (window-clear ,stream)
     ,@body))

(defvar *all-the-tests* nil)

(defmacro define-test ((name command-table) (stream) caption &body body)
  #+genera (declare (zwei:indentation 2 1))
  (check-type caption (or null string))
  (let ((command-name (clim-utils:fintern "~A-~A" 'com name)))
    `(progn
       (pushnew ',name *all-the-tests*)
       (define-command (,command-name :command-table ,command-table :menu t) ()
	 (write-test-caption ,caption)
	 (with-display-pane (,stream)
	   (,name ,stream)))
       (defun ,name (,stream)
	 ,@body))))

(defun write-test-caption (caption)
  (let ((stream (get-frame-pane *application-frame* 'caption-pane)))
    (window-clear stream)
    (when caption
      (filling-output (stream :fill-width '(80 :character))
	(write-string caption stream)))))


;;; General graphics and text drawing tests

(define-command-table graphics)

(defvar *color-wheel*
	(let ((colors (mapcar #'symbol-value
			      '(+black+ +red+ +yellow+ +green+ +cyan+ +blue+ +magenta+))))
	  (nconc colors colors)			;circular
	  colors))

(defvar *gray-wheel*
	(let ((grays (let ((grays nil))
		       (dotimes (i 7)
			 (let ((x (/ (mod (* i 4) 7) 7.0)))
			   (push (make-rgb-color x x x) grays)))
		       (nreverse grays))))
	  (nconc grays grays)			;circular
	  grays))

;; Try to get millimeters
(defun window-mm-transformation (window)
  (with-bounding-rectangle* (wl wt wr wb) (window-viewport window)
    (make-transformation 3.4 0 0 -3.4 (floor (- wr wl) 2) (floor (- wb wt) 2))))

(defmacro with-mm-transformation ((window -x -y +x +y) &body body)
  `(let ((transform (window-mm-transformation ,window)))
     (with-drawing-options (,window :transformation transform)
       (with-bounding-rectangle* (,-x ,-y ,+x ,+y)
				 (untransform-region transform (window-viewport ,window))
	 ,@body))))

(defun draw-grid (stream)
  (with-new-output-record (stream)
    (with-mm-transformation (stream -x -y +x +y)
      (draw-line* stream -x 0 +x 0 :line-thickness 2)
      (draw-line* stream 0 -y 0 +y :line-thickness 2)
      (do ((x (ceiling -x) (1+ x)))
	  ((> x +x))
	(unless (zerop x)
	  (if (zerop (mod x 10))
	      (draw-line* stream x -y x +y :line-thickness 1)
	      (draw-line* stream x -1 x +1 :line-thickness 1))))
      (do ((y (ceiling -y) (1+  y)))
	  ((> y +y))
	(unless (zerop y)
	  (if (zerop (mod y 10))
	      (draw-line* stream -x y +x y :line-thickness 1)
	      (draw-line* stream -1 y +1 y :line-thickness 1)))))))

(defun draw-multiple-rectangles (stream &optional (size 10) (sx 0) (sy 0))
  (with-mm-transformation (stream -x -y +x +y)
    (declare (ignore -x -y +x +y))
    (let ((x sx)
	  (y sy))
      (dolist (filled '(t nil))
	(dotimes (i 7)
	  (draw-rectangle* stream x y (+ x size) (+ y size)
			   :ink (nth i *color-wheel*) :filled filled)
	  (incf x (+ size 10)))
	(setq x sx)
	(incf y (+ size 10))
	(dotimes (i 7)
	  (draw-rectangle* stream x y (+ x size) (+ y size)
			   :ink (nth i *gray-wheel*) :filled filled)
	  (incf x (+ size 10)))
	(setq x sx)
	(incf y (+ size 10))))))



(define-test (draw-some-rectangles graphics) (stream)
  "Draw some rectangles, wait a few seconds, and refresh the window."
  (draw-grid stream)
  (draw-multiple-rectangles stream)
  (sleep 2)
  (window-refresh stream))

(define-test (rotated-scaled-rectangles graphics) (stream)
  "Draw some rectangles, first rotated then scaled, wait a few seconds, then refresh."
  (draw-grid stream)
  (with-scaling (stream 2 3)
    (with-rotation (stream (float (/ pi 6.0) 1.0))
      (draw-multiple-rectangles stream))))

(define-test (scaled-rotated-rectangles graphics) (stream)
  "Draw some rectangles, first scaled then rotated, wait a few seconds, then refresh."
  (draw-grid stream)
  (with-rotation (stream (float (/ pi 6.0) 1.0))
    (with-scaling (stream 2 3)
      (draw-multiple-rectangles stream))))

(defun draw-multiple-circles (stream &optional (size 10) (sx 0) (sy 0))
  (with-mm-transformation (stream -x -y +x +y)
    (declare (ignore -x -y +x +y))
    (let ((x sx)
	  (y sy))
      (dolist (filled '(t nil))
	(dotimes (i 7)
	  (draw-circle* stream x y size
			:ink (nth i *color-wheel*) :filled filled)
	  (incf x (+ size 10)))
	(setq x sx)
	(incf y (+ size 10))
	(dotimes (i 7)
	  (draw-circle* stream x size size
			:ink (nth i *gray-wheel*) :filled filled)
	  (incf x (+ size 10)))
	(setq x sx)
	(incf y (+ size 10))))))

(define-test (draw-some-circles graphics) (stream)
  "Draw some circles, wait a few seconds, and refresh the window."
  (draw-grid stream)
  (draw-multiple-circles stream)
  (sleep 2)
  (window-refresh stream))

(define-test (rotated-scaled-circles graphics) (stream)
  "Draw some circles, first rotated then scaled, wait a few seconds, then refresh."
  (draw-grid stream)
  (with-scaling (stream 2 3)
    (with-rotation (stream (float (/ pi 6.0) 1.0))
      (draw-multiple-circles stream))))

(define-test (scaled-rotated-circles graphics) (stream)
  "Draw some circles, first scaled then rotated, wait a few seconds, then refresh."
  (draw-grid stream)
  (with-rotation (stream (float (/ pi 6.0) 1.0))
    (with-scaling (stream 2 3)
      (draw-multiple-circles stream))))

(define-test (draw-some-arcs graphics) (stream)
  "Draw some arcs, wait a few seconds, and refresh the window."
  (formatting-table (stream)
    (dolist (angles `((0 ,(* 2 pi))
		      (0 ,(* 0.5 pi))
		      (,(* 0.5 pi) ,pi)
		      (,pi ,(* 1.5 pi))
		      (,(* 1.5 pi) ,(* 2 pi))))
      (formatting-row (stream)
	(formatting-cell (stream)
	  (format stream "Arc from ~3F to ~3F" (car angles) (second angles)))
	(formatting-cell (stream)
	  (draw-circle* stream 0 0 50
			:ink +red+ :filled t
			:start-angle (car angles) :end-angle (second angles)))
	(formatting-cell (stream)
	  (draw-circle* stream 0 0 50
			:ink +green+ :filled t
			:start-angle (second angles) :end-angle (car angles)))))))

(define-test (draw-some-points graphics) (stream)
  "Draw lots of points"
  (formatting-table (stream)
    (dotimes (size 10)
      (formatting-row (stream)
	(formatting-cell (stream)
	  (with-text-face (stream :italic)
	    (format stream "~D size" size)))
	(formatting-cell (stream)
	  (let ((points nil))
	    (dotimes (i 10)
	      (push (random 50) points)
	      (push (random 50) points))
	    (surrounding-output-with-border (stream)
	      (draw-points* stream points
			    :ink +red+
			    :line-thickness size))))))))


(defparameter *chess-board-clip-region*
    (let ((region +nowhere+)
	  (cell 80)
	  (grid 8))
      (dotimes (i grid)
	(dotimes (j grid)
	  (when (eq (evenp i) (evenp j))
	    (setq region
	      (region-union
	       (make-bounding-rectangle
		(* i cell) (* j cell) (* (1+ i) cell) (* (1+ j) cell))
	       region)))))
      region))

(defparameter *gettysburg-address*
	      "
Fourscore and seven years ago our forefathers
brought forth on this continent a new nation,
conceived in Liberty, and dedicated to the
proposition that all men are created equal.
Now we are engaged in a great civil war,
testing whether that nation, or any nation so
conceived and so dedicated, can long endure.
We are met on a great battlefield of that
war.  We have come to dedicate a portion of
that field, as a final resting place for
those who here gave their lives that that
nation might live.  It is altogether fitting
and proper that we do so.  But, in a larger
sense, we cannot dedicate -- we cannot
consecrate -- we cannot hallow this ground.
The brave men, living and dead, who struggled
here, have consecrated it far above our poor
power to add or detract.  The world will
little note, nor long remember, what we say
here, but it can never forget what they did
here.  It is for us, the living, rather, to
be dedicated here to the unfinished work
which they who fought here have thus far so
nobly advanced.  It is rather for us to be
here dedicated to the great task remaining
before us -- that from these honored dead we
take increased devotion to that cause for
which they gave the last full measure of
devotion -- that we here highly resolve that
these dead shall not have died in vain --
that this nation, under God, shall have a new
birth of freedom -- and that government of
the people, by the people, and for the
people, shall not perish from the earth.

			-- Abraham Lincoln, 19 November 1863
")

(define-test (gettysburg graphics) (stream)
  "The Gettysburg address."
  (write-string *gettysburg-address* stream))

(defun write-styled-gettysburg (stream)
  (flet ((limits (string)
	   (let ((start (search string *gettysburg-address*)))
	     (values start (and start (+ start (length string)))))))
    (multiple-value-bind (p1 p2) (limits "Fourscore and seven years ago")
      (multiple-value-bind (p3 p4) (limits "Abraham Lincoln")
	(write-string *gettysburg-address* stream :start 0 :end p1)
	(with-text-face (stream ':bold)
	  (write-string *gettysburg-address* stream :start p1 :end p2))
	(write-string *gettysburg-address* stream :start p2 :end p3)
	(with-text-face (stream ':italic)
	  (write-string *gettysburg-address* stream :start p3 :end p4))
	(write-string *gettysburg-address* stream :start p4)))))

(define-test (styled-gettysburg graphics) (stream)
  "The Gettysburg address, with text styles."
  (write-styled-gettysburg stream))

(define-test (blue-gettysburg graphics) (stream)
  "The Gettysburg address, in blue."
  (with-drawing-options (stream :ink +blue+)
    (write-styled-gettysburg stream)))

(define-test (draw-enstyled-text graphics) (stream)
  "Write all manner of styled text."
  (let ((*print-pretty* nil)
	(text "The quick brown fox jumped over the lazy dog.")
	(families '(nil :fix :serif :sans-serif))
	(faces '(nil :bold :italic (:bold :italic)))
	(sizes '(nil :normal :very-small :very-large :small :large :smaller :larger)))
    (with-end-of-line-action (stream :allow)
      (dolist (size sizes)
	(dolist (face faces)
	  (dolist (family families)
	    (let ((style (parse-text-style (list family face size))))
	      (fresh-line stream)
	      (with-text-style (stream style)
		(write-string text stream))
	      (format stream "  (~A)" style))))))))

;; Row of column headings
(defun abbreviated-regions-column-headings (regions stream &optional (title nil title-p))
  (formatting-row (stream)
    (formatting-cell (stream :align-x :right)
      (when title-p
	(with-text-face (stream :bold)
	  (write-string (string title) stream))))
    (dolist (r regions)
      (formatting-cell (stream :align-x :center)
	(with-text-face (stream :italic)
	  (if (listp r)
	      (format stream
		      (case (first r)
			(make-point "Point~&~:@{~D ~D~}")
			(make-line* "Line~&~:@{~D,~D~&~D,~D~}")
			(make-rectangle* "Rect~&~:@{~D,~D~&~D,~D~}"))
		      (rest r))
	      (format stream "~A" r)))))))

(defun region-test-comment (stream)
  (fresh-line stream)
  (terpri stream)
  (filling-output (stream :fill-width '(60 :character))
    (write-string "Erroneous results are shown in " stream)
    (with-text-face (stream :bold)
      (write-string "boldface" stream))
    (write-string ".  Select an element of the table with the mouse " stream)
    (write-string "to evaluate the corresponding test manually." stream)))

(defparameter *test-regions-for-region-equal*
	      '(+nowhere+
		+everywhere+
		(make-point 0 0)
		(make-point 1 1)
		(make-line* 0 0 10 10)
		(make-line* 10 10 20 20)
		(make-rectangle* 0 0 10 10)
		(make-rectangle* 10 10 20 20)))

(define-test (region-equal-tests graphics) (stream)
  "Exercise REGION-EQUAL."
  (formatting-table (stream :x-spacing "  ")
    (abbreviated-regions-column-headings
      *test-regions-for-region-equal* stream 'region-equal)
    (dolist (region1 *test-regions-for-region-equal*)
      (formatting-row (stream)
	(formatting-cell (stream :align-x :right)
	  (with-text-face (stream :italic)
	    (format stream "~A" region1)))
	(dolist (region2 *test-regions-for-region-equal*)
	  (formatting-cell (stream :align-x :center)
	    (handler-case			;guard against unimplemented cases
	        (let ((result (region-equal (eval region1) (eval region2)))
		      (correct-result (equal region1 region2)))
		  (with-output-as-presentation
		      (stream `(region-equal ,region1 ,region2) 'form
		       :single-box t)
		    (if (eq result correct-result)
			(format stream "~A" result)
			(with-text-face (stream :bold)
			  (format stream "~A" result)))))
	      (error ()
		(with-text-face (stream :bold)
		  (write-string "Error signalled" stream)))))))))
  (region-test-comment stream))

(defparameter *test-regions-for-region-contains-position-p*
	      ;; region position correct-result
	      '((+nowhere+ (0 0) nil)	;region position correct-result
		(+nowhere+ (5 5) nil)
		(+everywhere+ (0 0) t)
		(+everywhere+ (5 5) t)
		((make-point 0 0) (0 0) t)
		((make-point 0 0) (5 5) nil)
		((make-line* 0 0 10 10) (0 0) t)
		((make-line* 0 0 10 10) (5 5) t)
		((make-line* 0 0 10 10) (10 10) t)
		((make-line* 5 0 5 10) (0 0) nil)
		((make-line* 5 0 5 10) (5 5) t)
		((make-rectangle* 0 0 10 10) (0 0) t)
		((make-rectangle* 0 0 10 10) (5 5) t)
		((make-rectangle* 0 0 10 10) (10 10) t)
		((make-rectangle* 10 10 20 20) (5 5) nil)
		((make-rectangle* 10 10 20 20) (10 10) t)))

(define-test (region-contains-point-tests graphics) (stream)
  "Exercise REGION-CONTAINS-POSITION-P."
  (let ((regions nil)
	(positions nil))
    (dolist (result *test-regions-for-region-contains-position-p*)
      (pushnew (first result) regions :test #'equal)
      (pushnew (second result) positions :test #'equal))
    (setq regions (nreverse regions)
	  positions (nreverse positions))
    (formatting-table (stream :x-spacing "  ")
      (formatting-row (stream)
	(formatting-cell (stream)
	  (declare (ignore stream)))		;make a column for the row headings
	(dolist (position positions)
	  (formatting-cell (stream)
	    (with-text-face (stream :italic)
	      (format stream "(~D,~D)" (first position) (second position))))))
      (dolist (region regions)
	(formatting-row (stream)
	  (formatting-cell (stream :align-x :right)
	    (with-text-face (stream :italic)
	      (format stream "~A" region)))
	  (dolist (position positions)
	    (let ((res (find-if #'(lambda (result-entry)
				    (and (equal (first result-entry) region)
					 (equal (second result-entry) position)))
				*test-regions-for-region-contains-position-p*))
		  (x (first position))
		  (y (second position)))
	      (with-output-as-presentation
		  (stream `(region-contains-position-p ,region ,x ,y) 'form
		   :single-box t)
		(formatting-cell (stream :align-x :center)
		  (when res
		    (handler-case		;guard against unimplemented cases
		        (let* ((correct-result (third res))
			       (result (region-contains-position-p (eval region) x y)))
			  (with-text-face (stream (if (eq correct-result result) nil :bold))
			    (format stream "~A" result)))
		      (error ()
		        (with-text-face (stream :bold)
			  (write-string "Error signalled" stream))))))))))))
    (region-test-comment stream)))

(defparameter *test-regions-for-region-contains-region-p*
	      ;; (region1 region2 1-contains-2 2-contains-1)
	      ;; no need to include the case where the regions are
	      ;; region-equal, they will automatically be considerred.
	      '((+nowhere+ +everywhere+ nil t)
		(+nowhere+ (make-point 0 0) nil t)
		(+everywhere+ (make-point 0 0) t nil)
		((make-point 0 0) (make-point 1 1) nil nil)
		((make-point 0 0) (make-line* 0 0 10 10) nil t)
		((make-point 5 5) (make-line* 0 0 10 10) nil t)
		((make-point 5 5) (make-line* 5 0 5 10) nil t)
		(+nowhere+ (make-line* 0 0 10 10) nil t)
		(+everywhere+ (make-line* 0 0 10 10) t nil)
		((make-line* 0 0 10 10) (make-line* 10 10 20 20) nil nil)
		((make-rectangle* 0 0 10 10) (make-point 0 0) t nil)
		((make-rectangle* 0 0 10 10) (make-point 1 1) t nil)
		((make-rectangle* 0 0 10 10) (make-line* 0 0 10 10) t nil)
		((make-rectangle* 0 0 10 10) (make-rectangle* 2 3 6 7) t nil)
		((make-rectangle* 0 0 10 10) (make-rectangle* 10 10 20 20) nil nil)))

(define-test (region-contains-region-tests graphics) (stream)
  "Exercise REGION-CONTAINS-REGION-P."
  (let ((regions nil))
    (dolist (test *test-regions-for-region-contains-region-p*)
      (pushnew (first test) regions :test #'equal)
      (pushnew (second test) regions :test #'equal))
    (setq regions (nreverse regions))
    (flet ((lookup-result (region1 region2)
	     (dolist (result *test-regions-for-region-contains-region-p*)
	       (when (and (equal region1 (first result))
			  (equal region2 (second result)))
		 (return-from lookup-result (third result)))
	       (when (and (equal region1 (second result))
			  (equal region2 (first result)))
		 (return-from lookup-result (fourth result))))
	     (or (equal region1 region2) :none)))
      (formatting-table (stream :x-spacing "  ")
	(abbreviated-regions-column-headings regions stream 'region-contains-region-p)
	(dolist (region1 regions)
	  (formatting-row (stream)
	    (formatting-cell (stream :align-x :right)
	      (with-text-face (stream :italic)
		(format stream "~A" region1)))
	    (dolist (region2 regions)
	      (with-output-as-presentation
		  (stream `(region-contains-region-p ,region1 ,region2) 'form
		   :single-box t)
		(formatting-cell (stream :align-x :center)
		  (handler-case			;guard against unimplemented cases
		      (let ((res (lookup-result region1 region2))
			    (result (region-contains-region-p
				      (eval region1) (eval region2))))
			(if (eq res :none)
			    (write-char #\space stream)	;the presentation demands some ink
			    (with-text-face (stream (if (eq res result) nil :bold))
			      (format stream "~A" result))))
		    (error ()
		      (with-text-face (stream :bold)
			(write-string "Error signalled" stream)))))))))))
    (region-test-comment stream)))

(defparameter *test-regions-for-region-intersects-region-p*
	      ;; (region1 region2 intersects)
	      ;; +nowhere+ and +everywhere+ are known about by test
	      ;; function.  They need not be included here.
	      '(((make-point 0 0) (make-point 1 1) nil)
		((make-point 0 0) (make-point 5 5) nil)
		((make-point 0 0) (make-line* 0 0 10 10) t)
		((make-point 5 5) (make-line* 0 0 10 10) t)
		((make-point 5 5) (make-line* 5 0 5 10) t)
		((make-line* 0 0 10 10) (make-line* 10 10 20 20) t)
		((make-rectangle* 0 0 10 10) (make-point 0 0) t)
		((make-rectangle* 0 0 10 10) (make-point 1 1) t)
		((make-rectangle* 0 0 10 10) (make-line* 0 0 10 10) t)
		((make-rectangle* 0 0 10 10) (make-rectangle* 2 3 6 7) t)
		((make-rectangle* 0 0 10 10) (make-rectangle* 10 10 20 20) t)))

(define-test (region-intersects-region-tests graphics) (stream)
  "Exercise REGION-INTERSECTS-REGION-P."
  (let ((regions (list '+everywhere+ '+nowhere+)))
    (dolist (test *test-regions-for-region-intersects-region-p*)
      (pushnew (first test) regions :test #'equal)
      (pushnew (second test) regions :test #'equal))
    (setq regions (nreverse regions))
    (flet ((lookup-result (region1 region2)
	     (cond ((or (eq region1 '+nowhere+)
			(eq region2 '+nowhere+))
		    nil)
		   ((or (eq region1 '+everywhere+)
			(eq region2 '+everywhere+))
		    t)
		   (t (dolist (result *test-regions-for-region-intersects-region-p*)
			(when (and (equal region1 (first result))
				   (equal region2 (second result)))
			  (return-from lookup-result (third result)))
			(when (and (equal region1 (second result))
				   (equal region2 (first result)))
			  (return-from lookup-result (third result))))
		      (or (equal region1 region2) :none)))))
      (formatting-table (stream :x-spacing "  ")
	(abbreviated-regions-column-headings regions stream 'region-intersects-region-p)
	(dolist (region1 regions)
	  (formatting-row (stream)
	    (formatting-cell (stream :align-x :right)
	      (with-text-face (stream :italic)
		(format stream "~A" region1)))
	    (dolist (region2 regions)
	      (with-output-as-presentation
		  (stream `(region-intersects-region-p ,region1 ,region2) 'form
		   :single-box t)
		(formatting-cell (stream :align-x :center)
		  (handler-case			;guard against unimplemented cases
		      (let ((res (lookup-result region1 region2))
			    (result (region-intersects-region-p
				      (eval region1) (eval region2))))
			(if (eq res :none)
			    (write-char #\space stream)	;the presentation demands some ink
			    (with-text-face (stream (if (eq res result) nil :bold))
			      (format stream "~A" result))))
		    (error ()
		      (with-text-face (stream :bold)
			(write-string "Error signalled" stream)))))))))))
    (region-test-comment stream)))


;;; Hairy graphics drawing tests

(defmacro format-graphics-cell (stream function arguments)
  `(formatting-cell (,stream :align-x :center :align-y :center)
     (handler-case
	 (progn
	   (with-output-to-pixmap (pixmap-stream ,stream)
	     (apply ,function pixmap-stream ,arguments))
	   (with-room-for-graphics (,stream)
	     (apply ,function ,stream ,arguments)))
       (error ()
	 (write-string "ERROR" ,stream)))))

(defmacro formatting-graphics-samples ((stream title &optional (columns 4)) &body body)
  `(labels ((format-graphics-sample (stream label sample &rest keywords)
	      (let ((arguments (append (cdr sample) keywords)))
		(formatting-cell (stream)
		  (formatting-item-list (stream :n-columns 1)
		    (format-graphics-cell stream (car sample) arguments)
		    (formatting-cell (stream :align-x :center)
		      (with-text-family (stream :sans-serif)
			(write-string label stream))))))))
     (with-text-style (,stream '(:sans-serif :italic :normal))
       (format ,stream "~&~%~A~%" ,title))
     (with-scaling (,stream 0.5 0.5)
       (formatting-item-list (,stream :n-columns ,columns :x-spacing 10)
	 ,@body))))

(defparameter *basic-shapes* `(
  ("Rectangle" draw-rectangle* 0 0 90 100)
  ("Triangle" draw-polygon* (0 0 45 100 100 0))
  ("Circle" draw-circle* 50 50 50)
  ("Polygon" draw-polygon* (0 0 10 100 50 50 90 100 100 10))))

(define-test (basic-graphics-shapes graphics) (stream)
  "Test basic graphics shapes"
  (formatting-graphics-samples (stream "Filled shapes")
    (dolist (shape *basic-shapes*)
      (let ((label (first shape))
	    (sample (rest shape)))
	(format-graphics-sample stream label sample))))
  (formatting-graphics-samples (stream "Unfilled shapes")
    (dolist (shape *basic-shapes*)
      (let ((label (first shape))
	    (sample (rest shape)))
	(format-graphics-sample stream label sample :filled nil))))
  (formatting-graphics-samples (stream "Unfilled shapes, thickness 2")
    (dolist (shape *basic-shapes*)
      (let ((label (first shape))
	    (sample (rest shape)))
	(format-graphics-sample stream label sample :filled nil :line-thickness 2)))))

(define-test (transformed-graphics-shapes graphics) (stream)
  "Test transformed graphics shapes"
  (formatting-graphics-samples (stream "Scaled shapes")
    (with-scaling (stream 1.5 1.5)
      (dolist (shape *basic-shapes*)
	(let ((label (first shape))
	      (sample (rest shape)))
	  (format-graphics-sample stream label sample)))))
  (formatting-graphics-samples (stream "Stretched shapes")
    (with-scaling (stream 1.5 1)
      (dolist (shape *basic-shapes*)
	(let ((label (first shape))
	      (sample (rest shape)))
	  (format-graphics-sample stream label sample)))))
  (formatting-graphics-samples (stream "Rotated shapes")
    (with-rotation (stream 0.3)
      (dolist (shape *basic-shapes*)
	(let ((label (first shape))
	      (sample (rest shape)))
	  (format-graphics-sample stream label sample)))))
  (formatting-graphics-samples (stream "Stretched and rotated shapes")
    (with-scaling (stream 1.5 1)
      (with-rotation (stream 0.3)
	(dolist (shape *basic-shapes*)
	  (let ((label (first shape))
		(sample (rest shape)))
	    (format-graphics-sample stream label sample)))))))

(define-test (basic-graphics-inks graphics) (stream)
  "Test basic graphics inks"
  (formatting-graphics-samples (stream "Colors" 6)
    (let ((sample '(draw-rectangle* 0 0 90 100)))
      (format-graphics-sample stream "Red" sample :ink +red+)
      (format-graphics-sample stream "Green" sample :ink +green+)
      (format-graphics-sample stream "Blue" sample :ink +blue+)
      (format-graphics-sample stream "Yellow" sample :ink +yellow+)
      (format-graphics-sample stream "Cyan" sample :ink +cyan+)
      (format-graphics-sample stream "Magenta" sample :ink +magenta+)))
  (formatting-graphics-samples (stream "Grays" 10)
    (let ((sample '(draw-rectangle* 0 0 50 100)))
      (format-graphics-sample stream "Black" sample :ink +black+)
      (dotimes (i 8)
	(let ((luminance (/ (1+ i) 9.0)))
	  (format-graphics-sample stream
				  (format nil "~D%" (round (* luminance 100)))
				  sample :ink (make-gray-color luminance))))
      (format-graphics-sample stream "White" sample :ink +white+)))
  (formatting-graphics-samples (stream "Contrasting inks" 6)
    (let ((sample '(draw-polygon* (0 0 45 100 100 0))))
      (dotimes (i 6)
	(format-graphics-sample stream (format nil "Ink ~D" i) sample
				:ink (make-contrasting-inks 6 i))))))

(define-test (points-and-lines graphics) (stream)
  "Test drawing points"
  (formatting-graphics-samples (stream "Single Points")
    (format-graphics-sample stream "One Pixel" '(draw-point* 0 5))
    (format-graphics-sample stream "Large Point" '(draw-point* 0 0 :line-thickness 5)))
  (formatting-graphics-samples (stream "Some Lines")
    (format-graphics-sample stream "Some Lines"
      '(draw-lines* (0 0 10 100 50 50 90 100 100 10 40 50))))
  (formatting-graphics-samples (stream "Some Rectangles")
    (format-graphics-sample stream "Some Rectangles"
      '(draw-rectangles* (0 0 10 100 50 50 90 100 100 10 40 50)))
    (format-graphics-sample stream "A rectangle" '(draw-rectangle* 0 0 10 100))
    (format-graphics-sample stream "A rectangles" '(draw-rectangle* 50 50 90 100 ))
    (format-graphics-sample stream "A rectangle" '(draw-rectangle*  100 10 40 50)))
  (with-rotation (stream .5)
    (formatting-graphics-samples (stream "Some rotated rectangles")
      (format-graphics-sample stream "Some Rectangles"
        '(draw-rectangles* (0 0 10 100 50 50 90 100 100 10 40 50)))
      (format-graphics-sample stream "A rectangle" '(draw-rectangle* 0 0 10 100))
      (format-graphics-sample stream "A rectangle" '(draw-rectangle* 50 50 90 100 ))
      (format-graphics-sample stream "A rectangle" '(draw-rectangle*  100 10 40 50))))
  (formatting-graphics-samples (stream "Many Points")
    (format-graphics-sample stream "Many Small Points"
      '(draw-points* (0 0 10 110 50 50 90 110 100 10 50 40)))
    (format-graphics-sample stream "Many Large Points"
      '(draw-points* (0 0 10 100 50 50 90 100 100 10 60 80) :line-thickness 5))))

(define-test (rotated-text graphics) (stream)
  "Test rotated text"
  ;;--- Need to do this otherwise we get problems with replay because
  ;;--- of overlapping output records.
  (with-new-output-record (stream 'standard-sequence-output-record)
    (with-output-as-presentation (stream 1 'form)
      (draw-text* stream "Some fox jumped over something" 200 200
		  :towards-x 400 :towards-y 200))
    (with-output-as-presentation (stream 2 'form)
      (draw-text* stream "Some fox jumped over something" 200 200
		  :towards-x 200 :towards-y 400
		  :text-style '(nil :italic :large)))
    (with-output-as-presentation (stream 3 'form)
      (draw-text* stream "Some fox jumped over something" 200 200
		  :towards-x 0 :towards-y 200
		  :text-style '(nil :bold :large)))
    (with-output-as-presentation (stream 4 'form)
      (draw-text* stream "Some fox jumped over something" 200 200
		  :towards-x 200 :towards-y 0
		  :text-style '(nil :bold :small)))
    (do* ((x 300 (+ x 20))
	  (all-packages (list-all-packages)))
	((> x 1000))
      (with-output-as-presentation (stream 4 'form)
	(draw-text* stream
		    (let ((r ""))
		      (dotimes (i 5 r)
			(setq r (concatenate 'string
				  r " " (package-name (nth (random
							  (length
							   all-packages))
							 all-packages))))))
		    x 200
		    :towards-x x :towards-y 0)))))

(define-test (negative-extent graphics) (stream)
  "Draw things at negative values.  You should be able to scroll and see all of it"
  (let ((x 400)
	(y 400))
    (draw-point* stream (- x) 0 :ink +red+ :line-thickness 3)
    (draw-point* stream x 0 :ink +red+ :line-thickness 3)
    (draw-point* stream 0  (- y) :ink +red+ :line-thickness 3)
    (draw-point* stream 0 y :ink +red+ :line-thickness 3)
    (draw-line* stream (- x) 0 x 0)
    (draw-line* stream 0 (- y) 0 y)))

(define-test (pixmap-test graphics) (stream)
  "Test pixmap code"
  (with-sheet-medium (medium stream)
    (let ((pixmap (with-output-to-pixmap (ps stream :width 100 :height 100)
		    (draw-rectangle* ps 0 0 100 100 :ink +red+)
		    (surrounding-output-with-border (ps)
		      (draw-text* ps "hello" 10 50)))))
      (dotimes (i 5)
	(sleep 0.25)
	(copy-from-pixmap pixmap 0 0 100 100 stream (* i 100) (* i 100)))
      (deallocate-pixmap pixmap))
    (dotimes (j 5)
      (let ((pixmap (copy-to-pixmap medium (* j 100) (* j 100) 100 100)))
	(dotimes (i 5)
	  (unless (= i j)
	    (sleep 0.25)
	    (copy-from-pixmap pixmap 0 0 100 100 medium (* j 100) (* i 100))))
	(deallocate-pixmap pixmap)))))


#+allegro
(define-test (read-image-test graphics) (stream)
  "Test image reading code"
  (formatting-table (stream)
    (dolist (name '("woman" "escherknot" "tie_fighter" "mensetmanus" "calculator"))
      (formatting-row (stream)
	(formatting-cell (stream)
	  (write-string name stream))
	(formatting-cell (stream)
	  (let ((filename (format nil "/usr/include/X11/bitmaps/~A" name)))
	    (if (probe-file filename)
		(let ((pattern (make-pattern-from-bitmap-file
				filename
				:designs
				(list +background-ink+ +foreground-ink+))))
		  (draw-rectangle* stream 0 0
				   (pattern-width pattern)
				   (pattern-height pattern)
				   :ink pattern))
	      (write-string "not found" stream))))))))

#+allegro
(define-test (clipped-image-test graphics) (stream)
  "Test clipping of images"
  (formatting-table (stream)
    (dolist (name '("woman" "escherknot" "tie_fighter" "mensetmanus"))
      (formatting-row (stream)
	(formatting-cell (stream)
	  (write-string name stream))
	(formatting-cell (stream)
	  (let ((filename (format nil "/usr/include/X11/bitmaps/~A" name)))
	    (if (probe-file filename)
		(let ((pattern (make-pattern-from-bitmap-file
				filename
				:designs
				(list +background-ink+ +foreground-ink+))))
		  (with-drawing-options (stream :clipping-region
						*chess-board-clip-region*)
		    (draw-rectangle* stream 0 0
				     (pattern-width pattern)
				     (pattern-height pattern)
				     :ink pattern)))
	      (write-string "not found" stream))))))))




(define-test (draw-some-bezier-curves graphics) (stream)
  "Draw bezier curve"
  (let ((points (list 0 0 100 300 300 300 400 0 200 200 150 30 0 0)))
    (formatting-item-list (stream)
      (dolist (filled '(nil t))
	(formatting-cell (stream)
	  (with-scaling (stream 0.5)
	    (draw-bezier-curve* stream points :filled filled)
	    (do ((points points (cddr points)))
		((null points))
	      (draw-point* stream (car points) (cadr points)
			   :line-thickness 2 :ink +red+))))))))

(defparameter *named-colors*
 '(+white+ +black+ +red+ +green+ +blue+ +yellow+ +cyan+ +magenta+
   "snow" "ghost-white" "white-smoke" "gainsboro" "floral-white" "old-lace"
   "linen" "antique-white" "papaya-whip" "blanched-almond" "bisque"
   "peach-puff" "navajo-white" "moccasin" "cornsilk" "ivory" "lemon-chiffon"
   "seashell" "honeydew" "mint-cream" "azure" "alice-blue" "lavender"
   "lavender-blush" "misty-rose" "dark-slate-gray" "dim-gray" "slate-gray"
   "light-slate-gray" "gray" "light-gray" "midnight-blue" "navy-blue"
   "cornflower-blue" "dark-slate-blue" "slate-blue" "medium-slate-blue"
   "light-slate-blue" "medium-blue" "royal-blue" "dodger-blue" "deep-sky-blue"
   "sky-blue" "light-sky-blue" "steel-blue" "light-steel-blue" "light-blue"
   "powder-blue" "pale-turquoise" "dark-turquoise" "medium-turquoise"
   "turquoise" "light-cyan" "cadet-blue" "medium-aquamarine" "aquamarine"
   "dark-green" "dark-olive-green" "dark-sea-green" "sea-green"
   "medium-sea-green" "light-sea-green" "pale-green" "spring-green"
   "lawn-green" "chartreuse" "medium-spring-green" "green-yellow" "lime-green"
   "yellow-green" "forest-green" "olive-drab" "dark-khaki" "khaki"
   "pale-goldenrod" "light-goldenrod-yellow" "light-yellow" "gold"
   "light-goldenrod" "goldenrod" "dark-goldenrod" "rosy-brown" "indian-red"
   "saddle-brown" "sienna" "peru" "burlywood" "beige" "wheat" "sandy-brown"
   "tan" "chocolate" "firebrick" "brown" "dark-salmon" "salmon" "light-salmon"
   "orange" "dark-orange" "coral" "light-coral" "tomato" "orange-red"
   "hot-pink" "deep-pink" "pink" "light-pink" "pale-violet-red" "maroon"
   "medium-violet-red" "violet-red" "violet" "plum" "orchid" "medium-orchid"
   "dark-orchid" "dark-violet" "blue-violet" "purple" "medium-purple" "thistle"))

(define-test (colored-inks graphics) (stream)
  "Test colors"
  (let ((palette (frame-palette *application-frame*)))
    (with-text-style (stream '(:sans-serif :italic :normal))
      (format stream "~&~%~A~%" "Named Colors")
      (formatting-table (stream)
	(dolist (name *named-colors*)
	  (let ((color (if (symbolp name)
			   (symbol-value name)
			   (find-named-color name palette :errorp nil))))
	    (when color
	      (formatting-row (stream)
		(formatting-cell (stream)
		  (write-string (string name) stream))
		(formatting-cell (stream)
		  (draw-rectangle* stream 0 0 200 10 :ink color))))))))))

(define-test (patterned-graphics-shapes graphics) (stream)
  "Test patterned graphics shapes"
  (labels ((generate-stipple (ink1 ink2)
	     (make-rectangular-tile (make-pattern #2A((0 1) (1 0)) (list ink1 ink2)) 2 2))
	   (generate-tile (offset)
	     (make-rectangular-tile (make-pattern #2A((0 0 0 1 1 0 0 0)
						      (0 0 1 1 1 1 0 0)
						      (0 1 1 1 1 1 1 0)
						      (1 1 1 1 1 1 1 1)
						      (1 1 1 1 1 1 1 1)
						      (0 1 1 1 1 1 1 0)
						      (0 0 1 1 1 1 0 0)
						      (0 0 0 1 1 0 0 0))
						  (list +background-ink+ +foreground-ink+))
				    offset offset))
	   (generate-image (a b c d)
	     (make-pattern #2A((0 0 0 1 1 0 0 0)
			       (0 0 1 1 1 1 0 0)
			       (0 1 3 3 3 3 1 0)
			       (1 1 3 2 2 3 1 1)
			       (1 1 3 2 2 3 1 1)
			       (0 1 3 3 3 3 1 0)
			       (0 0 1 1 1 1 0 0)
			       (0 0 0 1 1 0 0 0))
			   (list a b c d))))
    (formatting-graphics-samples (stream "Stippled shapes")
      (let ((ink (generate-stipple +white+ +black+)))
	(dolist (shape *basic-shapes*)
	  (let ((label (first shape))
		(sample (rest shape)))
	    (format-graphics-sample stream label sample :ink ink)))))
    (formatting-graphics-samples (stream "Tiled shapes")
      (let ((ink (generate-tile 8)))
	(dolist (shape *basic-shapes*)
	  (let ((label (first shape))
		(sample (rest shape)))
	    (format-graphics-sample stream label sample :ink ink)))))
    (formatting-graphics-samples (stream "Tiled shapes with offset")
      (let ((sample '(draw-polygon* (0 0 10 100 100 100 90 0))))
	(dolist (offset '(4 8 12 16))
	  (format-graphics-sample stream (format nil "Offset ~D" offset)
				  sample :ink (generate-tile offset)))))
    (let ((sample '(draw-polygon* (0 0 10 100 100 100 90 0))))
      (formatting-graphics-samples (stream "Colored stipples" 5)
	(format-graphics-sample stream "Fore/Background" sample
				:ink (generate-stipple +foreground-ink+ +background-ink+))
	(format-graphics-sample stream "White/Black" sample
				:ink (generate-stipple +white+ +black+))
	(format-graphics-sample stream "Blue/White" sample
				:ink (generate-stipple +blue+ +white+))
	(format-graphics-sample stream "Red/Yellow" sample
				:ink (generate-stipple +red+ +yellow+))
	(format-graphics-sample stream "Green/Transparent" sample
				:ink (generate-stipple +green+ (make-opacity 0.1)))))
    (let ((sample '(draw-polygon* (0 0 0 100 100 50))))
      (formatting-graphics-samples (stream "Colored patterns")
	(format-graphics-sample stream "Black/White" sample
				:ink (generate-image +white+ +black+ +white+ +black+))
	(format-graphics-sample stream "w/Blue" sample
				:ink (generate-image +white+ +black+ +blue+ +black+))
	(format-graphics-sample stream "w/Red" sample
				:ink (generate-image +white+ +black+ +blue+ +red+))
	(format-graphics-sample stream "w/Yellow" sample
				:ink (generate-image +yellow+ +black+ +blue+ +red+))))))

(define-test (basic-line-styles graphics) (stream)
  "Test basic line style options"
  (formatting-graphics-samples (stream "Thick lines")
    (dolist (shape *basic-shapes*)
      (let ((label (first shape))
	    (sample (rest shape)))
	(format-graphics-sample stream label sample :filled nil :line-thickness 5))))
  (formatting-graphics-samples (stream "Dashed lines")
    (dolist (shape *basic-shapes*)
      (let ((label (first shape))
	    (sample (rest shape)))
	(format-graphics-sample stream label sample :filled nil :line-dashes t))))
  (formatting-graphics-samples (stream "Thick dashed lines")
    (dolist (shape *basic-shapes*)
      (let ((label (first shape))
	    (sample (rest shape)))
	(format-graphics-sample stream label sample :filled nil :line-thickness 3
				:line-dashes t))))
  (let ((polygon '(draw-polygon* (0 0 10 100 50 50 90 100 100 10) :filled nil)))
    (formatting-graphics-samples (stream "Thickness spectrum")
      (dolist (thickness '(0 1 2 5))
	(format-graphics-sample stream (format nil "Thickness ~D" thickness) polygon
				:line-thickness thickness)))
    (formatting-graphics-samples (stream "Dash spectrum")
      (dolist (dashes '(#(5 2) #(1 1) #(2 2) #(5 5)))
	(format-graphics-sample stream
				(format nil "Dashes ~Dx~D" (aref dashes 0) (aref dashes 1))
				polygon
				:line-dashes dashes)))
    (formatting-graphics-samples (stream "Line joints")
      (dolist (joint '(:miter :bevel :round :none))
	(format-graphics-sample stream
				(format nil "~A" joint)
				polygon
				:line-thickness 5
				:line-joint-shape joint))))
  (formatting-graphics-samples (stream "Line caps")
    (dolist (cap '(:butt :square :round :no-end-point))
      (format-graphics-sample stream
			      (format nil "~A" cap)
			      '(draw-line* 0 0 100 90)
			      :line-thickness 5
			      :line-cap-shape cap))))

(defparameter *basic-regions*
  `(("Rectangle" ,(make-rectangle* 0 0 90 100))
    ("Triangle" ,(make-polygon* '(0 0 45 100 100 0)))
    ("Circle" ,(make-ellipse* 50 50 50 0 0 50))
    ("Polygon" ,(make-polygon* '(0 0 10 100 50 50 90 100 100 10)))))


(defparameter *designs-to-compose-with*
  `(("Foreground" ,+foreground-ink+)
    ("Red" ,+red+)
    ("Nowhere" ,+nowhere+)
    ("50% Opacity" ,(make-opacity .5))
    ("Circle" ,(make-ellipse* 50 50 50 0 0 50))
    ("Stencil" ,(make-stencil #2A((0 0 0 1 1 0 0 0)
				  (0 0 1 1 1 1 0 0)
				  (0 1 1 1 1 1 1 0)
				  (1 1 1 1 1 1 1 1)
				  (1 1 1 1 1 1 1 1)
				  (0 1 1 1 1 1 1 0)
				  (0 0 1 1 1 1 0 0)
				  (0 0 0 1 1 0 0 0))))))

(defun format-compose-table (stream table-name composition)
  (with-text-style (stream '(:sans-serif :italic :normal))
    (format stream "~&~%~A~%" table-name))
  (formatting-table (stream)

    (formatting-row (stream)
      (formatting-cell (stream)
	(declare (ignore stream)))
      (dolist (design *designs-to-compose-with*)
	(format-graphics-cell stream
		       #'draw-rectangle* `(0 0 50 50 :ink ,(cadr design)))))

    (formatting-row (stream)
      (formatting-cell (stream)
	(declare (ignore stream)))
      (dolist (design *designs-to-compose-with*)
	(formatting-cell (stream :align-x :center)
	  (with-text-family (stream :sans-serif)
	    (write-string (car design) stream)))))

    (dolist (design1 *designs-to-compose-with*)
      (formatting-row (stream)
	(formatting-cell (stream :align-x :right :align-y :center)
	  (with-text-family (stream :sans-serif)
	    (write-string (car design1) stream)))
	(dolist (design2 *designs-to-compose-with*)
	  (let ((ink (funcall composition (cadr design1) (cadr design2))))
	    (format-graphics-cell stream
			   #'draw-rectangle* `(0 0 50 50 :ink ,ink))))))))

(define-test (complex-designs graphics) (stream)
  "Test some of the complex designs"

  (formatting-graphics-samples (stream "Opacities" 11)
    (let ((sample '(draw-rectangle* 0 0 50 100)))
      (dotimes (i 11)
	(let ((opacity (/ i 10.0)))
	  (format-graphics-sample stream
				  (format nil "~D%" (round (* opacity 100)))
				  sample :ink (make-opacity
					       opacity))))))

  (formatting-graphics-samples (stream "Regions as Designs")
    (let ((sample '(draw-rectangle* 0 0 100 100)))
      (dolist (region *basic-regions*)
	(let ((label (first region))
	      (design (second region)))
	  (format-graphics-sample stream label sample :ink design)))))

  (formatting-graphics-samples (stream "Stencils")
    (let ((sample '(draw-rectangle* 0 0 100 100))
	  (opaque (make-stencil #2A((0 0 0 1 1 0 0 0)
				    (0 0 1 1 1 1 0 0)
				    (0 1 1 1 1 1 1 0)
				    (1 1 1 1 1 1 1 1)
				    (1 1 1 1 1 1 1 1)
				    (0 1 1 1 1 1 1 0)
				    (0 0 1 1 1 1 0 0)
				    (0 0 0 1 1 0 0 0))))
	  (translucent (make-stencil #2A((0 0 0 .5 .5 0 0 0)
					 (0 0 .5 .5 .5 .5 0 0)
					 (0 .5 .5 .5 .5 .5 .5 0)
					 (.5 .5 .5 .5 .5 .5 .5 .5)
					 (.5 .5 .5 .5 .5 .5 .5 .5)
					 (0 .5 .5 .5 .5 .5 .5 0)
					 (0 0 .5 .5 .5 .5 0 0)
					 (0 0 0 .5 .5 0 0 0)))))
      (format-graphics-sample stream "opaque" sample :ink opaque)
      (format-graphics-sample stream "opaque-tile" sample
			      :ink (make-rectangular-tile opaque 8 8))
      (format-graphics-sample stream "translucent" sample :ink translucent)
      (format-graphics-sample stream "translucent-tile" sample
			      :ink (make-rectangular-tile translucent 8 8))))


  (format-compose-table stream "Compose Over" #'compose-over)
  (format-compose-table stream "Compose In" #'compose-in)
  (format-compose-table stream "Compose Out" #'compose-out)
  )

(define-test (draw-some-pixmap graphics) (stream)
  "Test the draw-pixmap function"
  (let ((pixmap (with-output-to-pixmap (p stream :width 50 :height 50)
		  (draw-rectangle* p 0 0 50 50 :ink +background-ink+)
		  (draw-circle* p 25 25 25 :ink +red+))))
    (formatting-item-list (stream)
      (dotimes (i 15)
	(formatting-cell (stream)
	  (draw-pixmap* stream pixmap 0 0 :function i))))))





;;--- ellipses


;;; Output recording

(define-command-table output-recording)



(define-test (draw-bullseye output-recording) (stream)
  "Draw a bullseye, wait, then refresh the window.  The display should look the same after the refresh."
  (draw-circle* stream 100 100 75 :ink (make-contrasting-inks 2 0))
  (draw-circle* stream 100 100 35 :ink (make-contrasting-inks 2 1))
  (sleep 2)
  (window-refresh stream))

(define-test (ordering-test-1 output-recording) (stream)
  "The three overlapping circles should have the correct sensitivity."
  (flet ((circle (x y r n)
	   (with-output-as-presentation (stream  n 'integer)
	     (draw-circle* stream x y r :ink (make-contrasting-inks 4 n)))))
    (circle 100  50 40 1)
    (circle  75 100 40 2)
    (circle 125 100 40 3))
  (stream-set-cursor-position stream 0 150)
  (format stream "~&~S" (accept 'integer :stream stream)))

(define-test (ordering-test-2a output-recording) (stream)
  "The three overlapping rectangles should have the correct sensitivity."
  (flet ((rect (ll tt rr bb n)
	   (with-output-as-presentation (stream  n 'integer)
	     (draw-rectangle* stream ll tt rr bb :ink (make-contrasting-inks 4 n)))))
    (rect  50  50 150 150 1)
    (rect   0   0 100 100 2)
    (rect  25  25 125 125 3))
  (stream-set-cursor-position stream 0 160)
  (format stream "~&~S" (accept 'integer :stream stream)))

(define-test (ordering-test-2b output-recording) (stream)
  "The three overlapping rectangles should have the correct sensitivity."
  (flet ((rect (ll tt rr bb n)
	   (with-output-as-presentation (stream  n 'integer)
	     (draw-rectangle* stream ll tt rr bb :ink (make-contrasting-inks 4 n)))))
    (rect 125 125 225 225 1)
    (rect   0   0 100 100 2)
    (rect  50  50 150 150 3))
  (stream-set-cursor-position stream 0 240)
  (format stream "~&~S" (accept 'integer :stream stream)))

(define-test (cursorpos-table output-recording) (stream)
  "Write some strings separated by horizontal cursor motion.  After refreshing, the output should look the same."
  (terpri stream)
  (let* ((width (bounding-rectangle-width stream))
	 (rect-width (- (floor width 11) 10)))
    (multiple-value-bind (x y)
	(stream-cursor-position stream)
      (with-end-of-line-action (stream :allow)
	(dotimes (i 11)
	  (stream-set-cursor-position stream x y)
	  (write-string (format nil "~1$" (float (/ i 10))) stream)
	  (incf x rect-width))))
    (terpri stream)
    (stream-force-output stream)
    (sleep 2)
    (window-refresh stream)))

(define-test (multiple-erases output-recording) (stream)
  "The four overlapping circles and a line and erase three of them"
  (flet ((circle (x y r n)
	   (with-output-as-presentation (stream  n 'integer)
	     (draw-circle* stream x y r :ink (make-contrasting-inks 4 n)))))
    (circle 100  50 40 1)
    (let ((records (list
		    (circle  75 100 40 2)
		    (circle 125 100 40 3)
		    (circle 200 200 40 3))))
      (draw-line* stream 100 50  200 200 :ink +white+)
      (sleep 2)
      (erase-output-record records stream))))


;;; Formatted output

(define-command-table formatted-output)

(define-test (row-table formatted-output) (stream)
  "A simple row-wise table."
  (formatting-table (stream :x-spacing '(2 :character))
    (dotimes (i 11)
      (formatting-row (stream)
	(dotimes (j 11)
	  (if (zerop i)				;first row
	      (if (zerop j)			;first cell
		  (formatting-cell (stream :align-x :right)
		    (with-text-face (stream :bold)
		      (write-string "*" stream)))
		  (formatting-cell (stream :align-x :right)
		    (with-text-face (stream :bold)
		      (format stream "~D" (1- j)))))
	    (if (zerop j)			;first column
		(formatting-cell (stream :align-x :right)
		  (with-text-face (stream :bold)
		    (format stream "~D" (1- i))))
	        (formatting-cell (stream :align-x :right)
		  (format stream "~D" (* (1- i) (1- j)))))))))))

(define-test (column-table formatted-output) (stream)
  "A simple column-wise table."
  (formatting-table (stream :x-spacing '(2 :character))
    (dotimes (j 11)
      (formatting-column (stream)
	(dotimes (i 11)
	  (if (zerop i)				;first row
	      (if (zerop j)			;first cell
		  (formatting-cell (stream :align-x :right)
		    (with-text-face (stream :bold)
		      (write-string "*" stream)))
		  (formatting-cell (stream :align-x :right)
		    (with-text-face (stream :bold)
		      (format stream "~D" (1- j)))))
	    (if (zerop j)			;first column
		(formatting-cell (stream :align-x :right)
		  (with-text-face (stream :bold)
		    (format stream "~D" (1- i))))
	        (formatting-cell (stream :align-x :right)
		  (format stream "~D" (* (1- i) (1- j)))))))))))

(define-test (equal-width-table formatted-output) (stream)
  "A table whose columns have equalized width."
  (formatting-table (stream :x-spacing '(2 :character) :equalize-column-widths t)
    (dotimes (i 10)
      (formatting-row (stream)
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" i))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i)))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i i)))))))

(define-test (multiple-columns-table formatted-output) (stream)
  "A table that has multiple columns."
  (formatting-table (stream :multiple-columns 2
			    :multiple-columns-x-spacing 25)
    (dotimes (i 10)
      (formatting-row (stream)
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" i))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i)))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i i)))))))

(define-test (equalized-multiple-columns-table formatted-output) (stream)
  "A table that has multiple equal-width columns."
  (formatting-table (stream :multiple-columns 2
			    :multiple-columns-x-spacing 25
			    :equalize-column-widths t)
    (dotimes (i 10)
      (formatting-row (stream)
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" i))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i)))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i i)))))))

(define-test (nested-table formatted-output) (stream)
  "Several nested tables, each surrounded by a border."
  (flet ((table (stream start)
	   (surrounding-output-with-border (stream)
	     (formatting-table (stream)
	       (repeat 3
		 (formatting-row (stream)
		   (formatting-cell (stream :align-x :right)
		     (format stream "~D" start))
		   (formatting-cell (stream :align-x :right)
		     (format stream "~D" (* start start))))
		 (incf start))))))
    (formatting-table (stream :y-spacing 10
			      :x-spacing 10)
      (dotimes (start 3)
	(formatting-row (stream)
	  (formatting-cell (stream :align-x :center)
	    (table stream start))
	  (formatting-cell (stream :align-x :center)
	    (table stream (* start 10))))))))

(define-test (cell-coordinates formatted-output) (stream)
  "This should produce a 3x3 table consisting of nine identical squares."
  (formatting-table (stream :y-spacing 10
			    :x-spacing 10)
    (repeat 3
      (formatting-row (stream)
	(repeat 3
	  (formatting-cell (stream)
	    (draw-rectangle* stream -20 -20 20 20)))))))

(define-test (checkerboard formatted-output) (stream)
  "This should produce a checkerboard pattern."
  (stream-set-cursor-position stream 10 10)
  (surrounding-output-with-border (stream)
    (formatting-table (stream :y-spacing 0 :x-spacing 0)
      (dotimes (i 8)
	(formatting-row (stream)
	  (dotimes (j 8)
	    (formatting-cell (stream)
	      (when (oddp (+ i j))
		(draw-rectangle* stream -20 -20 20 20)))))))))

(define-test (mixed-table formatted-output) (stream)
  "A table consisting of mixed text and graphics."
  (write-string "Prefix string " stream)
  (formatting-table (stream)
    (repeat 3
      (formatting-row (stream)
	(formatting-cell (stream)
	  (write-string "foo" stream))
	(repeat 3
	  (formatting-cell (stream)
	    (draw-rectangle* stream -20 -20 20 20)))))))

(define-test (simple-borders formatted-output) (stream)
  "Show several types of canned borders."
  (stream-set-cursor-position stream 10 10)
  (surrounding-output-with-border (stream :shape :rectangle)
    (write-string "a rectangle" stream))
  (stream-set-cursor-position stream 10 30)
  (surrounding-output-with-border (stream :shape :rectangle :ink +yellow+ :filled t)
    (write-string "a filled rectangle" stream))
  (stream-set-cursor-position stream 10 50)
  (surrounding-output-with-border (stream :shape :drop-shadow)
    (write-string "a dropshadow" stream))
  (stream-set-cursor-position stream 10 70)
  (surrounding-output-with-border (stream :shape :underline)
    (write-string "an underline" stream)))

(defparameter *green-line-map* '("Lechmere"
				 "Science Park"
				 "North Station"
				 "Haymarket"
				 "Government Center"
				 "Park Street"
				 "Boylston"
				 "Arlington"
				 "Copley"
				 (("Auditorium" "Kenmore"
				   (("Boston University" "Boston College")
				    ("Cleveland Circle"))
				   ("Longwood Avenue" "Reservoir" "Riverside"))
				  ("Prudential" "Symphony" "Northeastern" "Museum"
				   "Brigham Circle" "Heath" "Arborway"))))

(define-test (simple-graph formatted-output) (stream)
  "Draw a graph showing the Green Line."
  (labels ((next-stops (stop)
	     #+genera (declare (sys:downward-function))
	     (let ((next (cadr (find-green-line-stop stop *green-line-map*))))
	       (if (listp next)
		   (mapcar #'car next)
		 (list next))))
	   (find-green-line-stop (stop-name search-from)
	     #+genera (declare (sys:downward-function))
	     (do ((stops search-from (cdr stops)))
		 ((null stops))
	       (if (listp (car stops))
		   (let ((s (find-green-line-stop stop-name (car stops))))
		     (when s (return s)))
		   (when (string-equal stop-name (car stops))
		     (return stops)))))
	   (draw-node (node stream)
	     #+genera (declare (sys:downward-function))
	     (format stream "~A" node)))
    #+ignore (declare (dynamic-extent #'next-stops #'find-green-line-stop #'draw-node))
    (format-graph-from-root "Lechmere" #'draw-node #'next-stops
			    :orientation :vertical
			    :stream stream)))

(define-test (offset-table formatted-output) (stream)
  "Draw a table offset in the window.  After refreshing, it should look the same."
  (stream-set-cursor-position stream 20 20)
  (formatting-table (stream)
    (dotimes (i 10)
      (formatting-row (stream)
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" i))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i)))
	(formatting-cell (stream :align-x :right)
	  (format stream "~D" (* i i i))))))
  (sleep 2)
  (window-refresh stream))

#+allegro
(define-test (clos-metaobjects-graph formatted-output) (stream)
  "Draw a graph showing part of the CLOS class hierarchy"
  (let ((color (make-gray-color 0.7)))
    (format-graph-from-roots
     ;; dependee-mixin no longer exported -- smh 18may93
     (mapcar #'find-class '(clos:metaobject clos::dependee-mixin))
     #'(lambda (o s)
	 (let ((text (format nil "~A" (clos::class-name o))))
	   (multiple-value-bind (width height) (text-size s text)
	     (with-new-output-record (s)
	       (draw-rectangle* s 0 0 width height :filled t :ink color)
	       (draw-text* s text 0 0 :align-x :left :align-y :top)))))
     #'clos::class-direct-subclasses
     :stream stream
     :merge-duplicates t)))

(define-test (simple-node-centered-graphs formatted-output) (stream)
  "Two graphs, one with nodes centered"
  (let ((color (make-gray-color 0.7)))
    (with-text-size (stream :tiny)
      (formatting-item-list (stream)
	(dolist (center-nodes '(nil t))
	  (formatting-cell (stream)
	    (format-graph-from-roots
	     ;; dependee-mixin no longer exported -- smh 18may93
	     (mapcar #'find-class '(number))
	     #'(lambda (o s)
		 (let ((text (format nil "~A" (clos::class-name o))))
		   (multiple-value-bind (width height) (text-size s text)
		     (with-new-output-record (s)
		       (draw-rectangle* s 0 0 width height :filled t :ink color)
		       (draw-text* s text 0 0 :align-x :left :align-y :top)))))
	     #'clos::class-direct-subclasses
	     :stream stream
	     :center-nodes center-nodes
	     :merge-duplicates nil)))))))



(define-test (offset-graph formatted-output) (stream)
  "Draw a graph offset in the window.  After refreshing, it should look the same."
  (let ((orientation :horizontal))
    (stream-set-cursor-position stream 20 20)
    (macrolet ((make-node (&key name children)
		 `(list* ,name ,children)))
      (flet ((node-name (node)
	       (car node))
	     (node-children (node)
	       (cdr node)))
	(let* ((2a (make-node :name "2A"))
	       (2b (make-node :name "2B"))
	       (2c (make-node :name "2C"))
	       (1a (make-node :name "1A" :children (list 2a 2b)))
	       (1b (make-node :name "1B" :children (list 2b 2c)))
	       (root (make-node :name "0" :children (list 1a 1b))))
	  (format-graph-from-root root
				  #'(lambda (node s)
				      (write-string (node-name node) s))
				  #'node-children
				  :orientation orientation
				  :stream stream))))
    (sleep 2)
    (window-refresh stream)))

#+allegro
(define-test (hairy-graph-formatting formatted-output) (stream)
  "Some hairy re-entrant and circular graphs"
  (formatting-item-list (stream)
    (dolist (test '((nil (a (bbbb (ccc (dd eee) (fff ggg (hh (ii jj) kkk)))
				  (lll (mmm nnn) (ooo ppp qqq))) r (s t)))
		    (t (a #1=(b (c #2=(d e #3=(f g)) (hhhhhhhhhh (i j k) l (m n o)))))
		       (p (q (r (s t #3# u))))
		       (w (x #2# #1#)))
		    (t (a #11=(bbb (ccc (dd eee) (ffff g hhhhhhhh)))
			  (mm (nnn ooo #11#) (p #12=(q r (s t))))
			  (u (v (w (x #12#))))))
		    (t (a #21=(b (c (d #21# e)) (f #22=(g (h i) j) k) #22#) (m n)))
		    (t #34=(a #31=(b (c (d #31# e)) (f #32=(g (h i #31#) j) k) #32#)
			      (m #34# n)))
		    (t #51=(a #51#))
		    (t #41=(aaa (bbb (ccc #41#))))))
      (formatting-cell (stream)
	(destructuring-bind (merge . data) test
	  (flet ((printer-function (node stream)
		   (format stream "~A" (if (consp node) (car node) node)))
		 (child-function (node)
		   (and (consp node) (cdr node))))
	    (format-graph-from-roots
	      data #'printer-function #'child-function
	      :stream stream
	      :cutoff-depth 8
	      :merge-duplicates merge)))))))

(define-test (filled-output formatted-output) (stream)
  "Some simple tests of filled output.  The thing labelled <Click Here> should be sensitive."
  (filling-output (stream :fill-width '(30 :character))
    (format stream "jskd hjskad mxzz x mzxmuo sdhask iuysoda asdsa ")
    (present "<Click Here>" 'string :stream stream)
    (format stream " 8ewr7 erw hsjak sjdkha "))
  (terpri stream)
  (terpri stream)
  (filling-output (stream :fill-width '(30 :character))
    (format stream "jskd sdhask iuysoda asdsa ")
    (present "<Click Here>" 'string :stream stream)
    (format stream " 8ewr7 erw hsjak sjdkha "))
  (terpri stream)
  (terpri stream)
  (filling-output (stream :fill-width '(30 :character))
    (format stream "jskd sdha asdsa ")
    (present "<Click Here>" 'string :stream stream)
    (format stream " 8ewr7 erw hsjak sjdkha "))
  (terpri stream)
  (terpri stream)
  (accept 'string :stream stream))

(define-test (text-formatting formatted-output) (stream)
  "Another test of filled output, this time using a prefix string."
  (let ((minimum-width 12)
	(sample-text "Luke Luck likes lakes.
Luke's duck likes lakes.
Luke Luck licks lakes.
Luke's duck licks lakes.

Luke's duck licks the lakes Luke Luck likes.
Luke Luck licks the lakes Luke's duck likes."))
    (labels ((do-it (width stream)
	       (when (<= width minimum-width)
		 (return-from do-it))
	       (fresh-line stream)
	       (filling-output (stream :fill-width `(,width :character)
				       :after-line-break "$")
		 (write-string sample-text stream))
	       (terpri stream)))
      (do-it 20 stream))))


;;; Comprehensive table tests

(defvar *table-graphics-tests* nil)

(defmacro define-table-cell-test (name description &body body)
  (let ((function-name (make-symbol (format nil "~S-TABLE-TEST" name))))
    `(progn
       (defun ,function-name (stream) ,@body)
       (let ((old (assoc ',name *table-graphics-tests*)))
	 (if old
	     (setf (third old) ',description)
	     (setq *table-graphics-tests*
		   (nconc *table-graphics-tests*
			  (list '(,name ,function-name ,description)))))))))

(defun table-graphics-test (caption case continuation)
  (with-display-pane (stream)
    (let ((documentation (get-frame-pane *application-frame* 'caption-pane)))
      (let* ((entry (nth case *table-graphics-tests*))
	     (name (first entry))
	     (doc-string (third entry)))
	(window-clear documentation)
	(with-text-face (documentation :bold)
	  (write-string "Table tests" documentation))
	(fresh-line documentation)
	(format documentation "Test ~D (~A):  " case name)
	(write-string doc-string documentation)
	(write-string caption documentation)
	(force-output documentation))
      (stream-set-cursor-position stream 30 30)
      (funcall continuation stream)
      (draw-line* stream 30 15 30 45 :ink +green+)
      (draw-line* stream 15 30 45 30 :ink +green+)
      (fresh-line stream)
      (terpri stream)
      (write-string "Type any gesture to replay window" stream)
      (read-gesture :stream stream)
      (window-refresh stream))))

(defun do-table-cell-output (stream case-number)
  (funcall (second (nth case-number *table-graphics-tests*)) stream))

(defun table-graphics-test-row (&optional (case 0))
  (table-graphics-test " [row]" case
    #'(lambda (stream)
	(formatting-table (stream)
	  (formatting-row (stream)
	    (formatting-cell (stream)
	      (write-string "cell one" stream))
	    (formatting-cell (stream)
	      (write-string "cell two" stream))
	    (formatting-cell (stream)
	      (write-string "cell three" stream)))
	  (formatting-row (stream)
	    (formatting-cell (stream :align-y :bottom)
	      (write-string "graphics -->" stream))
	    (formatting-cell (stream)
	      (do-table-cell-output stream case))
	    (formatting-cell (stream :align-y :top)
	      (write-string "<-- graphics" stream)))
	  (formatting-row (stream)
	    (formatting-cell (stream)
	      (write-string "cell one" stream))
	    (formatting-cell (stream)
	      (write-string "cell two" stream))
	    (formatting-cell (stream)
	      (write-string "cell three" stream)))))))

(defun table-graphics-test-column (&optional (case 0))
  (table-graphics-test " [column]" case
    #'(lambda (stream)
	(formatting-table (stream)
	  (formatting-column (stream)
	    (formatting-cell (stream)
	      (write-string "cell one" stream))
	    (formatting-cell (stream :align-y :bottom)
	      (write-string "graphics -->" stream))
	    (formatting-cell (stream)
	      (write-string "cell one" stream)))
	  (formatting-column (stream)
	    (formatting-cell (stream)
	      (write-string "cell two" stream))
	    (formatting-cell (stream)
	      (do-table-cell-output stream case))
	    (formatting-cell (stream)
	      (write-string "cell two" stream)))
	  (formatting-column (stream)
	    (formatting-cell (stream)
	      (write-string "cell three" stream))
	    (formatting-cell (stream :align-y :top)
	      (write-string "<-- graphics" stream))
	    (formatting-cell (stream)
	      (write-string "cell three" stream)))))))

(define-table-cell-test rectangle "Normal, filled rectangle (0 0 20 20)"
  (draw-rectangle* stream 0 0 20 20))

(define-table-cell-test rect-and-circle "Rectangle (0 0 20 20), Red circle (20 20 10)"
  (draw-rectangle* stream 0 0 20 20)
  (draw-circle* stream 20 20 10 :ink +red+))

(define-table-cell-test minus-rectangle "Blue rectangle (-10 -10 10 10)"
  (draw-rectangle* stream -10 -10 10 10 :ink +blue+))

(define-table-cell-test text-rectangle "\"foo\" and rectangle (20 20 30 30)"
  (write-string "foo" stream)
  (draw-rectangle* stream 20 20 30 30))

(define-table-cell-test surrounding-output "surrounding-output-with-border"
  (surrounding-output-with-border (stream)
    (write-string "Foobar" stream)))

(define-table-cell-test stream-cursor-position "multiple-value-bind, draw-rectangle"
  (multiple-value-bind (x y)
      (stream-cursor-position stream)
    (draw-rectangle* stream x y (+ x 10) (+ y 10) :ink +yellow+)))

(define-table-cell-test simple-surrounding "create record, query size, surround with rect"
  (let ((record (with-new-output-record (stream)
		  (write-string "Foobar" stream))))
    (with-bounding-rectangle* (left top right bottom) record
      (multiple-value-bind (xoff yoff)
	  ;; damn, we have to convert because we want to draw in table-cell relative
	  ;; coordinates.
	  (convert-from-absolute-to-relative-coordinates
	    stream (output-record-parent record))
	(decf left xoff)
	(decf right xoff)
	(decf top yoff)
	(decf bottom yoff))
      (draw-rectangle* stream left top right bottom :filled nil :ink +cyan+))))

(define-table-cell-test simple-graphics-surrounding
			"create graphics-record, query size, surround with rect"
  (let ((record (with-new-output-record (stream)
		  (draw-circle* stream 10 10 10 :ink +blue+))))
    (with-bounding-rectangle* (left top right bottom) record
      (multiple-value-bind (xoff yoff)
	  ;; damn, we have to convert because we want to draw in table-cell relative
	  ;; coordinates.
	  (convert-from-absolute-to-relative-coordinates
	    stream (output-record-parent record))
	(decf left xoff)
	(decf right xoff)
	(decf top yoff)
	(decf bottom yoff))
      (draw-rectangle* stream left top right bottom :filled nil :ink +cyan+))))

(define-table-cell-test set-cursor-position "move the cursor, output some text"
  (stream-set-cursor-position stream 50 50)
  (write-string ">Wally<" stream))

(define-test (comprehensive-table-tests formatted-output) (stream)
  "Comprehensive tests of table formatting."
  (let ((initially t))
    (dotimes (i (length *table-graphics-tests*))
      (unless initially
	(fresh-line stream)
	(terpri stream)
	(write-string "Type any gesture for the next test" stream)
	(read-gesture :stream stream :timeout 2))
      (setq initially nil)
      (table-graphics-test-row i)
      (fresh-line stream)
      (terpri stream)
      (write-string "Any gesture to run the column test" stream)
      (read-gesture :stream stream)
      (table-graphics-test-column i))))


;;; Redisplay tests

(define-command-table redisplay)

(define-test (simple-redisplay redisplay) (stream)
  "A simple test of redisplay.  Click on pieces of the output to cause them to be redisplayed."
  (let* ((array (make-array 4 :initial-element 0))
	 (record (updating-output (stream)
		   (formatting-table (stream)
		     (formatting-row (stream)
		       (dotimes (i 4)
			 (formatting-cell (stream)
			   (updating-output (stream :unique-id (+ 10 i)
						    :cache-value (aref array i))
			     (with-output-as-presentation ( stream
							    i
							    'integer)
			       (format stream "[~D] = ~D" i (aref array i)))))))))))
    (loop
      (fresh-line stream)
      (redisplay record stream)
      (incf (aref array (accept '(integer 0 3) :stream stream))))))

(define-test (graphics-redisplay-1 redisplay) (stream)
  "Redisplay of non-overlapping graphics."
  (with-local-coordinates (stream)
    (let* ((list (list t nil t))
	   (top-record
	     (updating-output (stream)
	       (let ((x 50))
		 (dolist (elt list)
		   (updating-output (stream :unique-id x :cache-value elt)
		     (if elt
			 (draw-circle* stream x 50 45
				       :ink (make-contrasting-inks 2 0))
		         (draw-rectangle* stream (- x 40) 10 (+ x 40) 90
					  :ink (make-contrasting-inks 2 1))))
		   (incf x 100))))))
      (repeat 10
	(sleep 1)
	(let ((p (random (length list))))
	  (setf (nth p list) (not (nth p list))))
	(redisplay top-record stream)))))

(define-test (graphics-redisplay-2 redisplay) (stream)
  "Redisplay of overlapping graphics."
  (with-local-coordinates (stream)
    (let* ((list (list t nil t))
	   (top-record
	     (updating-output (stream)
	       (let ((x 50))
		 (dolist (elt list)
		   (updating-output (stream :unique-id x :cache-value elt)
		     (if elt
			 (draw-circle* stream x 50 45 :filled nil)
		         (draw-rectangle* stream (- x 40) 10 (+ x 40) 90 :filled nil)))
		   (incf x 60))))))
      (repeat 10
	(sleep 1)
	(let ((p (random (length list))))
	  (setf (nth p list) (not (nth p list))))
	(redisplay top-record stream)))))

(define-test (redisplay-overlapping redisplay) (stream)
  "Redisplay of nested overlapping graphics.  The inner rectangle should stay the same and visible the whole time."
  (flet ((draw (stream coords thickness)
	   (draw-rectangle* stream
			    (pop coords) (pop coords) (pop coords) (pop coords)
			    :filled nil :line-thickness thickness)))
    (let* ((tick 0)
	   (outer-coords '(100 100 200 200))
	   (outer-thickness 2)
	   (inner-coords '(125 125 175 175))
	   (inner-thickness 1)
	   (piece
	     (updating-output (stream)
	       (updating-output (stream :unique-id 'outer :cache-value tick)
		 (draw stream outer-coords outer-thickness))
	       (updating-output (stream :unique-id 'inner :cache-value 'constant)
		 (draw stream inner-coords inner-thickness)))))
      (sleep 2)
      (setq outer-thickness 1)
      (incf tick)
      (redisplay piece stream)
      (sleep 2)
      (setq outer-coords '(75 77 225 225))
      (incf tick)
      (redisplay piece stream))))

(define-test (redisplay-border redisplay) (stream)
  "Redisplay of bordered output.  The border should grow and shrink appropriately."
  (let* ((tick 0)
	 (value 1)
	 (record
	   (updating-output (stream)
	     (updating-output (stream :unique-id 'test
				      :cache-value tick)
	       (stream-set-cursor-position stream 10 10)
	       (surrounding-output-with-border (stream)
		 (format stream "Tick ~A" value))))))
    (repeat 5
      (sleep 1)
      (incf tick)
      (setq value (* value 10))
      (redisplay record stream))
    (repeat 5
      (sleep 1)
      (incf tick 1)
      (setq value (/ value 10))
      (redisplay record stream))))

(define-test (redisplay-graph redisplay) (stream)
  "Redisplay of a graph.  The nodes and edges should redisplay and move around correctly."
  (macrolet ((make-node (&key name children)
	       `(list* ,name ,children))
	     (node-name (node)
	       `(car ,node))
	     (node-children (node)
	       `(cdr ,node)))
    (let* ((3a (make-node :name "3A"))
	   (3b (make-node :name "3B"))
	   (2a (make-node :name "2A"))
	   (2b (make-node :name "2B"))
	   (2c (make-node :name "2C"))
	   (1a (make-node :name "1A" :children (list 2a 2b)))
	   (1b (make-node :name "1B" :children (list 2b 2c)))
	   (root (make-node :name "0" :children (list 1a 1b)))
	   (graph
	     (updating-output (stream :unique-id root)
	       (format-graph-from-root root
				       #'(lambda (node s)
					   (updating-output (s
							      :cache-value node)
					     (write-string (node-name node) s)))
				       #'cdr	;--- #'node-children
				       :stream stream))))
      (sleep 2)
      (setf (node-children 2a) (list 3a 3b))
      (redisplay graph stream)
      (sleep 2)
      (setf (node-children 2a) nil)
      (redisplay graph stream))))


;;; Simple tests of ACCEPT

(define-command-table presentations)

(define-test (string-accept presentations) (stream)
  "Simple test of ACCEPT-FROM-STRING.  The expected results are the integer 123 and pair of integers 123,456."
  (multiple-value-bind (object type)
      (accept-from-string 'integer "123")
    (format stream "~%~S ~S" object type))
  (multiple-value-bind (object type)
      (accept-from-string '(sequence integer) "123,456")
    (format stream "~%~S ~S" object type)))

(define-test (string-stream-accept presentations) (stream)
  "Simple test of ACCEPT using a string stream.  The expected results are the integer 123 and pair of integers 123,456."
  (multiple-value-bind (object type)
      (with-input-from-string (s "123")
	(accept 'integer :stream s))
    (format stream "~%~S ~S" object type))
  (multiple-value-bind (object type)
      (with-input-from-string (s "123,456")
	(accept '(sequence integer) :stream s))
    (format stream "~%~S ~S" object type)))

(define-test (highlighting-tests presentations) (stream)
  "Highlighting tests of various graphics.  Wave the mouse around to see if everything looks OK."
  (macrolet ((as-integer (x single-box &body body)
	       `(with-output-as-presentation ( stream
					       ,x
					       'integer
					      :single-box ,single-box)
		  ,@body)))
    (let ((delta-y 30)
	  (delta-x 10)
	  (number 0))
      (dolist (single-box '(nil t))
	(let ((x-offset 40))
	  (with-drawing-options (stream
				  :transformation (make-translation-transformation
						    delta-x delta-y))
	    (as-integer (incf number) single-box
			(draw-line* stream 0 0 20 20 :line-thickness 0))
	    (dolist (rest `((:filled t)
			    (:filled nil)
			    (:filled nil :line-thickness 2)))
	      (as-integer (incf number) single-box
			  (apply #'draw-circle*
				 stream (+ 10 x-offset) 10 10 rest))
	      (as-integer (incf number) single-box
			  (apply #'draw-rectangle*
				 stream (+ 40 x-offset) 0 (+ 60 x-offset) 20 rest))
	      (incf x-offset 80))))
	(incf delta-y 30))
      (dolist (single-box '(nil t :position :highlighting))
	(with-drawing-options (stream
				:transformation (make-translation-transformation
						  delta-x delta-y))
	  (as-integer (incf number) single-box
		      (draw-line* stream 0 0 20 20 :line-thickness 0)
		      (draw-circle* stream 50 10 10)))
	(incf delta-y 30))
      (stream-set-cursor-position stream 0 delta-y)))
  (loop
    (accept 'integer :stream stream)
    (fresh-line stream)))

(define-presentation-type drag-source ()
  :inherit-from 'integer)

(define-presentation-type drop-target ()
  :inherit-from 'integer)

(define-drag-and-drop-translator test-suite-dnd
    (drag-source string drop-target presentations)
    (presentation destination-presentation)
  (format nil "Dragged from ~D to ~D"
    (presentation-object presentation) (presentation-object destination-presentation)))

(define-test (drag-and-drop-tests presentations) (stream)
  "Try drag and drop"
  (formatting-table (stream :x-spacing "   ")
    (dotimes (i 3)
      (formatting-row (stream)
	(formatting-cell (stream)
	  (with-output-as-presentation (stream i 'drag-source)
	    (format stream "Drag source ~D" i)))
	(formatting-cell (stream)
	  (with-output-as-presentation (stream i 'drop-target)
	    (format stream "Drop target ~D" i))))))
  (loop
    (terpri stream)
    (accept 'string :stream stream)))


;;; Menus

(define-command-table menus-and-dialogs)

(define-test (simple-menu menus-and-dialogs) (stream)
  "A simple test of MENU-CHOOSE."
  (write-string
    (menu-choose '(("Whistle" :documentation "Select whistle")
		   ("Pat head"
		    :documentation "Pat head in various ways"
		    :items
		    (("with right hand" . "Pat head with right hand")
		     ("with left hand" . "Pat head with left hand")))
		   ("Rub Tummy" :items
		    (("clockwise" . "Rub tummy clockwise")
		     ("counter-clockwise" . "Rub tummy counter-clockwise")))
		   "Walk"
		   "Chew Gum")
		 :associated-window stream
		 :label "Select an activity")))


#+allegro
(define-test (more-simple-menus menus-and-dialogs) (stream)
  "Popup a few menus"
  (macrolet ((doit (&body body)
	       `(mp::with-timeout (1) ,@body)))
    (doit
     (menu-choose '("Whistle"
		    ("Pat head" :items
		     (("with right hand" . "Pat head with right hand")
		      ("with left hand" . "Pat head with left hand")))
		    ("Rub Tummy" :items
		     (("clockwise" . "Rub tummy clockwise")
		      ("counter-clockwise" . "Rub tummy counter-clockwise")))
		    "Walk"
		    "Chew Gum")
		  :associated-window stream
		  :label "Select an activity"))
    (doit
     (menu-choose '(1872812 "kdjfkdjf" 908909)
		  :label "foo" :text-style '(:fix :roman :huge)))
    (doit
     (menu-choose '("akjdfkjdf" "bdfkj" "cdfkj")
		  :label '("Foo" :text-style (:serif :bold :huge))
		  :text-style '(:fix :roman :huge)))
    (dotimes (i 5)
      (doit
       (menu-choose '("akjdfkjdf" "bdfkj" "cdfkj")
		    :label '("Foo" :text-style (:serif :bold :huge))
		    :printer #'princ)))))

(define-test (graphical-menu menus-and-dialogs) (stream)
  "A menu that contains graphics."
  (let ((icon-list '(blockhead bubblehead pinhead)))
    (labels ((draw-icon (icon stream)
	       #+genera (declare (sys:downward-function))
	       (ecase icon
		 (blockhead
		   (draw-rectangle* stream 0 0 20 20))
		 (bubblehead
		   (draw-circle* stream 10 10 10))
		 (pinhead
		   (draw-polygon* stream '(0 0 20 0 10 20)))))
	     (draw-icon-menu (menu presentation-type)
	       #+genera (declare (sys:downward-function))
	       (formatting-table (menu :y-spacing 5)
		 (dolist (icon icon-list)
		   (with-output-as-presentation ( menu
						  icon
						  presentation-type)
		     (formatting-row (menu)
		       (formatting-cell (menu)
			 (with-first-quadrant-coordinates (menu)
			   (draw-icon icon menu)))))))
	       nil))
      #+ignore (declare (dynamic-extent #'draw-icon #'draw-icon-menu))
      (with-menu (menu stream)
	(format stream "~S" (menu-choose-from-drawer menu 'menu-item #'draw-icon-menu))))))

(define-test (choose-compass-direction menus-and-dialogs) (stream)
  "A more complicated graphical menu.  Try pointing at one of the compass points."
  (labels ((draw-compass-point (stream ptype symbol x y)
	     #+genera (declare (sys:downward-function))
	     (with-output-as-presentation ( stream
					    symbol
					    ptype)
	       (draw-text* stream (symbol-name symbol) x y
			   :align-x :center :align-y :center
			   :text-style '(:sans-serif :roman :large))))
	   (draw-compass (stream ptype)
	     #+genera (declare (sys:downward-function))
	     (with-room-for-graphics (stream :first-quadrant nil)
	       (draw-line* stream 0 25 0 -25 :line-thickness 2)
	       (draw-line* stream 25 0 -25 0 :line-thickness 2)
	       (dolist (point '((n 0 -30) (s 0 30) (e 30 0) (w -30 0)))
		 (apply #'draw-compass-point stream ptype point)))))
    #+ignore (declare (dynamic-extent #'draw-compass-point #'draw-compass))
    (with-menu (menu stream :scroll-bars nil :label "Compass point")
      #-silica (setf (window-label menu) "Compass point")
      (format stream "~S" (menu-choose-from-drawer menu 'menu-item #'draw-compass)))))


;;; Dialogs

(define-test (simple-spreadsheet menus-and-dialogs) (stream)
  "A spreadsheet implemented using ACCEPT inside FORMATTING-TABLE inside ACCEPTING-VALUES."
  (let ((result (make-array '(3 3))))
    (accepting-values (stream)
      (formatting-table (stream :y-spacing (stream-line-height stream))
	(dotimes (row 3)
	  (formatting-row (stream)
	    (dotimes (cell 3)
	      (formatting-cell (stream)
		(let* ((id (+ (* 3 row) cell))
		       (default (or (aref result row cell) id)))
		  (setf (aref result row cell)
			(accept 'integer
				:prompt nil :default default
				:query-identifier id
				:stream stream)))))))))
    result))

(define-test (graphics-dialog menus-and-dialogs) (stream)
  "An ACCEPTING-VALUES dialog that has graphics inside of it."
  (graphics-dialog-internal stream))

(define-test (graphics-dialog-own-window menus-and-dialogs) (stream)
  "An own-window ACCEPTING-VALUES dialog that has graphics inside of it."
  (graphics-dialog-internal stream t))

(define-test (graphics-dialog-options menus-and-dialogs) (stream)
  "An ACCEPTING-VALUES dialog with options."
  (let ((own-window nil)
	(scroll-bars nil))
    (accepting-values (stream :align-prompts t)
      (setf own-window (accept 'boolean
			       :default own-window
			       :prompt "Own window"
			       :stream stream))
      (setf scroll-bars (accept '(member nil t :both :dynamic :vertical :horizontal)
				:view '(radio-box-view :orientation :vertical)
				:default scroll-bars
				:prompt "Scroll bars"
				:stream stream
				:active-p own-window)))
    (graphics-dialog-internal stream own-window (and own-window scroll-bars))))

(defun graphics-dialog-internal (stream &optional own-window scroll-bars)
  (labels ((display-color (object stream &key acceptably)
	     (declare (ignore acceptably))
	     (with-room-for-graphics (stream)
	       (draw-rectangle* stream 0 0 30 10 :ink
				(color-name-color object))))
	   (color-name-color (name)
	     (ecase name
	       (:foreground +foreground-ink+)
	       (:red +red+)
	       (:green +green+)
	       (:blue +blue+))))
    (let ((square-dimension 100)
	  (draw-circle t)
	  (draw-square t)
	  (draw-point t)
	  (draw-/-diagonal t)
	  (draw-\\-diagonal t)
	  (line-thickness 1)
	  (color-name :foreground)
	  (line-thickness-units :normal))
      (accepting-values (stream :own-window own-window
				:scroll-bars scroll-bars
				:label "Graphics Dialog")
	(setq square-dimension
	      (accept 'number :stream stream
		      :prompt "Size of square" :default square-dimension))
	(terpri stream)
	(setq draw-circle
	      (accept 'boolean :stream stream
		      :prompt "Draw the circle" :default draw-circle))
	(terpri stream)
	(setq draw-square
	      (accept 'boolean :stream stream
		      :prompt "Draw the square" :default draw-square))
	(terpri stream)
	(setq draw-point
	      (accept 'boolean :stream stream
		      :prompt "Draw point" :default draw-point))
	(terpri stream)
	(setq draw-/-diagonal
	      (accept 'boolean :stream stream
		      :prompt "Draw / diagonal" :default draw-/-diagonal))
	(terpri stream)
	(setq draw-\\-diagonal
	      (accept 'boolean :stream stream
		      :prompt "Draw \\ diagonal" :default draw-\\-diagonal))
	(terpri stream)
	(setq line-thickness
	      (accept 'number :stream stream
		      :prompt "Line thickness" :default line-thickness))
	(terpri stream)
	(setq line-thickness-units
	  (accept '(member :normal :point) :stream stream
		      :prompt "Line style units" :default line-thickness-units))
	(terpri stream)
	(setq color-name
	      (accept `((completion (:foreground :red :green :blue))
			:name-key ,#'identity
			:printer ,#'display-color)
		      :view #+allegro '(radio-box-view
					 :toggle-button-options (:indicator-type nil))
			    #-allegro (stream-default-view stream)
		      :stream stream
		      :prompt "Color" :default color-name))
	(terpri stream)
	(with-drawing-options (stream :ink (color-name-color color-name))
	  (with-room-for-graphics (stream)
	    (let ((radius (/ square-dimension 2)))
	      (with-drawing-options (stream :line-unit line-thickness-units
					    :line-thickness line-thickness)
		(when draw-square
		  (draw-polygon* stream (list 0 0
					      0 square-dimension
					      square-dimension square-dimension
					      square-dimension 0)
				 :line-joint-shape :miter
				 :filled nil))
		(when draw-circle
		  (draw-circle* stream radius radius radius
				:filled nil))
		(when draw-point
		  (draw-point* stream
			       (/ square-dimension 4)
			       (/ square-dimension 2)))
		(when draw-\\-diagonal
		  (draw-line* stream 0 square-dimension square-dimension 0
			      :line-cap-shape :round))
		(when draw-/-diagonal
		  (draw-line* stream 0 0 square-dimension square-dimension
			      :line-cap-shape :round))))))))))


(define-test (gadgets-dialog menus-and-dialogs) (stream)
  "An own-window ACCEPTING-VALUES dialog that has lots of gadgets inside of it."
  (gadgets-dialog-internal stream nil))

(define-test (ozone-dialog menus-and-dialogs) (stream)
  "Test of gadgets out in hyper space"
  (let ((x "hello"))
    (accepting-values (stream :own-window nil :label "Gadgets dialog")
      (stream-set-cursor-position stream 100000 0)
      (setq x (accept 'string :view +text-field-view+
		      :stream stream
		      :default x))
      (terpri stream))))

(define-test (window-stream menus-and-dialogs) (stream)
  "Simple test of open-window-stream"
  (declare (ignore stream))
  (let ((w (open-window-stream)))
    (window-expose w)
    (sleep 0.1)				; so why do I need this?
    (write-string "A window stream" w)
    (sleep 2)
    (destroy-frame (pane-frame w))))

(define-test (slider-dialog menus-and-dialogs) (stream)
  "Various sliders"
  (let ((d 0.5)
	(e 5)
	(f 5)
	(g 5))
    (accepting-values (stream :own-window nil :label "sliders dialog")
      (macrolet ((accepts (&rest accepts)
		   `(progn
		      ,@(mapcar #'(lambda (ac)
				    (destructuring-bind
					(var type &key view (prompt (format nil "~A" var))) ac
				      `(progn
					 (setq ,var (accept ',type
							    :stream stream
							    ,@(and view `(:view ,view))
							    :default ,var
							    :prompt ,prompt))
					 (terpri stream))))
				accepts))))
	(accepts
	 (d (float 0 1) :view '(slider-view :show-value-p t
				:decimal-places 2
				:min-label "min"
				:max-label "max"))
	 (e (integer 0 10) :view +slider-view+)
	 (f (float 0 100) :view '(slider-view :orientation :vertical :show-value-p t))
	 (g (integer 0 10) :view '(slider-view :orientation :vertical)))
	(terpri stream)))))

(define-test (list-pane-dialog menus-and-dialogs) (stream)
  "Various list panes"
  (let ((x :a-one))
    (accepting-values (stream)
      (setf x (accept '(member :a-one :b-two :c-three :d-four)
		      :stream stream
		      :view +list-pane-view+
		      :default x
		      :prompt "Value for X")))))

(define-test (readonly-gadget-dialog menus-and-dialogs) (stream)
  "Create a bunch of readonly gadgets"
  (accepting-values (stream :own-window nil :label "Gadgets dialog"
			    :align-prompts nil)
    (macrolet ((do-presents (view &rest p)
		 `(progn
		    ,@(mapcar #'(lambda (x)
				  `(progn
				     (present ',(car x) ',(cadr x)
					      ,@(and view `(:view ,view))
					      :prompt (format nil "~A,~A"
							',(cadr x)
							,(if (constantp view)
							     (if (eval view) `',(type-of (eval view)) :default)
							     `(if ,view (type-of ,view) :default)))
					      :stream stream)
				     (terpri stream)))
			      p))))

      (formatting-item-list (stream :n-columns 2)
	(formatting-cell (stream)
	  (with-aligned-prompts (stream)
	    (do-presents
	      nil
	      (5.0 real)
	      ("xxx" string)
	      (#P"/tmp/fooo" pathname)
	      (:a (member :a :b))
	      ((:a :b) (subset :a :b))
	      (( 3 4) (sequence integer))
	      ;; sequence-enumerated
	      (t boolean))))
	(formatting-cell (stream)
	  (with-aligned-prompts (stream)
	    (do-presents
	      +text-field-view+
	      (5.0 real)
	      ("xxx" string)
	      (#P"/tmp/fooo" pathname)
	      (:a (member :a :b))
	      ((:a :b) (subset :a :b))
	      (( 3 4) (sequence integer))
	      ;; sequence-enumerated
	      (t boolean))))

	(formatting-cell (stream)
	  (with-aligned-prompts (stream)
	    (do-presents
	      '(text-editor-view :nlines 5 :ncolumns 30)
	      ("      (do-presents
	  +text-field-view+
	  (5.0 real)
	(xxx string)
	(#P/tmp/foo pathname)
	(:a (member :a :b))
	((:a :b) (subset :a :b))
	(( 3 4) (sequence integer))
	;; sequence-enumerated
	(t boolean)) "
	       string))))

	(formatting-cell (stream)
	  (with-aligned-prompts (stream)
	    (do-presents
	      +slider-view+
	      (5.0 float)
	      (1 integer))))))))

(defun gadgets-dialog-internal (stream &optional own-window)
  (macrolet ((accepts (&rest accepts)
	       `(progn
		  ,@(mapcar #'(lambda (ac)
				(destructuring-bind
				    (var type &key view (prompt (format nil "~A" var))) ac
				  `(progn
				     (setq ,var (accept ',type
							:stream stream
							,@(and view `(:view ,view))
							:default ,var
							:prompt ,prompt))
				     (terpri stream))))
			    accepts))))
    (let ((a :red)
	  (b '(:green))
	  (c nil)
	  (d 0.5)
	  (e 5)
	  (f :blue)
	  (g 5)
	  (h "kjkdjf")
	  (i "kjdfjdf")
	  (j :red)
	  (k '(:green :blue))
	  (l :red))
      (accepting-values (stream :own-window own-window :label "Gadgets dialog")
	(accepts (a (member :red :blue :green))
		 (b (subset :red :blue :green))
		 (c boolean)
		 (d (float 0 1) :view '(slider-view :decimal-places 2))
		 (e (integer 0 10) :view +slider-view+)
		 (f (member :red :blue :green) :view +text-field-view+)
		 (g (integer 0 10) :view +text-field-view+)
		 (h string :view +text-field-view+)
		 (i string :view +text-editor-view+)
		 (j (member :red :blue :green) :view +list-pane-view+)
		 (k (subset :red :blue :green) :view +list-pane-view+)
		 (l (member :red :blue :green) :view +option-pane-view+))))))

(define-test (input-editor-tests menus-and-dialogs) (stream)
  "Type lots of input editor commands"
  (loop
    (fresh-line stream)
    (accept 'string :stream stream)))

(define-application-frame list-pane-frame ()
  ()
  (:panes
   (list-pane1
    (make-pane 'clim::list-pane
	       :items '("no scrolling" "a very long item to show" "short")
	       :min-height 10
	       :max-height +fill+
	       :min-width 0))
   (list-pane2
    (make-pane 'clim::list-pane
	       :items '("nil" "a very long item to show" "short")
	       :min-height 10
	       :max-height +fill+
	       :min-width 0))
   (list-pane3
    (make-pane 'clim::list-pane
	       :items '(":dynamic" "a very long item to show" "short")
	       :min-height 10
	       :max-height +fill+
	       :min-width 0))
   (list-pane4
    (make-pane 'clim::list-pane
	       :items '(":horizontal" "a very long item to show" "short")
	       :min-height 10
	       :max-height +fill+
	       :min-width 0))
   (list-pane5
    (make-pane 'clim::list-pane
	       :items '(":vertical" "a very long item to show" "short")
	       :min-height 10
	       :max-height +fill+
	       :min-width 0))
   (list-pane6
    (make-pane 'clim::list-pane
	       :items '(":both" "a very long item to show" "short")
	       :min-height 10
	       :max-height +fill+
	       :min-width 0)))
  (:layouts
   (default
       (vertically ()
	 list-pane1
	 (scrolling (:scroll-bars nil)
	   list-pane2)
	 (scrolling (:scroll-bars :dynamic)
	   list-pane3)
	 (scrolling (:scroll-bars :horizontal)
	   list-pane4)
	 (scrolling (:scroll-bars :vertical)
	   list-pane5)
	 (scrolling (:scroll-bars :both)
	   list-pane6)))))

(define-list-pane-frame-command (com-quit-list-pane :menu "Quit") ()
  (clim::frame-exit clim::*application-frame*))

(define-test (scrolled-list-pane-tests menus-and-dialogs) (stream)
  "Resize the window and check things work ok"
  (run-frame-top-level
   (make-application-frame 'list-pane-frame
			   :input-buffer (stream-input-buffer stream))))



;;;; Benchmarks

(define-command-table benchmarks)

(defvar *benchmarks* nil)

(defmacro define-benchmark ((name &key (iterations 1)) (stream) caption
			    &body body)
  #+genera (declare (zwei:indentation 2 1))
  (check-type caption (or null string))
  (let ((function-name (intern (format nil "~A-~A" 'benchmark name)
			       (symbol-package 'define-benchmark))))
    `(progn
       (defun ,function-name (&key (careful nil))
	 (labels ((body (,stream) ,@body))
	   (time-continuation ',name ,iterations #'body :careful careful)))
       (define-command (,name :command-table benchmarks :menu t) ()
	 (write-test-caption ,caption)
	 (,function-name :careful nil))
       (pushnew (list ',name ',caption) *benchmarks* :test #'eq :key #'car))))

;;--- It would be nice if we could measure consing, too

(defparameter *multiply-factor* 1)
(defparameter *scale-iterations* t)

(defun time-continuation (name iterations continuation &key (careful t))
  (with-display-pane (stream)
    (let ((results nil)
	  (vresults nil))
      (when careful
	(funcall continuation stream))
      (let ((initial-scale-iterations 1))
	#+genera (si:%gc-scavenge)
	#+cloe-runtime (gc-immediately)
	#+lucid (lucid-common-lisp:ephemeral-gc)
	#+allegro (excl::gc)
	#+ccl (ccl:gc)
	(#+genera si:inhibit-gc-flips #-genera progn
	  (repeat (* *multiply-factor* (if (not careful) 1 5))
	    (window-clear stream)
	    (do ((scale-iterations initial-scale-iterations (* scale-iterations 2)))
		(nil)
	      (let ((start-time (get-internal-real-time))
		    (vstart-time (get-internal-run-time)))
		(repeat (* iterations scale-iterations)
		  (#+genera mi:with-metering-enabled #-genera progn
		    (funcall continuation stream))
		  (force-output stream))
		(let ((vtime (/ (float (- (get-internal-run-time) vstart-time))
				internal-time-units-per-second))
		      (rtime (/ (float (- (get-internal-real-time) start-time))
				internal-time-units-per-second)))
		  (when (or (null *scale-iterations*)
			    (or (> vtime 1) (> rtime 1)))
		    (setq initial-scale-iterations scale-iterations)
		    (push (/ rtime scale-iterations) results)
		    (push (/ vtime scale-iterations) vresults)
		    (return nil))))))))
      (let ((time
	     (if (not careful)
		 (first results)
	       (let ((results (rest (butlast (sort results #'<)))))
		 (/ (apply #'+ results) (length results)))))
	    (vtime
	     (if (not careful)
		 (first vresults)
	       (let ((results (rest (butlast (sort vresults #'<)))))
		 (/ (apply #'+ results) (length results))))))
	(format (get-frame-pane *application-frame* 'caption-pane)
	    "~%Each run of ~A took about ~3$ real, ~3$ virtual, ~3$ v/r seconds"
	  name time vtime (float (/ vtime time)))
	time))))

;;; To distill the results down to something more easily interpretable by the
;;; uninitiated, we report aggregate results in seven categories; each aggregate
;;; is the geometric mean of the times reported for the separate tests.
(defparameter *summary-contributions* '(
  ("Graphics"
   clipped-shape-drawing
   stippled-shape-drawing
   filled-shape-drawing
   thick-shape-drawing
   transformed-shape-drawing
   unrecorded-shape-drawing
   shape-drawing
   clipped-line-drawing
   thick-line-drawing
   transformed-line-drawing
   unrecorded-line-drawing
   line-drawing)
  ("Text"
   text-output
   unrecorded-text-output
   stylish-text-output)
  ("Scrolling and refresh"
   graphics-refresh
   text-refresh
   graphics-scrolling
   text-scrolling)
  ("Mouse sensitivity"
   find-shape-presentations
   highlight-shape-presentations
   find-textual-presentations
   highlight-textual-presentations
   highlight-menu-items)
  ("Formatting"
   simple-table-formatting
   graphic-table-formatting
   compound-table-formatting
   simple-graph-formatting)
  ("Redisplay"
   basic-redisplay
   graphic-redisplay
   compound-redisplay)
  ("Menus and dialogs"
   simple-menu-choose
   cached-menu-choose
   simple-dialog
   window-dialog
   compound-dialog)))

(define-command (run-benchmarks :command-table benchmarks :menu t)
    (&key (pathname '(null-or-type pathname))
	  (comment 'string :default "no comment"))
  (unless (and pathname comment)
    (let ((stream (get-frame-pane *application-frame* 'display-pane)))
      (window-clear stream)
      (accepting-values (stream :align-prompts t)
	(setq pathname
	  (accept 'pathname :prompt "Pathname for results"
		  :stream stream :default pathname))
	(setq comment
	  (accept 'string :prompt "Comment describing this run"
		  :stream stream :default comment)))))
  (run-benchmarks-internal pathname comment))

(defun run-benchmarks-internal (pathname comment)
  (let ((data nil))
    (dolist (benchmark (reverse *benchmarks*))
      (let ((function (intern (format nil "~A-~A" 'benchmark (first benchmark))
			      (symbol-package 'run-benchmarks-internal))))
	(let ((time (funcall function :careful t)))
	  (push (list (first benchmark) time) data))))
    (setq data (reverse data))
    (when pathname
      (let ((*package* (or (find-package :common-lisp-user)
			   (error "Package COMMON-LISP-USER not found"))))
	(with-open-file (s pathname :if-exists :supersede :direction :output)
	  (format s ";Speed of ~A ~A" (lisp-implementation-type) (lisp-implementation-version))
	  (format s "~%;on ~A ~A.~%" (machine-type) (machine-instance))
	  (when comment (format s ";~A~%" comment))
	  (print data s))))))

(define-command (generate-report :command-table benchmarks :menu t)
    ()
  (let ((pathname nil)
	(specs nil))
    (let ((stream (get-frame-pane *application-frame* 'display-pane)))
      (window-clear stream)
      (accepting-values (stream :resynchronize-every-pass t)
	(terpri stream)
	(setq pathname (accept 'pathname :prompt "Pathname for report" :stream stream))
	(dolist (s specs)
	  (format stream "~%Name and pathname: ~A, ~A" (first s) (second s)))
	(terpri stream)
	(let* ((default '("Sun" "CLIM.report"))
	       (spec (accept '((sequence-enumerated string pathname))
			     :prompt "Enter name and pathname"
			     :stream stream
			     :default default)))
	  (when (not (eq spec default))
	    (setq specs (nconc specs (list (coerce spec 'list))))))))
    (generate-report-internal specs pathname)))

(defun generate-report-internal (specs pathname)
  ;; First collect the data files
  ;; DATA is ((short-name long-name results flavors-p)...)
  (let ((data
	  (clim-utils:with-standard-io-environment
	    (let ((data nil)
		  (*package* (or (find-package :common-lisp-user)
				 (error "Package COMMON-LISP-USER not found"))))
	      (dolist (name-and-pathname specs)
		(let* ((short-name (pop name-and-pathname))
		       (pathname (pop name-and-pathname)))
		  (with-open-file (file (pathname
					  (string-trim '(#\space) (namestring pathname)))
				   :direction :input)
		    (let ((comments
			    (with-output-to-string (s)
			      (loop
				(let ((ch (read-char file)))
				  ;; We're done when we hit a non-comment line
				  (when (not (char= ch #\; ))
				    (unread-char ch file)
				    (return))
				  (write-line (read-line file) s)))
			      #+genera
			      (format s "~\\date\\~%" (file-write-date file))
			      #-genera
			      (format s "Date: ~D~%" (file-write-date file))))
			  (file-data (read file)))
		      (push (list short-name comments file-data) data)))))
	      (nreverse data))))
	(done nil))
    (with-open-file (s pathname :direction :output)
      ;; Print out the key to the short names
      (dolist (short-and-long data)
	(let* ((short-name (pop short-and-long))
	       (long-name (pop short-and-long)))
	  (format s "~%~A = ~A" short-name long-name)))
      (flet ((print-out (description results)
	       (when results
		 (format s "~2%~A" description)
		 (let ((min nil))
		   (do ((name-and-number results (cdr name-and-number)))
		       ((null name-and-number))
		     (let ((number (second (car name-and-number))))
		       (when number
			 (setq min (if min (min number min) number)))))
		   (do ((name-and-number results (cdr name-and-number))
			(column 0 (1+ column)))
		       ((null name-and-number))
		     (let ((short-name (first (car name-and-number)))
			   (number (second (car name-and-number))))
		       (when (zerop (mod column 4))
			 (format s "~%  "))
		       (if number
			   (format s "~22@<~A ~2$ (~2$)~>"
			     short-name number (float (/ number min)))
			   (format s "~22@T"))))))))
	;; Print out the summaries
	(format s "~&~%Summary results (geometric means of a number of relevant tests):~%~%Category~28T")
	(dolist (short-name data)
	  (format s "~@12A" (car short-name)))
	(terpri)
	(terpri)
	(dolist (category-and-contributions *summary-contributions*)
	  (let* ((category (pop category-and-contributions))
		 (contributions category-and-contributions))
	    (format s "~&~A:~28T" category)
	    (dolist (datum data)
	      (let ((alist (third datum))
		    (total 1))
		(dolist (test contributions)
		  (multiple-value-bind (test weight)
		      (if (listp test)
			  (values (first test) (second test))
			  (values test 1))
		    (let ((value (cadr (assoc test alist))))
		      (if (not (null value))
			  (setq total (* total (* value weight)))
			  (return (setq total nil))))))
		(if (null total)
		    (format s "~@12A" "")
		    (format s "~2,1,12$" (expt total (/ (length contributions)))))))))
	;; Now print out all the results
	(dolist (function-and-description (reverse *benchmarks*))
	  (let* ((function (pop function-and-description))
		 (description (pop function-and-description))
		 (results nil))
	    (dolist (name-and-alist data)
	      (let* ((short-name (first name-and-alist))
		     (alist (third name-and-alist))
		     (key function)
		     (result (second (assoc key alist))))
		(when (or result results)
		  (push (list short-name result) results)
		  (pushnew key done))))
	    (loop
	      (when (not (and results (null (cadar results))))
		(return))
	      (pop results))
	    (setq results (nreverse results))
	    (print-out description results)))))))

;;; Graphics benchmarks

(defun line-drawing-kernel (stream n-lines &key (clear t))
  (when clear
    (window-clear stream))
  (let ((delta (floor 400 n-lines)))
    (do ((x 0 (+ x delta)))
	((>= x 400))
      (draw-line* stream x 0 (+ x delta) 50))))

(define-benchmark (line-drawing :iterations 10) (stream)
  "Draw lines"
  (line-drawing-kernel stream 50))

(define-benchmark (unrecorded-line-drawing :iterations 10) (stream)
  "Draw lines without output recording"
  (with-output-recording-options (stream :record nil)
    (line-drawing-kernel stream 50)))

(define-benchmark (transformed-line-drawing :iterations 10) (stream)
  "Draw lines under a nontrivial transformation"
  (let ((transformation (make-scaling-transformation 0.9 0.9)))
    (with-drawing-options (stream :transformation transformation)
      (line-drawing-kernel stream 50))))

(define-benchmark (thick-line-drawing :iterations 10) (stream)
  "Draw thick lines"
  (with-drawing-options (stream :line-thickness 3)
    (line-drawing-kernel stream 50)))

(define-benchmark (clipped-line-drawing :iterations 10) (stream)
  "Draw lines through a clipping region"
  (with-drawing-options (stream :clipping-region *chess-board-clip-region*)
    (line-drawing-kernel stream 50)))

(defun shape-drawing-kernel (stream n-shapes filled-p &key (clear t))
  (when clear
    (window-clear stream))
  (let ((delta (floor 400 n-shapes)))
    (do ((y 75)
	 (x 0 (+ x delta)))
	((>= x 400))
      (draw-rectangle* stream x y (+ x delta) (+ y delta)
		       :filled filled-p))
    (do ((y 150)
	 (x 0 (+ x delta))
	 (offset (floor delta 4)))
	((>= x 400))
      (draw-polygon* stream
		     (list (+ x offset) y
			   x (+ y delta)
			   (+ x delta (- offset)) (+ y delta)
			   (+ x delta) y)
		     :filled filled-p :closed t))
    (do ((y 225)
	 (x 0 (+ x delta))
	 (radius (floor delta 2)))
	((>= x 400))
      (draw-circle* stream (+ x radius) (+ y radius) radius
		    :filled filled-p))))

(define-benchmark (shape-drawing :iterations 5) (stream)
  "Draw unfilled shapes"
  (shape-drawing-kernel stream 10 nil))

(define-benchmark (unrecorded-shape-drawing :iterations 5) (stream)
  "Draw unfilled shapes, output recording disabled"
  (with-output-recording-options (stream :record nil)
    (shape-drawing-kernel stream 10 nil)))

(define-benchmark (transformed-shape-drawing :iterations 5) (stream)
  "Draw unfilled shapes under a nontrivial transformation"
  (let ((transformation (make-scaling-transformation 0.9 0.9)))
    (with-drawing-options (stream :transformation transformation)
      (shape-drawing-kernel stream 10 nil))))

(define-benchmark (thick-shape-drawing :iterations 5) (stream)
  "Draw shapes with thick lines"
  (with-drawing-options (stream :line-thickness 3)
    (shape-drawing-kernel stream 10 nil)))

(define-benchmark (filled-shape-drawing :iterations 5) (stream)
  "Draw filled shapes"
  (with-drawing-options (stream)
    (shape-drawing-kernel stream 10 t)))

(define-benchmark (stippled-shape-drawing :iterations 5) (stream)
  "Draw stippled shapes"
  (with-drawing-options (stream
			  :ink (make-rectangular-tile
				 (make-pattern #2a((0 1) (1 0))
					       (list +background-ink+ +foreground-ink+))
				 2 2))
    (shape-drawing-kernel stream 10 t)))

(define-benchmark (clipped-shape-drawing :iterations 5) (stream)
  "Draw shapes through a clipping region"
  (with-drawing-options (stream :clipping-region *chess-board-clip-region*)
    (shape-drawing-kernel stream 10 nil)))

;;; Text benchmarks

(define-benchmark (text-output :iterations 5) (stream)
  "Write strings"
  (repeat 10
    (write-string "Four" stream)
    (write-string " score and " stream)
    (write-string "seven" stream)
    (write-string " years ago, our fathers..." stream)
    (terpri stream)))

(define-benchmark (unrecorded-text-output :iterations 5) (stream)
  "Write strings, with output recording disabled"
  (with-output-recording-options (stream :record nil)
    (repeat 10
      (write-string "Four" stream)
      (write-string " score and " stream)
      (write-string "seven" stream)
      (write-string " years ago, our fathers..." stream)
      (terpri stream))))

(define-benchmark (stylish-text-output :iterations 5) (stream)
  "Write stylish strings"
  (repeat 10
    (with-text-family (stream :sans-serif)
      (with-text-size (stream :large)
	(write-string "Four" stream))
      (with-text-face (stream :italic)
	(write-string " score and " stream))
      (with-text-size (stream :large)
	(with-text-face (stream :bold)
	  (write-string "seven" stream)))
      (with-text-face (stream :italic)
	(write-string " years ago, our fathers..." stream))
      (terpri stream))))

(define-benchmark (clipped-text-output :iterations 3) (stream)
  "Draw text through a clipping region"
  (with-drawing-options (stream :clipping-region *chess-board-clip-region*)
    (repeat 5
      (write-string "Four" stream)
      (write-string " score and " stream)
      (write-string "seven" stream)
      (write-string " years ago, our fathers..." stream)
      (terpri stream))))

;;; Scrolling and refreshing benchmarks

(defun scroll-kernel (stream start-x start-y x-excursion y-excursion delta)
  (do ((x start-x)
       (y start-y (+ y delta)))
      ((= y y-excursion))
    (window-set-viewport-position stream x y)
    (force-output stream))
  (do ((x start-x)
       (y y-excursion (- y delta)))
      ((= y start-y))
    (window-set-viewport-position stream x y)
    (force-output stream))
  (do ((x start-x (+ x delta))
       (y start-y))
      ((= x x-excursion))
    (window-set-viewport-position stream x y)
    (force-output stream))
  (do ((x x-excursion (- x delta))
       (y start-y))
      ((= x start-x))
    (window-set-viewport-position stream x y)
    (force-output stream)))

(define-benchmark (text-scrolling) (stream)
  "Scroll a window full of text, horizontally and vertically"
  (multiple-value-bind (start-x start-y)
      (window-viewport-position stream)
    (repeat 25
      (write-string "Four score and seven years ago, our fathers..." stream)
      (terpri stream))
    (force-output stream)
    (window-set-viewport-position stream start-x start-y)
    (scroll-kernel stream start-x start-y 400 400 25)))

(define-benchmark (graphics-scrolling) (stream)
  "Scroll a window full of graphics, horizontally and vertically"
  (multiple-value-bind (start-x start-y)
      (window-viewport-position stream)
    (line-drawing-kernel stream 20)
    (shape-drawing-kernel stream 10 nil :clear nil)
    (scroll-kernel stream start-x start-y 400 400 25)))

(defun refresh-kernel (stream iterations start-x start-y)
  (force-output stream)
  (window-set-viewport-position stream start-x start-y)
  (repeat iterations
    (window-refresh stream)
    (force-output stream)))

(define-benchmark (text-refresh) (stream)
  "Refresh a window full of text"
  (multiple-value-bind (start-x start-y)
      (window-viewport-position stream)
    (repeat 25
      (write-string "Four score and seven years ago, our fathers..." stream)
      (terpri stream))
    (refresh-kernel stream 5 start-x start-y)))

(define-benchmark (graphics-refresh) (stream)
  "Refresh a window full of graphics"
  (multiple-value-bind (start-x start-y)
      (window-viewport-position stream)
    (line-drawing-kernel stream 20)
    (shape-drawing-kernel stream 10 nil :clear nil)
    (refresh-kernel stream 5 start-x start-y)))

;;; Mouse sensitivity benchmarks

(define-presentation-type shape ())
(define-presentation-type rect () :inherit-from 'shape)
(define-presentation-type square () :inherit-from 'rect)
(define-presentation-type circle () :inherit-from 'shape)
(define-presentation-type triangle () :inherit-from 'shape)

(defun shape-presenting-kernel (stream n-shapes)
  (let ((delta (floor 400 n-shapes)))
    (do ((y 0)
	 (x 0 (+ x delta)))
	((>= x 400))
      (let ((object (list 'line x y (+ x delta) (+ y 50))))
	(with-output-as-presentation (stream  object 'shape)
	  (draw-line* stream x y (+ x delta) (+ y 50)))))
    (do ((y 75)
	 (x 0 (+ x delta)))
	((>= x 400))
      (let ((object (list 'rectangle x y (+ x delta) (+ y delta))))
	(with-output-as-presentation (stream  object 'square)
	  (draw-rectangle* stream x y (+ x delta) (+ y delta) :filled nil))))
    (do ((y 150)
	 (x 0 (+ x delta))
	 (offset (floor delta 4)))
	((>= x 400))
      (let* ((coordinates (list (+ x offset) y
				x (+ y delta)
				(+ x delta (- offset)) (+ y delta)
				(+ x delta) y))
	     (object (cons 'polygon* coordinates)))
	(with-output-as-presentation (stream  object 'shape)
	  (draw-polygon* stream coordinates :filled nil))))
    (do ((y 225)
	 (x 0 (+ x delta))
	 (radius (floor delta 2)))
	((>= x 400))
      (let ((object (list 'circle (+ x radius) (+ y radius) radius)))
	(with-output-as-presentation (stream  object 'circle)
	  (draw-circle* stream (+ x radius) (+ y radius) radius :filled nil))))))

(defun text-presenting-kernel (stream)
  (with-text-family (stream :sans-serif)
    (repeat 15
      (with-output-as-presentation (stream  '(square 10) 'square)
	(write-string "This represents a square" stream))
      (write-string "  " stream)
      (with-output-as-presentation (stream  '(rectangle 30 20) 'rect)
	(write-string "This represents a rectangle" stream))
      (write-string "  " stream)
      (with-output-as-presentation (stream '(circle 50) 'circle)
	(write-string "This represents a circle" stream))
      (write-string "  " stream))))

(defun make-fake-input-context (presentation-type)
  (let ((type (list presentation-type)))
    (list type (list 'catch-tag type))))

(define-benchmark (find-shape-presentations) (stream)
  "Find graphical presentations"
  (shape-presenting-kernel stream 8)
  (let ((input-context (make-fake-input-context 'shape)))
    (do ((y 0 (+ y 10)))
	((>= y 300))
      (do ((x 0 (+ x 10)))
	  ((>= x 450))
	(find-innermost-applicable-presentation input-context stream x y)))))

(define-benchmark (highlight-shape-presentations) (stream)
  "Highlight graphical presentations"
  (shape-presenting-kernel stream 8)
  (let ((input-context (make-fake-input-context 'shape)))
    (do ((y 0 (+ y 25)))
	((>= y 300))
      (do ((x 0 (+ x 25)))
	  ((>= x 450))
	(let ((p (find-innermost-applicable-presentation input-context stream x y)))
	  (when (not (null p))
	    (set-highlighted-presentation stream p nil)))))))

(define-benchmark (find-textual-presentations) (stream)
  "Find textual presentations"
  (text-presenting-kernel stream)
  (let ((input-context (make-fake-input-context 'shape)))
    (do ((y 0 (+ y 10)))
	((>= y 200))
      (do ((x 0 (+ x 10)))
	  ((>= x 450))
	(find-innermost-applicable-presentation input-context stream x y)))))

(define-benchmark (highlight-textual-presentations) (stream)
  "Highlight textual presentations"
  (text-presenting-kernel stream)
  (let ((input-context (make-fake-input-context 'shape))
	(*pointer-documentation-output* nil))
    (do ((y 0 (+ y 15)))
	((>= y 200))
      (do ((x 0 (+ x 15)))
	  ((>= x 450))
	(stream-set-pointer-position stream x y)
	 (highlight-applicable-presentation
	   *application-frame* stream input-context nil)))))

(define-presentation-type benchmark-menu-item ())

(defun draw-menu-benchmark (stream)
  (fresh-line stream)
  (multiple-value-bind (x0 y0)
      (stream-cursor-position stream)
    (declare (ignore x0))
    (formatting-item-list (stream :move-cursor t)
      (dolist (item '(("Red" :value +red+)
		      ("Green" :value +green+)
		      ("Blue" :value +blue+)
		      ("Yellow" :value +yellow+)
		      ("Orange" :value +orange+)
		      ("White" :value +white+)
		      ("Black" :value +black+)))
	(with-output-as-presentation ( stream
				       item
				       'benchmark-menu-item
				      :single-box t)
	  (formatting-cell (stream)
	    (print-menu-item item stream)))))
    (fresh-line stream)
    (force-output stream)
    (multiple-value-bind (x1 y1)
	(stream-cursor-position stream)
      (declare (ignore x1))
      (values y0 y1))))

(define-benchmark (highlight-menu-items) (stream)
  "Highlight menu items"
  (multiple-value-bind (y0 y1)
      (draw-menu-benchmark stream)
    (let ((input-context (make-fake-input-context 'benchmark-menu-item))
	  (*pointer-documentation-output* nil))
      (repeat 10
	(do ((y y0 (+ y 3)))
	    ((>= y y1))
	  (stream-set-pointer-position stream 20 y)
	  (highlight-applicable-presentation
	    *application-frame* stream input-context nil))))))


;;; Formatting benchmarks

(define-benchmark (simple-table-formatting) (stream)
  "Format a simple table of numbers"
  (fresh-line stream)
  (formatting-table (stream :x-spacing '(2 :character))
    (dotimes (i 11)
      (formatting-row (stream)
	(dotimes (j 15)
	  (if (zerop i)				;first row
	      (if (zerop j)			;first cell
		  (formatting-cell (stream :align-x :right)
		    (with-text-face (stream :bold)
		      (write-string "*" stream)))
		  (formatting-cell (stream :align-x :right)
		    (with-text-face (stream :bold)
		      (format stream "~D" (1- j)))))
	    (if (zerop j)			;first column
		(formatting-cell (stream :align-x :right)
		  (with-text-face (stream :bold)
		    (format stream "~D" (1- i))))
	        (formatting-cell (stream :align-x :right)
		  (format stream "~D" (* (1- i) (1- j)))))))))))

(define-benchmark (compound-table-formatting) (stream)
  "Format several bordered tables"
  (flet ((table (stream start)
	   (surrounding-output-with-border (stream)
	     (formatting-table (stream)
	       (repeat 3
		 (formatting-row (stream)
		   (formatting-cell (stream :align-x :right)
		     (format stream "~D" start))
		   (formatting-cell (stream :align-x :right)
		     (format stream "~D" (* start start)))
		   (formatting-cell (stream :align-x :right)
		     (format stream "~D" (* start start start))))
		 (incf start))))))
    (fresh-line stream)
    (formatting-table (stream :y-spacing 10 :x-spacing 10)
      (dotimes (start 5)
	(formatting-row (stream)
	  (formatting-cell (stream :align-x :center)
	    (table stream start))
	  (formatting-cell (stream :align-x :center)
	    (table stream (* start 10))))))))

(define-benchmark (graphic-table-formatting) (stream)
  "Format a table of graphic elements"
  (fresh-line stream)
  (formatting-table (stream)
    (with-drawing-options (stream :line-thickness 2)
      (let ((scale 1.0))
	(repeat 8
	  (let ((scale (make-scaling-transformation (shiftf scale (+ scale 0.15))
						    (shiftf scale (+ scale 0.15)))))
	    (formatting-row (stream)
	      (formatting-cell (stream :align-x :center)
		(draw-line* stream 0 0 10 10 :transformation scale))
	      (formatting-cell (stream :align-x :center)
		(draw-rectangle* stream 0 0 10 10 :filled nil :transformation scale))
	      (formatting-cell (stream :align-x :center)
		(draw-polygon* stream '(0 10 5 0 10 10) :filled nil :transformation scale))
	      (formatting-cell (stream :align-x :center)
		(draw-circle* stream 5 5 5 :filled nil :transformation scale)))))))))

(define-benchmark (simple-graph-formatting :iterations 5) (stream)
  "Format a simple graph structure"
  (let ((map
	  '("Copley"
	    (("Auditorium" "Kenmore"
	      (("Boston University" "Boston College")
	       ("Cleveland Circle"))
	      ("Longwood Avenue" "Reservoir" "Riverside"))
	     ("Prudential" "Symphony" "Northeastern" "Museum"
	      "Brigham Circle" "Heath" "Arborway")))))
    (labels ((next-stops (stop)
	       #+genera (declare (sys:downward-function))
	       (let ((next (cadr (find-green-line-stop stop map))))
		 (if (listp next)
		     (mapcar #'car next)
		   (list next))))
	     (find-green-line-stop (stop-name search-from)
	       #+genera (declare (sys:downward-function))
	       (do ((stops search-from (cdr stops)))
		   ((null stops))
		 (if (listp (car stops))
		     (let ((s (find-green-line-stop stop-name (car stops))))
		       (when s (return s)))
		     (when (string-equal stop-name (car stops))
		       (return stops)))))
	     (draw-node (node stream)
	       #+genera (declare (sys:downward-function))
	       (format stream "~A" node)))
      #+ignore (declare (dynamic-extent #'next-stops #'find-green-line-stop #'draw-node))
      (fresh-line stream)
      (format-graph-from-root (first map) #'draw-node #'next-stops
			      :orientation :vertical
			      :stream stream))))


;;; Redisplay benchmarks

(defun make-random-generator ()
  (let ((state 107))
    #'(lambda (bound)
	(mod (setq state (ldb (byte 24 0) (+ (* state 51) 13))) bound))))

(define-benchmark (basic-redisplay) (stream)
  "Redisplay a simple table of numbers"
  (let ((table (make-array '(8 8)))
	(random (make-random-generator)))
    (dotimes (i 8)
      (dotimes (j 8)
	(setf (aref table i j) (* i j))))
    (let ((record
	    (updating-output (stream)
	      (formatting-table (stream :x-spacing '(2 :character))
		(dotimes (i 8)
		  (formatting-row (stream)
		    (dotimes (j 8)
		      (formatting-cell (stream :align-x :right)
			(updating-output (stream :unique-id (+ (* i 10) j)
						 :cache-value (aref table i j)
						 :cache-test #'=)
			  (format stream "~D" (aref table i j)))))))))))
      (repeat 5
	(setf (aref table (funcall random 8) (funcall random 8)) (funcall random 10000))
	(redisplay record stream)))))

(define-benchmark (graphic-redisplay) (stream)
  "Redisplay a table of graphic elements"
  (let ((table (make-array '(6 4))))
    (labels ((initialize-table (table)
	       (dotimes (i 6)
		 (setf (aref table i 0) (list 'line nil))
		 (setf (aref table i 1) (list 'square nil))
		 (setf (aref table i 2) (list 'triangle nil))
		 (setf (aref table i 3) (list 'circle nil))))
	     (display-table (table stream)
	       (updating-output (stream)
		 (with-drawing-options (stream :line-thickness 0)
		   (formatting-table (stream)
		     (dotimes (i 6)
		       (formatting-row (stream)
			 (dotimes (j 4)
			   (let* ((description (aref table i j))
				  (shape (first description))
				  (filled (second description)))
			     (updating-output (stream :unique-id (+ (* i 100) j)
						      :cache-value (copy-list (aref table i j))
						      :cache-test #'equal)
			       (formatting-cell (stream :align-x :center)
				 (case shape
				   (line
				     (draw-line* stream 0 0 20 20))
				   (square
				     (draw-rectangle* stream 0 0 20 20 :filled filled))
				   (triangle
				     (draw-polygon* stream '(0 20 10 0 20 20) :filled filled))
				   (circle
				     (draw-circle* stream 10 10 10 :filled filled))))))))))))))
      (initialize-table table)
      (let ((record (display-table table stream)))
	(dotimes (i 6)
	  (setf (second (aref table i 1)) t)
	  (redisplay record stream))
	(dotimes (i 6)
	  (setf (first (aref table i 0)) 'square)
	  (redisplay record stream))))))

(defparameter *fake-process-data* '(
  ("Null Process" "Arrest" 0.0)
  ("Idle Process" "Run" 17.466072)
  ("Process Scheduler" "Scheduler Wait" 1.2091997)
  ("Scavenger for Q using NFILE" "Scavenge Wait" 0.0)
  ("Metering Interface 1" "User Input" 8.7)
  ("Peek Frame 1" "Wait for exposure" 0.0)
  ("Terminal 1 Typein" "User Input" 0.0)
  ("Terminal 1 Typeout" "Stopped" 0.0)
  ("RPC Dispatch (S1 via UDP)" "Await Connections" 0.0)
  ("Blinkers for Main Screen" "Blinker" 2.2487752)
  ("Zmacs Windows" "Run" 41.28416)
  ("Keyboard" "Keyboard" 1.243278)
  ("Timer" "Timer Process" 0.036531385) ("Help 1" "Dead" 0.0)
  ("Standard Document Examiner 1" "User Input" 0.0)
  ("TCP Background" "TCP Background" 0.0)
  ("Mouse" "Mouse" 1.3642852)
  ("Update Status Line" "Update Status Line" 2.264106)
  ("Garbage Collector" "Await ephemeral full" 0.21376067)
  ("Fsmaint Frame 1" "Dead" 0.0) ("Converse Frame 1" "User Input" 0.0)
  ("Zmail background" "Zmail Background" 0.0)
  ("Main Zmail Window" "User Input" 0.0)
  ("Printer Queue Response Reader" "Printer Queue Reader Wait" 0.0)
  ("Dynamic Lisp Listener 1" "User Input" 0.0)
  ("Type definition background" "Type update" 0.0)
  ("Notification Delivery" "Notification Wait" 0.0)
  ("Screen Manager Background" "Screen Manage" 0.0)
  ("GC Daemon" "GC Daemon" 0.0)))

(define-benchmark (compound-redisplay) (stream)
  "Redisplay a large table of mixed text and graphics"
  (let ((table (copy-tree *fake-process-data*)))
    (labels ((display-table (table stream)
	       (updating-output (stream)
		 (with-text-size (stream :small)
		   (formatting-table (stream)
		     (with-text-face (stream :italic)
		       (formatting-row (stream)
			 (formatting-cell (stream :align-x :left)
			   (write-string "Process" stream))
			 (formatting-cell (stream :align-x :left)
			   (write-string "State" stream))
			 (formatting-cell (stream :align-x :left)
			   (write-string "Utilization" stream))))
		     (let ((counter 0))
		       (dolist (entry table)
			 (updating-output (stream :unique-id counter
						  :cache-value (copy-list entry)
						  :cache-test #'equal)
			   (let ((name (first entry))
				 (state (second entry))
				 (utilization (third entry)))
			     (formatting-row (stream)
			       (formatting-cell (stream :align-x :left)
				 (with-text-family (stream :sans-serif)
				   (write-string name stream)))
			       (updating-output (stream :unique-id (+ counter 1)
							:cache-value state
							:cache-test #'equalp)
				 (formatting-cell (stream :align-x :left)
				   (write-string state stream)))
			       (updating-output (stream :unique-id (+ counter 3)
							:cache-value utilization
							:cache-test #'=)
				 (formatting-cell (stream :align-x :left)
				   (draw-rectangle* stream 0 0 100 10 :filled nil)
				   (when (not (= utilization 0.0))
				     (draw-rectangle* stream 0 0 (1+ utilization) 10
						      :filled t)))))))
			 (incf counter 10))))))))
      (let ((record (display-table table stream)))
	(dolist (entry table)
	  (when (equal (second entry) "User Input")
	    (setf (second entry) "Run")
	    (redisplay record stream)
	    (setf (second entry) "User Input")
	    (incf (third entry) 5.0)
	    (redisplay record stream)))))))

;;; Menu and dialog benchmarks

(defmacro without-clim-input (&body body)
  `(let ((old (symbol-function 'read-gesture)))
     (unwind-protect
	 (progn
	   (setf (symbol-function 'read-gesture) #'ignore-clim-gesture)
	   (catch 'ignore-gesture ,@body))
       (setf (symbol-function 'read-gesture) old))))

(defun ignore-clim-gesture (&rest ignore)
  (declare (ignore ignore))
  (mp:process-sleep 4)
  (throw 'ignore-gesture nil))

(define-benchmark (simple-menu-choose :iterations 10) (stream)
  "Pop up a simple menu of colors"
  (without-clim-input
    (if #+allegro (typep (port stream) 'xm-silica::xt-port)
	#-allegro nil
	(sleep 0.1) ;; Avoid division by zero!
	(menu-choose '(("Red" :value +red+)
		       ("Green" :value +green+)
		       ("Blue" :value +blue+)
		       ("Yellow" :value +yellow+)
		       ("Orange" :value +orange+)
		       ("White" :value +white+)
		       ("Black" :value +black+))
		     :label "Please choose a color"
		     :printer #'print-menu-item	;no Macintosh menus, please
		     :cache nil
		     :associated-window stream))))

(define-benchmark (cached-menu-choose :iterations 10) (stream)
  "Pop up a cached menu of colors"
  (without-clim-input
    (if #+allegro (typep (port stream) 'xm-silica::xt-port)
	#-allegro nil
	(sleep 0.1) ;; Avoid division by zero!
	(menu-choose '(("Red" :value +red+)
		       ("Green" :value +green+)
		       ("Blue" :value +blue+)
		       ("Yellow" :value +yellow+)
		       ("Orange" :value +orange+)
		       ("White" :value +white+)
		       ("Black" :value +black+))
		     :label "Please choose a color"
		     :printer #'print-menu-item	;no Macintosh menus, please
		     :cache t
		     :unique-id 'test-color-menu
		     :associated-window stream))))

(define-benchmark (simple-dialog) (stream)
  "Present a simple dialog"
  (let ((table (make-array 20)))
    (dotimes (i 20)
      (setf (aref table i) (* i 123)))
    (without-clim-input
      (accepting-values (stream)
	(dotimes (i 20)
	  (setf (aref table i) (accept 'integer :stream stream
				       :query-identifier i
				       :default (aref table i)))
	  (terpri stream))))
    (reduce #'+ table)))

(define-benchmark (window-dialog) (stream)
  "Present a simple dialog in its own window"
  (let ((table (make-array 20)))
    (dotimes (i 20)
      (setf (aref table i) (* i 123)))
    (without-clim-input
      (accepting-values (stream :own-window t
				:label "Enter some numbers")
	(dotimes (i 20)
	  (setf (aref table i) (accept 'integer :stream stream
				       :query-identifier i
				       :default (aref table i)))
	  (terpri stream))))
    (reduce #'+ table)))

(define-benchmark (compound-dialog) (stream)
  "Present a compound dialog of text and graphics"
  (let ((square-dimension 100)
	(draw-circle t)
	(draw-square t)
	(draw-/-diagonal t)
	(draw-\\-diagonal t)
	(line-thickness 1)
	(line-thickness-units :normal))
    (without-clim-input
      (accepting-values (stream)
	(setq square-dimension
	      (accept 'number :stream stream
		      :prompt "Size of square" :default square-dimension))
	(terpri stream)
	(setq draw-circle
	      (accept 'boolean :stream stream
		      :prompt "Draw the circle" :default draw-circle))
	(terpri stream)
	(setq draw-square
	      (accept 'boolean :stream stream
		      :prompt "Draw the square" :default draw-square))
	(terpri stream)
	(setq draw-/-diagonal
	      (accept 'boolean :stream stream
		      :prompt "Draw / diagonal" :default draw-/-diagonal))
	(terpri stream)
	(setq draw-\\-diagonal
	      (accept 'boolean :stream stream
		      :prompt "Draw \\ diagonal" :default draw-\\-diagonal))
	(terpri stream)
	(setq line-thickness
	      (accept 'number :stream stream
		      :prompt "Line thickness" :default line-thickness))
	(terpri stream)
	(setq line-thickness-units
	      (accept '(member :normal :point) :stream stream
		      :prompt "Line style units" :default line-thickness-units))
	(terpri stream)
	(with-room-for-graphics (stream)
	  (let ((radius (/ square-dimension 2)))
	    (with-drawing-options (stream :line-unit line-thickness-units
					  :line-thickness line-thickness)
	      (when draw-square
		(draw-polygon* stream (list 0 0
					    0 square-dimension
					    square-dimension square-dimension
					    square-dimension 0)
			       :line-joint-shape :miter
			       :filled nil))
	      (when draw-circle
		(draw-circle* stream radius radius radius
			      :filled nil))
	      (when draw-/-diagonal
		(draw-line* stream 0 square-dimension square-dimension 0
			    :line-cap-shape :round))
	      (when draw-\\-diagonal
		(draw-line* stream 0 0 square-dimension square-dimension
			    :line-cap-shape :round)))))))))


(define-application-frame clim-tests ()
    ((history-class :initarg :history-class
		    :initform 'standard-tree-output-history
		    :reader clim-tests-history-class))
  (:command-table (clim-tests
		   :inherit-from (graphics
				  output-recording
				  formatted-output
				  redisplay
				  presentations
				  menus-and-dialogs
				  benchmarks)
		   :menu (("Graphics" :menu graphics)
			  ("Redisplay" :menu redisplay)
			  ("Benchmarks" :menu benchmarks)
			  ("Output Recording" :menu output-recording)
			  ("Formatted Output" :menu formatted-output)
			  ("Presentations" :menu presentations)
			  ("Menus and Dialogs" :menu menus-and-dialogs)
			  ("Exit Clim Tests" :command (exit-clim-tests)))))
  (:command-definer nil)
  (:panes
   (caption-pane :application
	      :scroll-bars :vertical
		 :height 50 :max-height 50
		 :output-record
		 (make-instance (clim-tests-history-class *application-frame*)))
   (display-pane :application
		 :output-record
		 (make-instance (clim-tests-history-class *application-frame*))))
  (:layouts
    (:default
      (vertically () caption-pane display-pane))))

(defmethod frame-standard-output ((frame clim-tests))
  (get-frame-pane frame 'display-pane))

#+genera
(define-genera-application clim-tests :select-key #\Circle
			   :width 600 :height 420)

(define-command (evaluate-form :command-table clim-tests)
    ((form 'form :gesture :select))
  (format t "~%The result of evaluating ~S is ~S" form (eval form)))

(define-command (exit-clim-tests :command-table clim-tests :menu t)
    ()
  (frame-exit *application-frame*))




