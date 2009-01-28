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

(defun typep* (OBJECT SIMPLE-TYPE)
  ;; NLC21MAY92
  ;; We need the following because the Lucid compiler seems to lose sometimes  
  ;; when compiling certain forms.  E.g. when doing a compile-file on:
  ;;       (TYPEP STREAM 'CLIM::CLX-WINDOW)
  ;; We get the following error:
  ;; >>Error: MAKE-LOAD-FORM is too complicated.
  ;;         (MAKE-LOAD-FORM #<Standard-Class CLIM::CLX-WINDOW>) =>
  ;;                               #<Standard-Class CLIM::CLX-WINDOW> NIL
  (typep OBJECT SIMPLE-TYPE))


;;;
;;; Operations associated with tv windows (mostly).
;;;

(defun window-under-mouse ()
  #FEATURE-CASE
  ((:mcclim (clim:pointer-sheet (clim:port-pointer
				 (clim:port clim:*application-frame*))))
   (:clim-2 (clim-internals::find-appropriate-window *standard-input*))
   (:clim-1.0 (clim::find-appropriate-window *standard-input*))
   (:clim-0.9 (ci::find-appropriate-window *standard-input*))
   ((not :clim) (tv:window-under-mouse))))

(defun window-clear (window)
  #FEATURE-CASE
  (((not :clim) (send window :clear-history))
   (:clim-0.9 (ci::window-clear window))
   ((or :clim-1.0 :clim-2) (clim:window-clear window))))

;;; This doesn't appear to be used at all with CLIM.
#-mcclim
(defun change-size (window new-width new-height)
  #FEATURE-CASE
  (((not :clim) (send window :set-size new-width new-height))
   (:clim-0.9 (ci::change-size window new-width new-height))
   ((or :clim-1.0 :clim-2)
    (clim::window-set-inside-size window new-width new-height))))

(defun pane-frame (pane)
  #FEATURE-CASE
  (((not :clim) (scl:send pane :superior))
   ((or :clim-0.9 :clim-2) (clim:pane-frame pane))
   (:clim-1.0
    (let ((p pane) f)
      (loop (or p (return))
	(when (setq f (clim::window-stream-to-frame p))
	  (return f))
	(setq p (clim:window-parent p)))))))

(defun redisplay-frame-pane (pane &optional (force-p t))
  #FEATURE-CASE
  ((:clim-0.9 (clim:redisplay-frame-pane pane))
   ((or :clim-1.0 :clim-2)
    ;; force-p is probably wrong for redisplayable panes:
    (clim:redisplay-frame-pane (pane-frame pane) pane :force-p force-p))
   ((not :clim)
    (let ((frame (scl:send pane :superior)))
      (scl:send frame :redisplay-pane pane force-p)))))

(defun sheet-parent (sheet)
  #FEATURE-CASE
  (((not :clim) (send sheet :superior))
   (:clim-0.9 (clim:sheet-parent sheet))
   ((or :clim-1.0 (and :clim-2 (not :mcclim))) (clim:window-parent sheet))
   (:mcclim (clim:sheet-parent sheet))))

(defmethod (setf sheet-parent) (new sheet)
  #FEATURE-CASE
  (((not :clim) (send sheet :set-superior new))
   (:clim-0.9 (setf (clim:sheet-parent sheet) new))
   ((or :clim-1.0 (and :clim-2 (not :mcclim)))
    (setf (clim:window-parent sheet) new))
   (:mcclim (sheet-adopt-child sheet new))))

(defun stream-current-text-style (STREAM)
  #FEATURE-CASE
  ((:clim-0.9
    (clim:stream-current-text-style 
     (if (typep stream 'ci::encapsulating-stream-mixin)
	 (slot-value stream 'ci::stream)
       stream)))
   (:CLIM-1.0
    ;; Functions such as stream-line-height and stream-string-width
    ;; need a fully merged text style.  This makes sure all text styles
    ;; are fully merged.
    (slot-value stream 'clim::merged-text-style))
   (:CLIM-2
    (clim:medium-text-style stream))))

(defun stream-merged-text-style (STREAM)
  #FEATURE-CASE
  ((:CLIM-1.0
     (slot-value stream 'clim::merged-text-style))
   (:CLIM-2
     (clim:medium-merged-text-style stream))))

(defun parse-text-style (LIST)
  #FEATURE-CASE
  ((:CLIM
     (clim:parse-text-style LIST))
   ))

(defun stream-line-height (stream &optional TEXT-STYLE)
  #FEATURE-CASE
  (((NOT :CLIM)
    TEXT-STYLE				; wrong - but what's right? (MT)
    (or (scl:send-if-handles stream :line-height) 12))
   (:CLIM-0.9
    (if TEXT-STYLE
	(clim:stream-line-height stream TEXT-STYLE)
	(clim:stream-line-height stream (stream-current-text-style stream))))
   ((or :CLIM-1.0 :clim-2)
    (if TEXT-STYLE
	(truncate (clim:stream-line-height stream
					   #+mcclim :text-style TEXT-STYLE))
        (truncate (clim:stream-line-height stream))))))

(defun stream-character-width (stream &optional (char #\m))
  ;; "m" is the usual character (the term "ems" is often used in typesetting
  ;; to indicate units of width).
  #FEATURE-CASE
  (((not :clim) (or (scl:send-if-handles stream :character-width char) 8))
   (:clim-0.9 (clim:stream-character-width STREAM char))
   (:clim-1.0 (if (clim:extended-output-stream-p stream)
		  (clim:stream-character-width STREAM char)
		8))
   (:clim-2 (if (clim:extended-output-stream-p stream)
		  (clim:stream-character-width STREAM char)
		8))))

(defun stream-string-width (STREAM string &key (start 0) end text-style)
  #FEATURE-CASE
  ((:CLIM-1.0
    (when (eq text-style clim:*null-text-style*)
      (setq text-style (stream-current-text-style stream)))
    (clim:stream-string-width STREAM string :start START :end END
			      :text-style TEXT-STYLE))
   (:CLIM
    (clim:stream-string-width STREAM string :start START :end END
			      :text-style TEXT-STYLE))
   ))

(defmethod stream-cursor-position* (stream)
  #FEATURE-CASE
  (((not :clim) (send stream :read-cursorpos))
   (:clim-0.9 (clim:stream-cursor-position* stream))
   (:clim-1.0 (cond ((clim:extended-output-stream-p stream)
		     (clim:stream-cursor-position* stream))
		    ((typep* stream 'clim::encapsulating-stream-mixin)
		     (stream-cursor-position* (slot-value stream 'clim::stream)))
		    (t (values 0 0))))
   (:clim-2 (if (clim:extended-output-stream-p stream)
		  (multiple-value-bind (x y)
		      (clim:stream-cursor-position stream)
		    ;; Its nice to assume that cursor positions are fixnums,
		    ;; even though postscript streams meaningfully use floats.
		    (values (truncate x) (truncate y)))
		  (values 0 0)))))

(defmethod stream-set-cursor-position* (stream x y)
  #FEATURE-CASE
  (((not :clim) (send stream :set-cursorpos x y))
   ((or :clim-0.9 :clim-1.0) (clim:stream-set-cursor-position* stream x y))
   ((and :clim-2 (not :mcclim)) (clim:stream-set-cursor-position stream x y))
   (:mcclim (setf (clim:stream-cursor-position stream) (values x y)))))

(defmethod stream-increment-cursor-position* (stream x y)
  #FEATURE-CASE
  (((not :clim) (send stream :increment-cursorpos x y))
   (:clim-0.9 (clim:stream-increment-cursor-position* stream x y))
   (:clim-1.0 (if (typep* stream 'clim::output-protocol-mixin)
		  (clim:stream-increment-cursor-position* stream x y)))
   (:clim-2 (clim:stream-increment-cursor-position stream x y))))

(defmethod stream-viewport (stream)
  ;;(declare (values left top right bottom))
  #FEATURE-CASE
  (((not :clim) (send stream :visible-cursorpos-limits))
   (:clim-0.9
    (clim:bounding-rectangle*
     (clim:sheet-region (clim:pane-viewport stream))))
   (:clim-1.0
    (if (typep stream 'clim::postscript-stream)
	(values 0 0 700 700)		; anything will do...
      (multiple-value-bind (left top right bottom)
	  (clim:rectangle-edges* (clim:window-viewport stream))
	(multiple-value-bind (xoff yoff)
	    (if (typep* stream 'clim::avv-stream)
		(values 0 0)
	      (clim:window-margins stream))
	  (values left top (- right xoff) (- bottom yoff))))))
   (:clim-2
    (cond ((not (clim:extended-output-stream-p stream)))
	  ((and (type-specifier-p #-mcclim 'postscript-clim::postscript-stream
				  #+mcclim 'clim-postscript::postscript-stream)
		(typep stream
		       #-mcclim 'postscript-clim::postscript-stream
		       #+mcclim 'clim-postscript::postscript-stream))
	   ;; width  = inches x 72
	   ;; height = inches x 72
	   (values 0 0 #.(* 72 7) #.(* 72 10)))
	  (t
	   (let ((v (and #-mcclim (not (typep stream
					      'clim-silica:pixmap-stream))
			 #+mcclim (not (typep (clim:medium-sheet
					       (clim:sheet-medium stream))
					      'climi::pixmap))
			 (clim:window-viewport stream))))
	     (if v (clim:rectangle-edges* v)
	       (values 0 0
		       (clim:bounding-rectangle-width stream)
		       (clim:bounding-rectangle-height stream)))))))))

(defmethod stream-viewport-size (stream)
  ;;(declare (values width height))
  (multiple-value-bind (left top right bottom) (stream-viewport stream)
      (values (- right left) (- bottom top)))) 

(defmacro sheet-inside-size (stream)
  #FEATURE-CASE
  ((:CLIM
     `(stream-viewport-size ,stream))
   ((NOT :CLIM)
    `(values (send ,stream :inside-size))))
  )

(defun stream-height (stream)
  "Height of the viewport."
  (multiple-value-bind (ignore height)
      (stream-viewport-size stream)
    (declare (ignore ignore))
    height))

(defmacro sheet-inside-width (stream) 
  #+clim `(values (stream-viewport-size ,stream))
  #-clim `(values (send ,stream :inside-size)))

(defmacro sheet-inside-height (stream) 
  #+clim `(stream-height ,stream)
  #-clim `(multiple-value-bind (w h) (send ,stream :inside-size)
	    (declare (ignore w))
	    h))

(defmacro sheet-left-margin-size (stream)
  stream
  #+clim 0					; KRA: 4/11/90: CLIM Doesn't use margins.
  #-clim `(tv:sheet-left-margin-size ,stream))

(defmacro sheet-top-margin-size (stream)
  stream
  #+clim 0
  #-clim `(tv:sheet-top-margin-size ,stream))

(defun beep () #+genera (scl:beep) #-genera (clim:beep))

;;;
;;; Mouse stuff
;;;

(defun interactive-stream-p (stream)
  #-clim (send stream :interactive)
  #+clim (clim:extended-input-stream-p stream))

(defmethod stream-set-pointer-position* (stream x y)
  "Set position of mouse, in stream coordinates."
  #FEATURE-CASE
  (((not :clim) (send stream :set-mouse-position x y))
   ((or :clim-0.9 :clim-1.0)
    (clim:stream-set-pointer-position* stream x y))
   ((and :clim-2 (not :mcclim))
    (clim:stream-set-pointer-position stream x y))
   (:mcclim
    (setf (clim:stream-pointer-position stream) (values x y)))))

(defmethod stream-pointer-position* (stream)
  "Get position of mouse, in stream coordinates."
  #FEATURE-CASE
  (((not :clim) (send stream :mouse-position))
   ((or :clim-0.9 :clim-1.0)
    (clim:stream-pointer-position* stream))
   (:clim-2
    (multiple-value-bind (x y)
	(clim:stream-pointer-position stream)
      (values (truncate x) (truncate y))))))

(defun pointer-input-rectangle* (&key (stream *standard-input*) left top right bottom)
  ;;(declare (values left top right bottom))
  #+clim (declare (ignore stream left top right bottom))
  #FEATURE-CASE
  (((not :clim)
    (multiple-value-bind (xoff yoff) (send stream :visible-cursorpos-limits)
      (decf left xoff)
      (decf right xoff)
      (decf top yoff)
      (decf bottom yoff)
      (let ((box (dw::make-box left top right bottom)))
	(setq box (tv:mouse-reshape-rectangle :initial-box box))
	(multiple-value-bind (a b c d) (dw::box-edges box)
	  (values (+ a xoff) (+ b yoff) (+ c xoff) (+ d yoff))))))))

;;;
;;; Frame stuff
;;;

(defvar *default-server-path*
    #FEATURE-CASE
  (((and :clim-1.0 :genera) '(:sheet :screen (tv:console-default-superior)))
   ((and :clim-1.0 :mcl)    '(:mcl))  
   ((and :clim-1.0 :xlib (not :genera)) '(:clx))
   (:clim-0.9 '(:clx))
   (:clim-2 '(:motif))
   ((not :clim) nil)))

(defvar *sheet-roots* nil)
(defvar *deactivated-frames* nil)
(defvar *activated-frames* nil)

(defmethod frame-top-level-process ((frame t))
  "Access the process associated with this frame."
  #FEATURE-CASE
  (((or :clim-1.0 (and :clim-2 (not :mcclim)))
    (second (car (member frame *activated-frames* :key #'car))))
   (:clim-0.9 (slot-value frame 'ws::top-level-process))
   ((not :clim) (scl:send frame :process))
   (:mcclim (climi::frame-process frame))))

(defun frame-manager (frame)
  #FEATURE-CASE
  ((:clim-1.0 (clim:window-parent (clim:frame-top-level-window frame)))
   (:clim-0.9 (clim:frame-manager frame))
   (:clim-2 (clim:frame-manager frame))
   ((not :clim) (scl:send frame :superior))))

(defun find-frame-manager (&key (if-exists :reuse))
  (declare (ignorable if-exists))
  #FEATURE-CASE
  ((:clim-2 (clim:find-frame-manager))
   (:clim-1.0
    (or
     (and
      *sheet-roots*
      (ecase if-exists
	(:reuse (car (last *sheet-roots*)))
	(:create nil)))
     (let ((r (apply #'clim:open-root-window
		     (mapcar #'eval *default-server-path*))))
       (push r *sheet-roots*)
       r)))
   (:clim-0.9 (ws::find-frame-manager))
   ((not :clim)
    (let ((w *terminal-io*))
      (or (and w (scl:send w :screen))
	  (tv:console-default-superior))))))

(defun get-reusable-frame (manager type)
  (declare (ignorable manager))
  #FEATURE-CASE
  (((not :clim)
    (let ((choices *deactivated-frames*))
      (dolist (item choices)
	(when (and (eq (frame-manager item) manager)
		   (typep (scl:send item :program) type))
	  (setq *deactivated-frames* (delete item *deactivated-frames*))
	  (return item)))))
   (:clim-2
    (let ((choices (clim:frame-manager-frames manager)))
      (dolist (item choices)
	(when (and (typep item type) (eq (clim:frame-state item) :disabled))
	  (return item)))))
   (:clim-1.0
    #-MCL
    (let ((choices *deactivated-frames*))
      (dolist (item choices)
	(when (typep item type)
	  (setq *deactivated-frames* (delete item *deactivated-frames*))
	  (return item))))
    #+MCL			; reuse doesn't work yet
    nil)
   (:clim-0.9
    (ws::get-reusable-frame manager type))))

(defun deactivate-frame (frame)
  (setq *activated-frames* (remove frame *activated-frames* :key #'car))
  (push frame *deactivated-frames*))

(defmethod reset-frame (frame &key title)
  "Prepare a frame for reuse."
  #FEATURE-CASE
  (((not :clim) frame)
   (:clim-2 (setf (clim:frame-pretty-name frame) title)
	      (clim:reset-frame frame))
   (:clim-1.0
    (progn
      #-MCL
      (setf (clim:frame-pretty-name frame) title)
      ;; might have an Abort left in it:
      (clim:stream-clear-input (clim:frame-top-level-window frame))
      frame))
   (:clim-0.9
    ;; BUG: title is wrong on reused frames(?).
    (clim:reset-frame frame :title title))))

;;; You pay a price for this, so set it to nil if resources are scarce.
(defvar *enable-backing-store* :when-mapped
  "One of :always, :when-mapped, :not-useful, or nil")

(defmethod start-frame (frame &key
			      (wait-until-done t)
			      master
			      (backing-store *enable-backing-store*))
  #FEATURE-CASE
  ((:clim-0.9
    (progn
      ;; If these are X windows, enable backing-store.
      #+xlib
      (let* ((pane (clim:frame-pane frame))
	     (port (clim:port pane)))
	(when (typep* port 'on-x::x-port)
	  (setf (xlib:window-backing-store (w::sheet-mirror! pane))
	    backing-store)))    
      (cond (master
	     ;; Change the event queue and reinitialize.
	     ;; How should this be undone if this frame is recycled?
	     (setf (slot-value frame 'ws::queue) (ws::frame-queue master))
	     (ci::initialize-stream-queues frame)
	     (clim:enable-frame frame)
	     (clim:panes-need-redisplay frame)
	     (clim:redisplay-frame-panes frame))
	    (t (clim:start-frame frame wait-until-done)))))
   (:clim-1.0
    (labels ((set-backing-store (window value)
	       #+xlib
	       (setf (xlib:window-backing-store (slot-value window 'clim::window))
		 value)
	       (dolist (child (slot-value window 'clim::children))
		 (set-backing-store child value))))
      (cond (master
	     (let ((b (clim:stream-input-buffer
		       (clim:frame-top-level-window master)))
		   (top-level-window (clim:frame-top-level-window frame)))
	       (labels ((set-input-buffer (window buffer)
			  (setf (clim:stream-input-buffer window) buffer)
			  (dolist (w (clim:window-children window))
			    (set-input-buffer w buffer))))
		 (set-input-buffer top-level-window b)
		 (set-backing-store (clim:frame-top-level-window frame)
				    backing-store)
		 (clim:window-expose top-level-window)
		 (clim:redisplay-frame-panes frame :force-p t)
		 ;; return the window just created
		 (values top-level-window))))
	    ((not wait-until-done)
	     (process-run-function
	      "Frame Top Level"
	      'start-frame frame
	      :wait-until-done t
	      :master nil
	      :backing-store backing-store)
	     frame)
	    (T
	     (push (list frame (current-process)) *activated-frames*)
	     (setq *deactivated-frames* (delete frame *deactivated-frames*))
	     #+xlib
	     (let ((xwin (slot-value (clim:frame-top-level-window frame) 'clim::window)))
	       (xlib:set-wm-class xwin "clim" "clim"))
	     (set-backing-store (clim:frame-top-level-window frame)
				backing-store)
	     (unwind-protect
		 (let ((clim::*frame-layout-changing-p* t)) ; forces redisplay
		   (clim:run-frame-top-level frame))
	       (deactivate-frame frame))))))
   (:clim-2
    (cond (master
	   (let ((b (clim:stream-input-buffer
		     (clim:frame-top-level-sheet master)))
		 (top-level-window (clim:frame-top-level-sheet frame)))
	     (labels ((set-input-buffer (window buffer)
			(setf (clim:stream-input-buffer window) buffer)
			(dolist (w (#-mcclim clim:window-children
				    #+mcclim clim:sheet-children
				    window))
			  (set-input-buffer w buffer))))
	       (set-input-buffer top-level-window b)
	       #-mcclim
	       (clim:window-expose top-level-window)
	       #+mcclim
	       (clim:enable-frame frame)
	       (clim:redisplay-frame-panes frame :force-p t)
	       ;; return the window just created
	       (values top-level-window))))
	  ((not wait-until-done)
	   (process-run-function
	    "Frame Top Level"
	    'start-frame frame
	    :wait-until-done t
	    :master nil
	    :backing-store backing-store)
	   frame)
	  (T
	   (push (list frame (current-process)) *activated-frames*)
	   (unwind-protect
	       (progn
		 (clim:run-frame-top-level frame))
	     (deactivate-frame frame)))))
   ((not :clim)
    (cond (master (error "MASTER operation not supported."))
	  ((not wait-until-done)
	   (scl:send frame :mouse-select))
	  (T
	   (setf (scl:symbol-value-in-instance frame 'tv:process) nil)
	   (let* ((superior (scl:send frame :superior))
		  (shadow (if (typep* superior 'tv:basic-screen)
			      (first (scl:send superior :exposed-inferiors))))
		  (dw:*program-frame* frame)
		  (dw:*program* (scl:send frame :program)))
	     (tv:with-window-shadowed-for-selection (shadow frame :reselect t)
	       (tv:window-call (frame :deactivate)
			       (unwind-protect
				   (dw::run-program-top-level dw:*program*)
				 (deactivate-frame frame))))))))))

(defun make-application-frame (type &key parent title 
					 (left 10) (top 10)
					 (width 500) (height 500))
  #FEATURE-CASE
  (((not :clim)
    (tv:make-window 'dw::program-frame  ; 'dw::program-frame-resource
		    :superior parent
		    :program type
		    :margin-components
		    '((dw:margin-drop-shadow-borders) (dw:margin-borders))
		    :program-state-variables `((dw::pretty-name ,title))))
   (:clim-0.9
    (let ((frame (clim:make-frame type :title title)))
      (ws::adopt-frame parent frame)
      (setf (ws::frame-prop frame :reusable) t)
      frame))
   (:clim-1.0
    ;; Also, note you can (currently) get into trouble in Lucid if you use a
    ;; single root window for multiple frames.  X events for the root can be
    ;; snagged by any frame's process without regard which frame should "own"
    ;; the event.  The workaround is to open a new root window for each frame.
    ;;--David Gadbois
    (clim:make-application-frame
     type
     ;; 9-15-93 When `launch-frame' calls us with a parent we need to
     ;; use it, otherwise mouse-sensitivity does not work. (Westy)
     :parent (or parent (find-frame-manager :if-exists :create))
     :pretty-name title
     :left left :top top :right (+ left width) :bottom (+ top height)))
   (:clim-2
    ;; what parent does this get?
    (let ((frame (clim:make-application-frame
		  type
		  :pretty-name title
		  :left left :top top
		  :width width :height height
		  ;; This is a kludge to solve spr8924 in clim 2.0.beta2:
		  #+allegro
		  :calling-frame
		  #+allegro
		  clim:*application-frame*
		  #+mcclim :frame-manager
		  #+mcclim parent)))
      frame))))

(defmethod size-frame (frame width height)
  #FEATURE-CASE
  (((not :clim) (scl:send frame :set-edges 0 0 width height))
   (:clim-0.9 (ws::size-frame frame width height))
   (:clim-2
    (clim:layout-frame frame width height))      
   (:clim-1.0
    (let ((window (clim:frame-top-level-window frame)))
      (multiple-value-bind (xmin ymin xmax ymax)
	  (clim:bounding-rectangle* (sheet-parent window))
	(declare (ignore xmin ymin))
	(multiple-value-bind (left top) (clim:bounding-rectangle* window)
	  (setf (clim:bounding-rectangle-max-x window) (min (+ left width) xmax))
	  (setf (clim:bounding-rectangle-max-y window) (min (+ top height) ymax))
	  (clim::layout-frame-panes frame window)))))))

(defmethod move-frame (frame left bottom)
  #FEATURE-CASE
  (((not :clim) (dw::position-window-near-carefully frame `(:point ,left ,bottom)))
   (:clim-0.9 (ws::move-frame frame left bottom))
   (:clim-1.0 (clim::position-window-near-carefully
	       (clim:frame-top-level-window frame)
	       left bottom))
   ((and :clim-2 (not :mcclim))
    (clim:position-sheet-carefully (clim:frame-top-level-sheet frame)
				   left bottom))
   ;; uhh...
   (:mcclim nil)))

(defmethod get-frame-pane (frame pane-name)
  #FEATURE-CASE
  (((not :clim) (scl:send frame :get-pane pane-name))
   (:clim-1.0 (clim:get-frame-pane frame pane-name))
   (:clim-2 (clim:get-frame-pane frame pane-name))))

(defmethod frame-current-layout (frame)
  #FEATURE-CASE
  (((or :clim-1.0 :clim-2) (clim:frame-current-layout frame))
   ((not :clim) (scl:send frame :configuration))))

(defmethod set-frame-layout (frame new-layout)
  #FEATURE-CASE
  ((:clim-1.0
    ;; In clim 1.0 at least, this does a THROW out of the command loop
    ;; to do certain window management functions, such as rebinding I/O
    ;; streams that correspond to the new layout.  So do this last in
    ;; a sequence of operations.
    (unless (eq new-layout (frame-current-layout frame))
      (clim:set-frame-layout frame new-layout)))
   (:clim-2
    (unless (eq new-layout (frame-current-layout frame))
      (setf (clim:frame-current-layout frame) new-layout)))
   ((not :clim)
    (unless (eq new-layout (frame-current-layout frame))
      (if (or (not (boundp 'dw::*program-frame*))
	      (not (eql frame dw::*program-frame*)))
	  (scl:send frame :set-configuration new-layout)
	(scl:send frame :synchronous-set-configuration new-layout))))))

(defmethod window-set-viewport-position* (stream left top)
  #FEATURE-CASE
  ((:clim-0.9 (clim:set-scroll-position stream left top))
   (:clim-1.0
     (clim:window-set-viewport-position* stream left top)
     (clim::redisplay-decorations stream))
   ((and :clim-2 (not :mcclim))
    (clim:window-set-viewport-position stream left top))
   (:mcclim
    (setf (clim:window-viewport-position stream) (values left top)))
   ((not :clim) (scl:send-if-handles stream :set-viewport-position left top))))

(defmethod window-history-limits (stream)
  ;;(declare (values left top right bottom))
  #+(or (not clim) clim-0.9) (declare (ignore stream))
  #FEATURE-CASE
  ((:clim-1.0
    (let ((history (clim:output-recording-stream-output-record stream)))
      (clim:bounding-rectangle* history)))
   (:clim-2
    (let ((history (clim:stream-output-history stream)))
      (clim:bounding-rectangle* history)))))

(defmethod select-frame (frame)
  #FEATURE-CASE
  (((not :clim) (scl:send frame :mouse-select))
   (:clim-1.0
    (let ((window (clim:frame-top-level-window frame)))
      #+xlib
      (xlib:map-window (slot-value window 'clim::window)) ; deiconify
      (clim:window-stack-on-top window)
      (force-output window)
      frame))
   (:clim-2
    (#+mcclim note-frame-deiconified
     #-mcclim clim-internals::note-frame-deiconified
     (clim:frame-manager frame) frame)
    (clim:raise-sheet (clim:frame-top-level-sheet frame))
    frame)))

(defun suggest-frame-size (frame-manager width height)
  #FEATURE-CASE
  (((not :clim)
    (multiple-value-bind (w h) (scl:send frame-manager :size)
      (setq width (min width w)
	    height (min height h))))
   (:clim-0.9
    (let ((graft (clim-shared:graft frame-manager)))
      (when graft
	(setq width (min width (clim-shared::graft-width-pixel graft))
	      height (min height (clim-shared::graft-height-pixel graft))))))
   (:clim-1.0
    (multiple-value-bind (w h) (clim:window-inside-size frame-manager)
      (setq width (min width w)
	    height (min height h))))
   (:clim-2
    (let ((graft #-mcclim (clim:graft frame-manager)
		 #+mcclim (clim:graft (port frame-manager))))
      (when graft
	#-mcclim
	(setq width (min width (silica::graft-pixel-width graft))
	      height (min height (silica::graft-pixel-height graft)))
	#+mcclim
	(setq width (min width (clim:graft-width graft :units :device))
	      height (min height (clim:graft-height graft :units :device))))))
   )
  (values width height))

(defun launch-frame 
    (type
     &key
     (backing-store :when-mapped)	; specific to X windows
     create				; NIL => try first to reuse an old instance
     master
     (title "Window Frame")
     (left 0) (bottom 0)
     (width 600) (height 400)
     (wait-until-done nil)		; T => spawn its own process
     (initializer nil)			; function of 1 arg
     &allow-other-keys)
  "The preferred way to make and expose an application frame."
  ;; MASTER is either NIL or another frame.
  ;; If it is a frame, the second frame acts as an extension of the first.
  (let* ((manager (if master (frame-manager master)
		    (find-frame-manager)))
	 (frame (if (not create) (get-reusable-frame manager type))))
    (when frame (reset-frame frame :title title))
    (if frame
	(size-frame frame width height)
      (setq frame (make-application-frame type
					  ;;:left (max 0 left) :top (max 0 (- height bottom))
					  :parent manager
					  :width width :height height
					  :title title)))
    (move-frame frame (max 0 left) (max 0 bottom))
    (multiple-value-bind (w h) (suggest-frame-size manager width height)
      (when (or (not (eql w width)) (not (eql h height)))
	 (size-frame frame w h)))
    (when initializer
      (let* ((application #+clim frame
			  #-clim (scl:send frame :program))
	     #-clim
	     (dw:*program* application)
	     #-clim
	     (dw:*program-frame* frame)
	     #+(and clim (not clim-0.9))
	     (clim:*application-frame* frame))
	(funcall initializer application)))
    (start-frame frame
		 :wait-until-done wait-until-done
		 :master master
		 :backing-store backing-store)))

(defmethod frame-exit (FRAME)
  #FEATURE-CASE
  ((:CLIM-0.9
    (clim::stop-frame FRAME))
   (:CLIM-2
    (clim:frame-exit FRAME))
   (:CLIM-1.0
    ;; buggo makes the frame stay up! 
    (let ((top (clim::frame-top-level-window frame)))
      (when top
	(setf (clim::window-visibility top) nil)
	(clim::force-output top)))
    (clim::frame-exit FRAME))
   ))

(defmacro for-each-frame ((symbol) &body body)
  "Iteratively bind SYMBOL to all enabled frames."
  #FEATURE-CASE
  (((not :clim)
    `(dolist (screen tv:all-the-screens)
       (dolist (,symbol (scl:send screen :inferiors))
	 (when (and (typep ,symbol 'dw:program-frame)
		    (scl:send ,symbol :inferiors))
	   ,@body))))
   (:clim-0.9
    `(when ws::*ports*
       ;; If there are no ports open, then there are no enabled frames.
       (dolist (child (graft-children (ws::frame-manager-sheet
				       (ws::find-frame-manager))))
	 (when (ci::sheet-enabled-p child)
	   (let ((,symbol (ws::sheet-frame child)))
	     (when (eq (ci::frame-state ,symbol) :enabled)
	       ,@body))))))
   (:clim-1.0
    `(dolist (root *sheet-roots*)
       (dolist (child (slot-value root 'clim::children))
	 (let ((,symbol (clim::window-stream-to-frame child)))
	   (when (and ,symbol (not (member ,symbol *deactivated-frames*))) ; kludge
	     ,@body)))))
   (:clim-2
    `(clim:map-over-ports
      #'(lambda (port)
	  (unless (eq (clim:port-type port) :postscript)
	    (dolist (,symbol (clim:frame-manager-frames
			      (clim:find-frame-manager :port port)))
	      (when (member (clim:frame-state ,symbol) '(:shrunk :enabled))
		,@body))))))))

(defun find-program-window (name &key
				 (create-p nil)
				 (wait-until-done nil)
				 (width 500)
				 (height 500))
  #FEATURE-CASE
  (((not :clim)
    (let ((screen (find-frame-manager)))
      (dw:find-program-window name :create-p create-p :console (scl:send screen :console))))
   (:clim
    (progn
      (for-each-frame (f)
		      (when (typep f name)
			(return-from find-program-window f)))
      (when create-p
	(launch-frame name
		      :title (string name)
		      :wait-until-done wait-until-done
		      :width width
		      :height height))))))


