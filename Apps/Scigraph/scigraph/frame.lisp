;;; -*- Syntax: Common-lisp; Package: GRAPH -*-
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

(in-package :graph)

#|

This file implements the function VIEW-GRAPHS as a way of getting a generic
program frame for viewing a list of graphs.  The program can be either 
temporary or persistent depending upon the keyword :wait-until-done.
The program consists of a single, main display pane where graphs get drawn.

In CLIM, the scigraph frame can also be made a "slave" of a second "master" frame
by providing the :master keyword.  The slave is like an extension of the master,
where presentations on the slave are mouse-sensitive, but the master's command
loop is responsible for reading and executing all commands.  For this to work,
the master must inherit the graph command table.

|#

;;;
;;; Make a pane that runs the redisplayer when asked to repaint, so that
;;; frame resizing also resizes the graphs.
;;;

#|
How to get a pane to redraw its contents when it's been reshaped:

The generic function to specialize on, as I told you, is
WS::SHEET-REGION-CHANGED (which should have been exported from CLIM,
but wasn't).  However, that generic function is invoked on a
clim-stream-pane every time something is added to the output history.
It's only invoked on its viewport when the actual space taken up really 
changes, but there's no easy way for you to specialize on the viewport
class.  So you should do the following (in the ws package, 'natch):
|#

#+clim-0.9
(progn
  ;; JPM: The MAP system also provides the following two methods.
  ;; Whichever gets loaded last wins.  Perhaps the common code should
  ;; be placed in a common file.

  (defmethod clim-shared::sheet-region-changed :after ((self ws::viewport-pane))
    (with-slots (ws::extent-pane) self
      (ws::viewport-extent-changed ws::extent-pane)))

  (defmethod ws::viewport-extent-changed ((pane ci::pane)) nil)

  (defclass reshape-display-function-mixin () ()
	    (:Documentation "Run the redisplay function when window gets reshaped."))

  (defmethod ws::viewport-extent-changed
      ((pane reshape-display-function-mixin))
    ;; Test to see if the frame is enabled to suppress an unnecessary
    ;; redisplay for the case where the frame is initially getting enabled.
    (when (eq (frame-state (pane-frame pane)) :enabled)
      ;; JPM.  This is done asynchronously, and could cause momentary
      ;; inconsistencies when done in a master/slave context, so inhibit
      ;; scheduling.
      (clim-utils:without-interrupts
	(pane-needs-redisplay pane)
	(redisplay-frame-pane pane))))

  (defclass viewer-display-pane (reshape-display-function-mixin
				 clim-stream-pane)
    ())

  ;; Inherit the graph command table.
  (make-command-table graph-viewer :inherit-from (graph))
  )

;;;
;;; Now define the scigraph viewer frame.
;;;

#+(and clim (not clim-0.9))
(define-application-frame graph-viewer ()
  ((display-pane :accessor display-pane)
   (graphs :initform nil :accessor frame-graphs)
   (display-settings :initform nil :accessor display-settings))
  #+clim-1.0
  (:panes
    ((display :application
	      :display-function 'redisplay-graphs
	      :default-text-style (parse-text-style '(:fix :roman :normal))
	      :end-of-line-action :allow
	      :end-of-page-action :allow
	      :display-after-commands nil
	      :initial-cursor-visibility nil
	      :scroll-bars :both
	      :stream-background +black+
	      :stream-foreground +white+)
     (documentation :pointer-documentation
		    :stream-background +black+
		    :stream-foreground +white+)))
  #+(and clim-2 (not :mcclim))
  (:panes
   (display 
    (scrolling
     () 
     (make-pane 'application-pane
		:display-function 'redisplay-graphs
		:display-time t
		:text-style 
		(parse-text-style '(:fix :roman :normal))
		:initial-cursor-visibility nil
		;; There is a clim bug whose workaround is to use a non-default
		;; output history.  The bug is displaying overlapping presentations
		;; in combination with a coordinate sorted set output history.  In
		;; our case, graph annotations sometimes get put into the history wrong.
		;; At that point, they lose their mouse sensitivity.
		:OUTPUT-RECORD
		(MAKE-INSTANCE 'CLIM:R-TREE-OUTPUT-HISTORY)
		))))
  ;; In McCLIM, the name of the pane goes with the top level pane in the
  ;; definition, which seems to follow the spec. But we want the name
  ;; to go with the application pane...
  #+mcclim
  (:panes
   (display :application
	    :display-function 'redisplay-graphs
	    :display-time t
	    :text-style 
	    (parse-text-style '(:fix :roman :normal))
	    :initial-cursor-visibility nil
	    :scroll-bars t))
  #+clim-2
  (:pointer-documentation t)
  #+clim-2
  (:layouts
   (default (vertically () display)))
  #+(or clim-1.0 clim-2)
  (:command-table (graph-viewer :inherit-from (:graph)))
  (:top-level (scigraph-top-level))
  )

#+clim-0.9
(clim:define-application-frame graph-viewer ()
  ((display-pane :accessor display-pane)
   (command-table :initform 'graph-viewer :accessor frame-command-table)
   (graphs :initform nil :accessor frame-graphs)
   (display-settings :initform nil :accessor display-settings))
  (:pane
   (clim:with-frame-slots (display-pane)
     (ws::viewing
       :subtransformationp t
       ;; KRA: There is something magic in this line.
       ;; If you remove it, it breaks.
       :hs 100 :vs 100 :hs+ clim:*fill* :hs- clim:*fill* :vs+ clim:*fill* :vs- clim:*fill*
       (setq display-pane (clim:make-pane 'viewer-display-pane
					  :initial-cursor-visibility nil
					  :display-function '(redisplay-graphs)
					  :display-time nil)))))
  (:top-level (scigraph-top-level)))

#-clim
(dw:define-program-framework graph-viewer
  :size-from-pane display
  :selectable nil
  :top-level (scigraph-top-level)
  :command-table (:inherit-from '("graph" "colon full command"
				  "standard arguments" "input editor compatibility")
		  :kbd-accelerator-p t)
  :state-variables ((frame-graphs nil)
		    (display-settings nil))
  :other-defflavor-options ((:conc-name nil))
  :panes
  ((display :display
	    :flavor dw:dynamic-window-pane
	    :redisplay-function 'redisplay-graphs
	    :incremental-redisplay :own-redisplayer
	    :redisplay-after-commands nil
	    :more-p nil
	    :blinker-p nil
	    :margin-components nil))) 

(defun scigraph-top-level (self)
  (let* ((*package* (find-package :graph)))
    (loop
      (with-simple-restart (scigraph-top-level "Abort to SCIGRAPH Top Level")
	#+clim-2
	(redisplay-frame-pane (get-frame-pane self 'display))
	#FEATURE-CASE
	((:clim-0.9 (clim-top-level self))
	 ((or :clim-1.0 :clim-2) (default-frame-top-level self))
	 ((not :clim)
	  (dw:default-command-top-level
	      self
	      :echo-stream 'ignore
	      :dispatch-mode :command-preferred)))))))

#+clim-0.9
(defmethod enable-frame :after ((frame graph-viewer))
   ;; Process all pending events for us, to get those
   ;; gratuitous repaint events from window managers handled early.
   (let ((g (frame-graphs frame)))
     (unwind-protect
	  (with-slots (ws::queue) frame
	    (setf (frame-graphs frame) nil)
	    (do ((event (ws::peek-event ws::queue) (ws::peek-event ws::queue)))
		((null event))
	      (setq event (ws::get-next-event ws::queue))
	      (when (typep event 'ws::repaint-event)
		;; This is always a repaint event when things are working correctly.
		;; Do a typecheck anyway, to guard against minor problems.
		(ws::default-execute-event event))))
       ;; Pretend there aren't any graphs while processing repaint events.
       (setf (frame-graphs frame) g))))

(defun redisplay-graphs (self stream)
  ;; Vertically stack the graphs to fill the pane.
  (apply #'fill-window-with-graphs
	 (frame-graphs self)
	 :stream stream
	 (display-settings self)))

#-clim-0.9
(defun view-graphs
    (graphs
     &key
     (columns 1)
     autoscale
     (reverse-video t)
     (backing-store :when-mapped)
     create
     master
     (type 'graph-viewer)
     (title "View Graphs")
     (left 0) (bottom 0)
     (width 600) (height 400)
     (wait-until-done nil)
     &allow-other-keys)
  "Display a list of graphs in an interactive program frame."
  (launch-frame type
		:backing-store backing-store
		:master master
		:create create
		:title title
		:width width
		:height height
		:left left
		:bottom bottom
		:wait-until-done wait-until-done
		:initializer
		#'(lambda (application)
		    (setf (frame-graphs application) graphs)
		    (setf (display-settings application)
			  `(:columns ,columns
			    :reverse-video ,reverse-video
			    :autoscale ,autoscale))
		    ;; Now we need to make sure the panes get sized BEFORE
		    ;; the pane displayer gets run.  By default, this happens
		    ;; in the opposite order.  Order is important because
		    ;; scigraph asks the pane how big it is before drawing
		    ;; the graph.
		    #+clim-2
		    (resize-sheet (frame-top-level-sheet application) width height)
		    )))

#+clim-0.9
(defun view-graphs
    (graphs
     &key
     (columns 1)
     autoscale
     (reverse-video t)
     (backing-store :when-mapped)
     create
     master
     (type 'graph-viewer)
     (command-table 'graph-viewer)
     (title "View Graphs")
     (left 0) (bottom 0)
     (width 600) (height 400)
     (wait-until-done nil)
     &allow-other-keys)
  "Display a list of graphs in an interactive program frame."
  ;; This is essentially launch-frame, with a few twists.
  ;; MASTER is either NIL or another frame.  If provided, the two frames
  ;; share a frame manager and an event queue.
  ;;
  (let* ((manager (if master (clim:frame-manager master) (ws::find-frame-manager)))
	 (frame (if (not create) (ws::get-reusable-frame manager type))))
    (when frame
      ;; BUG: title is wrong on reused frames(?).
      (clim:reset-frame frame :title title))
    (unless frame
      (setq frame (clim:make-frame type :title title))
      (ws::adopt-frame manager frame))
    (setf (ws::frame-prop frame :reusable) t)
    (setf (frame-graphs frame) nil)
    (when master
      ;; Change the event queue and reinitialize.
      ;; How should this be undone if this frame is recycled?
      (setf (slot-value frame 'ws::queue) (ws::frame-queue master))
      (ci::initialize-stream-queues frame))
    (ws::move-frame frame left bottom)
    (ws::size-frame frame width height)
    (window-clear (display-pane frame))
    
    ;; If these are X windows, enable backing-store.
    #+xlib
    (let* ((pane (clim:frame-pane frame))
	   (port (clim:port pane)))
      (when (typep port 'on-x::x-port)
	(setf (xlib:window-backing-store (w::sheet-mirror! pane))
	      backing-store)))
    
    (if (graph-p graphs) (setq graphs (list graphs)))
    (setf (frame-graphs frame) graphs)
    (setf (frame-command-table frame) command-table)
    (setf (display-settings frame)
	  `(:columns ,columns
	    :reverse-video ,reverse-video
	    :autoscale ,autoscale))
    (if (not master)
	(clim:start-frame frame wait-until-done)
	(progn
	  (clim:enable-frame frame)
	  (clim:panes-need-redisplay frame)
	  (clim:redisplay-frame-panes frame)))
    frame))


