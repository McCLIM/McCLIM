;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: thinkadot.lisp,v 1.14 1993/07/27 01:46:11 colin Exp $

(in-package :clim-demo)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Simulates a mechanical toy finite-state-machine called "Thinkadot".

;;; in all node-state stuff, t = left, nil = right

(defstruct td-node
  (direction t)
  left-successor
  right-successor
  x
  y
  (color-phase nil)
  (entry-p nil))

(defstruct td-exit
  (ball-p nil)
  x
  y)

(define-presentation-type entry-node ())


;;;       1     2     3
;;;       |\   / \   /|
;;;       | \ /   \ / |
;;;       |  4     5  |
;;;       | / \   / \ |
;;;       |/   \ /   \|
;;;       6     7     8
;;;      /|    / \    |\
;;;     LLL   RRR


(define-application-frame thinkadot ()
    ((node1)
     (node2)
     (node3)
     (node4)
     (node5)
     (node6)
     (node7)
     (node8)
     (all-nodes)
     (lexit)
     (rexit)
     (initialized :initform nil))
  (:panes
    (display :application
	     :display-function 'draw-the-display
	     :incremental-redisplay t))
  (:layouts
    (:default display)))

(defmethod initialize-thinkadot ((frame thinkadot))
  (multiple-value-bind (w h)
      (bounding-rectangle-size (get-frame-pane frame 'display))
    (let* ((left (round w 6))
	   (right (- w left))
	   (x-mid (round (+ left right) 2))
	   (l-mid (round (+ left x-mid) 2))
	   (r-mid (round (+ right x-mid) 2))
	   (top (round h 6))
	   (bot (- h top))
	   (y-mid (round (+ top bot) 2)))
      (with-slots (node1 node2 node3 node4 node5 node6 node7 node8 
		   all-nodes lexit rexit initialized) frame
	(setf lexit (make-td-exit :x (- left 25)  :y (+ bot 10)))
	(setf rexit (make-td-exit :x (+ right 25) :y (+ bot 10)))
	(setf node8 (make-td-node :x right :y bot
				  :left-successor rexit :right-successor rexit))
	(setf node7 (make-td-node :x x-mid :y bot
				  :left-successor lexit :right-successor rexit))
	(setf node6 (make-td-node :x left  :y bot
				  :left-successor lexit :right-successor lexit))
	(setf node5 (make-td-node :x r-mid :y y-mid
				  :left-successor node7 :right-successor node8))
	(setf node4 (make-td-node :x l-mid :y y-mid
				  :left-successor node6 :right-successor node7))
	(setf node3 (make-td-node :x right :y top
				  :left-successor node5 :right-successor node8 :entry-p t))
	(setf node2 (make-td-node :x x-mid :y top
				  :left-successor node4 :right-successor node5 :entry-p t))
	(setf node1 (make-td-node :x left  :y top
				  :left-successor node6 :right-successor node4 :entry-p t))
	(setf (td-node-color-phase node2) t
	      (td-node-color-phase node4) t
	      (td-node-color-phase node5) t
	      (td-node-color-phase node7) t)
	(setf all-nodes (list node1 node2 node3 node4 node5 node6 node7 node8))
	(setf initialized t)))))

(defvar *dot-radius* 10)
(defvar *light-color* (make-gray-color 0.667))
(defvar *dark-color* +black+)

(defmethod draw-the-display ((frame thinkadot) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (with-slots (all-nodes lexit rexit initialized) frame
    (unless initialized
      (initialize-thinkadot frame))
    (let ((id 0))
      (dolist (node all-nodes)
	(incf id)
	(let ((x (td-node-x node)) (y (td-node-y node)))
	  (updating-output (stream :unique-id id
				   :cache-value (td-node-direction node)
				   :cache-test #'eql)
	    #+ignore ; for debugging when you'd like to see the internal state
	    (if (td-node-direction node)
		(draw-line* stream (+ x 10) (- y 10) (- x 10) (+ y 10))
	        (draw-line* stream (+ x 10) (+ y 10) (- x 10) (- y 10)))
	    (if (eq (td-node-direction node) (td-node-color-phase node))
		(draw-circle* stream x y *dot-radius* :ink *light-color*)
	        (draw-circle* stream x y *dot-radius* :ink *dark-color*)))
	  (when (td-node-entry-p node)
	    (with-output-as-presentation (stream node 'entry-node
					  :single-box t)
	      (let* ((x1 (- x 20)) (x2 (+ x 20)) (y1 (- y 5 *dot-radius*)) (y2 (- y1 20)))
		(draw-line* stream x1 y2 x y1)
		(draw-line* stream x2 y2 x y1)))))))
    (macrolet ((draw-exit (exit)
		 `(let ((ball-p (td-exit-ball-p ,exit)))
		    (updating-output (stream :unique-id ',exit
					     :cache-value ball-p)
		      (when ball-p
			(draw-circle* stream (td-exit-x ,exit) (td-exit-y ,exit) *dot-radius*
				      :filled nil))))))
      (draw-exit lexit)
      (draw-exit rexit))))

(defun drop-a-marble (node &optional state-change-function)
  (loop
    (when (typep node 'td-exit)
      (setf (td-exit-ball-p node) t)
      (return))
    (let ((new-node (if (td-node-direction node)
			(td-node-left-successor node)
		        (td-node-right-successor node))))
      (setf (td-node-direction node) (not (td-node-direction node)))
      (when state-change-function (funcall state-change-function node))
      (setq node new-node))))

(define-thinkadot-command (com-drop-marble) ((node 'entry-node))
  (with-slots (lexit rexit) *application-frame*
    (setf (td-exit-ball-p lexit) nil
	  (td-exit-ball-p rexit) nil))
  (drop-a-marble node))

(define-presentation-to-command-translator drop-a-marble
    (entry-node com-drop-marble thinkadot
     :documentation "Drop a Marble")
    (object)
  `(,object))

(define-thinkadot-command (com-reset-left :menu "Reset-Left") ()
  (with-slots (lexit rexit all-nodes) *application-frame*
    (setf (td-exit-ball-p lexit) nil
	  (td-exit-ball-p rexit) nil)
    (dolist (node all-nodes)
      (setf (td-node-direction node) t))))

(define-thinkadot-command (com-reset-right :menu "Reset-Right") ()
  (with-slots (lexit rexit all-nodes) *application-frame*
    (setf (td-exit-ball-p lexit) nil
	  (td-exit-ball-p rexit) nil)
    (dolist (node all-nodes)
      (setf (td-node-direction node) nil))))

(define-thinkadot-command (com-exit :menu t) ()
  (frame-exit *application-frame*))



(define-demo "Thinkadot" thinkadot
  :left 100 :top 100 :width 300 :height 340)
