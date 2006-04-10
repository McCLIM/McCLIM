;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2005 by
;;;           Andy Hefner (ahefner@gmail.com)

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

(in-package :clim-demo)

;;; Demo of draggable graph nodes

(define-application-frame draggable-graph-demo () ()
  (:pane (make-pane 'application-pane
		    :width :compute
		    :height :compute
		    :display-function 'generate-graph
		    :display-time t)))

(defun generate-graph (frame pane)
  (format-graph-from-roots
   (list (find-class 'number))
   (lambda (object stream)
     (present (clim-mop:class-name object)
	      (presentation-type-of object)
	      :stream stream))
   #'clim-mop:class-direct-subclasses
   :stream pane))

(defun record-parent-chain (record)
  (and record
       (cons record
	     (record-parent-chain (output-record-parent record)))))

(defun find-graph-node (record)
  "Searches upward until a graph node parent of the supplied output record is found."
  (find-if #'graph-node-output-record-p (record-parent-chain record)))

(defun node-edges (node)
  (let (edges)
    (maphash
     (lambda (child edge)
       (declare (ignore child))
       (push edge edges))
     (slot-value node 'climi::edges-from))
    (maphash
     (lambda (parent edge)
       (declare (ignore parent))       
       (push edge edges))
     (slot-value node 'climi::edges-to))
    edges))

(defun redisplay-edges (graph edges)
  (dolist (edge edges)
    (with-slots (climi::from-node climi::to-node) edge
      (climi::layout-edge-1 graph climi::from-node climi::to-node))))

;;; (AH) McCLIM bug of the day:
;;;
;;; (I haven't looked in detail at the spec or McCLIM to confirm my
;;; assumptions here, but as I understand things..)  CLIM regions are
;;; immutable. Output records ARE mutable. A McCLIM output record can
;;; be used as a rectangular region corresponding to its bounding
;;; rectangle.  But this bounding rectangle is not immutable! So,
;;; region operations such as region-union may build a rectangle-set
;;; capturing the mutable output-record object, violating the
;;; immutability of regions and causing widespread panic and
;;; confusion.

(defun stupid-copy-rectangle (region)
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    (make-rectangle* x0 y0 x1 y1)))

(define-draggable-graph-demo-command (com-drag-node)
    ((record t) (x 'real) (y 'real))
  (let* ((graph-node (find-graph-node record))
	 (edges (node-edges graph-node))
	 (erase-region (stupid-copy-rectangle
                        (reduce (lambda (x &optional y)
                                  (if y (region-union x y) x))
                                edges))))
    (multiple-value-bind (px py) (output-record-position graph-node)
      (let ((graph (output-record-parent graph-node))
	    (x-offset (- x px))
	    (y-offset (- y py)))
	(assert (typep graph 'graph-output-record))
	(erase-output-record graph-node *standard-output*)
	(dolist (edge edges)
	  (clear-output-record edge))
	(when edges (repaint-sheet *standard-output* erase-region))
	(multiple-value-bind (final-x final-y)
	    (drag-output-record *standard-output* graph-node
				:erase-final t
				:finish-on-release t)
	  (setf (output-record-position graph-node)
		(values (- final-x x-offset) (- final-y y-offset)))
          
	  (add-output-record graph-node graph)
	  (redisplay-edges graph edges)
	  (repaint-sheet *standard-output* graph-node))))))
         
(define-presentation-to-command-translator record-dragging-translator
    (t com-drag-node draggable-graph-demo
       :tester ((presentation)
                (find-graph-node presentation)))
  (presentation x y)
  (list presentation x y))

;;; (CSR) This demo code is quite cool; visually, it's a little
;;; disconcerting to have the edges disappear when dragging, but
;;; that's acceptable, though I think it might be possible to preserve
;;; them by having a feedback function for the call to
;;; DRAG-OUTPUT-RECORD.
