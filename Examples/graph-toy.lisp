;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-DEMO; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Graph Toy Demo
;;;   Created: 2017-02
;;;    Author: Kyle Nusbaum <knusbaum@sdf.org>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2017 by Kyle Nusbaum

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
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(in-package :clim-demo)

(defun sub-array-for (arr length)
  (let ((orig-len (length arr)))
    (if (<= orig-len length)
        arr
        (make-array length :displaced-to arr :displaced-index-offset (- orig-len length)))))

(defun display-main (frame stream)
  (when (= 0 (length (val-array *application-frame*)))
    (return-from display-main nil))
  (let* ((left-x-padding 30)
         (pane (find-pane-named *application-frame* 'main-display))
         (values (sub-array-for (val-array *application-frame*) (max-xvals *application-frame*)))
         (maximum (if (> (length values) 0) (reduce #'max values) 0))
         (minimum (if (> (length values) 0) (reduce #'min values) 0))
         (variance (- maximum minimum))
         (rect (bounding-rectangle (sheet-region pane)))
         (width (rectangle-width rect))
         (height (- (rectangle-height rect) 100))
         (w-step (/ (- width left-x-padding) (length values)))
         (h-step (if (> variance 0) (/ height variance) 0))
         (y-label-count (min (truncate (/ height 100)) variance))
         (step-fn-x (let ((current left-x-padding))
                      (lambda ()
                        (truncate (setf current (+ current w-step))))))
         (fn-y (lambda (y)
                      (+ (- height (truncate (* h-step (+ y (- minimum))))) 50))))

    ;; Draw the title
    (draw-text* pane (format nil "~a" (title *application-frame*))
                (/ width 2) 10)
    
    ;; Draw the Y labels
    (loop for i from 0 to y-label-count
       with line-ink = (make-rgb-color 0.8 0.8 0.8)
       ;; Count based on the pre-truncated value (basic-val) to avoid skew
       for basic-val = minimum then (+ basic-val (/ variance y-label-count))
       for value = (truncate basic-val)
       for ypos = (funcall fn-y value)
       do (progn
            (clim:draw-text* pane (format nil "~a" value) 10 ypos)
            (draw-line* pane left-x-padding ypos width ypos :ink line-ink)))

    ;; Draw the graph data
    (reduce (lambda (acc yval)
              (let ((xpos (funcall step-fn-x))
                    (ypos (funcall fn-y yval)))
                (when (draw-values *application-frame*)
                  (clim:draw-text* pane (format nil "~a" yval) (- xpos 20) ypos))
                (when (not (eq acc nil))
                  (draw-line* pane (car acc) (cdr acc) xpos ypos))
                (cons xpos ypos)))
            values :initial-value nil)
    (format stream "Length: ~a~%" (length (val-array *application-frame*)))))

(define-application-frame graph-toy ()
  ((val-array :initform (make-array 8 :adjustable t :fill-pointer 0) :accessor val-array)
   (title :initform "Clim Example Graph" :initarg :title :accessor title)
   (max-xvals :initform 50 :initarg :max-xvals :accessor max-xvals)
   (draw-values :initform t :initarg :draw-values :accessor draw-values))
  (:default-initargs
    :width 800
    :height 600)
  (:panes
   (main-display
    :application
    :scroll-bars nil
    :display-function 'display-main
    :display-time :command-loop))
  (:layouts
   (default
       (horizontally ()
         main-display))))

(defclass new-value-event (window-manager-event)
  ((val :initarg :val :accessor val)))

(defclass refresh-event (window-manager-event) ())

(defmethod handle-event ((frame graph-toy) (event new-value-event))
  (with-application-frame (frame)
    (vector-push-extend (val event) (val-array frame))
    (let ((val-array-len (length (val-array frame)))
          (xval-count (max-xvals frame)))
      (when (>= val-array-len (* xval-count 2))
        (loop
           for i from 0 to (- xval-count 1)
           for j from (- val-array-len xval-count)
           do (setf (elt (val-array frame) i) (elt (val-array frame) j)))
        (setf (fill-pointer (val-array frame)) xval-count))
      (redisplay-frame-pane frame 'main-display))))

(defmethod handle-event ((frame graph-toy) (event refresh-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame 'main-display)))

(defun add-value (graph val)
  (queue-event (frame-top-level-sheet graph)
               (make-instance 'new-value-event
                              :sheet graph
                              :val val)))

(defun refresh-graph (graph)
  (queue-event (frame-top-level-sheet graph)
               (make-instance 'refresh-event
                              :sheet graph)))

(defun example-data (graph)
  (loop
     with current = 20
     do (progn
          (add-value graph current)
          (setf current (+ current (- (random 9) 4)))
          (sleep 0.5))))

(defmethod run-frame-top-level ((graph graph-toy) &key)
  (let ((example-data-proc (clim-sys:make-process #'(lambda () (example-data graph))
                                                  :name "Graph Updater")))
    (unwind-protect
         (call-next-method)
      (clim-sys:destroy-process example-data-proc))))
