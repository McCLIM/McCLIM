;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2004 by 
;;;           Tim Moore (moore@bricoworks.com)

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

(define-application-frame dragndrop ()
  ()
  (:pointer-documentation t)
  (:panes
   (interactor :interactor)
   (scratchpad :application :display-time nil :height 600 :scroll-bars nil))
  (:layouts
    (default
      (vertically ()
	(scrolling (:height 300)
	  scratchpad)
	interactor))))

(defclass shape ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defclass circle (shape)
  ((radius :accessor radius :initarg :radius))
  (:default-initargs :radius 50))

(define-dragndrop-command (com-add-circle)
    ((x 'real :prompt "x")
     (y 'real :prompt "y")
     (radius 'real :prompt "radius"))
  (with-output-as-presentation
      (t (make-instance 'circle :x x :y y :radius radius) 'circle)
    (draw-circle* *standard-output* x y radius )))

(define-dragndrop-command (com-quit-dragndrop :name "Quit")
    ()
  (frame-exit *application-frame*))

(define-presentation-to-command-translator translator-draw-circle
    (blank-area com-add-circle dragndrop
     :documentation "Add a circle")
    (object x y)
  `(,x ,y 50))

(define-dragndrop-command (com-clone-circle)
    ((original 'circle)
     (start-x 'real)
     (start-y 'real))
  ;; Track the pointer offset from the center of the original object
  (let ((x-offset (- (x original) start-x))
	(y-offset (- (y original) start-y)))
    (multiple-value-bind (final-x final-y)
	(dragging-output (t :finish-on-release t)
	  (draw-circle* *standard-output* (x original) (y original)
			(radius original)
			:filled nil ))
      (com-add-circle (+ final-x x-offset)
		      (+ final-y y-offset)
		      (radius original)))))

(define-presentation-to-command-translator translator-clone-circle
    (circle com-clone-circle dragndrop)
    (object x y)
  `(,object ,x ,y))

(defun drag-circles ()
  (run-frame-top-level (make-application-frame 'dragndrop)))
