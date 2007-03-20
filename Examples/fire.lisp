;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2001 by 
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)

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


;;; How to use the possibilities of the firelights, you have two
;;; possibilites :
;;; 1 - Click on a toggle-button : the color of the fire-pane
;;;     will change
;;; 2 - Click on the orange or green toggle-button, then move your
;;;     mouse-pointer on the fire-pane, and wait a few seconds.


(in-package :clim-internals)

(export '(fire-pane))

;; example gadget definition
(defclass fire-pane (standard-gadget) ())

(defmethod dispatch-repaint ((pane fire-pane) region)
  (repaint-sheet pane region))

(defmethod repaint-sheet ((pane fire-pane) region)
  (declare (ignore region))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (display-gadget-background pane +white+ 0 0 (- x2 x1) (- y2 y1))))

(defmethod handle-event ((pane fire-pane) (event window-repaint-event))
  (declare (ignorable event))
  (dispatch-repaint pane (sheet-region pane)))

(in-package :clim-demo)

;; callback functions

(defmethod handle-event :after ((pane clim-internals::fire-pane) (event pointer-event))
  (declare (ignorable event))
  #+nil
  (let ((label (clim-internals::gadget-label (clim-internals::radio-box-current-selection
					      (find-pane-named *application-frame* 'radio-box)))))
    (cond ((string= label "O")
	   (progn
	     (sleep 3)
	     (simulate-user-action (third (frame-panes *application-frame*)))))
	  ((string= label "G")
	   (progn
	     (sleep 5)
	     (simulate-user-action (first (frame-panes *application-frame*)))))
	  (t nil))))

(defmethod simulate-user-action ((pane toggle-button))
  (handle-event pane
		(make-instance 'pointer-button-press-event))
  (handle-event pane
		(make-instance 'pointer-button-release-event)))

(defun callback-red (gadget value)
  (declare (ignorable gadget))
  (when value
    (setf (clim-internals::gadget-current-color (find-pane-named *application-frame* 'fire))
	  (clim-internals::gadget-normal-color (find-pane-named *application-frame* 'fire)))))

(defun callback-orange (gadget value)
  (declare (ignore gadget))
  (when value 
    (setf (clim-internals::gadget-current-color (find-pane-named *application-frame* 'fire))
	  (clim-internals::gadget-highlighted-color (find-pane-named *application-frame* 'fire)))))

(defun callback-green (gadget value)
  (declare (ignore gadget))
  (when value
    (setf (clim-internals::gadget-current-color (find-pane-named *application-frame* 'fire))
	  (clim-internals::gadget-pushed-and-highlighted-color (find-pane-named *application-frame* 'fire)))))

;; test functions

(defun fire ()
  (run-frame-top-level (make-application-frame 'firelights)))

(defmethod fire-frame-top-level ((frame application-frame))
  (with-look-and-feel-realization ((frame-manager frame) frame)
    (setf (slot-value *application-frame* 'radio-box)
          (with-radio-box (:name 'radio-box)
            (first (frame-panes *application-frame*))
            (second (frame-panes *application-frame*))
            (radio-box-current-selection (third (frame-panes *application-frame*)))))
    (loop (event-read (find-pane-named frame 'fire)))))

(define-application-frame firelights ()
  ((radio-box :initform nil)
   (fire :initform nil))
  (:panes
   (fire      :fire
	      :width 30
	      :normal +red+
	      :highlighted +orange+
	      :pushed-and-highlighted +green+)
   (red-fire  :toggle-button
	      :label "R"
	      :value t
	      :width 30
	      :height 30
	      :normal +red+
	      :highlighted +red+
	      :pushed-and-highlighted +red+
	      :value-changed-callback 'callback-red)
   (green-fire  :toggle-button
		:label "G"
		:value nil
		:height 30
		:normal +green+
		:highlighted +green+
		:pushed-and-highlighted +green+
	        :value-changed-callback 'callback-green)
   (orange-fire  :toggle-button
		 :label "O"
		 :value nil
		 :height 30
		 :normal +orange+
		 :highlighted +orange+
		 :pushed-and-highlighted +orange+
		 :value-changed-callback 'callback-orange))
   (:layouts
    (default (horizontally () (vertically () red-fire orange-fire green-fire) fire)))
   #+NIL (:top-level (fire-frame-top-level . nil)))
