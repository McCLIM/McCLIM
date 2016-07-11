;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com), 
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 20014 by
;;;           Robert Strandh (robert.strandh@gmail.com)

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

;;;; The Repaint Protocol.

(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Repaint protocol functions.

(defmethod queue-repaint ((sheet basic-sheet) (event window-repaint-event))
  (queue-event sheet event))

(defmethod handle-repaint ((sheet basic-sheet) region)    
  (declare (ignore region))
  nil)

(defmethod repaint-sheet ((sheet basic-sheet) region)
  (handle-repaint sheet region)
  (dolist (child (sheet-children sheet))
    (when (and (sheet-enabled-p child))
      (let* ((child-region (region-intersection
			    (untransform-region
			     (sheet-transformation child)
			     region)
			    (sheet-region child))))
	(unless (eq child-region +nowhere+)
	  (repaint-sheet child child-region))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Repaint protocol classes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-REPAINTING-MIXIN.

(defclass standard-repainting-mixin () ())

(defmethod dispatch-event
    ((sheet standard-repainting-mixin) (event window-repaint-event)) 
  (queue-repaint sheet event))

(defmethod dispatch-repaint ((sheet standard-repainting-mixin) region)
  (when (sheet-mirror sheet)            ;only dispatch repaints, when the sheet has a mirror
    (queue-repaint sheet (make-instance 'window-repaint-event
                                        :sheet sheet
                                        :region (transform-region
                                                 (sheet-native-transformation sheet)
                                                 region)))))

(defmethod handle-event ((sheet standard-repainting-mixin)
			 (event window-repaint-event))
  ;;(repaint-sheet sheet (window-event-region event)))
  (handle-repaint sheet (window-event-region event))
  (propagate-repaint sheet sheet (window-event-region event)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IMMEDIATE-REPAINTING-MIXIN.

(defclass immediate-repainting-mixin () ())

(defmethod dispatch-event
    ((sheet immediate-repainting-mixin) (event window-repaint-event))
  (handle-repaint sheet (window-event-region event)))

(defmethod dispatch-repaint ((sheet immediate-repainting-mixin) region)
  (handle-repaint sheet region))

(defmethod handle-event ((sheet immediate-repainting-mixin)
			 (event window-repaint-event))
  (repaint-sheet sheet (window-event-region event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHEET-MUTE-REPAINTING-MIXIN.

(defclass sheet-mute-repainting-mixin () ())

(defmethod dispatch-repaint ((sheet sheet-mute-repainting-mixin) region)
  (when (sheet-mirror sheet)
    ;; Only dispatch repaints, when the sheet has a mirror.
    (queue-repaint sheet (make-instance 'window-repaint-event
			   :sheet sheet
			   :region (transform-region
				    (sheet-native-transformation sheet)
				    region)))))

;;; I know what the spec says about SHEET-MUTE-REPAINTING-MIXIN, but I don't
;;; think it's right; "repaint-sheet that does nothing" makes no sense.
;;; -- moore
#+nil
(defmethod repaint-sheet ((sheet sheet-mute-repainting-mixin) region)
  (declare (ignorable sheet region))
  (format *trace-output* "repaint ~S~%" sheet)
  (values))

(defmethod handle-repaint ((sheet sheet-mute-repainting-mixin) region)
  (declare (ignore region))
  nil)

(defclass clim-repainting-mixin
    (#+clim-mp standard-repainting-mixin #-clim-mp immediate-repainting-mixin)
  ()
  (:documentation "Internal class that implements repainting protocol based on
  whether or not multiprocessing is supported."))

;;;
;;;
;;;

(defun propagate-repaint (mirrored-sheet sheet region)
  (dolist (child (sheet-children sheet))
    (when (and (sheet-enabled-p child)
	       (not (typep child 'mirrored-sheet-mixin)))
      (let* ((native-child-region (region-intersection
				   region
				   (sheet-native-region child))))
	(unless (eq native-child-region +nowhere+)
	  ;; if child is a pane we need to repaint the background
	  (when (typep child 'basic-pane)
	    (with-bounding-rectangle* (x1 y1 x2 y2)
		native-child-region
	      (draw-rectangle* mirrored-sheet
			       x1 y1 x2 y2 :filled t
			       :clipping-region native-child-region :ink (pane-background child))))
	  (handle-repaint child (untransform-region
				 (sheet-native-transformation child)
				 native-child-region))
	  (propagate-repaint mirrored-sheet child region))))))
