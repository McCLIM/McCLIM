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

(defmethod dispatch-repaint ((sheet graft) region)
  (declare (ignore sheet region)))

;;; Internal flag used in sheets module. We could make it a macro which
;;; accumulates regions to repaint with region-union and at the end repaints
;;; whole bunch at one go. Could be useful for presentation highlighting when we
;;; have many small areas to redraw. That could improve the performance.
(defvar *inhibit-dispatch-repaint* nil
  "Used when we plan to repaint whole sheet and we suspect that children may
want to do the same.")

(defmethod dispatch-repaint :around ((sheet basic-sheet) region)
  (unless *inhibit-dispatch-repaint* (call-next-method)))

(defmethod queue-repaint ((sheet basic-sheet) (event window-repaint-event))
  (queue-event sheet event))

(defmethod handle-repaint ((sheet basic-sheet) region)    
  (declare (ignore region))
  nil)

(defmethod repaint-sheet :around ((sheet basic-sheet) region)
  (declare (ignore region))
  (when (and (sheet-mirror sheet)
	     (sheet-viewable-p sheet))
    (call-next-method)))

(defmethod handle-repaint :around ((sheet sheet-with-medium-mixin) region)
  (let ((medium (sheet-medium sheet)))
    (unless (eql region +nowhere+)
      (with-drawing-options (medium :clipping-region region)
	(call-next-method)))))

(defmethod repaint-sheet ((sheet basic-sheet) region)
  (labels ((effective-native-region (mirrored-sheet child region)
	     (if (eq mirrored-sheet child)
                 (transform-region
                    (%%sheet-native-transformation mirrored-sheet)
                    (region-intersection
                     (sheet-region mirrored-sheet)
                     region))
		 (effective-native-region mirrored-sheet
					  (sheet-parent child)
					  (transform-region
					   (sheet-transformation child)
					   (region-intersection
					    region
					    (sheet-region child)))))))
    (let ((r (bounding-rectangle
	      (untransform-region
	       (sheet-native-transformation sheet)
	       (effective-native-region (sheet-mirrored-ancestor sheet) sheet region)))))
      ;; This causes applications which want to do a double-buffered repaint,
      ;; such as the logic cube, to flicker. On the other hand, it also stops
      ;; things such as the listener wholine from overexposing their text.
      (handle-repaint sheet r))))

(defmethod repaint-sheet :after ((sheet sheet-parent-mixin) region)
  ;; propagate repaint to unmirrored sheets
  (labels ((propagate-repaint-1 (sheet region)
             (dolist (child (sheet-children sheet))
               (when (and (sheet-enabled-p child)
                          (not (sheet-direct-mirror child)))
                 (let ((child-region (region-intersection
                                      (untransform-region
                                       (sheet-transformation child)
                                       region)
                                      (sheet-region child))))
                   (unless (eq child-region +nowhere+)
                     (handle-repaint child child-region)
                     (propagate-repaint-1 child child-region)))))))
    (propagate-repaint-1 sheet region)))
	       
(defmethod repaint-sheet :after ((sheet sheet-with-medium-mixin) region)
  ;; FIXME: Shouldn't McCLIM always do this?
  (medium-force-output (sheet-medium sheet)))

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
  (repaint-sheet sheet (window-event-region event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IMMEDIATE-REPAINTING-MIXIN.

(defclass immediate-repainting-mixin () ())

(defmethod dispatch-event
    ((sheet immediate-repainting-mixin) (event window-repaint-event))
  (repaint-sheet sheet (window-event-region event)))

(defmethod dispatch-repaint ((sheet immediate-repainting-mixin) region)
  (repaint-sheet sheet region))

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

(defmethod handle-repaint ((sheet sheet-mute-repainting-mixin) region)
  (declare (ignore region))
  nil)

(defclass clim-repainting-mixin
    (#+clim-mp standard-repainting-mixin #-clim-mp immediate-repainting-mixin)
  ()
  (:documentation "Internal class that implements repainting protocol based on
  whether or not multiprocessing is supported."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; No Standard.

;; as present in silica's implementation
(defclass always-repaint-background-mixin () ())

;; never repaint the background (only for speed)
(defclass never-repaint-background-mixin () ())

;;; XXX: check if we can reintroduce with-double-buffering..
(defmethod handle-repaint :before ((sheet always-repaint-background-mixin) region)
  #+jd-test(sleep 0.1)                  ; we repaint whole thing around four times!
  (when (typep sheet 'never-repaint-background-mixin)
    (return-from handle-repaint))
  (labels ((effective-repaint-region (mirrored-sheet sheet region)
	     (if (eq mirrored-sheet sheet)
		 (region-intersection (sheet-region mirrored-sheet) region)
		 (effective-repaint-region mirrored-sheet
					   (sheet-parent sheet)
					   (transform-region (sheet-transformation sheet)
                                                             (region-intersection region
                                                                                  (sheet-region sheet)))))))
    (let* ((parent (sheet-mirrored-ancestor sheet))
           (native-sheet-region (effective-repaint-region parent sheet region)))
	  (with-sheet-medium (medium parent)
	    (with-drawing-options (medium :clipping-region native-sheet-region
					  :ink (pane-background sheet)
					  :transformation +identity-transformation+)
	      (with-bounding-rectangle* (left top right bottom)
		  native-sheet-region
		(medium-draw-rectangle* medium left top right bottom t)))))))
