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
      ;; %note-sheet-repaint-request is responsible for clearing to the background
      ;; color before repainting
      ;; This causes applications which want to do a double-buffered repaint,
      ;; such as the logic cube, to flicker. On the other hand, it also
      ;; stops things such as the listener wholine from overexposing their
      ;; text.
      (%note-sheet-repaint-request sheet r)
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
                     (%note-sheet-repaint-request child child-region)
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


