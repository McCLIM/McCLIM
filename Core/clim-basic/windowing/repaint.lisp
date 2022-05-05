;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000 by Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) Copyright 2000 by Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) Copyright 2000,2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The Repaint Protocol.
;;;

(in-package #:clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Repaint protocol functions.

(defmethod dispatch-repaint ((sheet graft) region)
  (declare (ignore sheet region)))

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
  (typecase region
    (nowhere-region)
    (everywhere-region
     (call-next-method))
    (otherwise
     (with-sheet-medium (medium sheet)
       (letf (((medium-clipping-region medium) region))
         (call-next-method))))))

(defmethod repaint-sheet ((sheet basic-sheet) region)
  (labels ((effective-native-region (msheet child region)
             (let ((intersection (region-intersection (sheet-region child) region)))
               (if (eq msheet child)
                   (transform-region (%%sheet-native-transformation msheet) intersection)
                   (effective-native-region msheet
                                            (sheet-parent child)
                                            (transform-region (sheet-transformation child)
                                                              intersection))))))
    ;; This causes applications which want to do a double-buffered repaint,
    ;; such as the logic cube, to flicker. On the other hand, it also stops
    ;; things such as the listener wholine from overexposing their text.
    (let ((msheet (sheet-mirrored-ancestor sheet)))
      ;; Do not call bounding-rectangle on region here. For +nowhere+ it gives
      ;; 0:0 0:0 (disregarding the native region). -- jd 2019-03-23
      (if (eql msheet sheet)
          (handle-repaint sheet (region-intersection (sheet-region sheet) region))
          (handle-repaint sheet (untransform-region
                                 (sheet-native-transformation sheet)
                                 (effective-native-region msheet sheet region)))))))

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
  (declare (ignore region))
  ;; FIXME: Shouldn't McCLIM always do this?
  (medium-finish-output sheet))

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
        (letf (((medium-clipping-region medium) native-sheet-region)
               ((medium-background medium) (pane-background sheet))
               ((medium-transformation medium) +identity-transformation+))
          (with-bounding-rectangle* (left top right bottom)
              native-sheet-region
            (medium-clear-area medium left top right bottom)))))))

;;; Integration with region and transformation changes

(defmethod (setf sheet-region) :around (region (sheet basic-sheet))
  (let ((old-region (sheet-region sheet)))
    (unless (region-equal region old-region)
      (call-next-method)
      (when (sheet-viewable-p sheet)
        (let* ((tr (sheet-transformation sheet))
               (r1 (rounded-bounding-rectangle (transform-region tr old-region)))
               (r2 (rounded-bounding-rectangle (transform-region tr region))))
          (unless (region-equal r1 r2)
            (repaint-sheet (sheet-parent sheet)
                           (region-union r1 r2))))))))

(defmethod (setf sheet-transformation) :around (transformation (sheet basic-sheet))
  (let ((old-transformation (sheet-transformation sheet)))
    (unless (transformation-equal transformation old-transformation)
      (call-next-method)
      (when (sheet-viewable-p sheet)
        (let* ((region (sheet-region sheet))
               (new-transformation (sheet-transformation sheet))
               (new-region (transform-region new-transformation region))
               (old-region (transform-region old-transformation region)))
          (unless (region-equal new-region old-region)
            (if (or (region-equal new-region +everywhere+)
                    (region-equal old-region +everywhere+))
                (repaint-sheet (sheet-parent sheet) +everywhere+)
                (repaint-sheet (sheet-parent sheet)
                               (region-union
                                (rounded-bounding-rectangle new-region)
                                (rounded-bounding-rectangle old-region))))))))))

(defun %set-sheet-region-and-transformation (sheet region transformation)
  (let ((old-transformation (sheet-transformation sheet))
        (old-region (sheet-region sheet)))
    (setf (sheet-region sheet) region
          (sheet-transformation sheet) transformation)
    (when (sheet-viewable-p sheet)
      (let ((new-region (transform-region (sheet-transformation sheet)
                                          (sheet-region sheet)))
            (old-region (transform-region old-transformation old-region)))
        (repaint-sheet (sheet-parent sheet)
                       (region-union
                        (rounded-bounding-rectangle new-region)
                        (rounded-bounding-rectangle old-region)))))))
