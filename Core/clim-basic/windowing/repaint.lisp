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

;;; NOTE the native region may be smaller than the sheet region.
;;; NOTE see #1280 to learn why SHEET-NATIVE-REGION* is introduced.
;;; FIXME caching.
(defgeneric sheet-native-region* (sheet)
  (:method ((sheet mirrored-sheet-mixin))
    (sheet-region sheet))
  (:method ((sheet basic-sheet))
    (region-intersection
     (transform-region (sheet-native-transformation sheet) (sheet-region sheet))
     (sheet-native-region* (sheet-parent sheet)))))

(defun sheet-visible-region (sheet)
  (if (sheet-direct-mirror sheet)
      (sheet-native-region* sheet)
      (untransform-region (sheet-native-transformation sheet)
                          (sheet-native-region* sheet))))

(defmethod repaint-sheet ((sheet basic-sheet) region)
  (let* ((visible (sheet-visible-region sheet))
         (clipped (region-intersection visible region)))
    (handle-repaint sheet clipped)
    (loop for child in (sheet-children sheet)
          for transformation = (sheet-transformation child)
          for child-region = (untransform-region transformation region)
          do (repaint-sheet child child-region))
    (when (typep sheet 'sheet-with-medium-mixin)
      (medium-finish-output sheet))))

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

(defmethod repaint-sheet ((sheet sheet-mute-repainting-mixin) region)
  (declare (ignore sheet region))
  (values))

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

(defmethod handle-repaint :before ((sheet always-repaint-background-mixin) region)
  (when (typep sheet 'never-repaint-background-mixin)
    (return-from handle-repaint))
  (with-sheet-medium (medium sheet)
    (with-bounding-rectangle* (x1 y1 x2 y2)
        (region-intersection region (sheet-visible-region sheet))
      (letf (((medium-background medium) (pane-background sheet)))
        (medium-clear-area medium x1 y1 x2 y2)))))

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
