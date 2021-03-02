(in-package #:clim-internals)


;;;; Coordinate Swizzling

;;; This implements what I call "coordinate swizzling", the illusion that
;;; sheets can be arbitrary large. The key idea here is that there is a
;;; certain kind freedom in choosing the native transformation. A little
;;; diagram to illustrate the involved transformations:

;;;
;;;                  NT                           NT = native transformation
;;;    sheet  ---------------->  mirror          PNT = parent's NT
;;;      |                         |              MT = mirror transformation
;;;      |                         |               T = sheet transformation
;;;      |                         |
;;;    T |                         | MT
;;;      |                         |
;;;      |                         |
;;;      |                         |
;;;      v          PNT            v
;;;    parent ----------------> parent
;;;                             mirror
;;;

;;; To setup both the mirror transformation (MT) and the mirror region (MR),
;;; we start with the mirror region. The window systems limitations are here:
;;; We can only have a certain size and its upper-left corner must be at the
;;; origin.

;;; Now the parent already has a mirror region (PMR) assigned, which obeys to
;;; the very same size restrictions. Since every part of MR outside of (PMR o
;;; MT^-1) is not visible, the first idea is to just clip it by the visible
;;; part:

;;;  MR_1 = intersection (SR o NT, PMR o MT^-1)             [mirror space]

;;; Since both NT and MT^-1 are not yet known let us reformulate that region
;;; in the parent mirror space:

;;;  MR_2 = MR_1 o MT                                       [parent mirror space]
;;;       = intersection (SR o NT, PMR o MT^-1) o MT
;;;       = intersection (SR o NT o MT, PMR o MT^-1 o MT)
;;;       = intersection (SR o (T o PNT o MT^-1) o MT, PMR)
;;;       = intersection (SR o T o PNT, PMR)

;;; MR_2 now is a good candidate for a mirror region. Unfortunately it is
;;; still in parent mirror space, so we transform it back, yielding MR_3:

;;;  MR_3 = MR_2 o MT^-1
;;;       = intersection (SR o T o PNT, PMR) o MT^-1

;;; Here the only unknown is the mirror transformation MT, we can still
;;; choose any as long as the window system limitations are met for both MR
;;; and MT.

;;; 1. MT should be a translation, whose delta x and y components are within
;;;    limits.

;;; 2. The size limitation of MR is already met, since MR_3's size is no
;;;    larger than PMR's size (which mets the limitations). [Remember that MT
;;;    was defined to be some translation].

;;; 3. MR_3's upper left corner should also be at the origin which nicely
;;;    defines MT^-1: Just choose this upper left corner coordinates as MT's x
;;;    and y deltas.

;;; So we can meet all criteria. The NT can easily be set up by the identity:

;;;    NT = T o PNT o MT^-1

;;; Notes

;;; . when the native transformation changes, we need to:

;;;  a. Redraw the mirror's contents since the mapping from the sheet space
;;;     to the mirror space (that is the native transformation) just changed.
;;;     Translational changes in the native transformation can be catered by
;;;     blittering, but then have a nice synchronization problem: Suppose
;;;     a repaint event is underway as we blitter from some region R_1 to
;;;     region R_2. Say the repaint event's region intersects with R_1. In
;;;     this case we just blittered pixels which were considered dirty into
;;;     R_2. Redrawing R_1 now does not repair the defect, since R_2 now also
;;;     contains dirty pixels. => oops, redraw error.
;;
;;;  b. Since the above above calculation took the parent's native
;;;     transformation into account, (and even the naively wanted mirror
;;;     region depends on the parent's native transformation), we need to
;;;     redo mirror geometry calculation for any child.
;;
;;;  c. I imagine more aggressive output records which remember the actual
;;;     octets which need to be send to the X server. These would contain
;;;     mirror coordinates and will need to be recalculated, when the native
;;;     transformation changes.

;;; => Changing the native transformation can be expensive, so we want a way
;;;    to minimize changes to the native transformation.

;;; What did we do? We clipped the wanted mirror region, SR o NT, inside the
;;; parent's mirror region to meet the window system limitations. We can make
;;; this clip region larger as long as we still come up with an mirror region,
;;; which meets the limits.


;;; Mirror geometry functions

(defparameter *configuration-event-p* nil
  "Flag used to inhibit setting mirror region and transformation to prevent
infinite recursion on (setf sheet-*).")

(defun %set-mirror-geometry (sheet mt mr)
  (unless (eql *configuration-event-p* sheet)
    (let ((port (port sheet))
          (old-mr (%sheet-mirror-region sheet))
          (old-mt (%sheet-mirror-transformation sheet)))
      ;; TOP-LEVEL-SHEET-MIXIN is a sheet representing the window, however we
      ;; can't always set its exact location and region (because the window
      ;; manager may add decorations or ignore our request altogether - like a
      ;; tiling window manager). -- jd 2020-11-30
      (unless (and old-mr (region-equal mr old-mr))
        (port-set-mirror-region port sheet mr))
      (unless (and old-mt (transformation-equal mt old-mt))
        (port-set-mirror-transformation port sheet MT))))
  (setf (%sheet-mirror-region sheet) mr)
  (setf (%sheet-mirror-transformation sheet) mt))

;;; This function makes the mirror "invisible" by putting it outside of the
;;; parent's mirror.
(let ((mt (make-translation-transformation -5 -5))
      (mr (make-rectangle* 0 0 1 1)))
  (defun %set-empty-mirror (sheet)
    (%set-mirror-geometry sheet mt mr)
    (with-slots (native-transformation device-transformation) sheet
      (setf native-transformation nil
            device-transformation nil))))

(defun update-mirror-geometry (sheet)
  ;; We can't manipulate grafts (and rasters)
  (when-let ((parent (sheet-parent sheet)))
    (assert (sheet-direct-mirror sheet))
    (let* ((region (sheet-region sheet))
           (s-tran (sheet-transformation sheet))
           (parent-region (sheet-region parent))
           (parent-n-tran (sheet-native-transformation parent))
           (sheet->parent-mirror (compose-transformations parent-n-tran s-tran))
           ;; clipped-region is expressed in the parent's mirror coordinates.
           (clipped-region
             (if (graftp parent)
                 ;; For the TOP-LEVEL-SHEET-PANE (when the parent is a GRAFT)
                 ;; we don't clip the sheet-region with the parent-region.
                 ;; -- admich 2019-05-30
                 (transform-region sheet->parent-mirror region)
                 (region-intersection
                  (transform-region parent-n-tran parent-region)
                  (transform-region sheet->parent-mirror region)))))
      ;; When the parent mirror is not a graft or a raster, then it has the
      ;; mirror region set. In that case we clip the mirror with it.
      (when-let* ((parent-msheet (sheet-mirrored-ancestor parent))
                  (parent-mirror-region (%sheet-mirror-region parent-msheet)))
        (setf clipped-region
              (region-intersection parent-mirror-region clipped-region)))
      (if (region-equal clipped-region +nowhere+)
          (%set-empty-mirror sheet)
          (with-bounding-rectangle* (x1 y1 x2 y2) clipped-region
            ;; No point in rounding coordinates here - composition below will
            ;; leave us with fractional dx and dy anyway. Moreover that may
            ;; distort a result of the call to TRANSFORMATION-EQUAL below and
            ;; cause unnecessary repaints. -- jd 2021-03-04
            (let* ((mt (make-translation-transformation x1 y1))
                   (mr (make-rectangle* 0 0 (- x2 x1) (- y2 y1)))
                   (new-n-tran (compose-transformations
                                (invert-transformation mt)
                                (compose-transformations parent-n-tran s-tran)))
                   (old-n-tran (%%sheet-native-transformation sheet)))
              (%set-mirror-geometry sheet mt mr)
              (unless (and old-n-tran
                           (transformation-equal new-n-tran old-n-tran))
                (invalidate-cached-transformations sheet)
                (%%set-sheet-native-transformation new-n-tran sheet)
                (when old-n-tran
                  ;; Native transformation has changed - repaint the sheet.
                  ;; Normally we would call dispatch-repaint, however the sheet
                  ;; transformation may change without intervening event reads
                  ;; (for instance in a display function that causes scrolling),
                  ;; and dispatching the repaint would not take effect until the
                  ;; next event read, causing a corrupted output during display.
                  ;; -- jd 2021-03-02
                  (repaint-sheet sheet (untransform-region new-n-tran mr))))))))))
