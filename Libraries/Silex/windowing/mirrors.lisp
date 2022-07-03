(in-package #:silex)


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
;;;
;;;  XXX this is not true - we need to repaint the sheet always when its
;;;  region changes (i.e due to a change to the transformation) - we need to
;;;  redraw it even if the native transformation stays the same. On the other
;;;  hand the native transformation never changes when the sheet region and
;;;  the sheet transformation are constant. In other words - don't redraw,
;;;  because the (SETF SHEET-TRANSFORMATION) :AROUND method already does that.
;;;  -- jd 2022-05-12
;;;
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

(defvar *configuration-event-p* nil
  "Flag used to inhibit setting mirror region and transformation to prevent
infinite recursion on (setf sheet-*).")

;;; This mirror is invisible because it doesn't intersect its parent's mirror.
(define-constant +nowhere-mirror+ (make-rectangle* -5 -5 -4 -4)
    :test #'region-equal)

(defun %set-mirror-geometry (sheet region)
  (when (region-equal region +nowhere+)
    (setf region +nowhere-mirror+))
  (let ((geometry (sheet-mirror-geometry sheet)))
    (setf (rectangle-edges* geometry)
          (if (eql *configuration-event-p* sheet)
              (bounding-rectangle* region)
              ;; TOP-LEVEL-SHEET-MIXIN is a sheet representing the window,
              ;; however we can't always set its exact location and region (the
              ;; window manager may i.e add decorations or ignore our request -
              ;; like a tiling window manager). -- jd 2020-11-30
              (port-set-mirror-geometry (port sheet) sheet region)))
    geometry))

(defun update-mirror-geometry (sheet)
  ;; We can't manipulate grafts (and rasters)
  (when-let ((parent (sheet-parent sheet)))
    (assert (sheet-direct-mirror sheet))
    (let* ((msheet (sheet-mirrored-ancestor parent))
           (region (sheet-region sheet))
           (s-tran (sheet-transformation sheet))
           (parent-n-tran (sheet-native-transformation parent))
           (sheet->parent-mirror (compose-transformations parent-n-tran s-tran))
           ;; mirror-region is expressed in the parent's mirror coordinates.
           (mirrored-region (transform-region sheet->parent-mirror region)))
      ;; Relation between the parent native region and its mirror is arbitrary.
      ;; That's why we first clip with the parent native region and then with
      ;; its mirror (keeping in mind that the graft doesn't clip it children).
      (flet ((clip (clip-sheet clip-region)
               (when (and clip-sheet clip-region (not (graftp clip-sheet)))
                 (setf mirrored-region
                       (region-intersection mirrored-region clip-region)))))
        (clip parent (sheet-native-region parent))
        (with-bounding-rectangle* (:width w :height h)
            (sheet-mirror-geometry msheet)
          (clip msheet (make-rectangle* 0 0 w h))))
      ;; %set-mirror-geometry will return the mirror region with coordinates
      ;; rounded according to the port-set-mirror-geometry policy.
      (let ((mirror-region (%set-mirror-geometry sheet mirrored-region)))
        (when (region-equal mirrored-region +nowhere+)
          (with-slots (native-transformation device-transformation) sheet
            (setf native-transformation nil
                  device-transformation nil)
            (return-from update-mirror-geometry)))
        (with-standard-rectangle* (x y) mirror-region
          (let* ((offset (make-translation-transformation (- x) (- y)))
                 (old-n-tran (%%sheet-native-transformation sheet))
                 (new-n-tran (compose-transformations offset sheet->parent-mirror)))
            (%%set-sheet-native-region (transform-region offset mirror-region) sheet)
            (unless (and old-n-tran (transformation-equal new-n-tran old-n-tran))
              (invalidate-cached-transformations sheet)
              (%%set-sheet-native-transformation new-n-tran sheet)
              (when old-n-tran
                ;; Native transformation has changed - repaint the sheet.
                ;;
                ;; We don't call dispatch-repaint because the transformation of
                ;; the sheet may change without intervening event reads and the
                ;; repaint would not happen before the next event is read, causing
                ;; a corrupted output during display. -- jd 2021-03-02
                (repaint-sheet sheet (sheet-native-region* sheet))))))))))
