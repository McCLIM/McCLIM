(in-package #:clim-standard)

;;; We assume the following limitations of the host window systems:
;;;
;;;  mirror transformations:
;;;   . can only be translations
;;;   . are limited to 16-bit signed integer deltas
;;;
;;;  mirror regions:
;;;   . can only be axis-aligend rectangles
;;;   . min-x = min-y = 0
;;;   . max-x, max-y < 2^16
;;;
;;; These are the limitations of the X Window System.


;;; Classes
(defclass standard-mirrored-sheet-mixin (mirrored-sheet-mixin)
  ((mirror-transformation
    :documentation "Our idea of the current mirror transformation. Might not be
correct if a foreign application changes our mirror's geometry."
    :initform +identity-transformation+
    :accessor %sheet-mirror-transformation)
   (mirror-region
    :documentation "Our idea of the current mirror region. Might not be correct
if a foreign application changes our mirror's geometry. Also note that this
might be different from the sheet's native region."
    :initform nil
    :accessor %sheet-mirror-region)))

(defclass standard-full-mirrored-sheet-mixin (standard-mirrored-sheet-mixin) ())
(defclass standard-single-mirrored-sheet-mixin (standard-mirrored-sheet-mixin) ())


;;; CLIM methods
(defmethod sheet-direct-mirror ((sheet standard-mirrored-sheet-mixin))
  (port-lookup-mirror (port sheet) sheet))

(defparameter *configuration-event-p* nil)

(defmethod handle-event ((sheet standard-mirrored-sheet-mixin)
			 (event window-configuration-event))
  (let ((x (window-configuration-event-x event))
	(y (window-configuration-event-y event))
	(width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    (let ((*configuration-event-p* sheet))
      (setf (sheet-transformation sheet) (make-translation-transformation x y))
      (setf (sheet-region sheet) (make-bounding-rectangle 0 0 width height)))))

(defparameter *mirrored-sheet-geometry-changed-p* nil)

(defmethod (setf sheet-transformation) :around (tr (sheet standard-mirrored-sheet-mixin))
  (when (sheet-mirror sheet)
    (let ((*mirrored-sheet-geometry-changed-p* sheet))
      (call-next-method))))

(defmethod (setf sheet-transformation) (region (sheet standard-full-mirrored-sheet-mixin))
  (declare (ignore region))
  (let ((old-native-transformation (%%sheet-native-transformation sheet)))
    (call-next-method)
    ;;(%%set-sheet-native-transformation old-native-transformation sheet)
    (update-mirror-geometry sheet :old-native-transformation old-native-transformation)))

(defmethod (setf sheet-region) :around (re (sheet standard-mirrored-sheet-mixin))
  (when (or (/= (bounding-rectangle-width re) (bounding-rectangle-width (sheet-region sheet)))
	    (/= (bounding-rectangle-height re) (bounding-rectangle-height (sheet-region sheet))))
    (let ((*mirrored-sheet-geometry-changed-p* sheet))
      (call-next-method)
      (dispatch-repaint sheet (sheet-region sheet)))))

(defmethod (setf sheet-region) (region (sheet standard-full-mirrored-sheet-mixin))
  (declare (ignore region))
  (let ((old-native-transformation (%%sheet-native-transformation sheet)))
    (call-next-method)
    ;;(%%set-sheet-native-transformation old-native-transformation sheet)
    (update-mirror-geometry sheet :old-native-transformation old-native-transformation)))

(defmethod note-sheet-transformation-changed ((sheet standard-full-mirrored-sheet-mixin))
  (dolist (child (sheet-children sheet))
    (let ((old-native-transformation (%%sheet-native-transformation child)))
      (call-next-method)
      (update-mirror-geometry child :old-native-transformation old-native-transformation))))

(defmethod note-sheet-transformation-changed :before ((sheet standard-single-mirrored-sheet-mixin))
  (when (sheet-mirror sheet)
    (update-mirror-geometry sheet)))

(defmethod note-sheet-region-changed :before ((sheet standard-single-mirrored-sheet-mixin))
  (when (sheet-mirror sheet)
    (update-mirror-geometry sheet)))

(defmethod invalidate-cached-transformations ((sheet standard-full-mirrored-sheet-mixin))
  (with-slots (native-transformation device-transformation) sheet
    (setf ;;native-transformation nil
     device-transformation nil))
  (loop for child in (sheet-children sheet)
     do (invalidate-cached-transformations child)))


;;; sheet/mirror notes
(defmethod note-sheet-enabled :after ((sheet standard-mirrored-sheet-mixin))
 (when (sheet-direct-mirror sheet)
   (port-enable-sheet (port sheet) sheet)))

(defmethod note-sheet-disabled :after ((sheet standard-mirrored-sheet-mixin))
 (when (sheet-direct-mirror sheet)
   (port-disable-sheet (port sheet) sheet)))

(defmethod %note-mirrored-sheet-child-enabled :after ((sheet standard-mirrored-sheet-mixin) child)
  (dispatch-repaint sheet (sheet-native-region child)))

(defmethod %note-mirrored-sheet-child-disabled :after ((sheet standard-mirrored-sheet-mixin) child)
  (dispatch-repaint sheet (sheet-native-region child)))

(defmethod %note-mirrored-sheet-child-region-changed :after
    ((sheet standard-mirrored-sheet-mixin) child)
  (unless (eql sheet *mirrored-sheet-geometry-changed-p*)
    (dispatch-repaint sheet (sheet-native-region child))))

(defmethod %note-mirrored-sheet-child-transformation-changed :after
    ((sheet standard-mirrored-sheet-mixin) child)
  (unless (eql sheet *mirrored-sheet-geometry-changed-p*)
    (dispatch-repaint sheet (sheet-native-region child))))

(defmethod %note-sheet-pointer-cursor-changed :after ((sheet standard-mirrored-sheet-mixin))
  (set-sheet-pointer-cursor (port sheet) sheet (sheet-pointer-cursor sheet)))

(defun %repaint-background (sheet child region)
  (labels ((effective-repaint-region (mirrored-sheet child region)
	     (if (eq mirrored-sheet child)
		 (region-intersection
		  (sheet-region mirrored-sheet)
		  region)
		 (effective-repaint-region mirrored-sheet
					   (sheet-parent child)
					   (transform-region
					    (sheet-transformation child)
					    (region-intersection
					     region
					     (sheet-region child)))))))
    (let ((native-child-region (effective-repaint-region sheet child region)))
      (with-sheet-medium (medium sheet)
	(with-drawing-options (medium :clipping-region native-child-region
				      :ink (pane-background child)
				      :transformation +identity-transformation+)
	  (with-bounding-rectangle* (left top right bottom)
              native-child-region
	    (medium-draw-rectangle* sheet left top right bottom t)))))))

(defmethod %note-mirrored-sheet-child-repaint-request
    ((sheet standard-mirrored-sheet-mixin) child region)
  (%repaint-background sheet child region))

(defmethod %note-mirrored-sheet-child-repaint-request
    ((sheet standard-mirrored-sheet-mixin) (child always-repaint-background-mixin) region)
  nil)

(defmethod %note-mirrored-sheet-child-repaint-request
    ((sheet standard-mirrored-sheet-mixin) (child never-repaint-background-mixin) region)
  nil)

(defmethod %note-sheet-repaint-request ((sheet always-repaint-background-mixin)
					region)
  (%repaint-background sheet sheet region))



;;; sheet-native-*

(defmethod sheet-native-transformation ((sheet standard-single-mirrored-sheet-mixin))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation +identity-transformation+))
    native-transformation))

(defmethod sheet-native-region ((sheet standard-full-mirrored-sheet-mixin))
  (with-slots (native-region) sheet
    (unless native-region
      (let ((this-region (transform-region (sheet-native-transformation sheet)
                                           (sheet-region sheet)))
            (parent (sheet-parent sheet)))
        (setf native-region
              (if parent
                  (region-intersection this-region
                                       (transform-region
                                        (invert-transformation
                                         (%sheet-mirror-transformation sheet))
                                        (sheet-native-region parent)))
                  this-region))))
    native-region))

(defmethod sheet-native-transformation ((sheet standard-full-mirrored-sheet-mixin))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
            (let ((parent (sheet-parent sheet)))
              (cond
                ((top-level-sheet-pane-p sheet)
                 +identity-transformation+)
                (parent
                 (compose-transformations
                  (invert-transformation
                   (%sheet-mirror-transformation sheet))
                  (compose-transformations
                   (sheet-native-transformation parent)
                   (sheet-transformation sheet))))
                (t
                 (compose-transformations
                  (invert-transformation
                   (%sheet-mirror-transformation sheet))
                  (sheet-transformation sheet)))))))
      native-transformation))


;;; Mirror geometry functions
(defun %set-mirror-geometry (sheet x1 y1 x2 y2)
  (let* ((MT (make-translation-transformation x1 y1))
	 (MR (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1)))))
    (setf (%sheet-mirror-region sheet) MR)
    (setf (%sheet-mirror-transformation sheet) MT)
    (when (and (sheet-direct-mirror sheet)
	       (not (eql *configuration-event-p* sheet)))
      (let ((port (port sheet))
	    (mirror (sheet-direct-mirror sheet)))
	(port-set-mirror-region port mirror MR)
	(port-set-mirror-transformation port mirror MT))
      (with-slots (native-transformation device-transformation) sheet
	(setf native-transformation nil
	      device-transformation nil)))))

;;; Reflecting a Sheet's Geometry to the Mirror.
(defun %sheet-mirror-region* (sheet)
  ;; For grafts or top-level-sheet's always read the mirror region
  ;; from the server, since it is not under our control.
  (if (or (null (sheet-parent sheet))
          (null (sheet-parent (sheet-parent sheet)))
          (null (%sheet-mirror-region sheet)))
      (make-rectangle* 0 0 #x10000 #x10000)
      (%sheet-mirror-region sheet)))

(defun %effective-mirror-region (sheet)
  ;; XXX is this really needed, can't we deduce this information more easily?
  (let* ((parent (sheet-parent sheet))
         (ancestor (and parent (sheet-mirrored-ancestor parent))))
    (if ancestor
        (region-intersection (%sheet-mirror-region* sheet)
                             (untransform-region (%sheet-mirror-transformation sheet)
                                                 (%effective-mirror-region ancestor)))
        (%sheet-mirror-region* sheet))))

(defgeneric update-mirror-geometry (sheet &key &allow-other-keys)
  (:documentation "Updates mirror geometry.")
  (:method ((sheet standard-mirrored-sheet-mixin) &key) nil))

(defmethod update-mirror-geometry ((sheet standard-single-mirrored-sheet-mixin) &key)
  (let* ((parent (sheet-parent sheet))
	 (sheet-region-in-native-parent
	  (region-intersection
	   (sheet-region parent)
	   (transform-region (sheet-transformation sheet)
			     (sheet-region sheet)))))
    (if (region-equal sheet-region-in-native-parent +nowhere+)
	(%set-mirror-geometry sheet -5 -5 1 1)
	(with-bounding-rectangle* (mx1 my1 mx2 my2)
	    sheet-region-in-native-parent
	  (%set-mirror-geometry sheet mx1 my1 mx2 my2)))))

(defmethod update-mirror-geometry ((sheet standard-full-mirrored-sheet-mixin)
                                   &key old-native-transformation)
  "This function reflects the current sheet region and sheet transformation
to the mirror. It also sets up the native transformation. This function is
supposed to be called whenever one of the following happens:

  - the sheet's transformation changed
  - the sheet's region changed
  - the parent's native transformation changed
  - the parent's transformation changed
  - the parent's mirror region changed

Also if the sheet's native transformation changes the mirror's contents need
to be redrawn, which is achieved by calling PORT-DIRTY-MIRROR-REGION.

Since changing the sheet's native transformation might thus be expensive,
this function tries to minimize changes to it. (although it does not try
very hard)."
  (when (graftp sheet)                  ; We cannot resize the graft
    (return-from update-mirror-geometry nil))
  ;; The native transformation has to changed or needs to be computed initially.
  (let* ((parent (sheet-parent sheet))
         (mirrored-ancestor (sheet-mirrored-ancestor parent))
         (sheet-region-in-native-parent
          ;; this now is the wanted sheet mirror region
          (transform-region (sheet-native-transformation parent)
                            (transform-region (sheet-transformation sheet)
                                              (sheet-region sheet)))))
    (when (region-equal sheet-region-in-native-parent +nowhere+)
      (%set-mirror-geometry sheet -5 -5 1 1)
      (return-from update-mirror-geometry))
    ;; mx1 .. my2 are is now the wanted mirror region in the
    ;; parent coordinate system.
    (with-bounding-rectangle* (mx1 my1 mx2 my2)
        sheet-region-in-native-parent
      (let (;; pw, ph is the width/height of the parent
            (pw (bounding-rectangle-width (%sheet-mirror-region* mirrored-ancestor)))
            (ph (bounding-rectangle-height (%sheet-mirror-region* mirrored-ancestor))))
        (labels ((choose (MT)
                   ;; -> fits-p mirror-region
                   (multiple-value-bind (x1 y1) (transform-position MT 0 0)
                     (let ((x2  (if (<= mx2 pw)
                                    mx2
                                    (floor (+ pw (min mx2 (+ #x8000 x1) #x8000)) 2)))
                           (y2  (if (<= my2 ph)
                                    my2
                                    (floor (+ ph (min my2 (+ #x8000 y1) #x8000)) 2))))
                       (when (and (< (- x2 x1) #x8000)
                                  (or (<= (max (- pw #x8000) mx1) x1 0) (coordinate= x1 mx1))
                                  (< (- y2 y1) #x8000)
                                  (or (<= (max (- pw #x8000) my1) y1 0) (coordinate= y1 my1))
                                  (> (round (- x2 x1)) 0)
                                  (> (round (- y2 y1)) 0))
                         (values t (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1)))))))))
          ;; Try reusing the native transformation:
          (when old-native-transformation
            (let ((MT (compose-transformations
                       (compose-transformations
                        (sheet-native-transformation parent)
                        (sheet-transformation sheet))
                       (invert-transformation old-native-transformation))))
              (multiple-value-bind (fits-p MR) (choose MT)
                (when fits-p
                  (setf (%sheet-mirror-region sheet) MR)
                  (setf (%sheet-mirror-transformation sheet) MT)
                  (when (and (sheet-direct-mirror sheet)
                             (not (eql *configuration-event-p* sheet)))
                    (let ((port (port sheet))
                          (mirror (sheet-direct-mirror sheet)))
                      (port-set-mirror-region port mirror MR)
                      (port-set-mirror-transformation port mirror MT)))
                  (return-from update-mirror-geometry) ))))
          ;; Try reusing the mirror transformation:
          (let ((MT (%sheet-mirror-transformation sheet)))
            (when MT
              (multiple-value-bind (fits-p MR) (choose MT)
                (when fits-p
                  (let ((native-transformation
                         ;; NT = T o PNT o -MT
                         (compose-transformations
                          (invert-transformation MT)
                          (compose-transformations (sheet-native-transformation parent)
                                                   (sheet-transformation sheet)))))
                    ;; finally reflect the change to the host window system
                    (setf (%sheet-mirror-region sheet) MR)
                    (setf (%sheet-mirror-transformation sheet) MT)
                    (when (and (sheet-direct-mirror sheet)
                               (not (eql *configuration-event-p* sheet)))
                      (let ((port (port sheet))
                            (mirror (sheet-direct-mirror sheet)))
                        (port-set-mirror-region port mirror MR)
                        (port-set-mirror-transformation port mirror MT)))
                    ;; update the native transformation if neccessary.
                    (unless (and old-native-transformation
                                 (transformation-equal native-transformation
                                                       old-native-transformation))
                      (invalidate-cached-transformations sheet)
                      (%%set-sheet-native-transformation native-transformation sheet)
                      (when old-native-transformation
                        ;; Full sheet contents are redrawn.
                        (climi::dispatch-repaint
                         sheet
                         (untransform-region native-transformation
                                             (%effective-mirror-region sheet))))))
                  (return-from update-mirror-geometry))))))
        ;; Otherwise just choose the geometry
        ;; Conditions to be met:
        ;;  x2 < #x8000 + x1
        ;;  x1 in [max(pw - #x8000, mx1), 0] u {mx1}
        ;;  x2 in [pw, min (#x8000, mx2)]    u {mx2}
        ;;
        ;; It can still happend, that we cannot meet the
        ;; window system limitations => the sheet is
        ;; unvisible.
        (let* ((x1 (if (>= mx1 0) (round mx1) (floor (max (- pw #x8000) mx1) 2)))
               (y1 (if (>= my1 0) (round my1) (floor (max (- ph #x8000) my1) 2)))
               (x2 (if (<= mx2 pw) mx2 (floor (+ pw (min mx2 (- #x8000 x1))) 2)))
               (y2 (if (<= my2 ph) my2 (floor (+ ph (min my2 (- #x8000 y1))) 2)))
               (MT (make-translation-transformation x1 y1))
               (MR (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1))))
               (native-transformation
                ;; NT = T o PNT o -MT
                (compose-transformations
                 (invert-transformation MT)
                 (compose-transformations
                  (sheet-native-transformation (sheet-parent sheet))
                  (sheet-transformation sheet))))
               (old-native-transformation
                (%%sheet-native-transformation sheet)))

          (cond ((and (> (round (- x2 x1)) 0)
                      (> (round (- y2 y1)) 0))
                 ;; finally reflect the change to the host window system
                 (setf (%sheet-mirror-region sheet) MR)
                 (setf (%sheet-mirror-transformation sheet) MT)
                 (when (and (sheet-direct-mirror sheet)
                            (not (eql *configuration-event-p* sheet)))
                   (let ((port (port sheet))
                         (mirror (sheet-direct-mirror sheet)))
                     (port-set-mirror-region port mirror MR)
                     (port-set-mirror-transformation port mirror MT)))
                 ;; update the native transformation if neccessary.
                 (unless (and old-native-transformation
                              (transformation-equal
                               native-transformation
                               old-native-transformation))
                   (invalidate-cached-transformations sheet)
                   (%%set-sheet-native-transformation native-transformation sheet)
                   (when old-native-transformation
                     ;; native transformation has changed - repaint the sheet
                     (climi::dispatch-repaint
                      sheet
                      (untransform-region native-transformation
                                          (%effective-mirror-region sheet))))))
                (t
                 (%set-mirror-geometry sheet -5 -5 1 1))))))))



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
