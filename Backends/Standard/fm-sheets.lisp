(in-package :clim-standard)

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
;;;

(defclass standard-full-mirrored-sheet-mixin (standard-mirrored-sheet-mixin)
  ())

(defmethod note-sheet-transformation-changed ((sheet standard-full-mirrored-sheet-mixin))
  (dolist (child (sheet-children sheet))
    (let ((old-native-transformation (%%sheet-native-transformation child)))
      (call-next-method)
      (update-mirror-geometry child old-native-transformation))))

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


(defmethod (setf sheet-region) (region (sheet standard-full-mirrored-sheet-mixin))
  (declare (ignore region))
  (let ((old-native-transformation (%%sheet-native-transformation sheet)))
    (call-next-method)
    ;;(%%set-sheet-native-transformation old-native-transformation sheet)
    (update-mirror-geometry sheet old-native-transformation)))

(defmethod (setf sheet-transformation) (region (sheet standard-full-mirrored-sheet-mixin))
  (declare (ignore region))
  (let ((old-native-transformation (%%sheet-native-transformation sheet)))
    (call-next-method)
    ;;(%%set-sheet-native-transformation old-native-transformation sheet)
    (update-mirror-geometry sheet old-native-transformation)))

(defmethod invalidate-cached-transformations ((sheet standard-full-mirrored-sheet-mixin))
  (with-slots (native-transformation device-transformation) sheet
    (setf ;;native-transformation nil
     device-transformation nil))
  (loop for child in (sheet-children sheet)
     do (invalidate-cached-transformations child)))

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
;;; this clip region larger as long as we still come up with an mirror
;;; region, which meets the limits.

(defun update-mirror-geometry (sheet old-native-transformation &key)
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
  (cond ((null (sheet-parent sheet))
	 ;; Ugh, we have no parent, this must be the graft, we
	 ;; cannot resize it can we?
	 nil)
	;; Otherwise, the native transformation has to changed or
	;; needs to be computed initially
	(t
	 (let* ((parent (sheet-parent sheet))
		(mirrored-ancestor (sheet-mirrored-ancestor parent))
		(sheet-region-in-native-parent
		 ;; this now is the wanted sheet mirror region
		 (transform-region (sheet-native-transformation parent)
				   (transform-region (sheet-transformation sheet)
						     (sheet-region sheet)))))
	   (when (region-equal sheet-region-in-native-parent +nowhere+)
	     ;; hmm
	     (setf (%sheet-mirror-transformation sheet)
		   (make-translation-transformation -5 -5))
	     (setf (%sheet-mirror-region sheet) (make-rectangle* 0 0 1 1))
	     (when (and (sheet-direct-mirror sheet)
			(not (eql *configuration-event-p* sheet)))
	       (port-set-mirror-region
		(port sheet)
		(sheet-direct-mirror sheet)
		(%sheet-mirror-region sheet))
	       (port-set-mirror-transformation
		(port sheet)
		(sheet-direct-mirror sheet)
		(%sheet-mirror-transformation sheet)))
	     (return-from update-mirror-geometry))
	   ;; mx1 .. my2 are is now the wanted mirror region in the
	   ;; parent coordinate system.
	   (with-bounding-rectangle* (mx1 my1 mx2 my2)
	       sheet-region-in-native-parent
	     (let ( ;; pw, ph is the width/height of the parent
		   (pw  (bounding-rectangle-width
			 (sheet-mirror-region mirrored-ancestor)))
		   (ph  (bounding-rectangle-height
			 (sheet-mirror-region mirrored-ancestor))))
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
		 ;; FIXME: There is a quote character on a line by
		 ;; itself here.  It looks like someone tried to
		 ;; avoid evaluation, but WHY?  Certainly not the
		 ;; right method to do that, especially without
		 ;; explanation.
		 ;;
		 ;; Try reusing the mirror transformation:
		 '
		 (let ((MT (%sheet-mirror-transformation sheet)))
		   (when MT
		     (multiple-value-bind (fits-p MR) (choose MT)
		       (when fits-p
			 (let ((native-transformation
				;; NT = T o PNT o -MT
				(compose-transformations
				 (invert-transformation MT)
				 (compose-transformations
				  (sheet-native-transformation parent)
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
					(transformation-equal
					 native-transformation
					 old-native-transformation))
			     (invalidate-cached-transformations sheet)
			     (%%set-sheet-native-transformation
			      native-transformation sheet)
			     (when old-native-transformation
			       (care-for-new-native-transformation
				sheet
				old-native-transformation
				native-transformation))))
			 (return-from update-mirror-geometry)
			 ))))
		 ;; Otherwise just choose
		 
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
			    (%%set-sheet-native-transformation
			     native-transformation sheet)
			    (when old-native-transformation
			      (care-for-new-native-transformation
			       sheet
			       old-native-transformation
			       native-transformation))))
			 (t
			  (setf (%sheet-mirror-transformation sheet)
				(make-translation-transformation -5 -5))
			  (setf (%sheet-mirror-region sheet)
				(make-rectangle* 0 0 1 1))
			  (when (and (sheet-direct-mirror sheet)
				     (not (eql *configuration-event-p* sheet)))
			    (port-set-mirror-region
			     (port sheet)
			     (sheet-direct-mirror sheet)
			     (%sheet-mirror-region sheet))
			    (port-set-mirror-transformation
			     (port sheet)
			     (sheet-direct-mirror sheet)
			     (%sheet-mirror-transformation sheet)))))))))))))

(defun care-for-new-native-transformation (sheet old-native-transformation native-transformation)
  "Internal and helper for UPDATE-MIRROR-GEOMETRY. This is called in
   case the native transformation changed and takes care that the
   sheet contents get redrawn as appropriate. It also attempts to
   save some redraws by blittering."
  ;;
  ;; compute D := -NT_old o NT_new
  ;;
  ;; if D is a translation then
  ;;    blitter from: (MR o -D) ^ MR  to: (MR o D) ^ MR
  ;;    clear MR \ (MR o -D)
  ;; else
  ;;    clear MR
  ;;
  (let* (;; Compute the transformation to get from an old coordinate in
         ;; the mirror coordinate system to its new location.
         (delta (compose-transformations
                 native-transformation
                 (invert-transformation old-native-transformation)))
         ;;
         (MR (effective-mirror-region sheet)))
    (declare (ignorable delta))
    ;; When this delta transformation is a translation, we can
    ;; possibly blitter the pixels. Otherwise not, since blittering
    ;; cannot account for say scaling or rotation.
    (cond 
;;;          <-- please leave this code commented out for now -->
;;;          ;; Blittering will never work reliable soon.
;;;          ;; --GB
;;;          ((translation-transformation-p delta)
;;;           ;; We want to bitter. So compute, dMR, the region in mirror
;;;           ;; coordinate space where MR should end up. Clip it to the actual
;;;           ;; mirror, which gives us the destination rectangle. Transform this
;;;           ;; destination back to the old space to get the source rectangle.
;;;           ;; Finally compute the region, which is not occupied by the
;;;           ;; destination and thus must be redrawn.
;;;           ;;
;;;           ;; Note that by using region operations, we automatically take care
;;;           ;; for the case that the window was scrolled too far to reuse any
;;;           ;; pixels.
;;;           (let* ((dMR  (transform-region delta MR))
;;;                  (dest (region-intersection dMR MR))
;;;                  (src  (untransform-region delta dest))
;;;                  (lack (region-difference MR dMR)))
;;;             ;; Now actually blitter, take care for empty regions.
;;;             (unless (or (region-equal src +nowhere+)
;;;                         (region-equal dest +nowhere+))
;;;               (let ((gc (xlib:create-gcontext :drawable (sheet-direct-mirror sheet))))
;;;                 (xlib:copy-area (sheet-direct-mirror sheet) gc
;;;                                 (floor (bounding-rectangle-min-x src))
;;;                                 (floor (bounding-rectangle-min-y src))
;;;                                 (floor (bounding-rectangle-width src))
;;;                                 (floor (bounding-rectangle-height src))
;;;                                 (sheet-direct-mirror sheet)
;;;                                 (floor (bounding-rectangle-min-x dest))
;;;                                 (floor (bounding-rectangle-min-y dest)))) )
;;;             ;; And handle the exposure
;;;             (unless (region-equal lack +nowhere+)
;;;               (xlib:clear-area (sheet-direct-mirror sheet)
;;;                                :x (floor (bounding-rectangle-min-x lack))
;;;                                :y (floor (bounding-rectangle-min-y lack))
;;;                                :width (floor (bounding-rectangle-width lack))
;;;                                :height (floor (bounding-rectangle-height lack))
;;;                                :exposures-p nil)
;;;               (handle-repaint sheet (untransform-region native-transformation lack)))))
          (t
           ;; Full sheet contents need to be redrawn, since transformation is no
           ;; translation.
           (climi::dispatch-repaint sheet
                             (untransform-region native-transformation MR)) ))))


;;; The generic function SHEET-MIRROR-REGION is not part of the CLIM
;;; II specification.  For that reason, there is no DEFGENERIC form
;;; for it in decls.lisp.  Since some Common Lisp compilers emit a
;;; warning if there is no explicit DEFGENERIC form, and in order to
;;; get a clean build, we include the DEFGENERIC form here.
;;;
;;; Reflecting a Sheet's Geometry to the Mirror.
;;; FIXME: Improve the previous comment.
(defun sheet-mirror-region (sheet)
  (check-type sheet (or standard-full-mirrored-sheet-mixin standard-graft))
  (cond
    ;; For grafts or top-level-sheet's always read the mirror region
    ;; from the server, since it is not under our control.
    ((or (null (sheet-parent sheet))
         (null (sheet-parent (sheet-parent sheet))))
     (make-rectangle* 0 0 #x10000 #x10000))
    (t
     ;; For other sheets just use the calculated value, which saves a
     ;; round trip.
     (or (%sheet-mirror-region sheet)
         ;; XXX what to do if the sheet has no idea about its region?
         ;; XXX can we consider calling sheet-mirror-region then an error?
         (make-rectangle* 0 0 #x10000 #x10000) ))))

;;; The generic function EFFECTIVE-MIRROR-REGION is not part of the
;;; CLIM II specification.  For that reason, there is no DEFGENERIC
;;; form for it in decls.lisp.  Since some Common Lisp compilers emit
;;; a warning if there is no explicit DEFGENERIC form, and in order to
;;; get a clean build, we include the DEFGENERIC form here.
(defgeneric effective-mirror-region (sheet))

(defmethod effective-mirror-region ((sheet standard-full-mirrored-sheet-mixin))
  ;; XXX is this really needed, can't we deduce this information more easily?
  (let* ((parent (sheet-parent sheet))
         (ancestor (and parent (sheet-mirrored-ancestor parent))))
    (if ancestor
        (region-intersection (sheet-mirror-region sheet)
                             (untransform-region (%sheet-mirror-transformation sheet)
                                                 (effective-mirror-region ancestor)))
        (sheet-mirror-region sheet))))

(defmethod effective-mirror-region ((sheet standard-graft))
  (sheet-mirror-region sheet))
