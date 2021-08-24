;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2002-2003 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the graphics state, mediums and line styles.
;;;

(in-package #:clim-internals)

;;; GRAPHICS-STATE class

;;; Factor out the graphics state portions of the output records so
;;; they can be manipulated seperately e.g., by incremental
;;; display. The individual slots of a graphics state are factored
;;; into mixin classes so that each output record can capture only the
;;; state that it needs. -- moore
;;;
;;; Now graphics-state is an ancestor of both medium and some of
;;; output-records. Thanks to that we can treat medium as
;;; graphics-state without consing new objects and assign its state
;;; from another graphics-state object. -- jd


(defclass graphics-state ()
  ()
  (:documentation "Stores those parts of the medium/stream graphics state
  that need to be restored when drawing an output record"))

(defclass gs-transformation-mixin (graphics-state)
  ((transformation :initarg :transformation :accessor graphics-state-transformation
                   :documentation "Medium transformation.")))

(defmethod initialize-instance :after ((obj gs-transformation-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'transformation)))
    (setf (slot-value obj 'transformation) (graphics-state-transformation medium))))

(defclass gs-ink-mixin (graphics-state)
  ((ink :initarg :ink :accessor graphics-state-ink)))

(defmethod initialize-instance :after ((obj gs-ink-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'ink)))
    (setf (slot-value obj 'ink) (graphics-state-ink medium))))

(defclass gs-clip-mixin (graphics-state)
  ((clipping-region :initarg :clipping-region :accessor graphics-state-clip
                    :documentation "Clipping region in stream coordinates.")))

(defmethod initialize-instance :after ((obj gs-clip-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'clipping-region)))
    (setf (slot-value obj 'clipping-region) (graphics-state-clip medium))))

(defclass gs-line-style-mixin (graphics-state)
  ((line-style :initarg :line-style :accessor graphics-state-line-style)))

(defmethod initialize-instance :after ((obj gs-line-style-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'line-style)))
    (setf (slot-value obj 'line-style) (graphics-state-line-style medium))))

(defgeneric graphics-state-line-style-border (record medium)
  (:method ((record gs-line-style-mixin) (medium medium))
    (/ (line-style-effective-thickness (graphics-state-line-style record)
                                       medium)
       2))
  (:method ((record gs-line-style-mixin) (sheet sheet))
    (with-sheet-medium (medium sheet)
      (graphics-state-line-style-border record medium))))

(defclass gs-text-style-mixin (graphics-state)
  ((text-style :initarg :text-style :accessor graphics-state-text-style)))

(defmethod initialize-instance :after ((obj gs-text-style-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'text-style)))
    (setf (slot-value obj 'text-style) (graphics-state-text-style medium))))

(defclass complete-medium-state
    (gs-ink-mixin gs-clip-mixin gs-line-style-mixin gs-text-style-mixin gs-transformation-mixin)
  ())

(defgeneric (setf graphics-state) (new-gs gs)
  (:method ((new-gs graphics-state) (gs graphics-state))
    #+(or) "This is a no-op, but :after methods don't work without a primary method.")
  (:method :after ((new-gs gs-ink-mixin) (gs gs-ink-mixin))
    (setf (graphics-state-ink gs) (graphics-state-ink new-gs)))
  (:method :after ((new-gs gs-clip-mixin) (gs gs-clip-mixin))
    (setf (graphics-state-clip gs) (graphics-state-clip new-gs)))
  (:method :after ((new-gs gs-line-style-mixin) (gs gs-line-style-mixin))
    (setf (graphics-state-line-style gs) (graphics-state-line-style new-gs)))
  (:method :after ((new-gs gs-text-style-mixin) (gs gs-text-style-mixin))
    (setf (graphics-state-text-style gs) (graphics-state-text-style new-gs)))
  (:method :after ((new-gs gs-transformation-mixin) (gs gs-transformation-mixin))
    (setf (graphics-state-transformation gs) (graphics-state-transformation new-gs))))


;;; MEDIUM class

(defclass transform-coordinates-mixin ()
  ;; This class is reponsible for transforming coordinates in an :around method
  ;; on medium-draw-xyz. It is mixed into basic-medium. We should document it
  ;; and mix it in the appropriate downstream backend-specific medium.  Mixing
  ;; in basic-medium makes hardware-based transformations hard. -- jd 2018-03-06
  ())

(defclass basic-medium (transform-coordinates-mixin complete-medium-state medium)
  ((foreground :initarg :foreground
               :initform +black+
               :accessor medium-foreground
               :reader foreground)
   (background :initarg :background
               :initform +white+
               :accessor medium-background
               :reader background)
   (ink :initarg :ink
        :initform +foreground-ink+
        :accessor medium-ink)
   (transformation :type transformation
                   :initarg :transformation
                   :initform +identity-transformation+
                   :accessor medium-transformation)
   (clipping-region :type region
                    :initarg :clipping-region
                    :initform +everywhere+
                    :documentation "Clipping region in the SHEET coordinates.")
   ;; always use this slot through its accessor, since there may
   ;; be secondary methods on it -RS 2001-08-23
   (line-style :initarg :line-style
               :initform (make-line-style)
               :accessor medium-line-style)
   ;; always use this slot through its accessor, since there may
   ;; be secondary methods on it -RS 2001-08-23
   (text-style :initarg :text-style
               :initform *default-text-style*
               :accessor medium-text-style)
   (default-text-style :initarg :default-text-style
                       :initform *default-text-style*
                       :accessor medium-default-text-style)
   (sheet :initarg :sheet
          :initform nil                 ; this means that medium is not linked to a sheet
          :reader medium-sheet
          :writer (setf %medium-sheet) )
   (port :initarg :port
         :accessor port)
   (drawable :initform nil
             :accessor %medium-drawable)
   (buffering-p :initform t
                :accessor medium-buffering-output-p))
  (:documentation "The basic class, on which all CLIM mediums are built."))

(defmethod medium-drawable ((medium basic-medium))
  (or (%medium-drawable medium)
      (when-let ((sheet (medium-sheet medium)))
        (sheet-mirror sheet))))

(defmethod (setf medium-drawable) (new-drawable (medium basic-medium))
  (setf (%medium-drawable medium) new-drawable))

(defmethod initialize-instance :after ((medium basic-medium) &rest args)
  (declare (ignore args))
  ;; Initial CLIPPING-REGION is in coordinates, given by initial
  ;; TRANSFORMATION, but we store it in SHEET's coords.
  (with-slots (clipping-region) medium
    (setf clipping-region (transform-region (medium-transformation medium)
                                            clipping-region))))

(defmethod medium-clipping-region ((medium basic-medium))
  (untransform-region (medium-transformation medium)
                      (slot-value medium 'clipping-region)))

(defmethod (setf medium-clipping-region) (region (medium basic-medium))
  (setf (slot-value medium 'clipping-region)
        (transform-region (medium-transformation medium)
                          region)))

(defmethod (setf medium-clipping-region) :after (region (medium medium))
  (declare (ignore region))
  (when-let ((sheet (medium-sheet medium)))
    (%invalidate-cached-device-regions sheet)))

(defmethod (setf medium-transformation) :after (transformation (medium medium))
  (declare (ignore transformation))
  (when-let ((sheet (medium-sheet medium)))
    (%invalidate-cached-device-transformations sheet)))

(defmethod medium-merged-text-style ((medium medium))
  (merge-text-styles (medium-text-style medium) (medium-default-text-style medium)))

;;; Medium Device functions

(defgeneric medium-device-transformation (medium)
  (:method ((medium medium))
    (if-let ((sheet (medium-sheet medium)))
      (sheet-device-transformation sheet)
      (medium-transformation medium))))

(defgeneric medium-device-region (medium)
  (:method ((medium medium))
    (if-let ((sheet (medium-sheet medium)))
      (sheet-device-region sheet)
      (medium-clipping-region medium))))

(defgeneric medium-native-transformation (medium)
  (:method ((medium medium))
    (if-let ((sheet (medium-sheet medium)))
      (sheet-native-transformation sheet)
      +identity-transformation+)))

(defgeneric medium-native-region (medium)
  (:method ((medium medium))
    (if-let ((sheet (medium-sheet medium)))
      (sheet-native-region sheet)
      (transform-region (medium-transformation medium)
                        (medium-clipping-region medium)))))


;;; Line-Style class

(defgeneric line-style-equalp (arg1 arg2))

(defclass standard-line-style (line-style)
  ((unit        :initarg :line-unit
                :initform :normal
                :reader line-style-unit
                :type (member :normal :point :coordinate))
   (thickness   :initarg :line-thickness
                :initform 1
                :reader line-style-thickness
                :type real)
   (joint-shape :initarg :line-joint-shape
                :initform :miter
                :reader line-style-joint-shape
                :type (member :miter :bevel :round :none))
   (cap-shape   :initarg :line-cap-shape
                :initform :butt
                :reader line-style-cap-shape
                :type (member :butt :square :round :no-end-point))
   (dashes      :initarg :line-dashes
                :initform nil
                :reader line-style-dashes
                :type (or (member t nil)
                          sequence))))

(defun make-line-style (&key (unit :normal) (thickness 1)
                          (joint-shape :miter) (cap-shape :butt)
                          (dashes nil))
  (make-instance 'standard-line-style
                 :line-unit unit
                 :line-thickness thickness
                 :line-joint-shape joint-shape
                 :line-cap-shape cap-shape
                 :line-dashes dashes))

(defmethod print-object ((self standard-line-style) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~{~S ~S~^ ~}"
            (mapcan (lambda (slot)
                      (when (slot-boundp self slot)
                        (list
                         (intern (symbol-name slot) :keyword)
                         (slot-value self slot))))
                    '(unit thickness joint-shape cap-shape dashes)))))

(defun line-style-scale (line-style medium)
  (let ((unit (line-style-unit line-style)))
    (ecase unit
      (:normal 1)
      (:point (let ((graft (graft medium)))
                (/ (graft-width graft)
                   (graft-width graft :units :inches)
                   72)))
      (:coordinate (let ((transformation (medium-transformation medium)))
                     (if (identity-transformation-p transformation)
                         1
                         (multiple-value-bind (x y)
                             (transform-distance transformation 0.71 0.71)
                           (sqrt (+ (expt x 2) (expt y 2))))))))))

(defmethod line-style-effective-thickness (line-style medium)
  (* (line-style-thickness line-style)
     (line-style-scale line-style medium)))

(defmethod line-style-effective-dashes (line-style medium)
  (when-let ((dashes (line-style-dashes line-style)))
    (cond ((not (eq (line-style-unit line-style) :normal))
           (let ((scale (line-style-scale line-style medium)))
             (flet ((scale (length)
                      (* scale length)))
               (declare (dynamic-extent #'scale))
               (if (eq dashes t)
                   (let ((scaled (scale 3))) ; arbitrary default length
                     (list scaled scaled))
                   (map 'list #'scale dashes)))))
          ((eq dashes t)
           '(3 3))
          (t
           dashes))))

(defmethod medium-miter-limit ((medium medium))
  #.(* 2 single-float-epsilon))

(defmethod line-style-equalp ((style1 standard-line-style)
                              (style2 standard-line-style))
  (and (eql (line-style-unit style1) (line-style-unit style2))
       (eql (line-style-thickness style1) (line-style-thickness style2))
       (eql (line-style-joint-shape style1) (line-style-joint-shape style2))
       (eql (line-style-cap-shape style1) (line-style-cap-shape style2))
       (eql (line-style-dashes style1) (line-style-dashes style2))))


;;; Misc ops
(defmacro with-output-buffered ((medium &optional (buffer-p t)) &body body)
  (with-gensyms (cont)
    `(flet ((,cont () ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-output-buffered ,medium (function ,cont) ,buffer-p))))

(defmethod invoke-with-output-buffered
    ((sheet sheet) continuation &optional (buffered-p t))
  (with-sheet-medium (medium sheet)
    (invoke-with-output-buffered medium continuation buffered-p)))

(defmethod invoke-with-output-buffered
    ((medium basic-medium) continuation &optional (buffered-p t))
  (unwind-protect
       (letf (((medium-buffering-output-p medium) buffered-p))
         (unless buffered-p
           (medium-force-output medium))
         (funcall continuation))
    (medium-force-output medium)))


;;; BASIC-MEDIUM class

(defmacro with-transformed-position ((transformation x y) &body body)
  `(multiple-value-bind (,x ,y) (transform-position ,transformation ,x ,y)
     ,@body))

(defmacro with-transformed-distance ((transformation dx dy) &body body)
  `(multiple-value-bind (,dx ,dy) (transform-distance ,transformation ,dx ,dy)
     ,@body))

(defmacro with-transformed-angles
    ((transformation clockwisep &rest angles) &body body)
  (let ((op (if clockwisep 'transform-angle 'untransform-angle)))
    `(let ,(loop for angle in angles
                 collect `(,angle (,op ,transformation ,angle)))
       ,@body)))

(defmacro with-transformed-positions ((transformation coord-seq) &body body)
  `(let ((,coord-seq (transform-positions ,transformation ,coord-seq)))
     ,@body))


;;; Pixmaps

(defmethod medium-copy-area ((from-drawable basic-medium) from-x from-y width height
                             to-drawable to-x to-y)
  (declare (ignore from-x from-y width height to-drawable to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))

(defmethod medium-copy-area (from-drawable from-x from-y width height
                             (to-drawable basic-medium) to-x to-y)
  (declare (ignore from-drawable from-x from-y width height to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))


;;; Medium-specific Drawing Functions

(defmethod medium-draw-point* :around ((medium transform-coordinates-mixin) x y)
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x y)
      (call-next-method medium x y))))

(defmethod medium-draw-points* :around ((medium transform-coordinates-mixin) coord-seq)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
      (call-next-method medium coord-seq))))

(defmethod medium-draw-line* :around ((medium transform-coordinates-mixin) x1 y1 x2 y2)
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (call-next-method medium x1 y1 x2 y2)))))

(defmethod medium-draw-lines* :around ((medium transform-coordinates-mixin) coord-seq)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
      (call-next-method medium coord-seq))))

(defmethod medium-draw-polygon* :around ((medium transform-coordinates-mixin) coord-seq closed filled)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
      (call-next-method medium coord-seq closed filled))))

(defmethod medium-draw-bezigon* :around ((medium transform-coordinates-mixin) coord-seq filled)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
      (call-next-method medium coord-seq filled))))

(defmethod medium-draw-bezigon* ((medium basic-medium) coord-seq filled)
  (let ((polygon-coord-seq (polygonalize* coord-seq)))
    (with-identity-transformation (medium)
      (medium-draw-polygon* medium polygon-coord-seq nil filled))))

(defun expand-rectangle-coords (left top right bottom)
  "Expand the two corners of a rectangle into a polygon coord-seq"
  (vector left top right top right bottom left bottom))

(defmethod medium-draw-rectangle* :around ((medium transform-coordinates-mixin) left top right bottom filled)
  (let ((tr (medium-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (multiple-value-bind (left top right bottom)
            (transform-rectangle* tr left top right bottom)
          (call-next-method medium left top right bottom filled))
        (medium-draw-polygon* medium (expand-rectangle-coords left top right bottom)
                              t filled))) )

(defmethod medium-draw-rectangles* :around ((medium transform-coordinates-mixin) position-seq filled)
  (let ((tr (medium-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (call-next-method medium (transform-positions tr position-seq) filled)
        (do-sequence ((left top right bottom) position-seq)
          (medium-draw-polygon* medium (vector left top
                                               left bottom
                                               right bottom
                                               right top)
                                t filled)))))

(defmethod medium-draw-ellipse* :around (medium center-x center-y
                                         radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                         start-angle end-angle filled)
  (when (<= (abs (- (mod start-angle (* 2 pi)) (mod end-angle (* 2 pi)))) short-float-epsilon)
    (setf start-angle 0
          end-angle (* 2 pi)))
  (call-next-method))

(defmethod medium-draw-ellipse* :around ((medium transform-coordinates-mixin) center-x center-y
                                         radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                         start-angle end-angle filled)
  (let* ((ellipse (make-elliptical-arc* center-x center-y
                                        radius-1-dx radius-1-dy
                                        radius-2-dx radius-2-dy
                                        :start-angle start-angle
                                        :end-angle end-angle))
         (transformed-ellipse (transform-region (medium-transformation medium)
                                                ellipse))
         (start-angle (ellipse-start-angle transformed-ellipse))
         (end-angle (ellipse-end-angle transformed-ellipse)))
    (multiple-value-bind (center-x center-y) (ellipse-center-point* transformed-ellipse)
      (multiple-value-bind (radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
          (ellipse-radii transformed-ellipse)
        (call-next-method medium center-x center-y
                          radius-1-dx radius-1-dy
                          radius-2-dx radius-2-dy
                          start-angle end-angle filled)))))

(defmethod medium-copy-area :around ((from-drawable transform-coordinates-mixin)
                                     from-x from-y width height
                                     to-drawable to-x to-y)
  (with-transformed-position ((medium-transformation from-drawable)
                              from-x from-y)
    (call-next-method from-drawable from-x from-y width height
                      to-drawable to-x to-y)))

(defmethod medium-copy-area :around (from-drawable from-x from-y width height
                                     (to-drawable  transform-coordinates-mixin)
                                     to-x to-y)
  (with-transformed-position ((medium-transformation to-drawable)
                              to-x to-y)
    (call-next-method from-drawable from-x from-y width height
                      to-drawable to-x to-y)))

;;; Fall-through Methods For Multiple Objects Drawing Functions

(defmethod medium-draw-points* ((medium transform-coordinates-mixin) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (do-sequence ((x y) coord-seq)
        (medium-draw-point* medium x y)))))

(defmethod medium-draw-lines* ((medium transform-coordinates-mixin) position-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr position-seq)
      (do-sequence ((x1 y1 x2 y2) position-seq)
        (medium-draw-line* medium x1 y1 x2 y2)))))

(defmethod medium-draw-rectangles* ((medium transform-coordinates-mixin) coord-seq filled)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (do-sequence ((x1 y1 x2 y2) coord-seq)
        (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium basic-medium))
  nil)

(defmethod medium-force-output ((medium basic-medium))
  nil)

(defmethod medium-clear-area ((medium basic-medium) left top right bottom)
  (draw-rectangle* medium left top right bottom :ink +background-ink+))

(defmethod medium-beep ((medium basic-medium))
  nil)

;;;;;;;;;

(defmethod engraft-medium ((medium basic-medium) port sheet)
  (declare (ignore port))
  (setf (%medium-sheet medium) sheet))

(defmethod degraft-medium ((medium basic-medium) port sheet)
  (declare (ignore port sheet))
  (setf (%medium-sheet medium) nil))

(defmethod allocate-medium ((port port) sheet)
  (make-medium port sheet))

(defmethod deallocate-medium ((port port) medium)
  (declare (ignorable port medium))
  nil)

(defmethod graft ((medium basic-medium))
  (and (medium-sheet medium)
       (graft (medium-sheet medium))))
