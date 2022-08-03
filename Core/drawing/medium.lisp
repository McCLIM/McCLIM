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

(defmethod graphics-state-line-style-border ((record gs-line-style-mixin) (medium medium))
  (/ (line-style-effective-thickness (graphics-state-line-style record)
                                     medium)
     2))

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

(defmethod (setf graphics-state) ((new-gs graphics-state) (gs graphics-state))
  #+(or) "This is a no-op, but :after methods don't work without a primary method.")
(defmethod (setf graphics-state) :after ((new-gs gs-ink-mixin) (gs gs-ink-mixin))
  (setf (graphics-state-ink gs) (graphics-state-ink new-gs)))
(defmethod (setf graphics-state) :after ((new-gs gs-clip-mixin) (gs gs-clip-mixin))
  (setf (graphics-state-clip gs) (graphics-state-clip new-gs)))
(defmethod (setf graphics-state) :after ((new-gs gs-line-style-mixin) (gs gs-line-style-mixin))
  (setf (graphics-state-line-style gs) (graphics-state-line-style new-gs)))
(defmethod (setf graphics-state) :after ((new-gs gs-text-style-mixin) (gs gs-text-style-mixin))
  (setf (graphics-state-text-style gs) (graphics-state-text-style new-gs)))
(defmethod (setf graphics-state) :after ((new-gs gs-transformation-mixin) (gs gs-transformation-mixin))
  (setf (graphics-state-transformation gs) (graphics-state-transformation new-gs)))


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
   (buffering-p :initform nil
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
  ;; Initial CLIPPING-REGION is in coordinates given by the initial
  ;; TRANSFORMATION, but we store it in SHEET's coords.
  (with-slots (clipping-region) medium
    (setf clipping-region (transform-region (medium-transformation medium)
                                            clipping-region))))

(defmethod medium-clipping-region ((medium basic-medium))
  (untransform-region (medium-transformation medium)
                      (slot-value medium 'clipping-region)))

(defmethod (setf medium-clipping-region) (region (medium basic-medium))
  (setf (slot-value medium 'clipping-region)
        (transform-region (medium-transformation medium) region)))

(defmethod medium-merged-text-style ((medium medium))
  (merge-text-styles (medium-text-style medium) (medium-default-text-style medium)))

;;; Medium Device functions

(defmethod medium-device-transformation ((medium medium))
  (if-let ((sheet (medium-sheet medium)))
    (sheet-device-transformation sheet)
    (medium-transformation medium)))

(defmethod medium-device-region ((medium medium))
  (if-let ((sheet (medium-sheet medium)))
    (sheet-device-region sheet)
    (transform-region (medium-device-transformation medium)
                      (medium-clipping-region medium))))

(defmethod medium-native-transformation ((medium medium))
  (if-let ((sheet (medium-sheet medium)))
    (sheet-native-transformation sheet)
    +identity-transformation+))

(defmethod medium-native-region ((medium medium))
  (if-let ((sheet (medium-sheet medium)))
    (sheet-native-region sheet)
    (transform-region (compose-transformations (medium-native-transformation medium)
                                               (medium-transformation medium))
                      (medium-clipping-region medium))))


;;; Line-Style class

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
      (:point (if-let ((graft (graft medium)))
                (/ (graft-width graft)
                   (graft-width graft :units :inches)
                   72)
                1))
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

(defun medium-effective-line-style (medium)
  (let ((line-style (medium-line-style medium)))
    (if (eq (line-style-unit line-style) :coordinate)
        (make-line-style :unit :normal
                         :thickness (line-style-effective-thickness line-style medium)
                         :dashes (line-style-effective-dashes line-style medium)
                         :joint-shape (line-style-joint-shape line-style)
                         :cap-shape (line-style-cap-shape line-style))
        line-style)))

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
    ((medium basic-medium) continuation &optional (buffered-p t))
  (unwind-protect
       (letf (((medium-buffering-output-p medium) buffered-p))
         (unless buffered-p
           (medium-force-output medium))
         (funcall continuation))
    (unless (medium-buffering-output-p medium)
      (medium-force-output medium))))

;;; Default method.
(defmethod invoke-with-output-buffered
    (sheet continuation &optional (buffered-p t))
  (declare (ignore buffered-p))
  (funcall continuation))


;;; Pixmaps

(defmethod medium-copy-area ((from-drawable basic-medium) from-x from-y width height
                             to-drawable to-x to-y)
  (declare (ignore from-x from-y width height to-drawable to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))

(defmethod medium-copy-area (from-drawable from-x from-y width height
                             (to-drawable basic-medium) to-x to-y)
  (declare (ignore from-drawable from-x from-y width height to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))

(defmethod invoke-with-output-to-pixmap ((medium basic-medium) cont &key width height)
  (unless (and width height)
    (error "WITH-OUTPUT-TO-PIXMAP: please provide :WIDTH and :HEIGHT."))
  (let* ((port (port medium))
         (pixmap (allocate-pixmap medium width height))
         (pixmap-medium (make-medium port (medium-sheet medium)))
         (drawing-plane (make-rectangle* 0 0 width height)))
    (degraft-medium pixmap-medium port medium)
    (letf (((medium-drawable pixmap-medium) pixmap)
           ((medium-clipping-region pixmap-medium) drawing-plane))
      (medium-clear-area pixmap-medium 0 0 width height)
      (funcall cont pixmap-medium)
      pixmap)))


;;; Medium-specific Drawing Functions

;;; TRANSFORM-COORDINATES-MIXIN methods change the medium transformation to
;;; identity in order to avoid transforming coordinates multiple times by the
;;; backends. On the other hand LINE-STYLE-EFFECTIVE-{THICKNESS,DASHES} uses the
;;; medium transformation when the line unit is :COORDINATE. We address this
;;; issue by supplementing a line style with the unit :normal. -- jd 2022-04-05

(defmacro with-identity-transformation* ((medium &rest coords) &body body)
  (with-gensyms (transformation)
    `(let ((,transformation (medium-transformation ,medium)))
       (if (identity-transformation-p ,transformation)
           (progn ,@body)
           (with-transformed-positions* (,transformation ,@coords)
             (letf (((medium-line-style ,medium) (medium-effective-line-style ,medium))
                    ((medium-transformation ,medium) +identity-transformation+))
               ,@body))))))

(defmethod medium-draw-point* :around ((medium transform-coordinates-mixin) x y)
  (with-identity-transformation* (medium x y)
    (call-next-method medium x y)))

(defmethod medium-draw-points* :around ((medium transform-coordinates-mixin) coord-seq)
  (with-identity-transformation* (medium coord-seq)
    (call-next-method medium coord-seq)))

(defmethod medium-draw-line* :around ((medium transform-coordinates-mixin) x1 y1 x2 y2)
  (with-identity-transformation* (medium x1 y1 x2 y2)
    (call-next-method medium x1 y1 x2 y2)))

(defmethod medium-draw-lines* :around ((medium transform-coordinates-mixin) coord-seq)
  (with-identity-transformation* (medium coord-seq)
    (call-next-method medium coord-seq)))

(defmethod medium-draw-polygon* :around ((medium transform-coordinates-mixin) coord-seq closed filled)
  (with-identity-transformation* (medium coord-seq)
    (call-next-method medium coord-seq closed filled)))

(defmethod medium-draw-bezigon* :around ((medium transform-coordinates-mixin) coord-seq filled)
  (with-identity-transformation* (medium coord-seq)
    (call-next-method medium coord-seq filled)))

(defun expand-rectangle-coords (left top right bottom)
  "Expand the two corners of a rectangle into a polygon coord-seq"
  (vector left top right top right bottom left bottom))

(defmethod medium-draw-rectangle* :around ((medium transform-coordinates-mixin) x1 y1 x2 y2 filled)
  (if (rectilinear-transformation-p (medium-transformation medium))
      (with-identity-transformation* (medium x1 y1 x2 y2)
        (call-next-method medium (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2) filled))
      (medium-draw-polygon* medium (expand-rectangle-coords x1 y1 x2 y2) t filled)))

(defmethod medium-draw-rectangles* :around ((medium transform-coordinates-mixin) coord-seq filled)
  (if (rectilinear-transformation-p (medium-transformation medium))
      (with-identity-transformation* (medium coord-seq)
        (call-next-method medium coord-seq filled))
      (do-sequence ((x1 y1 x2 y2) coord-seq)
        (medium-draw-polygon* medium (vector x1 y1 x1 y2 x2 y2 x2 y1) t filled))))

(defmethod medium-draw-ellipse* :around ((medium transform-coordinates-mixin)
                                         cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 filled)
  (let ((tr (medium-transformation medium)))
    (with-identity-transformation* (medium)
      (if (identity-transformation-p tr)
          (call-next-method)
          (multiple-value-bind (cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2)
              (transform-ellipse tr cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2)
            (call-next-method medium cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 filled))))))

(defmethod medium-copy-area :around ((from-drawable transform-coordinates-mixin)
                                     from-x from-y width height
                                     (to-drawable transform-coordinates-mixin) to-x to-y)
  (with-identity-transformation* (from-drawable from-x from-y)
    (with-identity-transformation* (to-drawable to-x to-y)
      (call-next-method from-drawable from-x from-y width height to-drawable to-x to-y))))

(defmethod medium-copy-area :around ((from-drawable transform-coordinates-mixin)
                                     from-x from-y width height
                                     to-drawable to-x to-y)
  (with-identity-transformation* (from-drawable from-x from-y)
    (call-next-method from-drawable from-x from-y width height to-drawable to-x to-y)))

(defmethod medium-copy-area :around (from-drawable from-x from-y width height
                                     (to-drawable  transform-coordinates-mixin)
                                     to-x to-y)
  (with-identity-transformation* (to-drawable to-x to-y)
    (call-next-method from-drawable from-x from-y width height to-drawable to-x to-y)))

#+ (or) ;; This is not the right thing to do because the transformation is lost.
(defmethod medium-draw-text* :around ((medium transform-coordinates-mixin) string x y start end
                                      align-x align-y toward-x toward-y transform-glyphs)
  (with-identity-transformation* (medium x y toward-x toward-y)
    (call-next-method medium string x y start end align-x align-y toward-x toward-y transform-glyphs)))

;;; Fallback methods relying on MEDIUM-DRAW-POLYGON*

(defmethod medium-draw-point* ((medium basic-medium) x y)
  (let ((radius (line-style-effective-thickness (medium-line-style medium) medium)))
    (medium-draw-circle* medium x y radius 0 (* 2 pi) t)))

(defmethod medium-draw-line* ((medium basic-medium) x1 y1 x2 y2)
  (medium-draw-polygon* medium (list x1 y1 x2 y2) nil nil))

(defmethod medium-draw-rectangle* ((medium basic-medium) x1 y1 x2 y2 filled)
  (medium-draw-polygon* medium (list x1 y1 x2 y1 x2 y2 x1 y2) t filled))

(defmethod medium-draw-circle* ((medium basic-medium) cx cy radius eta1 eta2 filled)
  (medium-draw-ellipse* medium cx cy radius 0 0 radius eta1 eta2 filled))

(defmethod medium-draw-ellipse* ((medium basic-medium)
                                 cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 filled)
  (let ((coords (polygonalize-ellipse cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 :filled filled)))
    (medium-draw-polygon* medium coords nil filled)))

#+ (or)
(defmethod medium-draw-circle* ((medium basic-medium) cx cy radius eta1 eta2 filled)
  (medium-draw-ellipse* medium cx cy radius 0 0 radius eta1 eta2 filled))

(defmethod medium-draw-bezigon* ((medium basic-medium) coord-seq filled)
  (let ((polygon-coord-seq (polygonalize-bezigon coord-seq)))
    (medium-draw-polygon* medium polygon-coord-seq nil filled)))

;;; Fall-through Methods For Multiple Objects Drawing Functions

(defmethod medium-draw-points* ((medium transform-coordinates-mixin) coord-seq)
  (do-sequence ((x y) coord-seq)
    (medium-draw-point* medium x y)))

(defmethod medium-draw-lines* ((medium transform-coordinates-mixin) position-seq)
  (do-sequence ((x1 y1 x2 y2) position-seq)
    (medium-draw-line* medium x1 y1 x2 y2)))

(defmethod medium-draw-rectangles* ((medium transform-coordinates-mixin) coord-seq filled)
  (do-sequence ((x1 y1 x2 y2) coord-seq)
    (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium basic-medium))
  nil)

(defmethod medium-force-output ((medium basic-medium))
  nil)

(defmethod medium-clear-area ((medium basic-medium) left top right bottom)
  (draw-rectangle* medium left top right bottom
                   :ink (compose-over (medium-background medium) +black+)))

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

(defmethod make-medium ((port port) sheet)
  (make-instance 'basic-medium :sheet sheet :port port))

(defmethod deallocate-medium ((port port) medium)
  (declare (ignorable port medium))
  nil)

(defmethod graft ((medium basic-medium))
  (and (medium-sheet medium)
       (graft (medium-sheet medium))))
