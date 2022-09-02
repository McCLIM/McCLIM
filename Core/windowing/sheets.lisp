;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000 by Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) Copyright 2000 by Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) Copyright 2000,2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) Copyright 2001 by Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) Copyright 2001 by Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) Copyright 2021 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The sheet protocol
;;;

(in-package #:clim-internals)

(defgeneric raise-sheet-internal (sheet parent))
(defgeneric bury-sheet-internal (sheet parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; non standard protocol

(defgeneric %invalidate-cached-device-transformations (sheet))
(defgeneric %invalidate-cached-device-regions (sheet)
  (:method (sheet)
    (declare (ignore sheet))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; conditions

(define-condition sheet-is-not-child (error) ())
(define-condition sheet-is-top-level (error) ())
(define-condition sheet-ordering-underspecified (error) ())
(define-condition sheet-is-not-ancestor (error) ())
(define-condition sheet-already-has-parent (error) ())
(define-condition sheet-is-ancestor (error) ())

(define-condition sheet-supports-only-one-child (error)
  ((sheet :initarg :sheet)))

(defmethod print-object ((object sheet-supports-only-one-child) stream)
  (format stream "~A~%single-child-composite-pane is allowed to have only one child."
          (slot-value object 'sheet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; sheet protocol class

(defclass basic-sheet (sheet)
  ((region :type region
           :initarg :region
           :initform (make-bounding-rectangle 0 0 100 100)
           :accessor sheet-region)
   (native-transformation :type (or null transformation)
                          :initform nil
                          :accessor %sheet-native-transformation)
   (native-region :type (or null region)
                  :initarg :native-region
                  :initform nil
                  :accessor %sheet-native-region)
   (device-transformation :type (or null transformation)
                          :initform nil)
   (device-region :type (or null region)
                  :initform nil)
   (pointer-cursor :accessor sheet-pointer-cursor
                   :initarg  :pointer-cursor
                   :initform :default)
   (enabled-p :type boolean
              :initarg :enabled-p
              :initform t
              :accessor sheet-enabled-p)))

(defmethod sheet-parent ((sheet basic-sheet))
  nil)

(defmethod sheet-children ((sheet basic-sheet))
  nil)

;;; This method is a canary which signals, that something is wrong
;;; with the inheritance in the sheet class, i.e that basic-sheet is
;;; before the sheet-multiple-child-mixin in the cpl. -- jd 2020-01-20
(defmethod sheet-adopt-child ((sheet basic-sheet) (child sheet))
  (error "~S attempting to adopt ~S." sheet child))

(defmethod sheet-adopt-child :after ((sheet basic-sheet) (child sheet))
  (note-sheet-adopted child)
  (when (sheet-grafted-p sheet)
    (note-sheet-grafted child)))

(defmethod sheet-disown-child :before
    ((sheet basic-sheet) (child sheet) &key (errorp t))
  (when (and (not (member child (sheet-children sheet))) errorp)
    (error 'sheet-is-not-child)))

(defmethod sheet-disown-child :after
    ((sheet basic-sheet) (child sheet) &key (errorp t))
  (declare (ignore errorp))
  (note-sheet-disowned child)
  (when (sheet-grafted-p sheet)
    (note-sheet-degrafted child)))

(defmethod sheet-siblings ((sheet basic-sheet))
  (when (not (sheet-parent sheet))
    (error 'sheet-is-not-child))
  (remove sheet (sheet-children (sheet-parent sheet))))

(defmethod sheet-enabled-children ((sheet basic-sheet))
  (delete-if-not #'sheet-enabled-p (copy-list (sheet-children sheet))))

(defmethod sheet-ancestor-p ((sheet basic-sheet)
                             (putative-ancestor sheet))
  (or (eq sheet putative-ancestor)
      (and (sheet-parent sheet)
           (sheet-ancestor-p (sheet-parent sheet) putative-ancestor))))

(defmethod raise-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(defmethod bury-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(defmethod shrink-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-top-level))

(defmethod reorder-sheets ((sheet basic-sheet) new-ordering)
  (when (set-difference (sheet-children sheet) new-ordering)
    (error 'sheet-ordering-underspecified))
  (when (set-difference new-ordering (sheet-children sheet))
    (error 'sheet-is-not-child))
  (setf (sheet-children sheet) new-ordering)
  sheet)

(defmethod sheet-viewable-p ((sheet basic-sheet))
  (and (sheet-parent sheet)
       (sheet-enabled-p sheet)
       (sheet-viewable-p (sheet-parent sheet))))

(defmethod sheet-occluding-sheets ((sheet basic-sheet) (child sheet))
  (labels ((fun (l)
                (cond ((eq (car l) child) '())
                      ((and (sheet-enabled-p (car l))
                            (region-intersects-region-p
                             (sheet-region (car l)) (sheet-region child)))
                       (cons (car l) (fun (cdr l))))
                      (t (fun (cdr l))))))
    (fun (sheet-children sheet))))

(defmethod map-over-sheets (function (sheet basic-sheet))
  (funcall function sheet)
  (map nil
       #'(lambda (child)
           (map-over-sheets function child))
       (sheet-children sheet)))

;;; Instead of defining yet another function we specialize on
;;; sequence. Thanks to that we can map over "all-but-parent" sheets
;;; with `(map-over-sheets function (sheet-children sheet))'.
(defmethod map-over-sheets (function (sheets list))
  (map nil
       #'(lambda (child)
           (map-over-sheets function child))
       sheets))

(defmethod (setf sheet-enabled-p) :around (enabled-p (sheet basic-sheet))
  (unless (eql enabled-p (sheet-enabled-p sheet))
    (call-next-method)
    (if enabled-p
        (note-sheet-enabled sheet)
        (note-sheet-disabled sheet))
    (dispatch-repaint (sheet-parent sheet)
                      (transform-region (sheet-transformation sheet)
                                        (sheet-region sheet)))))

(defmethod sheet-transformation ((sheet basic-sheet))
  (error "Attempting to get the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod (setf sheet-transformation) (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (error "Attempting to set the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod move-sheet ((sheet basic-sheet) x y)
  (let ((transf (sheet-transformation sheet))
        (region (sheet-region sheet)))
    (multiple-value-bind (old-x old-y)
        (bounding-rectangle-position (transform-region transf region))
      (unless (and (coordinate= old-x x)
                   (coordinate= old-y y))
        (let ((dx (- x old-x))
              (dy (- y old-y)))
         (setf (sheet-transformation sheet)
               (compose-transformation-with-translation transf dx dy)))))))

;;; RESIZE-SHEET dimensions WIDTH and HEIGHT are expressed in the device
;;; coordinates. When we resize the sheet its region is scaled without changing
;;; the transformation except for the following situations:
;;;
;;; - old-width=0 or old-height=0 we can't compute sx or sy
;;;
;;; - new-width=0 or new-height=0 we can't transform the region because it will
;;;   be canonicalized to +nowhere+ and the sheet position will be lost.
;;;
;;; In both cases we throw in the towel and replace the old region with a
;;; bounding rectangle (to preserve a position of the sheet). -- jd 2021-02-24
(defmethod resize-sheet ((sheet basic-sheet) width height)
  (let* ((region (sheet-region sheet))
         (transf (sheet-device-transformation sheet))
         (region* (transform-region transf region)))
    (with-bounding-rectangle* (x1 y1 x2 y2) region*
      (let ((new-width (max width 0))
            (new-height (max height 0))
            (old-width (- x2 x1))
            (old-height (- y2 y1)))
        (setf (sheet-region sheet)
              (if (or (= old-width 0) (= old-height 0)
                      (= new-width 0) (= new-height 0))
                  (multiple-value-bind (x1 y1)
                      (bounding-rectangle-position region)
                    (make-bounding-rectangle
                     x1 y1 (+ x1 new-width) (+ y1 new-height)))
                  (let* ((sx (/ new-width old-width))
                         (sy (/ new-height old-height))
                         (transf* (make-scaling-transformation* sx sy x1 y1))
                         (resized-region* (transform-region transf* region*)))
                    (untransform-region transf resized-region*))))))))

(defmethod move-and-resize-sheet ((sheet basic-sheet) x y width height)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (%set-sheet-region-and-transformation
       sheet
       (make-bounding-rectangle 0 0 width height)
       (compose-transformation-with-translation transform (- x old-x) (- y old-y))))))

(defmethod map-sheet-position-to-parent ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-position-to-child ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-parent ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-child ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-over-sheets-containing-position (function (sheet basic-sheet) x y)
  (map () #'(lambda (child)
              (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
                (when (region-contains-position-p (sheet-region child) tx ty)
                  (funcall function child))))
       (sheet-children sheet)))

(defmethod map-over-sheets-overlapping-region (function (sheet basic-sheet) region)
  (map () #'(lambda (child)
              (when (region-intersects-region-p
                     region
                     (transform-region
                      (if (eq child sheet)
                          +identity-transformation+
                          (sheet-transformation child))
                      (sheet-region child)))
                (funcall function child)))
       (sheet-children sheet)))

(defmethod child-containing-position ((sheet basic-sheet) x y)
  (loop for child in (sheet-children sheet)
        do (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
             (when (and (sheet-enabled-p child)
                        (region-contains-position-p (sheet-region child) tx ty))
               (return child)))))

(defmethod children-overlapping-region ((sheet basic-sheet) (region region))
  (loop for child in (sheet-children sheet)
        if (and (sheet-enabled-p child)
                (region-intersects-region-p
                 region
                 (transform-region (sheet-transformation child)
                                   (sheet-region child))))
          collect child))

(defmethod children-overlapping-rectangle* ((sheet basic-sheet) x1 y1 x2 y2)
  (children-overlapping-region sheet (make-rectangle* x1 y1 x2 y2)))

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor (eql nil)))
  (cond ((sheet-parent sheet)
         (compose-transformations (sheet-transformation sheet)
                                  (sheet-delta-transformation
                                   (sheet-parent sheet) ancestor)))
        (t +identity-transformation+)))

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor sheet))
  (cond ((eq sheet ancestor) +identity-transformation+)
        ((sheet-parent sheet)
         (compose-transformations (sheet-transformation sheet)
                                  (sheet-delta-transformation
                                   (sheet-parent sheet) ancestor)))
        (t (error 'sheet-is-not-ancestor))))

(defmethod sheet-allocated-region ((sheet basic-sheet) (child sheet))
  (reduce #'region-difference
          (mapcar #'(lambda (child)
                      (transform-region (sheet-transformation child)
                                        (sheet-region child)))
                  (cons child (sheet-occluding-sheets sheet child)))))

(defmethod sheet-direct-mirror ((sheet basic-sheet))
  nil)

(defmethod sheet-mirrored-ancestor ((sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (if (null parent)
        nil
        (sheet-mirrored-ancestor parent))))

(defmethod sheet-mirror ((sheet basic-sheet))
  (let ((mirrored-ancestor (sheet-mirrored-ancestor sheet)))
    (if (null mirrored-ancestor)
        nil
        (sheet-direct-mirror mirrored-ancestor))))

(defmethod graft ((sheet basic-sheet))
  nil)

(defmethod note-sheet-grafted ((sheet basic-sheet))
  (mapc #'note-sheet-grafted (sheet-children sheet)))

(defmethod note-sheet-degrafted ((sheet basic-sheet))
  (mapc #'note-sheet-degrafted (sheet-children sheet)))

(defmethod note-sheet-adopted ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disowned ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-enabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-region-changed ((sheet basic-sheet))
  nil)

(defmethod note-sheet-transformation-changed ((sheet basic-sheet))
  nil)

(defmethod sheet-native-transformation ((sheet basic-sheet))
  (or (slot-value sheet 'native-transformation)
      (setf (slot-value sheet 'native-transformation)
            (if-let ((parent (sheet-parent sheet)))
              (compose-transformations
               (sheet-native-transformation parent)
               (sheet-transformation sheet))
              +identity-transformation+))))

;;; Native region is volatile, and is only computed at the first
;;; request when it's equal to nil.
(defmethod sheet-native-region ((sheet basic-sheet))
  (or (slot-value sheet 'native-region)
      (setf (slot-value sheet 'native-region)
            (let ((this-native-region (transform-region
                                       (sheet-native-transformation sheet)
                                       (sheet-region sheet)))
                  (parent (sheet-parent sheet)))
              (if (null parent)
                  this-native-region
                  (region-intersection this-native-region
                                       (sheet-native-region parent)))))))

(defmethod sheet-device-transformation ((sheet basic-sheet))
  (or (slot-value sheet 'device-transformation)
      (setf (slot-value sheet 'device-transformation)
            (let ((medium (sheet-medium sheet)))
              (compose-transformations
               (sheet-native-transformation sheet)
               (if medium
                   (medium-transformation medium)
                   +identity-transformation+))))))

(defmethod sheet-device-region ((sheet basic-sheet))
  (or (slot-value sheet 'device-region)
      (setf (slot-value sheet 'device-region)
            (if-let ((medium (sheet-medium sheet)))
              (region-intersection
               (sheet-native-region sheet)
               (transform-region (sheet-device-transformation sheet)
                                 (medium-clipping-region medium)))
              (sheet-native-region sheet)))))

(defmethod invalidate-cached-transformations ((sheet basic-sheet))
  (with-slots (native-transformation device-transformation) sheet
    (setf native-transformation nil
          device-transformation nil))
  (mapc #'invalidate-cached-transformations (sheet-children sheet)))

(defmethod invalidate-cached-regions ((sheet basic-sheet))
  (with-slots (native-region device-region) sheet
    (setf native-region nil
          device-region nil))
  (mapc #'invalidate-cached-regions (sheet-children sheet)))

(defmethod %invalidate-cached-device-transformations ((sheet basic-sheet))
  (with-slots (device-transformation) sheet
    (setf device-transformation nil))
  (mapc #'%invalidate-cached-device-transformations (sheet-children sheet)))

(defmethod %invalidate-cached-device-regions ((sheet basic-sheet))
  (with-slots (device-region) sheet
    (setf device-region nil))
  (mapc #'%invalidate-cached-device-regions (sheet-children sheet)))

(defmethod (setf sheet-transformation) :after (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (invalidate-cached-transformations sheet)
  (invalidate-cached-regions sheet)
  (map-over-sheets #'(lambda (sheet)
                       (when (sheet-direct-mirror sheet)
                         (update-mirror-geometry sheet)))
                   sheet)
  (note-sheet-transformation-changed sheet))

(defmethod (setf sheet-region) :after (region (sheet basic-sheet))
  (declare (ignore region))
  (invalidate-cached-regions sheet)
  (map-over-sheets #'(lambda (sheet)
                       (when (sheet-direct-mirror sheet)
                         (update-mirror-geometry sheet)))
                   sheet)
  (note-sheet-region-changed sheet))

(defmethod (setf medium-clipping-region) :after (region (medium medium))
  (declare (ignore region))
  (when-let ((sheet (medium-sheet medium)))
    (%invalidate-cached-device-regions sheet)))

(defmethod (setf medium-transformation) :after (transformation (medium medium))
  (declare (ignore transformation))
  (when-let ((sheet (medium-sheet medium)))
    (%invalidate-cached-device-transformations sheet)))

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet basic-sheet))
  (declare (ignore cursor))
  (unless (sheet-direct-mirror sheet)
    (let ((msheet (sheet-mirrored-ancestor sheet)))
      (set-sheet-pointer-cursor (port msheet) msheet (sheet-pointer-cursor msheet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet parent mixin

(defclass sheet-parent-mixin ()
  ((parent :initform nil :accessor sheet-parent)))

(defmethod sheet-adopt-child :before (sheet (child sheet-parent-mixin))
  (when (and (sheet-parent child) (not (eq sheet (sheet-parent child))))
    (error 'sheet-already-has-parent))
  (when (sheet-ancestor-p sheet child)
    (error 'sheet-is-ancestor)))

(defmethod sheet-adopt-child :after (sheet (child sheet-parent-mixin))
  (setf (sheet-parent child) sheet))

(defmethod sheet-disown-child :after (sheet
                                      (child sheet-parent-mixin)
                                      &key (errorp t))
  (declare (ignore sheet errorp))
  (setf (sheet-parent child) nil))

(defmethod raise-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (raise-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (raise-mirror (port sheet) sheet)))

(defmethod bury-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (bury-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (bury-mirror (port sheet) sheet)))

(defmethod graft ((sheet sheet-parent-mixin))
  (and (sheet-parent sheet) (graft (sheet-parent sheet))))

(defmethod map-sheet-position-to-parent ((sheet sheet-parent-mixin) x y)
  (transform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-position-to-child ((sheet sheet-parent-mixin) x y)
  (untransform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-rectangle*-to-parent
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (transform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

(defmethod map-sheet-rectangle*-to-child
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (untransform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet leaf mixin

(defclass sheet-leaf-mixin () ())

(defmethod sheet-children ((sheet sheet-leaf-mixin))
  nil)

(defmethod sheet-adopt-child ((sheet sheet-leaf-mixin) (child sheet))
  (error "Leaf sheet attempting to adopt a child"))

(defmethod sheet-disown-child
    ((sheet sheet-leaf-mixin) (child sheet) &key (errorp t))
  (declare (ignorable errorp))
  (error "Leaf sheet attempting to disown a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet single child mixin

(defclass sheet-single-child-mixin ()
  ((child :initform nil :accessor sheet-child)))

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (and (sheet-child sheet) (list (sheet-child sheet))))

(defmethod sheet-adopt-child :before ((sheet sheet-single-child-mixin)
                                      (child sheet-parent-mixin))
  (when (sheet-child sheet)
    (error 'sheet-supports-only-one-child :sheet sheet)))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin)
                              (child sheet-parent-mixin))
  (setf (sheet-child sheet) child))

(defmethod sheet-disown-child ((sheet sheet-single-child-mixin)
                               (child sheet-parent-mixin)
                               &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-child sheet) nil))

(defmethod raise-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))

(defmethod bury-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet multiple child mixin

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil :accessor sheet-children)))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin)
                              (child sheet-parent-mixin))
  (push child (sheet-children sheet)))

(defmethod sheet-disown-child ((sheet sheet-multiple-child-mixin)
                               (child sheet-parent-mixin)
                               &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-children sheet) (delete child (sheet-children sheet))))

(defmethod raise-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
        (cons sheet (delete sheet (sheet-children parent)))))

(defmethod bury-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
        (append (delete sheet (sheet-children parent)) (list  sheet))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet geometry classes

(defclass sheet-identity-transformation-mixin ()
  ())

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defclass sheet-transformation-mixin ()
  ((transformation :initform +identity-transformation+
                   :initarg :transformation
                   :accessor sheet-transformation)))

(defclass sheet-translation-mixin (sheet-transformation-mixin)
  ())

(defmethod (setf sheet-transformation) :before
    ((transformation transformation) (sheet sheet-translation-mixin))
  (unless (translation-transformation-p transformation)
    (error "Attempting to set the SHEET-TRANSFORMATION of a ~
            SHEET-TRANSLATION-TRANSFORMATION-MIXIN to a non ~
            translation transformation")))

(defclass sheet-y-inverting-transformation-mixin (sheet-transformation-mixin)
  ()
  (:default-initargs :transformation (make-transformation 1 0 0 -1 0 0)))

(defmethod (setf sheet-transformation) :before
    ((transformation transformation)
     (sheet sheet-y-inverting-transformation-mixin))
  (unless (y-inverting-transformation-p transformation)
    (error "Attempting to set the SHEET-TRANSFORMATION of a ~
            SHEET-Y-INVERTING-TRANSFORMATION-MIXIN to a non Y ~
            inverting transformation")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirrored sheet

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

(defclass mirrored-sheet-mixin ()
  ((port
    :initform nil
    :initarg :port
    :accessor port)
   (mirror
    :initform nil
    :reader sheet-direct-mirror
    :writer (setf %sheet-direct-mirror))
   (native-transformation
    :initform +identity-transformation+)
   (native-region
    :initform +nowhere+)
   (mirror-geometry
    :initform (make-bounding-rectangle -5 -5 1 1)
    :accessor sheet-mirror-geometry
    :documentation "Our idea of the current mirror geometry. Might not be
correct if a foreign application changes our mirror's geometry. Also note that
this might be different from the sheet's native region and transformation.")))

(defmethod sheet-mirrored-ancestor ((sheet mirrored-sheet-mixin))
  sheet)

(defmethod sheet-mirror ((sheet mirrored-sheet-mixin))
  (sheet-direct-mirror sheet))

(defmethod note-sheet-grafted :before ((sheet mirrored-sheet-mixin))
  (unless (port sheet)
    (error "~S called on sheet ~S, which has no port?!" 'note-sheet-grafted sheet))
  (realize-mirror (port sheet) sheet))

(defmethod note-sheet-degrafted :after ((sheet mirrored-sheet-mixin))
  (destroy-mirror (port sheet) sheet))

(defmethod (setf sheet-enabled-p) :after
    (new-value (sheet mirrored-sheet-mixin))
  (if new-value
      (port-enable-sheet (port sheet) sheet)
      (port-disable-sheet (port sheet) sheet)))

(defmethod (setf sheet-pretty-name) :after (new-name (sheet mirrored-sheet-mixin))
  (port-set-mirror-name (port sheet) sheet new-name))

(defmethod (setf sheet-icon) :after (new-value (sheet mirrored-sheet-mixin))
  (port-set-mirror-icon (port sheet) sheet new-value))

(defmethod invalidate-cached-transformations ((sheet mirrored-sheet-mixin))
  (with-slots (native-transformation device-transformation) sheet
    ;; (setf native-transformation nil)
    (setf device-transformation nil))
  (mapc #'invalidate-cached-transformations (sheet-children sheet)))

;;; Coordinate swizzling

;;; UPDATE-MIRROR-REGION is responsible for setting the native region and the
;;; native transformation of the mirrored sheet. The native transformation is
;;; NIL when the mirror is not visible.

(defmethod sheet-native-region ((sheet mirrored-sheet-mixin))
  (unless (%sheet-native-transformation sheet)
    (return-from sheet-native-region +everywhere+))
  (or (%sheet-native-region sheet) +everywhere+))

(defmethod sheet-native-transformation ((sheet mirrored-sheet-mixin))
  (or (%sheet-native-transformation sheet) +identity-transformation+))

;;; Top-level sheets

(defclass top-level-sheet-mixin ()
  ((focused-sheet :initform nil :accessor focused-sheet)
   ;; The NAME slot intentionally uses the same slot name as the NAME
   ;; in the PANE class so that both collapse into a single effective
   ;; slot in e.g. the TOP-LEVEL-SHEET-PANE class.
   (name :initarg :name :reader sheet-name)
   (%pretty-name :initarg :pretty-name :accessor clime:sheet-pretty-name)
   (icon :initarg :icon :accessor sheet-icon
         :documentation "If non-NIL, an array pattern or a sequence of
                         array patterns that should be used by the
                         host's window manager to represent the sheet,
                         for example when its mirror is iconified."))
  (:default-initargs
   :icon nil
   :name 'top-level
   :pretty-name "McCLIM Window"))

(defun get-top-level-sheet (sheet)
  "Returns the root window for sheet or nil."
  (if (typep sheet '(or top-level-sheet-mixin null))
      sheet
      (get-top-level-sheet (sheet-parent sheet))))

(defmethod shrink-sheet ((sheet top-level-sheet-mixin))
  (port-shrink-sheet (port sheet) sheet))

;;; Unmanaged sheet is not managed by the window manager.
(defclass unmanaged-sheet-mixin () ())


;;; Sheets as bounding rectangles

;;; Somewhat hidden in the spec, we read (section 4.1.1 "The Bounding
;;; Rectangle Protocol")
;;;

;;; | bounding-rectangle* region [Generic Function]
;;; |
;;; |      [...] The argument region must be either a bounded region [...] or
;;; |      some other object that obeys the bounding rectangle protocol, such
;;; |      as a sheet or an output record. [...]

(defmethod bounding-rectangle* ((sheet sheet))
  (bounding-rectangle* (sheet-region sheet)))

;;; The null sheet

(defclass null-sheet (basic-sheet) ())
