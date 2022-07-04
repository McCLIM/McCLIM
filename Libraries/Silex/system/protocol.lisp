;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001,2002 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) Copyright 2006 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Collect all the class and generic function definitions in the Spec in one
;;; file that is compiled and loaded early.
;;;
(in-package #:silex)

(defmacro define-protocol-class (name super-classes &optional slots &rest options)
  (let* ((sym-name (symbol-name name))
         (protocol-predicate
           (protocol-predicate-name name))
         (predicate-docstring
           (concatenate 'string
                        "Protocol predicate checking for class " sym-name)))
    `(progn
       (defclass ,name ,super-classes ,slots ,@options)

       ;; This adds a DUMMY slot to the protocol class that signals an
       ;; error in its initfunction. Thus attempting to make an
       ;; instance of the class signals an error.
       ;;
       ;; For subclasses, the slot is not added (as the method is
       ;; EQL-specialized on the protocol class itself) so that no
       ;; runtime time or space overhead is incurred.
       (defmethod c2mop:compute-slots ((class (eql (find-class ',name))))
         (list* (make-instance 'c2mop:standard-effective-slot-definition
                               :name         'dummy
                               :allocation   :instance
                               :initform     '#1=(error "~S is a protocol class ~
                                                         and thus cannot be ~
                                                         instantiated"
                                                        ',name)
                               :initfunction (lambda () #1#))
                (call-next-method)))

       (let ((the-class (find-class ',name)))
         (setf (documentation the-class 'type) "CLIM protocol class"))

       (defgeneric ,protocol-predicate (object)
         (:method ((object t))
           nil)
         (:method ((object ,name))
           t)
         (:documentation ,predicate-docstring))
       ',name)))

;; Since the declaim form for functions looks clumsy and is syntax-wise
;; different from defun, we define us a new declfun, which fixes this.

(defmacro declfun (name lambda-list)
  `(declaim (ftype (function
                    ,(let ((q lambda-list)
                           res)
                       (do () ((or (null q)
                                   (member (car q) '(&optional &rest &key))))
                         (push 't res)
                         (pop q))
                       (when (eq (car q) '&optional)
                         (push '&optional res)
                         (pop q)
                         (do () ((or (null q)
                                     (member (car q) '(&rest &key))))
                           (pop q)
                           (push 't res)))
                       (when (eq (car q) '&rest)
                         (push '&rest res)
                         (pop q)
                         (push 't res)
                         (pop q))
                       (when (eq (car q) '&key)
                         (push '&key res)
                         (pop q)
                         (do () ((or (null q)
                                     (member (car q) '(&allow-other-keys))))
                           (push (list (intern (string (if (consp (car q))
                                                           (if (consp (caar q))
                                                               (caaar q)
                                                               (caar q))
                                                           (car q)))
                                               :keyword)
                                       't)
                                 res)
                           (pop q)))
                       (when (eq (car q) '&allow-other-keys)
                         (push '&allow-other-keys res)
                         (pop q))
                       (reverse res))
                    t)
                   ,name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *protocols* (make-hash-table :test #'equalp))
  (defclass protocol ()
    ((name :initarg :name :reader name)
     (sups :initarg :sups :reader sups)
     (decl :initarg :decl :reader decl)
     (syms :initarg :syms :reader syms))))

(defmacro define-protocol (protocol-name super &rest forms)
  (let ((names (mapcar #'second forms)))
    (setf (gethash protocol-name *protocols*)
          (make-instance 'protocol
           :name protocol-name :sups super :decl forms :syms names)))
  (flet ((expand-declare (form)
           (if (stringp (first form))
               `(define-protocol ,(first form) (,protocol-name) ,@(rest form))
               (destructuring-bind (type name &rest description) form
                 `(progn
                    (export ',name)
                    ,(ecase type
                       ((:protocol-class :protocol-class*)
                        `(progn
                           (export ',(protocol-predicate-name name))
                           (define-protocol-class ,name ,@description)))
                       ((:class :class* :mixin :mixin*)
                        `(quote ,name))
                       ((:condition :condition*)
                        `(quote ,name))
                       ((:type :type*)
                        `(quote ,name))
                       ((:constant :constant* :variable :variable*)
                        `(quote ,name))
                       ((:function :function* :constructor :constructor*)
                        `(declfun ,name ,@description))
                       ((:generic :generic*)
                        `(defgeneric ,name ,@description))
                       ((:reader :reader*)
                        `(defgeneric ,name (instance) ,@description))
                       ((:accessor :accessor*)
                        `(progn
                           (defgeneric ,name (instance) ,@description)
                           (defgeneric (setf ,name) (new-value instance))))
                       ((:macro :macro*)
                        `(quote ,name))
                       ((:symbol :symbol*)
                        `(quote ,name))))))))
    `(progn ,@(mapcar #'expand-declare forms))))

;;; expand-exports protocol-name &key with-extensions
;;; expand-definitions protocol-name

'(protocol (:name geometry-substrate :label "Geometry Substrate")
  (protocol (:name regions :label "3 Regions"))
  (class (:name design :abstract t))
  (class (:name region :super design)
   (accessor (:name medium-background))
   (accessor (:name medium-foreground))
   (generic  (:name medium-current-text-style)
    (argument (:name medium :type medium))))
  (function (:name draw-rectangles*)
   (argument (:name sheet))
   (argument (:name point1)))
  (enum (:name foobar)
   (item (:name +foobar+) 3)
   (item (:name +foobar+) 4)))



;;; "Part I: Overview and Conventions" doesn't have any protocol classes
;;; defined, so no need for a separate page for it.

(define-protocol "former climi" ()
  ;; FIXME these symbols should be pruned or made a proper interface.
  (:symbol* letf)
  (:symbol* atan*)
  (:symbol* now)
  (:symbol* compute-decay)
  (:symbol* reparameterize-ellipse)
  (:symbol* normalize-angle)
  (:symbol* normalize-angle*)
  (:symbol* expand-point-seq)
  (:symbol* coord-seq->point-seq)
  (:symbol* cubic-bezier-dimension-min-max)
  (:symbol* ELLIPSE-SIMPLIFIED-REPRESENTATION)
  (:symbol* ELLIPSE-BOUNDING-RECTANGLE*)
  (:symbol* %ELLIPSE-ANGLE->POSITION)
  (:symbol* %%SET-SHEET-NATIVE-TRANSFORMATION)
  (:symbol* %%SET-SHEET-NATIVE-REGION)
  (:symbol* %%SHEET-NATIVE-TRANSFORMATION)
  (:symbol* %%SHEET-NATIVE-REGION)
  (:symbol* NATIVE-REGION)
  (:symbol* NATIVE-TRANSFORMATION)
  (:symbol* DEVICE-REGION)
  (:symbol* DEVICE-TRANSFORMATION)
  (:symbol* ELLIPSE-CUBIC-BEZIER-POINTS)
  (:symbol* TRANSFORM-COORDINATES-MIXIN)
  (:symbol* %SET-SHEET-REGION-AND-TRANSFORMATION)
  (:symbol* %rgba-value)
  (:symbol* %pattern-rgba-value)
  (:symbol* %collapse-pattern)
  (:symbol* %ub8-stencil)
  (:symbol* %rgba-pattern)
  (:symbol* *configuration-event-p*)
  (:symbol* EXPAND-RECTANGLE-COORDS)
  (:symbol* *all-ports*)
  (:symbol* ink)
  (:symbol* clipping-region)
  (:symbol* transformation)
  (:symbol* line-style)
  (:symbol* text-style)
  (:symbol* coordinates))

;;; "Part II: Geometry Substrate"
(define-protocol "Geometry Substrate" ())

(define-protocol "3 Regions" ("Geometry Substrate")
  ("3.1 General Regions"
   (:protocol-class design ())
   (:protocol-class region (design))
   (:protocol-class path (region bounding-rectangle))
   (:protocol-class area (region bounding-rectangle))
   (:type coordinate)
   (:function coordinate (n))
   (:function* coordinate-epsilon ())
   (:function* coordinate= (x y))
   (:function* coordinate/= (x y))
   (:function* coordinate<= (x y))
   (:function* coordinate-between (c1 x c2))
   (:function* coordinate-between* (low x high))
   (:type* standard-rectangle-coordinate-vector)
   (:class* nowhere-region (region))
   (:class* everywhere-region (region))
   (:constant +everywhere+ region)
   (:constant +nowhere+ region)
   ("3.1.1 The Region Predicate Protocol"
    (:generic region-equal (region1 region2))
    (:generic region-contains-region-p (region1 region2))
    (:generic region-contains-position-p (region x y))
    (:generic region-intersects-region-p (region1 region2)))
   ("3.1.2 Region Composition Protocol"
    (:protocol-class region-set (region bounding-rectangle))
    (:class standard-region-union (region-set))
    (:class standard-region-intersection (region-set))
    (:class standard-region-difference (region-set))
    (:class* standard-region-complement (region-set))
    (:class* standard-rectangle-set (region-set))
    (:generic region-set-regions (region &key normalize))
    (:generic map-over-region-set-regions (fun region &key normalize))
    (:generic region-union (region1 region2))
    (:generic region-intersection (region1 region2))
    (:generic region-difference (region1 region2))
    (:generic* region-complement (region))
    (:condition* region-set-not-rectangular (error))))
  ("3.2 Other Region Types"
   ("3.2.1 Points"
    (:protocol-class point (region bounding-rectangle))
    (:class standard-point (point))
    (:constructor make-point (x y))
    (:generic point-position (point))
    (:generic point-x (point))
    (:generic point-y (point)))
   ("3.2.2 Polygons and Polylines"
    (:protocol-class polygon (bezigon))
    (:protocol-class polyline (polybezier))
    (:class standard-polygon (polygon))
    (:constructor make-polygon (point-seq))
    (:constructor make-polygon* (coord-seq))
    (:class standard-polyline (standard-polyline))
    (:constructor make-polyline (point-seq &key closed))
    (:constructor make-polyline* (coord-seq &key closed))
    (:generic polygon-points (polygon-or-polyline))
    (:generic map-over-polygon-coordinates (fun polygon-or-polyline))
    (:generic map-over-polygon-segments (fun polygon-or-polyline))
    (:generic polyline-closed (polyline)))
   ("3.2.3 Lines"
    (:protocol-class line (polyline))
    (:class standard-line (line))
    (:constructor make-line (start-point end-point))
    (:constructor make-line* (x1 y1 x2 y2))
    (:generic line-start-point* (line))
    (:generic line-end-point* (line))
    (:generic line-start-point (line))
    (:generic line-end-point (line)))
   ("3.2.4 Rectangles"
    (:protocol-class rectangle (polygon))
    (:class standard-rectangle (rectangle))
    (:constructor make-rectangle (point1 point2))
    (:constructor make-rectangle* (x1 y1 x2 y2))
    (:generic rectangle-edges* (rectangle))
    (:generic rectangle-min-point (rectangle))
    (:generic rectangle-max-point (rectangle))
    (:generic rectangle-min-x (rectangle))
    (:generic rectangle-min-y (rectangle))
    (:generic rectangle-max-x (rectangle))
    (:generic rectangle-max-y (rectangle))
    (:generic rectangle-width (rectangle))
    (:generic rectangle-height (rectangle))
    (:generic rectangle-size (rectangle)))
   ("3.2.5 Ellipses and Elliptical Arcs"
    (:protocol-class ellipse (area))
    (:protocol-class elliptical-arc (path))
    (:class standard-ellipse (ellipse))
    (:constructor make-ellipse (center rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
    (:constructor make-ellipse* (cx cy rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
    (:class standard-elliptical-arc (elliptical-arc))
    (:constructor make-elliptical-arc (center rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
    (:constructor make-elliptical-arc* (cx cy rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
    (:generic ellipse-center-point* (elliptical-object))
    (:generic ellipse-center-point (elliptical-object))
    (:generic ellipse-radii (elliptical-object))
    (:generic ellipse-start-angle (elliptical-object))
    (:generic ellipse-end-angle (elliptical-object)))
   ("3.2.6 The Bezigon and Bezier Curve Protocol (McCLIM extension)"
    (:protocol-class* bezigon (area))
    (:protocol-class* polybezier (path))
    (:class* standard-bezigon (bezigon))
    (:constructor* make-bezigon (point-seq))
    (:constructor* make-bezigon* (coord-seq))
    (:class* standard-polybezier (polybezier))
    (:constructor* make-polybezier (point-seq))
    (:constructor* make-polybezier* (coord-seq))
    (:generic* bezigon-points (object))
    (:generic* bezigon-order (object))
    (:generic* map-over-bezigon-segments (fun object))
    (:function* map-over-bezigon-segments* (fun coord-seq order)))))

(define-protocol "4 Bounding Rectangles" ("Geometry Substrate")
  ("4.1 Bounding Rectangles"
   (:protocol-class bounding-rectangle ())
   (:class standard-bounding-rectangle (bounding-rectangle rectangle))
   (:constructor make-bounding-rectangle (x1 y1 x2 y2))
   (:generic bounding-rectangle* (region))
   (:generic bounding-rectangle (region))
   (:macro with-bounding-rectangle* ((&rest variables) region &body body))
   (:macro* with-standard-rectangle* ((&rest variables) rectangle &body body))
   (:generic bounding-rectangle-position (region))
   (:generic bounding-rectangle-min-x (region))
   (:generic bounding-rectangle-min-y (region))
   (:generic bounding-rectangle-max-x (region))
   (:generic bounding-rectangle-max-y (region))
   (:generic bounding-rectangle-width (region))
   (:generic bounding-rectangle-height (region))
   (:generic bounding-rectangle-size (region))
   (:function* copy-bounding-rectangle (region))
   (:function* rounded-bounding-rectangle (region))))

(define-protocol "5 Affine Transformations" ("Geometry Substrate")
  ("5.1 Transformations"
   (:protocol-class transformation ())
   (:function* get-transformation (transformation))
   (:class* standard-transformation (transformation))
   (:class* standard-identity-transformation)
   (:class* standard-translation (transformation))
   (:class* standard-hairy-transformation (transformation))
   (:constant +identity-transformation+ transformation)
   (:macro* with-transformed-position ((transformation x y) &body body))
   (:macro* with-transformed-distance ((transformation dx dy) &body body))
   (:macro* with-transformed-angles ((transformation clockwisep &rest angles) &body body))
   (:macro* with-transformed-positions ((transformation coord-seq) &body body))
   (:macro* with-transformed-positions* ((transformation &rest coord-seq) &body body))
   (:function* transform-angle (transformation phi))
   (:function* untransform-angle (transformation phi))
   ("5.1.1 Transformation Conditions"
    (:condition transformation-error (error) (:initargs :points))
    (:condition transformation-underspecified (error) (:initargs :points))
    (:condition reflection-underspecified (error) (:initargs :points))
    (:condition singular-transformation (error) (:initargs :transformation))
    (:condition* rectangle-transformation-error (error) (:initargs :rect))))
  ("5.2 Transformation Constructors"
   (:constructor make-translation-transformation (dx dy))
   (:constructor make-rotation-transformation (angle &optional origin))
   (:constructor make-rotation-transformation* (angle &optional x0 y0))
   (:constructor make-scaling-transformation (sx sy &optional origin))
   (:constructor make-scaling-transformation* (sx sy &optional x0 y0))
   (:constructor make-reflection-transformation (point1 point2))
   (:constructor make-reflection-transformation* (x1 y1 x2 y2))
   (:constructor make-transformation (mxx mxy myx myy tx ty))
   (:constructor make-3-point-transformation (p1 p2 p3 p1* p2* p3*))
   (:constructor make-3-point-transformation* (x1 y1 x2 y2 x3 y3 x1* y1* x2* y2* x3* y3*)))
  ("5.3 Transformation Protocol"
   ("5.3.1 Transformation Predicates"
    (:generic transformation-equal (transformation1 transformation2))
    (:generic identity-transformation-p (transformation))
    (:generic invertible-transformation-p (transformation))
    (:generic translation-transformation-p (transformation))
    (:generic reflection-transformation-p (transformation))
    (:generic rigid-transformation-p (transformation))
    (:generic even-scaling-transformation-p (transformation))
    (:generic scaling-transformation-p (transformation))
    (:generic rectilinear-transformation-p (transformation))
    (:generic* y-inverting-transformation-p (transformation)))
   ("5.3.2 Composition of Transformations"
    (:generic compose-transformations (transformation1 transformation2))
    (:generic invert-transformation (transformation))
    (:function compose-translation-with-transformation (transformation dx dy))
    (:function compose-scaling-with-transformation (transformation sx sy &optional origin))
    (:function compose-rotation-with-transformation (transformation angle &optional origin))
    (:function compose-transformation-with-translation (transformation dx dy))
    (:function compose-transformation-with-scaling (transformation sx sy &optional origin))
    (:function compose-transformation-with-rotation (transformation angle &optional origin)))
   ("5.3.3 Applying Transformations"
    (:generic transform-region (transformation region))
    (:generic untransform-region (transformation region))
    (:generic transform-position (transformation x y))
    (:function* transform-positions (transformation coord-seq))
    (:function* transform-position-sequence (seq-type transformation coord-seq))
    (:generic untransform-position (transformation x y))
    (:generic transform-distance (transformation dx dy))
    (:generic untransform-distance (transformation dx dy))
    (:generic transform-rectangle* (transformation x1 y1 x2 y2))
    (:generic untransform-rectangle* (transformation x1 y1 x2 y2)))))

;;; "Part IV: Sheet and Medium Output Facilities" number comes from CLIM
;;; specification, but it is defined earlier in Silex.
(define-protocol "Sheet and Medium Output Facilities" ())

(define-protocol "10 Drawing Options" ("Sheet and Medium Output Facilities")
  ("10.1 Medium Components"
   (:protocol-class medium ())
   (:class basic-medium (medium))
   (:accessor medium-background)
   (:accessor medium-foreground)
   (:accessor medium-ink)
   (:accessor medium-transformation)
   (:accessor medium-clipping-region)
   (:accessor medium-line-style)
   (:accessor medium-default-text-style)
   (:accessor medium-text-style)
   (:generic medium-current-text-style (medium)) ; redundant with m-merged-t-s
   (:generic medium-merged-text-style (medium))
   (:reader medium-sheet)
   (:reader medium-drawable)
   (:reader* medium-device-transformation)
   (:reader* medium-device-region)
   (:reader* medium-native-transformation)
   (:reader* medium-native-region)
   ;; Graphics state mixin
   (:class* graphics-state)
   (:mixin* gs-transformation-mixin)
   (:mixin* gs-ink-mixin)
   (:mixin* gs-clip-mixin)
   (:mixin* gs-line-style-mixin)
   (:mixin* gs-text-style-mixin)
   (:mixin* complete-medium-state)
   (:reader* graphics-state-transformation)
   (:reader* graphics-state-ink)
   (:reader* graphics-state-clip)
   (:reader* graphics-state-line-style)
   (:reader* graphics-state-text-style)
   (:generic* graphics-state-line-style-border (record medium))
   (:generic* (setf graphics-state) (new-value graphics-state)))
  ("10.2 Drawing Option Binding Forms"
   (:macro* with-medium-options ((medium args) &body body))
   (:macro with-drawing-options ((medium &rest drawing-options
                                         &key ink
                                         transformation
                                         clipping-region
                                         line-style
                                         text-style
                                         &allow-other-keys)
                                 &body body))
   (:generic invoke-with-drawing-options (medium cont &rest drawing-options &key &allow-other-keys))
   ("10.2.1 Transformation \"Convenience\" Forms"
    (:macro with-translation ((medium dx dy) &body body))
    (:macro with-scaling ((medium sx &optional sy origin) &body body))
    (:macro with-rotation ((medium angle &optional origin) &body body))
    (:macro with-identity-transformation)
    (:generic invoke-with-identity-transformation (medium continuation))
    (:generic invoke-with-local-coordinates (medium continuation x y))
    (:generic invoke-with-first-quadrant-coordinates (medium continuation x y)))
   ("10.2.2 Estabilishing Local Coordinate System"
    (:macro with-local-coordinates ((medium &optional x y) &body body))
    (:macro with-first-quadrant-coordinates ((medium &optional x y) &body body))))
  ("10.3 Line Styles"
   (:protocol-class line-style () ()
                    (:default-initargs
                     :line-unit :normal
                     :line-thickness 1
                     :line-joint-shape :miter
                     :line-cap-shape :butt
                     :line-dashes nil))
   (:class standard-line-style (line-style))
   (:constructor make-line-style (&key unit thickness joint-shape cap-shape dashes))
   (:generic* line-style-equalp (style1 style2))
   ("10.3.1 Line Style Protocol and Line Style Suboptions"
    (:reader line-style-unit)
    (:reader line-style-thickness)
    (:reader line-style-joint-shape)
    (:reader line-style-cap-shape)
    (:reader line-style-dashes))
   ("10.3.2 Contrasting Dash Patterns"
    (:function make-contrasting-dash-patterns (n &optional k))
    (:generic contrasting-dash-pattern-limit (port)))))
(define-protocol "11 Text Styles" ("Sheet and Medium Output Facilities")
  ("11.1 Text Style"
   (:protocol-class text-style () () (:default-initargs :text-family nil :text-face nil :text-size nil))
   (:class standard-text-style (text-style))
   (:generic* text-style-equalp (style1 style2))
   (:constructor make-text-style (family face size))
   (:constant *default-text-style*)
   (:constant *undefined-text-style*)
   ("11.1.1 Text Style Protocol and Text Style Suboptions"
    (:reader text-style-components)
    (:reader text-style-family)
    (:reader text-style-face)
    (:reader text-style-size)
    (:function parse-text-style (style-spec))
    (:function* parse-text-style* (style))
    (:function* normalize-font-size (size))
    (:generic merge-text-styles (style1 style2))
    (:generic text-style-ascent (text-style medium))
    (:generic text-style-descent (text-style medium))
    (:generic text-style-width (text-style medium))
    (:generic text-style-height (text-style medium))
    (:generic text-style-fixed-width-p (text-style medium))
    (:generic text-size (medium string &key text-style start end))
    (:generic* text-style-leading (text-style medium)
      (:method (text-style medium) 1.2))
    (:generic* text-style-character-width (text-style medium char)
      (:method (text-style medium char)
        (text-size medium char :text-style text-style)))
    (:generic* text-bounding-rectangle* (medium string &key text-style start end align-x align-y direction))))
  ("11.2 Text Style Binding Forms"
   (:macro with-text-style ((medium text-style) &body body))
   (:generic invoke-with-text-style (medium cont text-style))
   (:macro with-text-family ((medium family) &body body))
   (:macro with-text-face ((medium face) &body body))
   (:macro with-text-size ((medium size) &body body)))
  ("11.3 Controlling Text Style Mappings"
   (:generic text-style-mapping (port text-style &optional character-set))
   (:generic (setf text-style-mapping) (mapping port text-style &optional character-set))
   (:class device-font-text-style (text-style))
   (:function device-font-text-style-p (object))
   (:generic make-device-font-text-style (display-device device-font-name))))

(define-protocol "12 Graphics" ("Sheet and Medium Output Facilities")
  ("12.5 Drawing Functions"
   ("12.5.1 Basic Drawing Functions"
    (:macro* def-sheet-trampoline (name (&rest args)))
    (:macro* def-graphic-op (name (&rest args)))
    (:function draw-point (sheet point &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit &allow-other-keys))
    (:function draw-point* (sheet x y &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit &allow-other-keys))
    (:function draw-points (sheet point-seq &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit &allow-other-keys))
    (:function draw-points* (sheet position-seq &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit &allow-other-keys))
    (:function draw-line (sheet point1 point2 &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-line* (sheet x1 y1 x2 y2 &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-lines (sheet point-seq &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-lines* (sheet position-seq &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-polygon (sheet point-seq &rest drawing-options &key (filled t) (closed t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape &allow-other-keys))
    (:function draw-polygon* (sheet position-seq &rest drawing-options &key (filled t) (closed t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape &allow-other-keys))
    (:function draw-rectangle (sheet point1 point2 &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape &allow-other-keys))
    (:function draw-rectangle* (sheet x1 y1 x2 y2 &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape &allow-other-keys))
    (:function draw-rectangles (sheet points &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape &allow-other-keys))
    (:function draw-rectangles* (sheet position-seq &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape &allow-other-keys))
    (:function draw-ellipse (sheet center rdx1 rdy1 rdx2 rdy2 &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-ellipse* (sheet cx cy rdx1 rdy1 rdx2 rdy2 &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-circle (sheet center radius &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-circle* (sheet cx cy radius &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-text (sheet text point &rest drawing-options &key (start 0) (end nil) (align-x :left) (align-y :baseline) (toward-point nil toward-point-p) transform-glyphs ink clipping-region transformation text-style text-family text-face text-size &allow-other-keys))
    (:function draw-text* (sheet text x y &rest drawing-options &key (start 0) (end nil) (align-x :left) (align-y :baseline) (toward-x (1+ x)) (toward-y y) transform-glyphs ink clipping-region transformation text-style text-family text-face text-size &allow-other-keys))
    (:function* draw-triangle (sheet point1 point2 point3 &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape &allow-other-keys))
    (:function* draw-triangle* (sheet x1 y1 x2 y2 x3 y3 &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape &allow-other-keys))
    (:function* draw-bezigon (sheet point-seq &rest drawing-args &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape &allow-other-keys))
    (:function* draw-bezigon* (sheet position-seq &rest drawing-args &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape &allow-other-keys))
    (:function* draw-image (sheet pattern point &rest drawing-options))
    (:function* draw-image* (sheet pattern x y &rest drawing-options)))
   ("12.5.2 Compound Drawing Functions"
    (:function draw-arrow (medium point1 point2 &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape (to-head t) from-head (head-length 10) (head-width 5) (head-filled nil) angle &allow-other-keys))
    (:function draw-arrow* (medium x1 y1 x2 y2 &rest drawing-options &key ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape (to-head t) from-head (head-length 10) (head-width 5) (head-filled nil) angle &allow-other-keys))
    (:function draw-oval (medium center rx ry &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function draw-oval* (medium cx cy rx ry &rest drawing-options &key (filled t) ink clipping-region transformation line-style line-thickness line-unit line-dashes line-cap-shape &allow-other-keys))
    (:function* draw-rounded-rectangle* (sheet x1 y1 x2 y2
                                               &rest args &key
                                               (radius 7)
                                               (radius-x radius)
                                               (radius-y radius)
                                               (radius-left  radius-x)
                                               (radius-right radius-x)
                                               (radius-top    radius-y)
                                               (radius-bottom radius-y)
                                               filled &allow-other-keys))))
  ("12.6 Pixmaps"
   (:generic allocate-pixmap (medium width height))
   (:generic deallocate-pixmap (pixmap))
   (:reader pixmap-width)
   (:reader pixmap-height)
   (:reader pixmap-depth)
   (:function copy-to-pixmap (source src-x src-y width height &optional pixmap dst-x dst-y))
   (:function copy-from-pixmap (pixmap src-x src-y width height destination dst-x dst-y))
   (:generic copy-area (medium src-x src-y width height dst-x dst-y))
   (:generic medium-copy-area (source src-x src-y width height destination dst-x dst-y))
   (:macro with-output-to-pixmap ((medium-var medium &key width height) &key body))
   (:generic invoke-with-output-to-pixmap (medium cont &key width height)))
  ("12.7 Graphics Protocols"
   ("12.7.2 Medium-specific Drawing Functions"
    (:generic medium-draw-point* (medium x y))
    (:generic medium-draw-points* (medium coord-seq))
    (:generic medium-draw-line* (medium x1 y1 x2 y2))
    (:generic medium-draw-lines* (medium coord-seq))
    (:generic medium-draw-polygon* (medium coord-seq closed filled))
    (:generic medium-draw-rectangles* (medium coord-seq filled))
    (:generic medium-draw-rectangle* (medium left top right bottom filled))
    (:generic medium-draw-ellipse* (medium cx cy rdx1 rdy1 rdx2 rdy2 start-angle end-angle filled))
    (:generic medium-draw-text* (medium string x y start end align-x align-y toward-x toward-y transform-glyphs))
    (:generic* medium-draw-bezigon* (medium coord-seq filled)))
   ("12.7.3 Other Medium-specific Output Functions"
    (:generic medium-finish-output (medium))
    (:generic medium-force-output (medium))
    (:generic medium-clear-area (medium x1 y1 x2 y2))
    (:generic medium-beep (medium))
    (:generic beep (&optional medium))
    ;; Moved from "15.6 Buffering the Output"
    (:accessor* medium-buffering-output-p)
    (:macro with-output-buffered (medium &optional (buffer-p t)))
    (:generic invoke-with-output-buffered (medium cont &optional buffered-p))
    (:macro* with-output-to-drawing-stream ((stream backend destination &rest args) &body body))
    (:generic* invoke-with-output-to-drawing-stream (cont backend destination &key &allow-other-keys))
    (:reader* medium-miter-limit)
    (:generic* line-style-effective-thickness (line-style medium))
    (:generic* line-style-effective-dashes (line-style medium)))))

(define-protocol "13 Drawing in Color" ("Sheet and Medium Output Facilities")
  ;; (:protocol-class design ()) ; defined in regions
  ("13.3 Color"
   (:protocol-class color (design))
   (:class* standard-color (color))
   (:constructor make-rgb-color (red green blue))
   (:constructor make-ihs-color (intensity hue saturation))
   (:constructor make-gray-color (luminance))
   (:constructor* make-named-color (name red green blue))
   (:reader color-rgb)
   (:reader color-ihs)
   (:reader* color-rgba)
   ("13.3.1 Standard Color Names and Constants"
    (:constant +red+)
    (:constant +green+)
    (:constant +blue+)
    (:constant +cyan+)
    (:constant +magenta+)
    (:constant +yellow+)
    (:constant +black+)
    (:constant +white+))
   ("13.3.2 Contrastin Colors"
    (:constructor make-contrasting-inks (n &optional k))
    (:reader contrasting-inks-limit)))
  ("13.4 Opacity"
   (:protocol-class opacity (design))
   (:constructor make-opacity (value))
   (:constant +transparent-ink+)
   (:reader opacity-value))
  ("13.6 Indirect Inks"
   (:constant +foreground-ink+)
   (:constant +background-ink+)
   (:class* indirect-ink (design))
   (:function* indirect-ink-p (design))
   (:function* indirect-ink-ink (indirect-ink)))
  ("13.7 Flipping Ink"
   (:class* standard-flipping-ink (design))
   (:generic make-flipping-ink (design1 design2))
   (:constant +flipping-ink+)
   (:reader flipping-ink-design1)
   (:reader flipping-ink-design2)))

(define-protocol "14 General Design" ("Sheet and Medium Output Facilities")
  ("14.1 The Compositing Protocol"
   (:generic compose-over (design1 design2))
   (:generic compose-in (ink mask))
   (:generic compose-out (ink mask)))
  ("14.2 Patterns and Stencils"
   (:constructor make-pattern (array designs))
   (:constructor make-stencil (array))
   ;; v constructor specified in "14.3 Tiling"
   (:constructor make-rectangular-tile (design width height))
   ;; v constructor specified in "E.2 SUpported for Reading Bitmap Files"
   (:constructor make-pattern-from-bitmap-file (pathname &key format designs))
   (:variable* *bitmap-file-readers*)
   (:variable* *bitmap-file-writers*)
   (:macro* define-bitmap-file-reader (bitmap-format (&rest args) &body body))
   (:macro* define-bitmap-file-writer (format (&rest args) &body body))
   (:function* bitmap-format-supported-p (format))
   (:function* bitmap-output-supported-p (format))
   (:condition unsupported-bitmap-format (error))
   (:function* read-bitmap-file (pathname &key (format :bitmap)))
   (:function* write-bitmap-file (image pathname &key (format :bitmap)))
   (:reader pattern-width)
   (:reader pattern-height)
   ;; Extensions
   (:protocol-class* pattern (design))
   (:class* stencil (pattern))
   (:class* indexed-pattern (pattern))
   (:class* image-pattern (pattern))
   (:class* rectangular-tile (pattern))
   ;; v constructed by transform-region
   (:class* transformed-pattern (transformed-design pattern))
   (:reader* pattern-array)
   (:reader* pattern-designs)
   (:reader* transformed-design-design)
   (:reader* transformed-design-transformation)
   (:reader* rectangular-tile-design))
  ("14.5 Arbitrary Designs"
   (:constructor* make-uniform-compositum (ink opacity-value))
   (:class* transformed-design (design)) 
   (:class* masked-compositum (design))
   (:reader* compositum-mask)
   (:reader* compositum-ink)
   (:class* in-compositum (masked-compositum))
   (:class* uniform-compositum (in-compositum))
   (:class* out-compositum (masked-compositum))
   (:class* over-compositum (design))
   (:reader compositum-foreground)
   (:reader compositum-background)
   (:generic* design-ink (design x y))
   (:function* design-ink* (design x y))
   (:generic* design-equalp (design1 design2))
   (:generic draw-design (medium design &key ink filled clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape text-style text-family text-face text-size))
   (:function draw-pattern* (medium pattern x y &key clipping-region transformation)))
  ;; Minor issue: The generic functions underlying the functions described in
  ;; this and the preceding chapter will be documented later. This will allow
  ;; for programmer-defined design classes. This also needs to describe how to
  ;; decode designs into inks. --- SWM (copied from CLIM II spec)
  ("14.7 Design Protocol"))

;;; Part III: Windowing Substrate
(define-protocol "Windowing Substrate" ())

(define-protocol "7 Properties of Sheets" ("Windowing Substrate")
  ("7.1 Basic Sheet Classes"
   (:protocol-class sheet ())
   (:class basic-sheet (sheet))
   (:reader* sheet-name)
   (:accessor* sheet-pretty-name)
   (:accessor* sheet-icon)
   (:accessor* sheet-pointer-cursor))
  ("7.2 Relationships Between Classes"
   ("7.2.1 Sheet Relationship Functions"
    (:reader sheet-parent)
    (:reader sheet-children)
    (:reader* sheet-child)
    (:generic sheet-adopt-child (sheet child))
    (:generic sheet-disown-child (sheet child &key errorp))
    (:generic sheet-siblings (sheet))
    (:generic sheet-enabled-children (sheet))
    (:generic sheet-ancestor-p (sheet putative-ancestor))
    (:generic raise-sheet (sheet))
    (:generic bury-sheet (sheet))
    (:generic reorder-sheets (sheet new-ordering))
    (:generic* shrink-sheet (sheet))
    (:condition* sheet-is-not-child (error))
    (:condition* sheet-is-top-level (error))
    (:condition* sheet-ordering-underspecified (error))
    (:condition* sheet-is-not-ancestor (error))
    (:condition* sheet-already-has-parent (error))
    (:condition* sheet-supports-only-one-child (error))
    (:accessor sheet-enabled-p)
    (:generic sheet-viewable-p (sheet))
    (:generic sheet-occluding-sheets (sheet child))
    (:generic map-over-sheets (fun sheet)))
   ("7.2.2 Sheet Genealogy Classes"
    (:mixin sheet-parent-mixin)
    (:mixin sheet-leaf-mixin)
    (:mixin sheet-single-child-mixin)
    (:mixin sheet-multiple-child-mixin)))   
  ("7.3 Sheet Geometry"
   ("7.3.1 Sheet Geometry Functions"
    (:accessor sheet-transformation)
    (:accessor sheet-region)
    (:generic move-sheet (sheet x y))
    (:generic resize-sheet (sheet width height))
    (:generic move-and-resize-sheet (sheet x y width height))
    (:generic map-sheet-position-to-parent (sheet x y))
    (:generic map-sheet-position-to-child (sheet x y))
    (:generic map-sheet-rectangle*-to-parent (sheet x1 y1 x2 y2))
    (:generic map-sheet-rectangle*-to-child (sheet x1 y1 x2 y2))
    (:generic map-over-sheets-containing-position (fun sheet x y))
    (:generic map-over-sheets-overlapping-region (fun sheet region))
    (:generic child-containing-position (sheet x y))
    (:generic children-overlapping-region (sheet region))
    (:generic children-overlapping-rectangle* (sheet x1 y1 x2 y2))
    (:generic sheet-delta-transformation (sheet ancestor))
    (:generic sheet-allocated-region (sheet child)))
   ("7.3.1 Sheet Geometry Classes"
    (:mixin sheet-identity-transformation-mixin)
    (:mixin sheet-translation-mixin)
    (:mixin sheet-y-inverting-transformation-mixin)
    (:mixin sheet-transformation-mixin))))

(define-protocol "8 Sheet Protocols" ("Windowing Substrate")
  ("8.1 Input Protocol"
   ("8.1.1 Input Protocol Functions"
    (:generic sheet-event-queue (sheet))
    (:generic process-next-event (port &key wait-function timeout))
    (:accessor port-keyboard-input-focus)
    (:generic* note-input-focus-changed (sheet state)
      (:documentation "Called when a sheet receives or loses the keyboard input
focus. STATE argument is T when the sheet gains focus and NIL otherwise. This
is a McCLIM extension."))
    (:generic distribute-event (port event))
    (:generic dispatch-event (port event))
    (:generic queue-event (port event))
    (:generic* schedule-event (client event delay))
    (:generic handle-event (port event))
    (:generic event-read (client))
    (:generic event-read-no-hang (client))
    (:generic event-peek (client &optional event-type))
    (:generic event-unread (client event))
    (:generic event-listen (client))
;;; Extensions involving wait-function
;;;
;;; wait-function in principle behaves like for process-next-event (and for
;;; single-threaded run it is exactly what happens - we pass it to the port
;;; method). It is not called in a busy loop but rather after some input wakes
;;; up blocking backend-specific wait function. Then we call wait-function.
;;; -- jd 2019-03-26
    (:generic* event-read-with-timeout (client &key timeout wait-function)
      (:documentation "Reads event from the event queue. Function returns when event is succesfully
read, timeout expires or wait-function returns true. Time of wait-function call
depends on a port."))
    (:generic* event-listen-or-wait (client &key timeout wait-function)
      (:documentation "When wait-function is nil then function waits for available event. Otherwise
function returns when wait-function predicate yields true. Time of wait-function
call depends on a port."))
    ;; Event queue protocol (a mailbox with compression, maybe move to system?)
    (:protocol-class* event-queue ())
    (:accessor* event-queue-port)
    (:class* simple-event-queue (event-queue))
    (:class* concurrent-event-queue (event-queue))
    (:generic* schedule-event-queue (queue event delay))
    (:generic* event-queue-read (event-queue)
      (:documentation "Reads one event from the queue, if there is no event, hang
until here is one."))

    (:generic* event-queue-read-no-hang (event-queue)
      (:documentation "Reads one event from the queue, if there is no event just
return NIL."))
    (:generic* event-queue-read-with-timeout (event-queue timeout wait-function)
      (:documentation "Waits until wait-function returns true, event queue
is not empty or none of the above happened before a timeout.

- Returns (values nil :wait-function) if wait-function returns true
- Reads and returns one event from the queue if it is not empty
- Returns (values nil :timeout) otherwise."))
    (:generic* event-queue-append (event-queue item)
      (:documentation "Append the item at the end of the queue. Does event compression."))
    (:generic* event-queue-prepend (event-queue item)
      (:documentation "Prepend the item to the beginning of the queue."))
    (:generic* event-queue-peek (event-queue)
      (:documentation "Peeks the first event in a queue. Queue is left unchanged.
If queue is empty returns NIL."))
    (:generic* event-queue-peek-if (predicate event-queue)
      (:documentation "Goes through the whole event queue and returns the first
event, which satisfies PREDICATE. Queue is left unchanged. Returns NIL if there
is no such event."))
    (:generic* event-queue-listen (event-queue)
      (:documentation "Returns true if there are any events in the queue. Otherwise
returns NIL."))
    (:generic* event-queue-listen-or-wait (event-queue &key timeout wait-function)
      (:documentation "Waits until wait-function returns true, event queue
is not empty or none of the above happened before a timeout.

- Returns (values nil :wait-function) when wait-function returns true
- Returns true when there are events in the queue before a timeout
- Returns (values nil :timeout) otherwise.")))
   ("8.1.2 Input Protocol Classes"
    (:mixin standard-sheet-input-mixin)
    (:mixin immediate-sheet-input-mixin)
    (:mixin sheet-mute-input-mixin)
    (:mixin delegate-sheet-input-mixin)
    (:accessor delegate-sheet-delegate)
    (:mixin* clim-sheet-input-mixin)))
  ("8.2 Standard Device Events"
   (:protocol-class event () () (:default-initargs :timestamp nil))
   (:macro* define-event-class (name superclasses slots &rest options))
   (:reader event-timestamp)
   (:reader event-type)
   (:class device-event (event) () (:default-initargs :sheet nil :modifier-state nil))
   (:reader* device-event-x)
   (:reader* device-event-y)
   (:reader* device-event-native-x)
   (:reader* device-event-native-y)
   (:reader* device-event-native-graft-x)
   (:reader* device-event-native-graft-y)
   (:reader event-sheet)
   (:reader event-modifier-state)
   (:class keyboard-event (device-event) () (:default-initargs :key-name nil))
   (:reader keyboard-event-key-name)
   (:reader keyboard-event-character)
   (:class key-press-event (keyboard-event))
   (:class key-release-event (keyboard-event))
   (:class pointer-event (device-event) () (:default-initargs :pointer nil :button nil :x nil :y nil))
   (:reader pointer-event-x)
   (:reader pointer-event-y)
   (:reader pointer-event-native-x)
   (:reader pointer-event-native-y)
   (:reader pointer-event-pointer)
   (:class pointer-button-event (pointer-event))  
   (:reader pointer-event-button)
   (:class pointer-button-press-event (pointer-button-event))
   (:class pointer-button-release-event (pointer-button-event))
   (:class pointer-button-hold-event (pointer-button-event))
   (:class pointer-click-event (pointer-button-event))
   (:class pointer-double-click-event (pointer-button-event))
   (:class pointer-click-and-hold-event (pointer-button-event))
   (:class* pointer-scroll-event (pointer-button-event))
   (:reader* pointer-event-delta-x)
   (:reader* pointer-event-delta-y)
   (:class pointer-motion-event (pointer-event))
   (:class pointer-boundary-event (pointer-motion-event))
   ;; Returns:
   ;; (member :ancestor :virtual :inferior :nonlinear :nonlinear-virtual nil)
   (:generic pointer-boundary-event-kind (pointer-boundary-event))
   (:generic* pointer-update-state (pointer event))
   (:class pointer-enter-event (pointer-boundary-event))
   (:class pointer-exit-event (pointer-boundary-event))
   (:class* pointer-grab-enter-event (pointer-enter-event))
   (:class* pointer-grab-leave-event (pointer-exit-event))
   (:class* pointer-ungrab-enter-event (pointer-enter-event))
   (:class* pointer-ungrab-leave-event (pointer-exit-event))
   (:class window-event (event) () (:default-initargs :region))
   (:reader window-event-region)
   (:reader window-event-native-region)
   (:reader window-event-mirrored-sheet)
   (:class window-configuration-event (window-event))
   (:reader window-configuration-event-x)
   (:reader window-configuration-event-y)
   (:reader window-configuration-event-width)
   (:reader window-configuration-event-height)
   (:class window-repaint-event (window-event))
   (:class* window-map-event (window-event))
   (:class* window-unmap-event (window-event))
   (:class* window-destroy-event (window-event))
   (:class window-manager-event (window-event) () (:default-initargs :sheet))
   (:class window-manager-delete-event (window-manager-event))
   (:class* window-manager-focus-event (window-manager-event))
   (:class* window-manager-iconify-event (window-manager-event))
   (:class* window-manager-deiconify-event (window-manager-event))
   (:class timer-event (event))
   (:constant +pointer-left-button+ fixnum)
   (:constant +pointer-middle-button+ fixnum)
   (:constant +pointer-right-button+ fixnum)
   (:constant* +pointer-wheel-up+ fixnum)
   (:constant* +pointer-wheel-down+ fixnum)
   (:constant* +pointer-wheel-left+ fixnum)
   (:constant* +pointer-wheel-right+ fixnum)
   (:constant +shift-key+ fixnum)
   (:constant +control-key+ fixnum)
   (:constant +meta-key+ fixnum)
   (:constant +super-key+ fixnum)
   (:constant +hyper-key+ fixnum)
   (:constant* +alt-key+ fixnum))
  ("8.3 Output Protocol"
   ("8.3.3 Output Protocol Functions"
    (:mixin standard-sheet-output-mixin)
    (:mixin sheet-mute-output-mixin)
    (:mixin sheet-with-medium-mixin)
    (:mixin permanent-medium-sheet-output-mixin)
    (:mixin temporary-medium-sheet-output-mixin))
   ("8.3.4 Associating a Medium with a Sheet"
    (:macro with-sheet-medium ((medium sheet) &body body))
    (:macro with-sheet-medium-bound ((medium sheet) &body body))
    (:generic invoke-with-sheet-medium-bound (cont medium sheet))
    (:reader sheet-medium)
    ("8.3.4.1 Grafting and Degrafting of Mediums"
     (:generic allocate-medium (port sheet))
     (:generic deallocate-medium (port medium))
     (:generic make-medium (port sheet))
     (:generic engraft-medium (medium port sheet))
     (:generic degraft-medium (medium port sheet)))))
  ("8.4 Repaint Protocol"
   ("8.4.1 Repaint Protocol Functions"
    (:generic* dispatch-repaint (sheet region))
    (:generic queue-repaint (sheet repaint-event))
    (:generic handle-repaint (sheet region))
    (:generic repaint-sheet (sheet region)))
   ("8.4.2 Repaint Protocol Classes"
    (:mixin standard-repainting-mixin)
    (:mixin immediate-repainting-mixin)
    (:mixin sheet-mute-repainting-mixin)
    (:mixin* always-repaint-background-mixin)
    (:mixin* never-repaint-background-mixin)
    (:mixin* clim-repainting-mixin)))
  ("8.5 Sheet Notification Protocol"
   ("8.5.1 Relationship to Window System Change Notifications"
    (:generic note-sheet-grafted (sheet))
    (:generic note-sheet-degrafted (sheet))
    (:generic note-sheet-adopted (sheet))
    (:generic note-sheet-disowned (sheet))
    (:generic note-sheet-enabled (sheet))
    (:generic note-sheet-disabled (sheet)))
   ("8.5.2 Sheet Geometry Notifications"
    ;; More to be written. --- RSL
    (:generic note-sheet-region-changed (sheet))
    (:generic note-sheet-transformation-changed (sheet)))))

(define-protocol "9 Ports, Grafts and Mirrored Sheets" ("Windowing Substrate")
  ("9.2 Ports"
   (:protocol-class port ())
   (:class basic-port)
   (:function find-port (&key (server-path *default-server-path*)))
   (:generic* find-port-type (symbol))
   (:variable *default-server-path*)
   (:reader port)
   (:macro* with-port ((port-var server &rest args &key &allow-other-keys) &body body))
   (:function* invoke-with-port (continuation server &rest args &key &allow-other-keys))
   (:macro with-port-locked ((port) &body body))
   (:generic invoke-with-port-locked (port continuation))
   (:function map-over-ports (fun))
   (:reader port-server-path)
   (:reader port-name)
   (:reader port-type)
   (:reader* port-modifier-state)
   (:generic port-properties (port indicator))
   (:generic (setf port-properties) (property port indicator))
   (:generic restart-port (port))
   (:generic destroy-port (port))
   ;; basic-port extra accessors
   (:accessor* port-grafts)
   (:accessor* frame-managers)
   (:accessor* port-event-process)
   (:accessor* port-lock)
   (:reader* port-text-style-mappings)
   (:reader* port-keyboard-input-focus)
   (:accessor* port-pointer)
   (:reader* port-cursors)
   (:reader* port-selections)
   (:accessor* port-grabbed-sheet)
   (:accessor* port-pressed-sheet))
  ("McCLIM extension: Font listing"
   (:generic* port-all-font-families
       (port &key invalidate-cache &allow-other-keys)
     (:documentation
      "Returns the list of all FONT-FAMILY instances known by PORT.
With INVALIDATE-CACHE, cached font family information is discarded, if any."))
   (:generic* font-family-name (font-family)
     (:documentation
      "Return the font family's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style family for this port."))
   (:generic* font-family-port (font-family)
     (:documentation "Return the port this font family belongs to."))
   (:generic* font-family-all-faces (font-family)
     (:documentation
      "Return the list of all font-face instances for this family."))
   (:generic* font-face-name (font-face)
     (:documentation
      "Return the font face's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style face for this port."))
   (:generic* font-face-family (font-face)
     (:documentation "Return the font family this face belongs to."))
   (:generic* font-face-all-sizes (font-face)
     (:documentation
      "Return the list of all font sizes known to be valid for this font,
if the font is restricted to particular sizes.  For scalable fonts, arbitrary
sizes will work, and this list represents only a subset of the valid sizes.
See font-face-scalable-p."))
   (:generic* font-face-scalable-p (font-face)
     (:documentation
      "Return true if this font is scalable, as opposed to a bitmap font.  For
a scalable font, arbitrary font sizes are expected to work."))
   (:generic* font-face-text-style (font-face &optional size)
     (:documentation
      "Return an extended text style describing this font face in the specified
size.  If size is nil, the resulting text style does not specify a size."))
   (:class* font-family)
   (:class* font-face)
   (:class* basic-font-family)
   (:class* basic-font-face))
  ("9.3 Grafts"
   (:class graft ())
   (:function* graftp (graft))
   (:constructor make-graft (port &key orientation units))
   (:generic sheet-grafted-p (sheet))
   (:function find-graft (&key (port nil) (server-path *default-server-path*) (orientation :default) (units :device)))
   (:reader graft)
   (:generic map-over-grafts (fun port))
   (:macro with-graft-locked ((graft) &body body))
   (:reader graft-orientation)
   (:reader graft-units)
   (:generic graft-width (graft &key units))
   (:generic graft-height (graft &key units))
   (:function graft-pixels-per-millimeter (graft &key orientation))
   (:function graft-pixels-per-inch (graft &key orientation))
   (:generic* graft-pixel-aspect-ratio (graft)))
  ("9.4 Mirrors and Mirrored Sheets"
   (:mixin mirrored-sheet-mixin)
   (:mixin* top-level-sheet-mixin)
   (:mixin* unmanaged-sheet-mixin)
   (:function* get-top-level-sheet (sheet))
   ("9.4.1 Mirror Functions"
    (:generic sheet-direct-mirror (sheet))
    (:generic sheet-mirrored-ancestor (sheet))
    (:generic sheet-mirror (sheet))
    (:generic realize-mirror (port mirrored-sheet))
    (:generic destroy-mirror (port mirrored-sheet))
    (:generic raise-mirror (port sheet))
    (:generic bury-mirror (port sheet))
    (:generic* port-set-mirror-name (port sheet name))
    (:generic* port-set-mirror-icon (port sheet icon))
    (:generic* port-set-mirror-geometry (port sheet region))
    (:generic* port-enable-sheet (port sheet))
    (:generic* port-disable-sheet (port sheet))
    (:generic* port-shrink-sheet (port sheet))
    (:accessor* sheet-mirror-geometry)
    (:generic* update-mirror-geometry (sheet))
    (:generic* (setf %sheet-direct-mirror) (new-val sheet)))
   ;; Minor issue: Do these functions work on any sheet, or only on sheets that
   ;; have a mirror, or only on sheets that have a direct mirror? Also, define
   ;; what a "native coordinate" are. Also, do sheet-device-transformation and
   ;; sheet-device-region really account for the user's transformation and
   ;; clipping region? --- SWM
   ("9.4.2 Internal Interfaces for Native Coordinates"
    (:reader sheet-native-transformation)
    (:reader sheet-native-region)
    (:reader sheet-device-transformation)
    (:reader sheet-device-region)
    (:generic invalidate-cached-transformations (sheet))
    (:generic invalidate-cached-regions (sheet))))
  ("22.4 The Pointer Protocol"
   (:protocol-class pointer ())
   (:class standard-pointer)
   (:accessor pointer-sheet)
   ;; returns +pointer-left-button+ etc (enum)
   (:reader pointer-button-state)
   (:reader pointer-position)
   (:generic (setf pointer-position) (x y pointer))
   (:accessor pointer-cursor)
   (:macro* with-pointer-grabbed ((port sheet &key pointer multiple-window) &body body))
   (:generic* port-grab-pointer (port pointer sheet &key multiple-window)
              (:documentation "Grab the specified pointer."))
   (:generic* port-ungrab-pointer (port pointer sheet)
              (:documentation "Ungrab the specified pointer."))
   (:generic* set-sheet-pointer-cursor (port sheet cursor)
              (:documentation "Sets the cursor associated with SHEET. CURSOR is a symbol, as described in the Franz user's guide."))))
