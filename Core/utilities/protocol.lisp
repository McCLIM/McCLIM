;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Low level protocol (with extensions).
;;;

(in-package #:climi)

;;; B The CLIM-SYS Package

;;; B.1 Resources
(pledge :macro defresource (name parameters &key constructor initializer deinitializer matcher initial-copies))
(pledge :macro using-resource ((variable name &rest parameters) &body body))
(declfun allocate-resource (name &rest parameters))
(declfun deallocate-resource (name object))
(declfun clear-resource (name))
(declfun name-resource (fun name))

;;; B.2 Multiprocessing
(pledge :variable *multiprocessing-p*)
(declfun make-process (fun &key name))
(declfun destroy-process (process))
(declfun current-process nil)
(declfun all-processes nil)
(declfun processp (object))
(declfun process-name (process))
(declfun process-state (process))
(declfun process-whostate (process))
(declfun process-wait (reason predicate))
(declfun process-wait-with-timeout (reason timeout predicate))
(declfun process-yield nil)
(declfun process-interrupt (process fun))
(declfun disable-process (process))
(declfun enable-process (process))
(declfun restart-process (process))
(pledge :macro without-scheduling (&body body))
(declfun atomic-incf (reference))
(declfun atomic-decf (reference))
(declfun make-condition-variable nil)
(declfun condition-wait (cv lock &optional timeout))
(declfun condition-notify (cv))

;;; B.3 Locks
(declfun make-lock (&optional name))
(pledge :macro with-lock-held ((place &optional state) &body body))
(declfun make-recursive-lock (&optional name))
(pledge :macro with-recursive-lock-held ((place &optional state) &body body))

;;; B.4 Multiple Value setf
(pledge :macro defgeneric* (name lambda-list &body options))
(pledge :macro defmethod* (name lambda-list &body body))

;;; C Encapsulating Streams
(define-protocol-class encapsulating-stream ()) ; initargs (:stream)
(pledge :class standard-encapsulating-stream (encapsulating-stream))
(defgeneric encapsulating-stream-stream (encapsulating-stream)
  (:documentation "The stream encapsulated by an encapsulating stream"))

;;; Geometry Substrate

;;; 3 Regions

;;; 3.1 General Regions
(define-protocol-class design nil)
(define-protocol-class region (design))
(define-protocol-class path (region bounding-rectangle))
(define-protocol-class area (region bounding-rectangle))
(pledge :type coordinate)
(declfun coordinate (n))
(declfun coordinate-epsilon nil)
(declfun coordinate= (x y))
(declfun coordinate/= (x y))
(declfun coordinate<= (x y))
(declfun coordinate-between (c1 x c2))
(declfun coordinate-between* (low x high))
(pledge :type standard-rectangle-coordinate-vector)
(pledge :class nowhere-region (region))
(pledge :class everywhere-region (region))
(pledge :constant +everywhere+ region)
(pledge :constant +nowhere+ region)

;;; 3.1.1 The Region Predicate Protocol
(defgeneric region-equal (region1 region2))
(defgeneric region-contains-region-p (region1 region2))
(defgeneric region-contains-position-p (region x y))
(defgeneric region-intersects-region-p (region1 region2))

;;; 3.1.2 Region Composition Protocol
(define-protocol-class region-set (region bounding-rectangle))
(pledge :class standard-region-union (region-set))
(pledge :class standard-region-intersection (region-set))
(pledge :class standard-region-difference (region-set))
(pledge :class standard-region-complement (region-set))
(pledge :class standard-rectangle-set (region-set))
(defgeneric region-set-regions (region &key normalize))
(defgeneric map-over-region-set-regions (fun region &key normalize))
(defgeneric region-union (region1 region2))
(defgeneric region-intersection (region1 region2))
(defgeneric region-difference (region1 region2))
(defgeneric region-complement (region))
(declfun region-exclusive-or (region1 region2))
(pledge :condition region-set-not-rectangular (error))

;;; 3.2 Other Region Types

;;; 3.2.1 Points
(define-protocol-class point (region bounding-rectangle))
(pledge :class standard-point (point))
(declfun make-point (x y))
(defgeneric point-position (point))
(defgeneric point-x (point))
(defgeneric point-y (point))

;;; 3.2.2 Polygons and Polylines
(define-protocol-class polygon (bezigon))
(define-protocol-class polyline (polybezier))
(pledge :class standard-polygon (polygon))
(declfun make-polygon (point-seq))
(declfun make-polygon* (coord-seq))
(pledge :class standard-polyline (standard-polyline))
(declfun make-polyline (point-seq &key closed))
(declfun make-polyline* (coord-seq &key closed))
(defgeneric polygon-points (polygon-or-polyline))
(defgeneric map-over-polygon-coordinates (fun polygon-or-polyline))
(defgeneric map-over-polygon-segments (fun polygon-or-polyline))
(defgeneric polyline-closed (polyline))

;;; 3.2.3 Lines
(define-protocol-class line (polyline))
(pledge :class standard-line (line))
(declfun make-line (start-point end-point))
(declfun make-line* (x1 y1 x2 y2))
(defgeneric line-start-point* (line))
(defgeneric line-end-point* (line))
(defgeneric line-start-point (line))
(defgeneric line-end-point (line))

;;; 3.2.4 Rectangles
(define-protocol-class rectangle (polygon))
(pledge :class standard-rectangle (rectangle))
(declfun make-rectangle (point1 point2))
(declfun make-rectangle* (x1 y1 x2 y2))
(defgeneric rectangle-edges* (rectangle))
(defgeneric rectangle-min-point (rectangle))
(defgeneric rectangle-max-point (rectangle))
(defgeneric rectangle-min-x (rectangle))
(defgeneric rectangle-min-y (rectangle))
(defgeneric rectangle-max-x (rectangle))
(defgeneric rectangle-max-y (rectangle))
(defgeneric rectangle-width (rectangle))
(defgeneric rectangle-height (rectangle))
(defgeneric rectangle-size (rectangle))

;;; 3.2.5 Ellipses and Elliptical Arcs
(define-protocol-class ellipse (area))
(define-protocol-class elliptical-arc (path))
(pledge :class standard-ellipse (ellipse))
(declfun make-ellipse (center rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(declfun make-ellipse* (cx cy rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(pledge :class standard-elliptical-arc (elliptical-arc))
(declfun make-elliptical-arc (center rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(declfun make-elliptical-arc* (cx cy rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(defgeneric ellipse-center-point* (elliptical-object))
(defgeneric ellipse-center-point (elliptical-object))
(defgeneric ellipse-radii (elliptical-object))
(defgeneric ellipse-start-angle (elliptical-object))
(defgeneric ellipse-end-angle (elliptical-object))

;;; 3.2.6 The Bezigon and Bezier Curve Protocol (McCLIM extension)
(define-protocol-class bezigon (area))
(define-protocol-class polybezier (path))
(pledge :class standard-bezigon (bezigon))
(declfun make-bezigon (point-seq))
(declfun make-bezigon* (coord-seq))
(pledge :class standard-polybezier (polybezier))
(declfun make-polybezier (point-seq))
(declfun make-polybezier* (coord-seq))
(defgeneric bezigon-points (object))
(defgeneric bezigon-order (object))
(defgeneric map-over-bezigon-segments (fun object))
(declfun map-over-bezigon-segments* (fun coord-seq order))

;;; 4 Bounding Rectangles

;;; 4.1 Bounding Rectangles
(define-protocol-class bounding-rectangle nil)
(pledge :class standard-bounding-rectangle (bounding-rectangle rectangle))
(declfun make-bounding-rectangle (x1 y1 x2 y2))
(defgeneric bounding-rectangle* (region))
(defgeneric bounding-rectangle (region))
(pledge :macro with-bounding-rectangle* ((&rest variables) region &body body))
(pledge :macro with-standard-rectangle* ((&rest variables) rectangle &body body))
(defgeneric bounding-rectangle-position (region))
(defgeneric bounding-rectangle-min-x (region))
(defgeneric bounding-rectangle-min-y (region))
(defgeneric bounding-rectangle-max-x (region))
(defgeneric bounding-rectangle-max-y (region))
(defgeneric bounding-rectangle-width (region))
(defgeneric bounding-rectangle-height (region))
(defgeneric bounding-rectangle-size (region))
(declfun copy-bounding-rectangle (region))
(declfun rounded-bounding-rectangle (region))

;;; 5 Affine Transformations

;;; 5.1 Transformations
(define-protocol-class transformation nil)
(declfun get-transformation (transformation))
(pledge :class standard-transformation (transformation))
(pledge :class standard-identity-transformation)
(pledge :class standard-translation (transformation))
(pledge :class standard-hairy-transformation (transformation))
(pledge :constant +identity-transformation+ transformation)
(pledge :macro with-transformed-position ((transformation x y) &body body))
(pledge :macro with-transformed-distance ((transformation dx dy) &body body))
(pledge :macro with-transformed-angles ((transformation clockwisep &rest angles) &body body))
(pledge :macro with-transformed-positions ((transformation coord-seq) &body body))
(pledge :macro with-transformed-positions* ((transformation &rest coord-seq) &body body))
(declfun transform-angle (transformation phi))
(declfun untransform-angle (transformation phi))

;;; 5.1.1 Transformation Conditions
(pledge :condition transformation-error (error) (:initargs :points))
(pledge :condition transformation-underspecified (error) (:initargs :points))
(pledge :condition reflection-underspecified (error) (:initargs :points))
(pledge :condition singular-transformation (error) (:initargs :transformation))
(pledge :condition rectangle-transformation-error (error) (:initargs :rect))

;;; 5.2 Transformation Constructors
(declfun make-translation-transformation (dx dy))
(declfun make-rotation-transformation (angle &optional origin))
(declfun make-rotation-transformation* (angle &optional x0 y0))
(declfun make-scaling-transformation (sx sy &optional origin))
(declfun make-scaling-transformation* (sx sy &optional x0 y0))
(declfun make-reflection-transformation (point1 point2))
(declfun make-reflection-transformation* (x1 y1 x2 y2))
(declfun make-transformation (mxx mxy myx myy tx ty))
(declfun make-3-point-transformation (p1 p2 p3 p1* p2* p3*))
(declfun make-3-point-transformation* (x1 y1 x2 y2 x3 y3 x1* y1* x2* y2* x3* y3*))

;;; 5.3 Transformation Protocol

;;; 5.3.1 Transformation Predicates
(defgeneric transformation-equal (transformation1 transformation2))
(defgeneric identity-transformation-p (transformation))
(defgeneric invertible-transformation-p (transformation))
(defgeneric translation-transformation-p (transformation))
(defgeneric reflection-transformation-p (transformation))
(defgeneric rigid-transformation-p (transformation))
(defgeneric even-scaling-transformation-p (transformation))
(defgeneric scaling-transformation-p (transformation))
(defgeneric rectilinear-transformation-p (transformation))
;;; McCLIM extension:
(defgeneric y-inverting-transformation-p (transformation))

;;; 5.3.2 Composition of Transformations
(defgeneric compose-transformations (transformation1 transformation2))
(defgeneric invert-transformation (transformation))
(declfun compose-translation-with-transformation (transformation dx dy))
(declfun compose-scaling-with-transformation (transformation sx sy &optional origin))
(declfun compose-rotation-with-transformation (transformation angle &optional origin))
(declfun compose-transformation-with-translation (transformation dx dy))
(declfun compose-transformation-with-scaling (transformation sx sy &optional origin))
(declfun compose-transformation-with-rotation (transformation angle &optional origin))

;;; 5.3.3 Applying Transformations
(defgeneric transform-region (transformation region))
(defgeneric untransform-region (transformation region))
(defgeneric transform-position (transformation x y))
(declfun transform-positions (transformation coord-seq))
(declfun transform-position-sequence (seq-type transformation coord-seq))
(defgeneric untransform-position (transformation x y))
(defgeneric transform-distance (transformation dx dy))
(defgeneric untransform-distance (transformation dx dy))
(defgeneric transform-rectangle* (transformation x1 y1 x2 y2))
(defgeneric untransform-rectangle* (transformation x1 y1 x2 y2))

;;; Sheet and Medium Output Facilities

;;; 10 Drawing Options

;;; 10.1 Medium Components
(define-protocol-class medium nil)
(pledge :class basic-medium (medium))
(defgeneric medium-background (instance))
(defgeneric (setf medium-background) (new-value instance))
(defgeneric medium-foreground (instance))
(defgeneric (setf medium-foreground) (new-value instance))
(defgeneric medium-ink (instance))
(defgeneric (setf medium-ink) (new-value instance))
(defgeneric medium-transformation (instance))
(defgeneric (setf medium-transformation) (new-value instance))
(defgeneric medium-clipping-region (instance))
(defgeneric (setf medium-clipping-region) (new-value instance))
(defgeneric medium-line-style (instance))
(defgeneric (setf medium-line-style) (new-value instance))
(defgeneric medium-default-text-style (instance))
(defgeneric (setf medium-default-text-style) (new-value instance))
(defgeneric medium-text-style (instance))
(defgeneric (setf medium-text-style) (new-value instance))
(defgeneric medium-current-text-style (medium))
(defgeneric medium-merged-text-style (medium))
(defgeneric medium-sheet (instance))
(defgeneric medium-drawable (instance))
(defgeneric medium-device-transformation (instance))
(defgeneric medium-device-region (instance))
(defgeneric medium-native-transformation (instance))
(defgeneric medium-native-region (instance))
(pledge :class graphics-state)
(pledge :mixin gs-transformation-mixin)
(pledge :mixin gs-ink-mixin)
(pledge :mixin gs-clip-mixin)
(pledge :mixin gs-line-style-mixin)
(pledge :mixin gs-text-style-mixin)
(pledge :mixin complete-medium-state)
(defgeneric graphics-state-transformation (instance))
(defgeneric graphics-state-ink (instance))
(defgeneric graphics-state-clip (instance))
(defgeneric graphics-state-line-style (instance))
(defgeneric graphics-state-text-style (instance))
(defgeneric graphics-state-line-style-border (record medium))
(defgeneric (setf graphics-state) (new-value graphics-state))

;;; 10.2 Drawing Option Binding Forms
(pledge :macro with-medium-options ((medium args) &body body))
(pledge :macro with-drawing-options ((medium &rest drawing-options &key ink transformation clipping-region line-style text-style &allow-other-keys) &body body))
(defgeneric invoke-with-drawing-options (medium cont &rest drawing-options &key &allow-other-keys))

;;; 10.2.1 Transformation "Convenience" Forms
(pledge :macro with-translation ((medium dx dy) &body body))
(pledge :macro with-scaling ((medium sx &optional sy origin) &body body))
(pledge :macro with-rotation ((medium angle &optional origin) &body body))
(pledge :macro with-identity-transformation)
(defgeneric invoke-with-identity-transformation (medium continuation))
(defgeneric invoke-with-local-coordinates (medium continuation x y))
(defgeneric invoke-with-first-quadrant-coordinates (medium continuation x y))

;;; 10.2.2 Estabilishing Local Coordinate System
(pledge :macro with-local-coordinates ((medium &optional x y) &body body))
(pledge :macro with-first-quadrant-coordinates ((medium &optional x y) &body body))

;;; 10.3 Line Styles
(define-protocol-class line-style nil nil (:default-initargs :line-unit :normal :line-thickness 1 :line-joint-shape :miter :line-cap-shape :butt :line-dashes nil))
(pledge :class standard-line-style (line-style))
(declfun make-line-style (&key unit thickness joint-shape cap-shape dashes))
(defgeneric line-style-equalp (style1 style2))

;;; 10.3.1 Line Style Protocol and Line Style Suboptions
(defgeneric line-style-unit (instance))
(defgeneric line-style-thickness (instance))
(defgeneric line-style-joint-shape (instance))
(defgeneric line-style-cap-shape (instance))
(defgeneric line-style-dashes (instance))

;;; 10.3.2 Contrasting Dash Patterns
(declfun make-contrasting-dash-patterns (n &optional k))
(defgeneric contrasting-dash-pattern-limit (port))

;;; 11 Text Styles

;;; 11.1 Text Style
(define-protocol-class text-style nil nil (:default-initargs :text-family nil :text-face nil :text-size nil))
(pledge :class standard-text-style (text-style))
(defgeneric text-style-equalp (style1 style2))
(declfun make-text-style (family face size))
(pledge :constant *default-text-style*)
(pledge :constant *undefined-text-style*)

;;; 11.1.1 Text Style Protocol and Text Style Suboptions
(defgeneric text-style-components (instance))
(defgeneric text-style-family (instance))
(defgeneric text-style-face (instance))
(defgeneric text-style-size (instance))
(declfun parse-text-style (style-spec))
(declfun parse-text-style* (style))
(declfun normalize-font-size (size))
(defgeneric merge-text-styles (style1 style2))
(defgeneric text-style-ascent (text-style medium))
(defgeneric text-style-descent (text-style medium))
(defgeneric text-style-width (text-style medium))
(defgeneric text-style-height (text-style medium))
(defgeneric text-style-fixed-width-p (text-style medium))
(defgeneric text-size (medium string &key text-style start end))
(defgeneric text-style-leading (text-style medium) (:method (text-style medium) 1.2))
(defgeneric text-style-character-width (text-style medium char) (:method (text-style medium char) (text-size medium char :text-style text-style)))
(defgeneric text-bounding-rectangle* (medium string &key text-style start end align-x align-y direction))

;;; 11.2 Text Style Binding Forms
(pledge :macro with-text-style ((medium text-style) &body body))
(defgeneric invoke-with-text-style (medium cont text-style))
(pledge :macro with-text-family ((medium family) &body body))
(pledge :macro with-text-face ((medium face) &body body))
(pledge :macro with-text-size ((medium size) &body body))

;;; 11.3 Controlling Text Style Mappings
(defgeneric text-style-mapping (port text-style &optional character-set))
(defgeneric (setf text-style-mapping) (mapping port text-style &optional character-set))
(pledge :class device-font-text-style (text-style))
(declfun device-font-text-style-p (object))
(defgeneric make-device-font-text-style (display-device device-font-name))
(defgeneric device-font-name (instance))

;;; 12 Graphics

;;; 12.5 Drawing Functions

;;; 12.5.1 Basic Drawing Functions
(pledge :macro def-sheet-trampoline (name (&rest args)))
(pledge :macro def-graphic-op (name (&rest args)))
(declfun draw-point (sheet point &rest drawing-options &key &allow-other-keys))
(declfun draw-point* (sheet x y &rest drawing-options &key  &allow-other-keys))
(declfun draw-points (sheet point-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-points* (sheet position-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-line (sheet point1 point2 &rest drawing-options &key &allow-other-keys))
(declfun draw-line* (sheet x1 y1 x2 y2 &rest drawing-options &key &allow-other-keys))
(declfun draw-lines (sheet point-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-lines* (sheet position-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-polygon (sheet point-seq &rest drawing-options &key (filled t) (closed t) &allow-other-keys))
(declfun draw-polygon* (sheet position-seq &rest drawing-options &key (filled t) (closed t) &allow-other-keys))
(declfun draw-rectangle (sheet point1 point2 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rectangle* (sheet x1 y1 x2 y2 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rectangles (sheet points &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rectangles* (sheet position-seq &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-ellipse (sheet center rdx1 rdy1 rdx2 rdy2 &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-ellipse* (sheet cx cy rdx1 rdy1 rdx2 rdy2 &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-circle (sheet center radius &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-circle* (sheet cx cy radius &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-text (sheet text point &rest drawing-options &key (start 0) (end nil) (align-x :left) (align-y :baseline) (toward-point nil toward-point-p) transform-glyphs &allow-other-keys))
(declfun draw-text* (sheet text x y &rest drawing-options &key (start 0) (end nil) (align-x :left) (align-y :baseline) (toward-x (1+ x)) (toward-y y) transform-glyphs &allow-other-keys))
(declfun draw-triangle (sheet point1 point2 point3 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-triangle* (sheet x1 y1 x2 y2 x3 y3 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-bezigon (sheet point-seq &rest drawing-args &key (filled t) &allow-other-keys))
(declfun draw-bezigon* (sheet position-seq &rest drawing-args &key (filled t) &allow-other-keys))
(declfun draw-image (sheet pattern point &rest drawing-options))
(declfun draw-image* (sheet pattern x y &rest drawing-options))

;;; 12.5.2 Compound Drawing Functions
(declfun draw-arrow (medium point1 point2 &rest drawing-options &key (to-head t) from-head (head-length 10) (head-width 5) (head-filled nil) angle &allow-other-keys))
(declfun draw-arrow* (medium x1 y1 x2 y2 &rest drawing-options &key (to-head t) from-head (head-length 10) (head-width 5) (head-filled nil) angle &allow-other-keys))
(declfun draw-oval (medium center rx ry &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-oval* (medium cx cy rx ry &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rounded-rectangle* (sheet x1 y1 x2 y2 &rest args &key (radius 7) (radius-x radius) (radius-y radius) (radius-left radius-x) (radius-right radius-x) (radius-top radius-y) (radius-bottom radius-y) filled &allow-other-keys))

;;; 12.6 Pixmaps
(defgeneric allocate-pixmap (medium width height))
(defgeneric deallocate-pixmap (pixmap))
(defgeneric pixmap-width (instance))
(defgeneric pixmap-height (instance))
(defgeneric pixmap-depth (instance))
(defgeneric copy-to-pixmap (source src-x src-y width height &optional pixmap dst-x dst-y))
(defgeneric copy-from-pixmap (pixmap src-x src-y width height destination dst-x dst-y))
(defgeneric copy-area (medium src-x src-y width height dst-x dst-y))
(defgeneric medium-copy-area (source src-x src-y width height destination dst-x dst-y))
(pledge :macro with-output-to-pixmap ((medium-var medium &key width height) &key body))
(defgeneric invoke-with-output-to-pixmap (medium cont &key width height))

;;; 12.7 Graphics Protocols

;;; 12.7.2 Medium-specific Drawing Functions
(defgeneric medium-draw-point* (medium x y))
(defgeneric medium-draw-points* (medium coord-seq))
(defgeneric medium-draw-line* (medium x1 y1 x2 y2))
(defgeneric medium-draw-lines* (medium coord-seq))
(defgeneric medium-draw-polygon* (medium coord-seq closed filled))
(defgeneric medium-draw-rectangles* (medium coord-seq filled))
(defgeneric medium-draw-rectangle* (medium left top right bottom filled))
(defgeneric medium-draw-ellipse* (medium cx cy rdx1 rdy1 rdx2 rdy2 start-angle end-angle filled))
(defgeneric medium-draw-text* (medium string x y start end align-x align-y toward-x toward-y transform-glyphs))
(defgeneric medium-draw-bezigon* (medium coord-seq filled))

;;; 12.7.3 Other Medium-specific Output Functions
(defgeneric medium-finish-output (medium))
(defgeneric medium-force-output (medium))
(defgeneric medium-clear-area (medium x1 y1 x2 y2))
(defgeneric medium-beep (medium))
(defgeneric beep (&optional medium))
(defgeneric medium-buffering-output-p (instance))
(defgeneric (setf medium-buffering-output-p) (new-value instance))
(pledge :macro with-output-buffered (medium &optional (buffer-p t)))
(defgeneric invoke-with-output-buffered (medium cont &optional buffered-p))
(pledge :macro with-output-to-drawing-stream ((stream backend destination &rest args) &body body))
(defgeneric invoke-with-output-to-drawing-stream (cont backend destination &key &allow-other-keys))

(defgeneric medium-miter-limit (medium)
  (:documentation
   "If LINE-STYLE-JOINT-SHAPE is :MITER and the angle between two
   consequent lines is less than the values return by
   MEDIUM-MITER-LIMIT, :BEVEL is used instead."))

(defgeneric line-style-effective-thickness (line-style medium)
  (:documentation
   "Returns the thickness in device units of a line,
rendered on MEDIUM with the style LINE-STYLE."))

(defgeneric line-style-effective-dashes (line-style medium)
  (:documentation
   "Return a dash length or a sequence of dash lengths device units
for a dashed line, rendered on MEDIUM with the style LINE-STYLE."))

;;; 13 Drawing in Color

;;; 13.3 Color
(define-protocol-class color (design))
(pledge :class standard-color (color))
(declfun make-rgb-color (red green blue))
(declfun make-ihs-color (intensity hue saturation))
(declfun make-gray-color (luminance))
(declfun make-named-color (name red green blue))
(defgeneric color-rgb (instance))
(defgeneric color-ihs (instance))
(defgeneric color-rgba (instance))
(defgeneric highlight-shade (ink)
  (:documentation
   "Produce an alternate shade of the given ink for the purpose of highlighting.
    Typically the ink will be brightened, but very light inks may be darkened."))

;;; 13.3.1 Standard Color Names and Constants
(pledge :constant +red+)
(pledge :constant +green+)
(pledge :constant +blue+)
(pledge :constant +cyan+)
(pledge :constant +magenta+)
(pledge :constant +yellow+)
(pledge :constant +black+)
(pledge :constant +white+)

;;; 13.3.2 Contrastin Colors
(declfun make-contrasting-inks (n &optional k))
(defgeneric contrasting-inks-limit (instance))

;;; 13.4 Opacity
(define-protocol-class opacity (design))
(declfun make-opacity (value))
(pledge :constant +transparent-ink+)
(defgeneric opacity-value (instance))

;;; 13.6 Indirect Inks
(pledge :constant +foreground-ink+)
(pledge :constant +background-ink+)
(pledge :variable *foreground-ink*)
(pledge :variable *background-ink*)
(pledge :class indirect-ink (design))
(declfun indirect-ink-p (design))
(declfun indirect-ink-ink (indirect-ink))

;;; 13.7 Flipping Ink
(pledge :class standard-flipping-ink (design))
(defgeneric make-flipping-ink (design1 design2))
(pledge :constant +flipping-ink+)
(defgeneric flipping-ink-design1 (instance))
(defgeneric flipping-ink-design2 (instance))

;;; 14 General Design

;;; 14.1 The Compositing Protocol
(defgeneric compose-over (design1 design2))
(defgeneric compose-in (ink mask))
(defgeneric compose-out (ink mask))

;;; 14.2 Patterns and Stencils
(declfun make-pattern (array designs))
(declfun make-stencil (array))
(declfun make-rectangular-tile (design width height))
(declfun make-pattern-from-bitmap-file (pathname &key format designs))
(pledge :variable *bitmap-file-readers*)
(pledge :variable *bitmap-file-writers*)
(pledge :macro define-bitmap-file-reader (bitmap-format (&rest args) &body body))
(pledge :macro define-bitmap-file-writer (format (&rest args) &body body))
(declfun bitmap-format-supported-p (format))
(declfun bitmap-output-supported-p (format))
(pledge :condition unsupported-bitmap-format (error))
(declfun read-bitmap-file (pathname &key (format :bitmap)))
(declfun write-bitmap-file (image pathname &key (format :bitmap)))
(defgeneric pattern-width (instance))
(defgeneric pattern-height (instance))
(define-protocol-class pattern (design) ()
  (:documentation "Abstract class for all pattern-like designs."))
(pledge :class stencil (pattern))
(pledge :class indexed-pattern (pattern))
(pledge :class image-pattern (pattern))
(pledge :class rectangular-tile (pattern))
(pledge :class transformed-pattern (transformed-design pattern))
(defgeneric pattern-array (instance))
(defgeneric pattern-designs (instance))
(defgeneric transformed-design-design (instance))
(defgeneric transformed-design-transformation (instance))
(defgeneric rectangular-tile-design (instance))

;;; 14.5 Arbitrary Designs
(declfun make-uniform-compositum (ink opacity-value))
(pledge :class transformed-design (design))
(pledge :class masked-compositum (design))
(defgeneric compositum-mask (instance))
(defgeneric compositum-ink (instance))
(pledge :class in-compositum (masked-compositum))
(pledge :class uniform-compositum (in-compositum))
(pledge :class out-compositum (masked-compositum))
(pledge :class over-compositum (design))
(defgeneric compositum-foreground (instance))
(defgeneric compositum-background (instance))
(defgeneric design-ink (design x y))
(declfun design-ink* (design x y))
(defgeneric design-equalp (design1 design2))
(defgeneric draw-design (medium design &key ink filled clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape text-style text-family text-face text-size))
(declfun draw-pattern* (medium pattern x y &key clipping-region transformation))

;;; 14.7 Design Protocol

;;; Windowing Substrate

;;; 7 Properties of Sheets

;;; 7.1 Basic Sheet Classes
(define-protocol-class sheet nil)
(pledge :class basic-sheet (sheet))
(defgeneric sheet-name (instance))
(defgeneric sheet-pretty-name (instance))
(defgeneric (setf sheet-pretty-name) (new-value instance))
(defgeneric sheet-icon (instance))
(defgeneric (setf sheet-icon) (new-value instance))
(defgeneric sheet-pointer-cursor (instance))
(defgeneric (setf sheet-pointer-cursor) (new-value instance))

;;; 7.2 Relationships Between Classes

;;; 7.2.1 Sheet Relationship Functions
(defgeneric sheet-parent (instance))
(defgeneric sheet-children (instance))
(defgeneric sheet-child (instance))
(defgeneric sheet-adopt-child (sheet child))
(defgeneric sheet-disown-child (sheet child &key errorp))
(defgeneric sheet-siblings (sheet))
(defgeneric sheet-enabled-children (sheet))
(defgeneric sheet-ancestor-p (sheet putative-ancestor))
(defgeneric raise-sheet (sheet))
(defgeneric bury-sheet (sheet))
(defgeneric reorder-sheets (sheet new-ordering))
(defgeneric shrink-sheet (sheet))
(pledge :condition sheet-is-not-child (error))
(pledge :condition sheet-is-top-level (error))
(pledge :condition sheet-ordering-underspecified (error))
(pledge :condition sheet-is-not-ancestor (error))
(pledge :condition sheet-already-has-parent (error))
(pledge :condition sheet-supports-only-one-child (error))
(defgeneric sheet-enabled-p (instance))
(defgeneric (setf sheet-enabled-p) (new-value instance))
(defgeneric sheet-viewable-p (sheet))
(defgeneric sheet-occluding-sheets (sheet child))
(defgeneric map-over-sheets (fun sheet))

;;; 7.2.2 Sheet Genealogy Classes
(pledge :mixin sheet-parent-mixin)
(pledge :mixin sheet-leaf-mixin)
(pledge :mixin sheet-single-child-mixin)
(pledge :mixin sheet-multiple-child-mixin)

;;; 7.3 Sheet Geometry

;;; 7.3.1 Sheet Geometry Functions
(defgeneric sheet-transformation (instance))
(defgeneric (setf sheet-transformation) (new-value instance))
(defgeneric sheet-region (instance))
(defgeneric (setf sheet-region) (new-value instance))
(defgeneric move-sheet (sheet x y))
(defgeneric resize-sheet (sheet width height))
(defgeneric move-and-resize-sheet (sheet x y width height))
(defgeneric map-sheet-position-to-parent (sheet x y))
(defgeneric map-sheet-position-to-child (sheet x y))
(defgeneric map-sheet-rectangle*-to-parent (sheet x1 y1 x2 y2))
(defgeneric map-sheet-rectangle*-to-child (sheet x1 y1 x2 y2))
(defgeneric map-over-sheets-containing-position (fun sheet x y))
(defgeneric map-over-sheets-overlapping-region (fun sheet region))
(defgeneric child-containing-position (sheet x y))
(defgeneric children-overlapping-region (sheet region))
(defgeneric children-overlapping-rectangle* (sheet x1 y1 x2 y2))
(defgeneric sheet-delta-transformation (sheet ancestor))
(defgeneric sheet-allocated-region (sheet child))

;;; 7.3.1 Sheet Geometry Classes
(pledge :mixin sheet-identity-transformation-mixin)
(pledge :mixin sheet-translation-mixin)
(pledge :mixin sheet-y-inverting-transformation-mixin)
(pledge :mixin sheet-transformation-mixin)

;;; 8 Sheet Protocols

;;; 8.1 Input Protocol

;;; 8.1.1 Input Protocol Functions
(defgeneric sheet-event-queue (sheet))
(defgeneric process-next-event (port &key wait-function timeout))
(defgeneric port-keyboard-input-focus (instance))
(defgeneric (setf port-keyboard-input-focus) (new-value instance))
(defgeneric note-input-focus-changed (sheet state)
  (:documentation "Called when a sheet receives or loses the keyboard input
focus. STATE argument is T when the sheet gains focus and NIL otherwise. This
is a McCLIM extension."))
(defgeneric distribute-event (port event))
(defgeneric dispatch-event (client event))
(defgeneric queue-event (client event))
(defgeneric schedule-event (client event delay))
(defgeneric handle-event (client event))
(defgeneric event-read (client))
(defgeneric event-read-no-hang (client))
(defgeneric event-peek (client &optional event-type))
(defgeneric event-unread (client event))
(defgeneric event-listen (client))
(defgeneric event-read-with-timeout (client &key timeout wait-function) (:documentation "Reads event from the event queue. Function returns when event is succesfully
read, timeout expires or wait-function returns true. Time of wait-function call
depends on a port."))
(defgeneric event-listen-or-wait (client &key timeout wait-function) (:documentation "When wait-function is nil then function waits for available event. Otherwise
function returns when wait-function predicate yields true. Time of wait-function
call depends on a port."))

(define-protocol-class event-queue nil)
(defgeneric event-queue-port (instance))
(defgeneric (setf event-queue-port) (new-value instance))
(defgeneric event-queue-head (instance))
(defgeneric (setf event-queue-head) (new-value instance))
(defgeneric event-queue-tail (instance))
(defgeneric (setf event-queue-tail) (new-value instance))
(pledge :class simple-event-queue (event-queue))
(pledge :class concurrent-event-queue (event-queue))

(defgeneric schedule-event-queue (queue event delay))

(defgeneric event-queue-read (event-queue)
  (:documentation "Reads one event from the queue, if there is no event, hang
until here is one."))

(defgeneric event-queue-read-no-hang (event-queue)
  (:documentation "Reads one event from the queue, if there is no event just
return NIL."))

(defgeneric event-queue-read-with-timeout (event-queue timeout wait-function)
  (:documentation "Waits until wait-function returns true, event queue
is not empty or none of the above happened before a timeout.

- Returns (values nil :wait-function) if wait-function returns true
- Reads and returns one event from the queue if it is not empty
- Returns (values nil :timeout) otherwise."))

(defgeneric event-queue-append (event-queue item)
  (:documentation "Append the item at the end of the queue. Does event compression."))

(defgeneric event-queue-prepend (event-queue item)
  (:documentation "Prepend the item to the beginning of the queue."))

(defgeneric event-queue-peek (event-queue)
  (:documentation "Peeks the first event in a queue. Queue is left unchanged.
If queue is empty returns NIL."))

(defgeneric event-queue-peek-if (predicate event-queue)
  (:documentation "Goes through the whole event queue and returns the first
event, which satisfies PREDICATE. Queue is left unchanged. Returns NIL if there
is no such event."))

(defgeneric event-queue-listen (event-queue)
  (:documentation "Returns true if there are any events in the queue. Otherwise
returns NIL."))

(defgeneric event-queue-listen-or-wait (event-queue &key timeout wait-function)
  (:documentation "Waits until wait-function returns true, event queue
is not empty or none of the above happened before a timeout.

- Returns (values nil :wait-function) when wait-function returns true
- Returns true when there are events in the queue before a timeout
- Returns (values nil :timeout) otherwise."))

;;; 8.1.2 Input Protocol Classes
(pledge :mixin standard-sheet-input-mixin)
(pledge :mixin immediate-sheet-input-mixin)
(pledge :mixin sheet-mute-input-mixin)
(pledge :mixin delegate-sheet-input-mixin)
(defgeneric delegate-sheet-delegate (instance))
(defgeneric (setf delegate-sheet-delegate) (new-value instance))
(pledge :mixin clim-sheet-input-mixin)

;;; 8.2 Standard Device Events
(define-protocol-class event nil nil (:default-initargs :timestamp nil))
(pledge :macro define-event-class (name superclasses slots &rest options))
(defgeneric event-timestamp (instance))
(defgeneric event-type (instance))
(pledge :class device-event (event) nil (:default-initargs :sheet nil :modifier-state nil))
(defgeneric device-event-x (instance))
(defgeneric device-event-y (instance))
(defgeneric device-event-native-x (instance))
(defgeneric device-event-native-y (instance))
(defgeneric event-sheet (instance))
(defgeneric event-modifier-state (instance))
(pledge :class keyboard-event (device-event) nil (:default-initargs :key-name nil))
(defgeneric keyboard-event-key-name (instance))
(defgeneric keyboard-event-character (instance))
(pledge :class key-press-event (keyboard-event))
(pledge :class key-release-event (keyboard-event))
(pledge :class pointer-event (device-event) nil (:default-initargs :pointer nil :button nil :x nil :y nil))
(defgeneric pointer-event-x (instance))
(defgeneric pointer-event-y (instance))
(defgeneric pointer-event-native-x (instance))
(defgeneric pointer-event-native-y (instance))
(defgeneric pointer-event-pointer (instance))
(pledge :class pointer-button-event (pointer-event))
(defgeneric pointer-event-button (instance))
(pledge :class pointer-button-press-event (pointer-button-event))
(pledge :class pointer-button-release-event (pointer-button-event))
(pledge :class pointer-button-hold-event (pointer-button-event))
(pledge :class pointer-click-event (pointer-button-event))
(pledge :class pointer-double-click-event (pointer-button-event))
(pledge :class pointer-click-and-hold-event (pointer-button-event))
(pledge :class pointer-scroll-event (pointer-button-event))
(defgeneric pointer-event-delta-x (instance))
(defgeneric pointer-event-delta-y (instance))
(pledge :class pointer-motion-event (pointer-event))
(pledge :class pointer-boundary-event (pointer-motion-event))
(defgeneric synthesize-pointer-motion-event (port pointer)
  (:documentation "Create a CLIM pointer motion event based on the current pointer state."))
(declfun synthesize-boundary-events (port event))
(defgeneric pointer-boundary-event-kind (pointer-boundary-event))

(defgeneric pointer-update-state (pointer event)
  (:documentation "Called by port event dispatching code to update the modifier
and button states of the pointer."))

(pledge :class pointer-enter-event (pointer-boundary-event))
(pledge :class pointer-exit-event (pointer-boundary-event))
(pledge :class pointer-grab-enter-event (pointer-enter-event))
(pledge :class pointer-grab-leave-event (pointer-exit-event))
(pledge :class pointer-ungrab-enter-event (pointer-enter-event))
(pledge :class pointer-ungrab-leave-event (pointer-exit-event))
(pledge :class window-event (event) nil (:default-initargs :region))
(defgeneric window-event-region (instance))
(defgeneric window-event-native-region (instance))
(defgeneric window-event-mirrored-sheet (instance))
(pledge :class window-configuration-event (window-event))
(pledge :class window-repaint-event (window-event))
(pledge :class window-map-event (window-event))
(pledge :class window-unmap-event (window-event))
(pledge :class window-destroy-event (window-event))
(pledge :class window-manager-event (window-event) nil (:default-initargs :sheet))
(pledge :class window-manager-delete-event (window-manager-event))
(pledge :class window-manager-focus-event (window-manager-event))
(pledge :class window-manager-iconify-event (window-manager-event))
(pledge :class window-manager-deiconify-event (window-manager-event))
(pledge :class timer-event (event))
(pledge :class lambda-event (event))
(defgeneric lambda-event-thunk (instance))
(pledge :macro with-synchronization (sheet test &body body))
(pledge :constant +pointer-left-button+ fixnum)
(pledge :constant +pointer-middle-button+ fixnum)
(pledge :constant +pointer-right-button+ fixnum)
(pledge :constant +pointer-wheel-up+ fixnum)
(pledge :constant +pointer-wheel-down+ fixnum)
(pledge :constant +pointer-wheel-left+ fixnum)
(pledge :constant +pointer-wheel-right+ fixnum)
(pledge :constant +shift-key+ fixnum)
(pledge :constant +control-key+ fixnum)
(pledge :constant +meta-key+ fixnum)
(pledge :constant +super-key+ fixnum)
(pledge :constant +hyper-key+ fixnum)
(pledge :constant +alt-key+ fixnum)

;;; 8.3 Output Protocol

;;; 8.3.3 Output Protocol Functions
(pledge :mixin standard-sheet-output-mixin)
(pledge :mixin sheet-mute-output-mixin)
(pledge :mixin sheet-with-medium-mixin)
(pledge :mixin permanent-medium-sheet-output-mixin)
(pledge :mixin temporary-medium-sheet-output-mixin)

;;; 8.3.4 Associating a Medium with a Sheet
(pledge :macro with-sheet-medium ((medium sheet) &body body))
(pledge :macro with-sheet-medium-bound ((medium sheet) &body body))
(defgeneric invoke-with-sheet-medium-bound (cont medium sheet))
(defgeneric sheet-medium (instance))

;;; 8.3.4.1 Grafting and Degrafting of Mediums
(defgeneric allocate-medium (port sheet))
(defgeneric deallocate-medium (port medium))
(defgeneric make-medium (port sheet))
(defgeneric engraft-medium (medium port sheet))
(defgeneric degraft-medium (medium port sheet))

;;; 8.4 Repaint Protocol

;;; 8.4.1 Repaint Protocol Functions
(defgeneric dispatch-repaint (sheet region))
(defgeneric queue-repaint (sheet region))
(defgeneric handle-repaint (sheet region))
(defgeneric repaint-sheet (sheet region))

;;; 8.4.2 Repaint Protocol Classes
(pledge :mixin standard-repainting-mixin)
(pledge :mixin immediate-repainting-mixin)
(pledge :mixin sheet-mute-repainting-mixin)
(pledge :mixin always-repaint-background-mixin)
(pledge :mixin never-repaint-background-mixin)
(pledge :mixin clim-repainting-mixin)

;;; 8.5 Sheet Notification Protocol

;;; 8.5.1 Relationship to Window System Change Notifications
(defgeneric note-sheet-grafted (sheet))
(defgeneric note-sheet-degrafted (sheet))
(defgeneric note-sheet-adopted (sheet))
(defgeneric note-sheet-disowned (sheet))
(defgeneric note-sheet-enabled (sheet))
(defgeneric note-sheet-disabled (sheet))

;;; 8.5.2 Sheet Geometry Notifications
(defgeneric note-sheet-region-changed (sheet))
(defgeneric note-sheet-transformation-changed (sheet))

;;; 9 Ports, Grafts and Mirrored Sheets

;;; 9.2 Ports
(define-protocol-class port nil)
(pledge :class basic-port)
(declfun find-port (&key (server-path *default-server-path*)))
(defgeneric find-port-type (symbol))
(pledge :variable *default-server-path*)
(defgeneric port (instance))
(pledge :macro with-port ((port-var server &rest args &key &allow-other-keys) &body body))
(declfun invoke-with-port (continuation server &rest args &key &allow-other-keys))
(pledge :macro with-port-locked ((port) &body body))
(defgeneric invoke-with-port-locked (port continuation))
(declfun map-over-ports (fun))
(defgeneric port-server-path (instance))
(defgeneric port-name (instance))
(defgeneric port-type (instance))
(defgeneric port-modifier-state (instance))
(defgeneric port-properties (port indicator))
(defgeneric (setf port-properties) (property port indicator))
(defgeneric restart-port (port))
(defgeneric destroy-port (port))
(defgeneric port-grafts (instance))
(defgeneric (setf port-grafts) (new-value instance))
(defgeneric frame-managers (instance))
(defgeneric (setf frame-managers) (new-value instance))
(defgeneric port-event-process (instance))
(defgeneric (setf port-event-process) (new-value instance))
(defgeneric port-lock (instance))
(defgeneric (setf port-lock) (new-value instance))
(defgeneric port-text-style-mappings (instance))
(defgeneric port-pointer (instance))
(defgeneric (setf port-pointer) (new-value instance))
(defgeneric port-cursors (instance))
(defgeneric port-selections (instance))
(defgeneric port-grabbed-sheet (instance))
(defgeneric (setf port-grabbed-sheet) (new-value instance))
(defgeneric port-pressed-sheet (instance))
(defgeneric (setf port-pressed-sheet) (new-value instance))
(declfun stored-object (port selection))
(declfun remove-stored-object (port selection))
(defgeneric port-handles-text-input-p (port))

;;; McCLIM extension: Font listing
(defgeneric port-all-font-families (port &key invalidate-cache &allow-other-keys)
  (:documentation "Returns the list of all FONT-FAMILY instances known by PORT.
With INVALIDATE-CACHE, cached font family information is discarded, if any."))

(defgeneric font-family-name (font-family)
  (:documentation "Return the font family's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style family for this port."))

(defgeneric font-family-port (font-family)
  (:documentation "Return the port this font family belongs to."))

(defgeneric font-family-all-faces (font-family)
  (:documentation "Return the list of all font-face instances for this family."))

(defgeneric font-face-name (font-face)
  (:documentation "Return the font face's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style face for this port."))

(defgeneric font-face-family (font-face)
  (:documentation "Return the font family this face belongs to."))

(defgeneric font-face-all-sizes (font-face)
  (:documentation "Return the list of all font sizes known to be valid for this font,
if the font is restricted to particular sizes.  For scalable fonts, arbitrary
sizes will work, and this list represents only a subset of the valid sizes.
See font-face-scalable-p."))

(defgeneric font-face-scalable-p (font-face)
  (:documentation "Return true if this font is scalable, as opposed to a bitmap font.  For
a scalable font, arbitrary font sizes are expected to work."))

(defgeneric font-face-text-style (font-face &optional size)
  (:documentation "Return an extended text style describing this font face in the specified
size.  If size is nil, the resulting text style does not specify a size."))

(pledge :class font-family)
(pledge :class font-face)
(pledge :class basic-font-family)
(pledge :class basic-font-face)

;;; 9.3 Grafts
(pledge :class graft nil)
(declfun graftp (graft))
(defgeneric make-graft (port &key orientation units))
(defgeneric sheet-grafted-p (sheet))
(declfun find-graft (&key (port nil) (server-path *default-server-path*) (orientation :default) (units :device)))
(defgeneric graft (instance))
(defgeneric map-over-grafts (fun port))
(pledge :macro with-graft-locked ((graft) &body body))
(defgeneric graft-orientation (instance))
(defgeneric graft-units (instance))
(defgeneric graft-width (graft &key units))
(defgeneric graft-height (graft &key units))
(declfun graft-pixels-per-millimeter (graft &key orientation))
(declfun graft-pixels-per-inch (graft &key orientation))
(defgeneric graft-pixel-aspect-ratio (graft))

;;; 9.4 Mirrors and Mirrored Sheets
(pledge :mixin mirrored-sheet-mixin)
(pledge :mixin top-level-sheet-mixin)
(pledge :mixin unmanaged-sheet-mixin)
(declfun get-top-level-sheet (sheet))

;;; 9.4.1 Mirror Functions
(defgeneric sheet-direct-mirror (sheet))
(defgeneric sheet-mirrored-ancestor (sheet))
(defgeneric sheet-mirror (sheet))
(defgeneric realize-mirror (port mirrored-sheet))
(defgeneric destroy-mirror (port mirrored-sheet))
(defgeneric raise-mirror (port sheet))
(defgeneric bury-mirror (port sheet))
(defgeneric port-set-mirror-name (port sheet name))
(defgeneric port-set-mirror-icon (port sheet icon))
(defgeneric port-set-mirror-geometry (port sheet region))
(defgeneric port-enable-sheet (port sheet))
(defgeneric port-disable-sheet (port sheet))
(defgeneric port-shrink-sheet (port sheet))
(defgeneric sheet-mirror-geometry (instance))
(defgeneric (setf sheet-mirror-geometry) (new-value instance))
(defgeneric update-mirror-geometry (sheet))
(defgeneric (setf %sheet-direct-mirror) (new-val sheet))

;;; 9.4.2 Internal Interfaces for Native Coordinates
(defgeneric sheet-native-transformation (instance))
(defgeneric sheet-native-region (instance))
(defgeneric sheet-device-transformation (instance))
(defgeneric sheet-device-region (instance))
(defgeneric invalidate-cached-transformations (sheet))
(defgeneric invalidate-cached-regions (sheet))

;;; 22.4 The Pointer Protocol
(define-protocol-class pointer nil)
(pledge :class standard-pointer)
(defgeneric pointer-sheet (instance))
(defgeneric (setf pointer-sheet) (new-value instance))
(defgeneric pointer-button-state (instance))
(defgeneric pointer-position (instance))
(defgeneric* (setf pointer-position) (x y pointer))
(defgeneric pointer-cursor (instance))
(defgeneric (setf pointer-cursor) (new-value instance))
(pledge :macro with-pointer-grabbed ((port sheet &key pointer multiple-window) &body body))
(defgeneric port-force-output (port)
  (:documentation "Flush the output buffer of PORT, if there is one."))
(defgeneric port-grab-pointer (port pointer sheet &key multiple-window)
  (:documentation "Grab the specified pointer."))
(defgeneric port-ungrab-pointer (port pointer sheet)
  (:documentation "Ungrab the specified pointer."))
(defgeneric set-sheet-pointer-cursor (port sheet cursor)
  (:documentation "Sets the cursor associated with SHEET. CURSOR is a symbol, as described in the Franz user's guide."))
