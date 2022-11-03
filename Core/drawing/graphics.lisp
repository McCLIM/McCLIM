;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2003 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001 Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001,2002 Alexey Dejneka
;;;  (c) copyright 2002 Brian Spilsbury
;;;  (c) copyright 2002,2003 Gilbert Baumann <gbaumann@common-lisp.net>
;;;  (c) copyright 2003-2008 Andy Hefner <ahefner@common-lisp.net>
;;;  (c) copyright 2005,2006 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2005 Rudi Schlatte <rschlatte@common-lisp.net>
;;;  (c) copyright 2008 Troels Henriksen <thenriksen@common-lisp.net>
;;;  (c) copyright 2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2016 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2018 Elias Mårtenson <lokedhs@gmail.com>
;;;  (c) copyright 2019-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;  (c) copyright 2016-2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Graphics.
;;;

(in-package #:clim-internals)

(defun do-graphics-with-options-internal
    (medium orig-medium func &rest args
     &key ink clipping-region transformation
       line-style
       line-unit line-thickness
       (line-dashes nil dashes-p)
       line-joint-shape line-cap-shape
       (text-style nil text-style-p)
       (text-family nil text-family-p)
       (text-face nil text-face-p)
       (text-size nil text-size-p)
     &allow-other-keys)
  (declare (ignore args))
  (flet ((compute-ink ()
           (when ink
             (unless (design-equalp ink (medium-ink medium))
               ink)))
         (compute-line ()
           (when (or line-style line-unit line-thickness dashes-p
                     line-joint-shape line-cap-shape)
             (let* ((old-line-style (medium-line-style medium))
                    (line-style (or line-style old-line-style))
                    (line-unit (or line-unit (line-style-unit line-style)))
                    (line-thickness (or line-thickness (line-style-thickness line-style)))
                    (line-dashes (if dashes-p line-dashes (line-style-dashes line-style)))
                    (line-joint-shape (or line-joint-shape
                                          (line-style-joint-shape line-style)))
                    (line-cap-shape (or line-cap-shape (line-style-cap-shape line-style)))
                    (new-line-style (make-line-style :unit line-unit
                                                     :thickness line-thickness
                                                     :joint-shape line-joint-shape
                                                     :cap-shape line-cap-shape
                                                     :dashes line-dashes)))
               (unless (line-style-equalp old-line-style new-line-style)
                 new-line-style))))
         (compute-text ()
           (when (or text-style text-family-p text-face-p text-size-p)
             (let* ((merged-text-style (medium-merged-text-style medium))
                    (text-style (if text-style-p
                                    (merge-text-styles text-style merged-text-style)
                                    merged-text-style))
                    (text-style (if (or text-family-p text-face-p text-size-p)
                                    (merge-text-styles (make-text-style text-family
                                                                        text-face
                                                                        text-size)
                                                       text-style)
                                    text-style)))
               (unless (text-style-equalp text-style merged-text-style)
                 text-style))))
         (compute-transformation ()
           (when transformation
             (compose-transformations (medium-transformation medium) transformation)))
         (compute-clipping-region ()
           (let ((old-clipping-region (medium-clipping-region medium)))
             (when (and clipping-region
                        (not (and old-clipping-region
                                  (or (eq clipping-region +everywhere+)
                                      (eq clipping-region old-clipping-region)
                                      (region-contains-region-p clipping-region
                                                                old-clipping-region)))))
               (region-intersection clipping-region old-clipping-region)))))
    (macrolet ((with-options (bindings &body body)
                 (loop for (place form) in bindings
                       for old-value = (gensym "OLD")
                       for new-value = (gensym "NEW")
                       collect old-value into old-vars
                       collect `(,new-value ,form) into new-vars
                       collect `(when ,new-value
                                  (setf ,old-value ,place
                                        ,place ,new-value))
                         into sets
                       collect `(when ,new-value
                                  (setf ,place ,old-value))
                         into undo
                       finally (return `(let (,@old-vars ,@new-vars)
                                          (unwind-protect (progn ,@sets ,@body)
                                            ,@(nreverse undo)))))))
      (with-options (((medium-ink medium) (compute-ink))
                     ((medium-line-style medium) (compute-line))
                     ((medium-text-style medium) (compute-text))
                     ((medium-transformation medium) (compute-transformation))
                     ((medium-clipping-region medium) (compute-clipping-region)))
        (when orig-medium
          (funcall func orig-medium))))))

;;; The generic function DO-GRAPHICS-WITH-OPTIONS is internal to the
;;; CLIM-INTERNALS package.  It is used in the expansion of the macro
;;; WITH-MEDIUM-OPTIONS.
(defgeneric do-graphics-with-options (medium function &rest options)
  (:method (medium func &rest options)
    (declare (ignore options))
    (maybe-funcall func medium))
  (:method ((medium medium) func &rest options)
    (let ((*foreground-ink* (medium-foreground medium))
          (*background-ink* (medium-background medium)))
      (apply #'do-graphics-with-options-internal medium medium func options))))

(defmacro with-medium-options ((medium args) &body body)
  `(flet ((graphics-op (medium)
            (declare (ignorable medium))
            ,@body))
     (declare (dynamic-extent #'graphics-op))
     (apply #'do-graphics-with-options ,medium #'graphics-op ,args)))

(defmacro with-drawing-options ((medium &rest drawing-options) &body body)
  (with-stream-designator (medium '*standard-output*)
    (with-gensyms (gcontinuation cont-arg)
      `(flet ((,gcontinuation (,cont-arg)
                (declare (ignore ,cont-arg))
                ,@body))
         (declare (dynamic-extent #',gcontinuation))
         (invoke-with-drawing-options
          ,medium #',gcontinuation ,@drawing-options)))))

(defmethod invoke-with-drawing-options ((medium medium) continuation
                                        &rest drawing-options
                                        &key ink transformation clipping-region
                                        line-style text-style
                                        &allow-other-keys)
  (declare (ignore ink transformation clipping-region line-style text-style))
  (with-medium-options (medium drawing-options)
    (funcall continuation medium)))

;;; Compatibility with real CLIM
(defmethod invoke-with-drawing-options
    (medium continuation &rest drawing-options)
  (declare (ignore drawing-options))
  (funcall continuation medium))

(defmethod invoke-with-identity-transformation
    ((medium medium) continuation)
  (letf (((medium-transformation medium) +identity-transformation+))
    (funcall continuation medium)))

(defmethod invoke-with-local-coordinates (medium cont x y)
  ;; For now we do as real CLIM does.
  ;; Default seems to be the cursor position.
  ;; Moore suggests we use (0,0) if medium is no stream.
  ;;
  ;; Furthermore, the specification is vague about possible scalings ...
  (orf x 0)
  (orf y 0)
  (multiple-value-bind (mxx mxy myy myx tx ty)
      (get-transformation (medium-transformation medium))
    (declare (ignore tx ty))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   mxx mxy myy myx
                                   x y))
        (funcall cont medium)))))

(defmethod invoke-with-first-quadrant-coordinates (medium cont x y)
  ;; First we do the same as invoke-with-local-coordinates but rotate and deskew
  ;; it so that it becomes first-quadrant. We do this by simply measuring the
  ;; length of the transformed x and y "unit vectors".  [That is (0,0)-(1,0) and
  ;; (0,0)-(0,1)] and setting up a transformation which features an upward
  ;; pointing y-axis and a right pointing x-axis with a length equal to above
  ;; measured vectors.
  (orf x 0)
  (orf y 0)
  (let* ((tr (medium-transformation medium))
         (xlen
          (multiple-value-bind (dx dy) (transform-distance tr 1 0)
            (sqrt (+ (expt dx 2) (expt dy 2)))))
         (ylen
          (multiple-value-bind (dx dy) (transform-distance tr 0 1)
            (sqrt (+ (expt dx 2) (expt dy 2))))))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   xlen 0 0 (- ylen)
                                   x y))
        (funcall cont medium)))))

;;; 10.3 Line Styles

;;; 10.3.2 Contrasting Dash Patterns

(defconstant +contrasting-dash-patterns+
  ;; Must be at least eight according to the specification (Section
  ;; 10.3.2 Contrasting Dash Patterns).
  #(#(2 2) #(2 4)     #(2 8)     #(2 16) ; dots with varying empty space
           #(4 2)     #(8 2)     #(16 2) ; varying dashes with minimum empty space
           #(2 2 4 2) #(2 2 8 2)))       ; mixed

(defmethod contrasting-dash-pattern-limit (port)
  (declare (ignore port))
  (length +contrasting-dash-patterns+))

(defun make-contrasting-dash-patterns (n &optional k)
  (let ((contrasting-dash-patterns +contrasting-dash-patterns+))
    (unless (<= 1 n (length contrasting-dash-patterns))
      (error "The argument N = ~D is out of range [1, ~D]"
             n (length contrasting-dash-patterns)))
    (unless (or (null k) (<= 0 k (1- n)))
      (error "The argument K = ~D is out of range [0, ~D]" k (1- n)))
    (if (null k)
        (subseq contrasting-dash-patterns 0 n)
        (aref contrasting-dash-patterns k))))

;;;
;;; DRAW-DESIGN
;;

(defmethod draw-design (medium (design point)
                        &rest options &key &allow-other-keys)
  (with-medium-options (medium options)
    (medium-draw-point* medium (point-x design) (point-y design))))

(defmethod draw-design (medium (design polyline)
                        &rest options &key &allow-other-keys)
  (with-medium-options (medium options)
    (let ((coords (expand-point-seq (polygon-points design)))
          (closed (polyline-closed design)))
     (medium-draw-polygon* medium coords closed nil))))

(defmethod draw-design (medium (design polygon)
                        &rest options &key (filled t) &allow-other-keys)
  (with-medium-options (medium options)
    (let ((coords (expand-point-seq (polygon-points design))))
      (medium-draw-polygon* medium coords t filled))))

(defmethod draw-design (medium (design polybezier)
                        &rest options &key &allow-other-keys)
  (with-medium-options (medium options)
    (let ((coords (expand-point-seq (bezigon-points design))))
      (medium-draw-bezigon* medium coords nil))))

(defmethod draw-design (medium (design bezigon)
                        &rest options &key (filled t) &allow-other-keys)
  (with-medium-options (medium options)
    (let ((coords (expand-point-seq (bezigon-points design))))
     (medium-draw-bezigon* medium coords filled))))

(defmethod draw-design (medium (design line)
                        &rest options &key &allow-other-keys)
  (with-medium-options (medium options)
    (multiple-value-bind (x1 y1) (line-start-point* design)
      (multiple-value-bind (x2 y2) (line-end-point* design)
        (medium-draw-line* medium x1 y1 x2 y2)))))

(defmethod draw-design (medium (design rectangle)
                        &rest options &key (filled t) &allow-other-keys)
  (with-medium-options (medium options)
    (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* design)
      (medium-draw-rectangle* medium x1 y1 x2 y2 filled))))

(defmethod draw-design (medium (design ellipse)
                        &rest options &key (filled t) &allow-other-keys)
  (with-medium-options (medium options)
    (multiple-value-bind (cx cy) (ellipse-center-point* design)
      (multiple-value-bind (r1x r1y r2x r2y) (ellipse-radii design)
        (let ((sa (or (ellipse-start-angle design) 0.0))
              (ea (or (ellipse-end-angle design) (* 2.0 pi))))
          (medium-draw-ellipse* medium cx cy r1x r1y r2x r2y sa ea filled))))))

(defmethod draw-design (medium (design elliptical-arc)
                        &rest options &key &allow-other-keys)
  (with-medium-options (medium options)
    (multiple-value-bind (cx cy) (ellipse-center-point* design)
      (multiple-value-bind (r1x r1y r2x r2y) (ellipse-radii design)
        (let ((sa (or (ellipse-start-angle design) 0.0))
              (ea (or (ellipse-end-angle design) (* 2.0 pi))))
          (medium-draw-ellipse* medium cx cy r1x r1y r2x r2y sa ea nil))))))

(defmethod draw-design (medium (design standard-region-union)
                        &rest options &key &allow-other-keys)
  (map-over-region-set-regions (lambda (region)
                                 (apply #'draw-design medium region options))
                               design))

(defmethod draw-design (medium (design standard-rectangle-set)
                        &rest options &key &allow-other-keys)
  ;; ### we can do better (faster) than this.
  (map-over-region-set-regions (lambda (region)
                                 (apply #'draw-design medium region options))
                               design))

(defmethod draw-design (medium (design standard-region-intersection)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :clipping-region design options))

(defmethod draw-design (medium (design standard-region-complement)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :clipping-region design options))

(defmethod draw-design (medium (design (eql +nowhere+))
                        &rest options &key &allow-other-keys)
  (declare (ignore medium design options))
  nil)

(defmethod draw-design ((medium medium) (design everywhere-region)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium
         (bounding-rectangle (medium-clipping-region medium)) options))

;;;

(defmethod draw-design (medium (color color)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

(defmethod draw-design (medium (color opacity)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

(defmethod draw-design (medium (color standard-flipping-ink)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

(defmethod draw-design (medium (color indirect-ink)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium +everywhere+ :ink color options))

;;;

(defmethod draw-design (medium (design over-compositum)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium (compositum-background design) options)
  (apply #'draw-design medium (compositum-foreground design) options))

(defmethod draw-design (medium (design in-compositum)
                        &rest options &key &allow-other-keys)
  (let ((mask (compositum-mask design)))
    (if (regionp mask)
        (apply #'draw-design medium mask :ink (compositum-ink design) options)
        (apply #'draw-design medium +everywhere+ :ink design options))))

(defmethod draw-design (medium (design out-compositum)
                        &rest options &key &allow-other-keys)
  (let ((mask (compositum-mask design)))
    (if (regionp mask)
        (apply #'draw-design medium (region-complement mask)
               :ink (compositum-ink design)
               options)
        (apply #'draw-design medium +everywhere+
               :ink design
               options))))

;;;

(defmethod draw-design (medium (pattern pattern)
                        &key clipping-region transformation &allow-other-keys)
  ;; It is said, that DRAW-PATTERN* performs only translation from the supplied
  ;; transformation. If we draw pattern with a DRAW-DESIGN we do apply full
  ;; transformation. That way we have open door for easy drawing transformed
  ;; patterns without compromising the specification. -- jd 2018-09-08
  (let ((width (pattern-width pattern))
        (height (pattern-height pattern)))
    (flet ((draw-it ()
             (draw-rectangle* medium 0 0 width height
                              :ink (transform-region (medium-transformation medium) pattern))))
      (if (or clipping-region transformation)
          (with-drawing-options (medium :clipping-region clipping-region
                                        :transformation  transformation)
            (draw-it))
          (draw-it)))))

(defmethod draw-design (medium (pattern transformed-pattern)
                        &key clipping-region transformation &allow-other-keys)
  (flet ((draw-it ()
           (let* ((pattern-tr (transformed-design-transformation pattern))
                  (pattern-ds (transformed-design-design pattern))
                  (ink-tr (compose-transformations (medium-transformation medium) pattern-tr))
                  (width (pattern-width pattern-ds))
                  (height (pattern-height pattern-ds))
                  (region (transform-region pattern-tr (make-rectangle* 0 0 width height))))
             (draw-design medium region :ink (transform-region ink-tr pattern-ds)))))
    (if (or clipping-region transformation)
        (with-drawing-options (medium :clipping-region clipping-region
                                      :transformation  transformation)
          (draw-it))
        (draw-it))))

(defun draw-pattern* (medium pattern x y &key clipping-region transformation)
  ;; Note: I believe the sample implementation in the spec is incorrect. --GB
  ;; Note: It is just slightly incorrect - patterns are rectangular objects
  ;; aligned with XY axis. For drawing transformed designs we need to transform
  ;; said rectangular region hence we need to use DRAW-DESIGN. -- jd 2018-09-05
  (check-type pattern pattern)
  (labels ((draw (x y sx sy)
             ;; As I read the spec, the pattern itself is not transformed, so we
             ;; should draw the full (untransformed) pattern at the transformed x/y
             ;; coordinates. This requires we revert to the identity transformation
             ;; before drawing the rectangle. -Hefner
             (let* (;; Effective design
                    (effective-design  (transformed-design-design pattern))
                    (design-rectangle  (make-rectangle*
                                        0 0
                                        (pattern-width effective-design)
                                        (pattern-height effective-design)))
                    ;; Effective pattern transformation
                    (pattern-transform (transformed-design-transformation pattern))
                    (pattern-region    (transform-region pattern-transform design-rectangle))
                    ;; Final transformation and region. Adjust for
                    ;; offsets introduced by PATTERN-TRANSFORM and
                    ;; axis flipping introduced by the medium
                    ;; transformation.
                    (final-transform   (with-bounding-rectangle* (x1 y1 x2 y2)
                                           pattern-region
                                         (compose-transformations
                                          (make-translation-transformation
                                           (- x x1 (if (minusp sx) (- x2 x1) 0))
                                           (- y y1 (if (minusp sy) (- y2 y1) 0)))
                                          pattern-transform)))
                    (final-region      (transform-region
                                        final-transform design-rectangle))
                    (final-ink         (transform-region final-transform effective-design)))
               (with-identity-transformation (medium)
                 (draw-design medium final-region :ink final-ink))))
           (prepare-and-draw (transformation)
             ;; Capture the translation and axis-flipping aspects of
             ;; TRANSFORMATION.
             (multiple-value-bind (tx ty)
                 (transform-position transformation x y)
               (multiple-value-bind (sx sy)
                   (transform-distance transformation 1 1)
                 (draw tx ty (signum sx) (signum sy))))))
    (if (or clipping-region transformation)
        (with-drawing-options (medium :clipping-region clipping-region
                                      :transformation  transformation)
          (prepare-and-draw (medium-transformation medium)))
        (prepare-and-draw (medium-transformation medium)))))
