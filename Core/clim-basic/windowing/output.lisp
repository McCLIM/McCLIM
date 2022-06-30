;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

(defclass standard-sheet-output-mixin ()
  ())

(defclass sheet-mute-output-mixin ()
  ())

(defclass sheet-with-medium-mixin ()
  ((medium :initform nil
           :reader sheet-medium
           :writer (setf %sheet-medium))))

(macrolet ((frob (fn &rest args)
             `(defmethod ,fn ,(substitute '(medium sheet-with-medium-mixin)
                                          'medium
                                          args)
                ;; medium arg is really a sheet
                (let ((medium (sheet-medium medium)))
                  ,(if (symbolp fn)
                       `(,fn ,@args)
                       `(funcall #',fn ,@args))))))
  (frob medium-foreground medium)
  (frob medium-background medium)
  (frob (setf medium-foreground) design medium)
  (frob (setf medium-background) design medium)
  (frob medium-ink medium)
  (frob (setf medium-ink) design medium)
  (frob medium-transformation medium)
  (frob (setf medium-transformation) transformation medium)
  (frob medium-clipping-region medium)
  (frob (setf medium-clipping-region) region medium)
  (frob medium-line-style medium)
  (frob (setf medium-line-style) line-style medium)
  (frob medium-default-text-style medium)
  (frob (setf medium-default-text-style) text-style medium)
  (frob medium-text-style medium)
  (frob (setf medium-text-style) text-style medium)
  (frob medium-current-text-style medium)
  (frob medium-beep medium))

;;; Trampoline.
(defmethod invoke-with-output-buffered
    ((sheet sheet-with-medium-mixin) continuation &optional (buffered-p t))
  (let ((medium (sheet-medium sheet)))
    (invoke-with-output-buffered medium continuation buffered-p)))

(defclass temporary-medium-sheet-output-mixin (sheet-with-medium-mixin)
  ())

(defclass permanent-medium-sheet-output-mixin (sheet-with-medium-mixin)
  ())

(defmethod initialize-instance :after
    ((sheet permanent-medium-sheet-output-mixin) &key port)
  ;; hmm,
  (setf (%sheet-medium sheet) (make-medium port sheet))
  ;; hmm...
  (engraft-medium (sheet-medium sheet) (port sheet) sheet))

(defmacro with-sheet-medium ((medium sheet) &body body)
  (check-type medium symbol)
  (let ((fn (gensym)))
    `(labels ((,fn (,medium)
                ,(declare-ignorable-form* medium)
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-sheet-medium-bound #',fn nil ,sheet))))

(defmacro with-sheet-medium-bound ((sheet medium) &body body)
  (check-type medium symbol)
  (let ((fn (gensym)))
    `(labels ((,fn  (,medium)
                ,(declare-ignorable-form* medium)
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-sheet-medium-bound #',fn ,medium ,sheet))))

(defgeneric invoke-with-sheet-medium-bound (continuation medium sheet)
  (:argument-precedence-order sheet medium continuation)
  (:method (continuation medium sheet)
    (declare (ignore sheet))
    (funcall continuation medium))
  (:method (continuation medium (sheet permanent-medium-sheet-output-mixin))
    (declare (ignore medium))
    (funcall continuation (sheet-medium sheet)))
  (:method (continuation medium (sheet temporary-medium-sheet-output-mixin))
    (if-let ((sheet-medium (sheet-medium sheet)))
      (funcall continuation sheet-medium)
      (let ((port (port sheet)))
        (if (null medium)
            (let ((new-medium (allocate-medium port sheet)))
              (unwind-protect
                   (progn
                     (engraft-medium new-medium port sheet)
                     (setf (%sheet-medium sheet) new-medium)
                     (funcall continuation new-medium))
                (setf (%sheet-medium sheet) nil)
                (degraft-medium new-medium port sheet)
                (deallocate-medium port new-medium)))
            (unwind-protect
                 (progn
                   (engraft-medium medium port sheet)
                   (setf (%sheet-medium sheet) medium)
                   (funcall continuation medium))
              (setf (%sheet-medium sheet) nil)
              (degraft-medium medium port sheet)))))))

(defmethod do-graphics-with-options ((sheet sheet) func &rest options)
  (with-sheet-medium (medium sheet)
    (let ((*foreground-ink* (medium-foreground medium))
          (*background-ink* (medium-background medium)))
      (apply #'do-graphics-with-options-internal medium sheet func options))))

(defmethod invoke-with-drawing-options
    ((sheet sheet) continuation &rest drawing-options)
  (with-sheet-medium (medium sheet)
    (with-medium-options (medium drawing-options)
      ;; We need to pass SHEET to CONTINUATION (not MEDIUM, like we used to) so
      ;; that output recording works.
      (funcall continuation sheet))))

(defmethod invoke-with-identity-transformation
    ((sheet sheet) continuation)
  (with-sheet-medium (medium sheet)
    (letf (((medium-transformation medium) +identity-transformation+))
      (funcall continuation sheet))))


;;; 12 Graphics

(defun draw-point (sheet point
                   &rest args
                   &key ink clipping-region transformation
                        line-style line-thickness line-unit
                   &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (multiple-value-bind (x y) (point-position point)
      (medium-draw-point* medium x y))))

(defun draw-point* (sheet x y
                    &rest args
                    &key ink clipping-region transformation
                         line-style line-thickness line-unit
                    &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-point* medium x y)))

(defun draw-points (sheet point-seq
                    &rest args
                    &key ink clipping-region transformation
                         line-style line-thickness line-unit
                    &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-points* medium (expand-point-seq point-seq))))

(defun draw-points* (sheet coord-seq
                     &rest args
                     &key ink clipping-region transformation
                          line-style line-thickness line-unit
                     &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   line-style line-thickness line-unit))
  (with-medium-options (sheet args)
    (medium-draw-points* medium coord-seq)))

(defun draw-line (sheet point1 point2
                  &rest args
                  &key ink clipping-region transformation line-style
                       line-thickness line-unit line-dashes line-cap-shape
                  &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (x1 y1) (point-position point1)
      (multiple-value-bind (x2 y2) (point-position point2)
        (medium-draw-line* medium x1 y1 x2 y2)))))

(defun draw-line* (sheet x1 y1 x2 y2
                   &rest args
                   &key ink clipping-region transformation line-style
                        line-thickness line-unit line-dashes line-cap-shape
                   &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-line* medium x1 y1 x2 y2)))

(defun draw-lines (sheet point-seq
                   &rest args
                   &key ink clipping-region transformation line-style
                        line-thickness line-unit line-dashes line-cap-shape
                   &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-lines* medium (expand-point-seq point-seq))))

(defun draw-lines* (sheet coord-seq
                    &rest args
                    &key ink clipping-region transformation line-style
                         line-thickness line-unit line-dashes line-cap-shape
                    &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-lines* medium coord-seq)))

(defun draw-polygon (sheet point-seq
                     &rest args
                     &key (filled t) (closed t) ink clipping-region
                          transformation line-style line-thickness
                          line-unit line-dashes line-joint-shape line-cap-shape
                     &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-polygon* medium (expand-point-seq point-seq) closed filled)))

(defun draw-polygon* (sheet coord-seq
                      &rest args
                      &key (filled t) (closed t) ink clipping-region
                           transformation line-style line-thickness line-unit
                           line-dashes line-joint-shape line-cap-shape
                      &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-polygon* medium coord-seq closed filled)))

(defun draw-bezigon (sheet point-seq
                     &rest args
                     &key (filled t) ink clipping-region
                          transformation line-style line-thickness
                          line-unit line-dashes line-joint-shape line-cap-shape
                     &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-bezigon* medium (expand-point-seq point-seq) filled)))

(defun draw-bezigon* (sheet coord-seq
                      &rest args
                      &key (filled t) ink clipping-region
                           transformation line-style line-thickness line-unit
                           line-dashes line-joint-shape line-cap-shape
                      &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-bezigon* medium coord-seq filled)))

(defun draw-rectangle (sheet point1 point2
                       &rest args
                       &key (filled t) ink clipping-region transformation
                            line-style line-thickness line-unit
                            line-dashes line-joint-shape
                       &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (x1 y1) (point-position point1)
      (multiple-value-bind (x2 y2) (point-position point2)
        (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))))

(defun draw-rectangle* (sheet x1 y1 x2 y2
                        &rest args
                        &key (filled t) ink clipping-region transformation
                             line-style line-thickness line-unit line-dashes
                             line-joint-shape
                        &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))

(defun draw-rectangles (sheet points
                        &rest args
                        &key (filled t) ink clipping-region transformation
                             line-style line-thickness line-unit line-dashes
                             line-joint-shape
                        &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (loop for point in points
          nconcing (multiple-value-bind (x y) (point-position point)
                     (list x y)) into position-seq
          finally (medium-draw-rectangles* medium position-seq filled))))

(defun draw-rectangles* (sheet position-seq
                         &rest args
                         &key (filled t) ink clipping-region transformation
                              line-style line-thickness line-unit
                              line-dashes line-joint-shape
                         &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (medium-draw-rectangles* medium position-seq filled)))

(defun draw-triangle (sheet point1 point2 point3
                      &rest args
                      &key (filled t) ink clipping-region transformation
                           line-style line-thickness line-unit line-dashes
                           line-joint-shape
                      &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (apply #'draw-polygon sheet (list point1 point2 point3)
         :filled filled :closed t args))

(defun draw-triangle* (sheet x1 y1 x2 y2 x3 y3
                       &rest args
                       &key (filled t) ink clipping-region transformation
                            line-style line-thickness line-unit line-dashes
                            line-joint-shape
                       &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-joint-shape))
  (apply #'draw-polygon* sheet (list x1 y1 x2 y2 x3 y3)
         :filled filled :closed t args))

(defun draw-ellipse (sheet center-point
                     radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                     &rest args
                     &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
                          ink clipping-region transformation line-style
                          line-thickness line-unit line-dashes line-cap-shape
                     &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (multiple-value-setq (start-angle end-angle)
    (normalize-angle* start-angle end-angle))
  (with-medium-options (sheet args)
    (multiple-value-bind (center-x center-y) (point-position center-point)
      (medium-draw-ellipse* medium
                            center-x center-y
                            radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                            start-angle end-angle filled))))

(defun draw-ellipse* (sheet center-x center-y
                      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                      &rest args
                      &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
                           ink clipping-region transformation line-style
                           line-thickness line-unit line-dashes line-cap-shape
                      &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-ellipse* medium
                          center-x center-y
                          radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                          start-angle end-angle filled)))

(defun draw-circle (sheet center-point radius
                    &rest args
                    &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
                         ink clipping-region transformation
                         line-style line-thickness line-unit line-dashes
                         line-cap-shape
                    &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (multiple-value-bind (center-x center-y) (point-position center-point)
      (medium-draw-ellipse* medium
                            center-x center-y
                            radius 0 0 radius
                            start-angle end-angle filled))))

(defun draw-circle* (sheet center-x center-y radius
                     &rest args
                     &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi))
                          ink clipping-region transformation line-style
                          line-thickness line-unit line-dashes line-cap-shape
                     &allow-other-keys)
  (declare (ignore ink clipping-region transformation line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (medium-draw-ellipse* medium
                          center-x center-y
                          radius 0 0 radius
                          start-angle end-angle filled)))

(defun draw-text (sheet string point
                  &rest args
                  &key (start 0) (end nil)
                       (align-x :left) (align-y :baseline)
                       (toward-point nil toward-point-p)
                       transform-glyphs
                       ink clipping-region transformation
                       text-style text-family text-face text-size
                  &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   text-style text-family text-face text-size))
  (with-medium-options (sheet args)
    (multiple-value-bind (x y) (point-position point)
      (multiple-value-bind (toward-x toward-y)
          (if toward-point-p
              (point-position toward-point)
              (values (1+ x) y))
        (medium-draw-text* medium string x y
                           start end
                           align-x align-y
                           toward-x toward-y transform-glyphs)))))

(defun draw-text* (sheet string x y
                   &rest args
                   &key (start 0) (end nil)
                        (align-x :left) (align-y :baseline)
                        (toward-x (1+ x)) (toward-y y) transform-glyphs
                        ink clipping-region transformation
                        text-style text-family text-face text-size
                   &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   text-style text-family text-face text-size))
  (with-medium-options (sheet args)
    (medium-draw-text* medium string x y
                       start end
                       align-x align-y
                       toward-x toward-y transform-glyphs)))

(defun draw-arrow (sheet point-1 point-2
                   &rest args
                   &key ink clipping-region transformation
                        line-style line-thickness
                        line-unit line-dashes line-cap-shape
                        (to-head t) from-head (head-length 10) (head-width 5)
                        (head-filled nil) angle
                   &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   line-style line-thickness
                   line-unit line-dashes line-cap-shape
                   to-head from-head head-length head-width
                   head-filled angle))
  (multiple-value-bind (x1 y1) (point-position point-1)
    (multiple-value-bind (x2 y2) (point-position point-2)
      (apply #'draw-arrow* sheet x1 y1 x2 y2 args))))

(defun draw-arrow* (sheet x1 y1 x2 y2
                    &rest args
                    &key ink clipping-region transformation
                         line-style line-thickness
                         line-unit line-dashes line-cap-shape
                         (to-head t) from-head (head-length 10) (head-width 5)
                         (head-filled nil) angle
                    &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (with-medium-options (sheet args)
    (with-translation (sheet x2 y2)
      (unless angle
        (let ((dx (- x1 x2))
              (dy (- y1 y2)))
          (if (and (zerop dx)
                   (zerop dy))
              (setf angle 0.0)
              (setf angle (atan* dx dy)))))
      (with-rotation (sheet angle)
        (let* ((end 0.0)
               (start (sqrt (+ (expt (- x2 x1) 2)
                               (expt (- y2 y1) 2))))
               (p end)
               (q start)
               (line-style (medium-line-style sheet))
               (thickness (line-style-effective-thickness line-style sheet))
               (width/2 (/ head-width 2))
               (a (atan (/ width/2 head-length)))
               (offset (if (and head-length (not (zerop head-length)))
                           (/ thickness (* 2 (sin a )))
                           0.0))
               (tip-to-peak (+ head-length
                               offset
                               (- (* thickness 0.5 (sin a)))))) ;; okay, a guess..
          (when (not head-filled)
            (when to-head   (incf p offset))
            (when from-head (decf q offset)))
          (if (and to-head
                   from-head
                   (< (abs (- start end)) (* 2 tip-to-peak)))
              (let ((width (* 0.5 (+ head-width thickness)
                              (/ (abs (- start end))
                                 (* 2 tip-to-peak)) )))
                (draw-polygon* sheet
                               (list end 0
                                     (/ start 2) width
                                     start 0
                                     (/ start 2) (- width))
                               :filled head-filled
                               :line-thickness 0))
              (progn
                (when to-head
                  (draw-polygon* sheet
                                 (list (+ p head-length) (- width/2)
                                       p 0
                                       (+ p head-length) width/2)
                                 :filled head-filled
                                 :closed nil))
                (when from-head
                  (draw-polygon* sheet
                                 (list (- q head-length) (- width/2)
                                       q 0
                                       (- q head-length) width/2)
                                 :filled head-filled
                                 :closed nil))

                (unless (< q p)
                  (when head-filled
                    (when to-head   (incf p offset))
                    (when from-head (decf q offset)))
                  (draw-line* sheet q 0 p 0)))))))))

(defun draw-oval (sheet center-pt x-radius y-radius
                  &rest args
                  &key (filled t) ink clipping-region transformation
                       line-style line-thickness line-unit
                       line-dashes line-cap-shape
                  &allow-other-keys)
  (declare (ignore filled ink clipping-region transformation
                   line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (multiple-value-bind (x1 y1) (point-position center-pt)
    (apply #'draw-oval* sheet x1 y1 x-radius y-radius args)))

(defun draw-oval* (sheet center-x center-y x-radius y-radius
                   &rest args
                   &key (filled t) ink clipping-region transformation
                        line-style line-thickness line-unit
                        line-dashes line-cap-shape
                   &allow-other-keys)
  (declare (ignore ink clipping-region transformation
                   line-style line-thickness
                   line-unit line-dashes line-cap-shape))
  (check-type x-radius (real 0))
  (check-type y-radius (real 0))
  (with-medium-options (sheet args)
    (if (or (coordinate= x-radius 0) (coordinate= y-radius 0))
        (draw-circle* sheet center-x center-y (max x-radius y-radius)
                      :filled filled)
        (if (coordinate<= y-radius x-radius)
            (let ((x1 (- center-x x-radius)) (x2 (+ center-x x-radius))
                  (y1 (- center-y y-radius)) (y2 (+ center-y y-radius)))
              (if filled
                  ;; Kludge coordinates, sometimes due to rounding the
                  ;; lines don't connect.
                  (draw-rectangle* sheet (floor x1) y1 (ceiling x2) y2)
                  (draw-lines* sheet (list (floor x1) y1 (ceiling x2) y1
                                           (floor x1) y2 (ceiling x2) y2)))
              (draw-circle* sheet x1 center-y y-radius
                            :filled filled
                            :start-angle (* pi 0.5)
                            :end-angle (* pi 1.5))
              (draw-circle* sheet x2 center-y y-radius
                            :filled filled
                            :start-angle (* pi 1.5)
                            :end-angle (* pi 2.5)))
            (with-rotation (sheet (/ pi 2) (make-point center-x center-y))
              (draw-oval* sheet center-x center-y y-radius x-radius
                          :filled filled)) ))))


;;; Generic graphic operation methods

(defmacro def-sheet-trampoline (name (&rest args))
  (with-gensyms (stream medium)
    `(defmethod ,name ((,stream sheet) ,@args)
       (with-sheet-medium (,medium ,stream)
         (,name ,medium ,@args)))))

(defmacro def-graphic-op (name (&rest args))
  (let ((method-name (symbol-concat '#:medium- name '*)))
    `(eval-when (:execute :load-toplevel :compile-toplevel)
       (def-sheet-trampoline ,method-name ,args))))

(def-graphic-op draw-point (x y))
(def-graphic-op draw-points (coord-seq))
(def-graphic-op draw-line (x1 y1 x2 y2))
(def-graphic-op draw-lines (coord-seq))
(def-graphic-op draw-polygon (coord-seq closed filled))
(def-graphic-op draw-bezigon (coord-seq filled))
(def-graphic-op draw-rectangle (left top right bottom filled))
(def-graphic-op draw-rectangles (position-seq filled))
(def-graphic-op draw-ellipse (center-x center-y
                                  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                  start-angle end-angle filled))
(def-graphic-op draw-circle (center-x center-y radius start-angle end-angle filled))
(def-graphic-op draw-text (string x y
                               start end
                               align-x align-y
                               toward-x toward-y transform-glyphs))

(def-sheet-trampoline medium-clear-area (left top right bottom))
(def-sheet-trampoline medium-finish-output ())
(def-sheet-trampoline medium-force-output ())
(def-sheet-trampoline medium-beep ())

(defmethod medium-clear-area :around ((medium output-recording-stream) left top right bottom)
  (declare (ignore left top right bottom))
  (when (stream-drawing-p medium)
    (call-next-method)))

(defmethod medium-copy-area ((from-drawable sheet) fx fy w h (to-drawable sheet) tx ty)
  (with-sheet-medium (from-drawable from-drawable)
    (with-sheet-medium (to-drawable to-drawable)
      (medium-copy-area from-drawable fx fy w h to-drawable tx ty))))

(defmethod medium-copy-area ((from-drawable sheet) fx fy w h to-drawable tx ty)
  (with-sheet-medium (from-drawable from-drawable)
    (medium-copy-area from-drawable fx fy w h to-drawable tx ty)))

(defmethod medium-copy-area (from-drawable fx fy w h (to-drawable sheet) tx ty)
  (with-sheet-medium (to-drawable to-drawable)
    (medium-copy-area from-drawable fx fy w h to-drawable tx ty)))

(defmethod invoke-with-output-to-pixmap ((sheet sheet) cont &key width height)
  (with-sheet-medium (medium sheet)
    (invoke-with-output-to-pixmap medium cont :width width :height height)))

(defmethod draw-design ((medium sheet) (design everywhere-region)
                        &rest options &key &allow-other-keys)
  (apply #'draw-design medium
         (bounding-rectangle (sheet-region medium)) options))

(defun draw-rounded-rectangle* (sheet x1 y1 x2 y2
                                      &rest args &key
                                      (radius 7)
                                      (radius-x radius)
                                      (radius-y radius)
                                      (radius-left  radius-x)
                                      (radius-right radius-x)
                                      (radius-top    radius-y)
                                      (radius-bottom radius-y)
                                      filled &allow-other-keys)
  "Draw a rectangle with rounded corners"
  (apply #'invoke-with-drawing-options sheet
    (lambda (medium)
      (declare (ignore medium))
      (let ((medium sheet))
        (if (not (and (>= (- x2 x1) (* 2 radius-x))
                      (>= (- y2 y1) (* 2 radius-y))))
            (draw-rectangle* medium x1 y1 x2 y2)
            (with-grown-rectangle* ((ix1 iy1 ix2 iy2) (x1 y1 x2 y2)
                                    :radius-left   (- radius-left)
                                    :radius-right  (- radius-right)
                                    :radius-top    (- radius-top)
                                    :radius-bottom (- radius-bottom))
              (let ((zl (zerop radius-left))
                    (zr (zerop radius-right))
                    (zt (zerop radius-top))
                    (zb (zerop radius-bottom)))
                (if filled
                    (progn              ; Filled
                      (unless (or zl zt)
                        (draw-ellipse* medium
                                       ix1 iy1 radius-left
                                       0 0 radius-top
                                       :filled t))
                      (unless (or zr zt)
                        (draw-ellipse* medium
                                       ix2 iy1 radius-right
                                       0 0 radius-top
                                       :filled t))
                      (unless (or zl zb)
                        (draw-ellipse* medium
                                       ix1 iy2 radius-left
                                       0 0 radius-bottom
                                       :filled t))
                      (unless (or zr zb)
                        (draw-ellipse* medium
                                       ix2 iy2 radius-right
                                       0 0 radius-bottom
                                       :filled t))
                      (draw-rectangle* medium x1 iy1 x2 iy2 :filled t)
                      (draw-rectangle* medium ix1 y1 ix2 iy1 :filled t)
                      (draw-rectangle* medium ix1 iy2 ix2 y2 :filled t))
                    (progn              ; Unfilled
                      (unless (or zl zt)
                        (draw-ellipse* medium
                                       ix1 iy1 (- radius-left)
                                       0 0 (- radius-top)
                                       :start-angle (/ pi 2) :end-angle pi
                                       :filled nil))
                      (unless (or zr zt)
                        (draw-ellipse* medium
                                       ix2 iy1 (- radius-right)
                                       0 0 (- radius-top)
                                       :start-angle 0 :end-angle (/ pi 2)
                                       :filled nil))
                      (unless (or zl zb)
                        (draw-ellipse* medium
                                       ix1 iy2 (- radius-left)
                                       0 0 (- radius-bottom)
                                       :start-angle pi :end-angle (* 3/2 pi)
                                       :filled nil))
                      (unless (or zr zb)
                        (draw-ellipse* medium
                                       ix2 iy2 (- radius-right)
                                       0 0 (- radius-bottom)
                                       :start-angle (* 3/2 pi)
                                       :filled nil))
                      (labels ((fx (y p x1a x2a x1b x2b)
                                 (draw-line* medium
                                             (if p x1a x1b) y (if p x2a x2b) y))
                               (fy (x p y1a y2a y1b y2b)
                                 (draw-line* medium
                                             x (if p y1a y1b) x (if p y2a y2b))))
                        (fx y1 zt x1 x2 ix1 ix2)
                        (fy x1 zl y1 y2 iy1 iy2)
                        (fx y2 zb x1 x2 ix1 ix2)
                        (fy x2 zr y1 y2 iy1 iy2)))))))))
   (with-keywords-removed (args '(:radius :radius-x :radius-y
                                  :radius-left :radius-right
                                  :radius-top  :radius-bottom))
     args)))
