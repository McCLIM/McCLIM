;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2006 by David Lichteblau <david@lichteblau.com>
;;;
;;;  (c) Copyright 2022 by Michael South <msouth@msouth.org>
;;;
;;; ---------------------------------------------------------------------------

(defpackage #:clim-demo.drawing-benchmark
  (:use #:clim #:clim-lisp)
  (:export #:drawing-benchmark #:run-drawing-benchmark))
(in-package #:clim-demo.drawing-benchmark)

(defvar *glider*
  (make-rectangular-tile
   (make-pattern-from-bitmap-file
    (merge-pathnames #p"images/glider.png"
                     (asdf:system-source-directory :clim-examples)))
   100 100))

(defvar *transformed-glider*
  (transform-region (make-rotation-transformation* (/ pi 4)) *glider*))

(defconstant +max-color-planes+ 5)
(defconstant +max-rows+ 5)
(defconstant +max-cols+ 5)
(defconstant +grid-element-margin+ 15)

(defvar *inks*
  (let* ((r (make-array (list +max-color-planes+ +max-rows+ +max-cols+))))
    (dotimes (i +max-color-planes+)
      (dotimes (j +max-rows+)
        (dotimes (k +max-cols+)
          (setf (aref r i j k)
                (make-rgb-color (float (/ (1+ i) +max-color-planes+))
                                (float (/ (1+ j) +max-rows+))
                                (float (/ (1+ k) +max-cols+)))))))
    r))

(defvar +help-text+
  "\"Random, ad hoc\" colors are calculated before drawing each shape.
\"Multiple, calc once\" colors are calculated once and reused.

Scaling or Translation transformations are performed before each timed test.

\"Do/undo\" are complementary pairs, effectively an Identity transformation.

\"Accumulative\" transformations have some final effect.")

(define-application-frame drawing-benchmark ()
  ()
  (:panes
   (canvas :application
           :min-width 600
           :min-height 600
           :incremental-redisplay nil
           :display-time t)
   (shape
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "rectangle" :id :rectangle))
      (make-pane 'toggle-button :label "polygon" :id :polygon)
      (make-pane 'toggle-button :label "ellipse" :id :ellipse)
      (make-pane 'toggle-button :label "text" :id :text)
      (make-pane 'toggle-button :label "text*" :id :text*)))
   (inking
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "random, ad hoc" :id :random))
      (make-pane 'toggle-button :label "multiple, calc once" :id :multiple)
      (make-pane 'toggle-button :label "red" :id :red)
      (make-pane 'toggle-button :label "flipping ink" :id :flipping)
      (make-pane 'toggle-button :label "indirect" :id :indirect)
      (make-pane 'toggle-button :label "pattern" :id :pattern)
      (make-pane 'toggle-button :label "pattern*" :id :pattern*)))
   (grid-size
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "1" :id 1))
      (make-pane 'toggle-button :label "2" :id 2)
      (make-pane 'toggle-button :label "3" :id 3)
      (make-pane 'toggle-button :label "4" :id 4)
      (make-pane 'toggle-button :label "5" :id 5)))
   (recording
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "Off" :id 'off))
      (make-pane 'toggle-button :label "On" :id 'on)))
   (transform-ops
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "Scale do/undo" :id 'scale-ident))
      (make-pane 'toggle-button :label "Scale accumulative" :id 'scale-accum)
      (make-pane 'toggle-button :label "Xlate do/undo" :id 'xlate-ident)
      (make-pane 'toggle-button :label "Xlate accumulative" :id 'xlate-accum)
      (make-pane 'toggle-button :label "Scale & Xlate do/undo" :id 'sx-ident)
      (make-pane 'toggle-button :label "Scale & Xlate accum." :id 'sx-accum)))
   (max-transforms
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "0" :id 0))
      (make-pane 'toggle-button :label "4" :id 4)
      (make-pane 'toggle-button :label "16" :id 16)
      (make-pane 'toggle-button :label "64" :id 64)))
   (results :application
            :min-width 600
            :min-height 75
            :end-of-line-action :scroll
            :incremental-redisplay nil
            :display-time t))
  (:layouts
   (default
    (vertically ()
      (horizontally ()
        (vertically ()
          (horizontally ()
            (labelling (:label "Shape") shape)
            (labelling (:label "Grid Size") grid-size)
            (labelling (:label "Ink") inking)
            (labelling (:label "Output Recording") recording))
          (labelling (:label "Co-ord Transformations")
            (horizontally ()
              (labelling (:label "Operations") transform-ops)
              (labelling (:label "Max Xforms") max-transforms))))
        canvas)
      (labelling (:label "Results") results)))))

(defun do-transforms (stream ops iterations)
  (dotimes (i iterations)
        (setf (sheet-transformation stream)
              (compose-transformations
               (sheet-transformation stream)
               (ecase ops
                 (scale-ident
                  (if (evenp i)
                      ;; Do it
                      (make-scaling-transformation* (/ 1 1.01) (/ 1 1.01))
                      ;; Undo it
                      (make-scaling-transformation* 1.01 1.01)))
                 (scale-accum
                  ;; Make it smaller then bigger to avoid skewing the timing too
                  ;; badly (bigger box is slower), but don't let it go back to
                  ;; STANDARD-IDENTITY-TRANSFORM.
                  (if (evenp i)
                      (make-scaling-transformation* 0.90 0.90)
                      (make-scaling-transformation* 1.10 1.10)))
                 (xlate-ident
                  (if (evenp i)
                      (make-translation-transformation 110 110)
                      (make-translation-transformation -110 -110)))
                 (xlate-accum
                  (make-translation-transformation 2 2))
                 (sx-ident
                  (if (evenp i)
                      (compose-transformation-with-scaling
                       (make-translation-transformation 100 100)
                       0.5 0.5)
                      (compose-transformation-with-translation
                       (make-scaling-transformation* 2.0 2.0)
                       -100 -100)))
                 (sx-accum
                 (compose-transformation-with-scaling
                  (make-translation-transformation 2 2)
                  1.001 1.001)))))
    (format *debug-io* "; [xform iteration ~D] result ~A~%"
            i (sheet-transformation stream))))

(defun timed-test (stream shape inking num-cols num-rows)
  "Perform selected drawing operations for 4 seconds. Return # operations/sec."
  (let* ((pane-width (rectangle-width (sheet-region stream)))
         (pane-height (rectangle-height (sheet-region stream)))
         (ele-size
           (float
            (max
             (min (/ (- pane-width (* (1+ num-cols) +grid-element-margin+)) num-cols)
                  (/ (- pane-height (* (1+ num-rows) +grid-element-margin+)) num-rows))
             (* 2.5 +grid-element-margin+))))
         (ele-size/2 (/ ele-size 2.0))
         (itups internal-time-units-per-second)
         (start (get-internal-real-time))
         (stop (+ start (* 5 itups)))
         (n 0))
    (do ()
        ((>= (get-internal-real-time) stop))
      ;; Clear visible region so we can see text "n" numbers incrementing.
      (multiple-value-bind (x1 y1 x2 y2)
          (bounding-rectangle* (sheet-region stream))
        (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+))
      (dotimes (xn num-cols)
        (dotimes (yn num-rows)
          (let* ((x1 (float (+ +grid-element-margin+
                               (* xn (+ ele-size +grid-element-margin+)))))
                 (x2 (+ x1 ele-size))
                 (y1 (float (+ +grid-element-margin+
                               (* yn (+ ele-size +grid-element-margin+)))))
                 (y2 (+ y1 ele-size))
                 (ink (ecase inking
                        (:random (make-rgb-color (random 1.0d0)
                                                 (random 1.0d0)
                                                 (random 1.0d0)))
                        (:multiple (aref *inks* (mod n +max-color-planes+)
                                         (mod xn +max-cols+)
                                         (mod yn +max-rows+)))
                        (:red +red+)
                        (:flipping +flipping-ink+)
                        (:indirect +foreground-ink+)
                        (:pattern *glider*)
                        (:pattern* *transformed-glider*))))
            (ecase shape
              (:rectangle (draw-rectangle* stream x1 y1 x2 y2 :ink ink :filled t))
              (:polygon (draw-polygon* stream (list x1 y1 x2 y1 x1 y2)
                                       :ink ink :closed t))
              (:ellipse (draw-ellipse* stream
                                       (+ x1 ele-size/2)
                                       (+ y1 ele-size/2)
                                       ele-size/2
                                       +grid-element-margin+
                                       +grid-element-margin+
                                       (- ele-size/2 +grid-element-margin+)
                                       :ink ink :fillet t))
              (:text (draw-text* stream (format nil "Bla ~D" n) x1 (+ y1 10.0) :ink ink))
              (:text* (draw-text* stream (format nil "Bla ~D" n) (+ x1 ele-size/2) (+ y1 ele-size/2)
                                  :ink ink :align-x :center :align-y :center)))
            (incf n)))))
    (finish-output stream)
    (truncate (+ (/ n (/ (- (get-internal-real-time) start) itups)) 0.5))))

(defun execute-benchmark (frame stream)
  (let* (
         (results (find-pane-named frame 'results))
         (shape (gadget-id (gadget-value (find-pane-named frame 'shape))))
         (inking (gadget-id (gadget-value (find-pane-named frame 'inking))))
         (ops (gadget-id (gadget-value (find-pane-named frame 'transform-ops))))
         (max-transforms
           (gadget-id (gadget-value (find-pane-named frame 'max-transforms))))
         (recording (gadget-id (gadget-value (find-pane-named frame 'recording))))
         (cols (gadget-id (gadget-value (find-pane-named frame 'grid-size))))
                                        ; Number of columns to display
         (rows cols))                   ; Number of rows
    (setf (stream-recording-p stream) (eq recording 'on))
    (window-clear stream)
    (window-clear results)
    ;; XFORM-ITERATIONS should be an even number for each loop; this will
    ;; prevent having an unmatched set of do/undo transformations.
    (do* ((loop-cnt 0 (1+ loop-cnt))
          (xform-iterations 0 (ash 1 (* loop-cnt 2)))) ; 0 4 16 64
         ((> xform-iterations max-transforms))
      (setf (sheet-transformation stream) +identity-transformation+)
      (window-clear stream)
      (format *debug-io* ";; Time test ~D: ~D transforms~%" loop-cnt xform-iterations)
      (do-transforms stream ops xform-iterations)
      (format *debug-io* "; Final Sheet xform ~A~%" (sheet-transformation stream))
      (finish-output *debug-io*)
      (finish-output stream)
      (let* ((ops-sec (timed-test stream shape inking cols rows))
             (s (list recording
                      xform-iterations
                      ops
                      (list cols rows)
                      inking
                      shape
                      ops-sec)))
        (format results
                "~:@{Record ~A, ~D ~A xforms, ~{[~Dx~D]~} ~A-ink ~A: ~:D ops/s~}~%"
                s)
        (format *debug-io*
                "~:<(:recording . :~A) ~:_(:num-transforms . ~D) ~:_(:transforms . :~A) ~:_(:grid . ~A) ~:_(:ink . :~A) ~:_(:shape . :~A) ~:_(:ops-sec . ~D)~:>~%"
                s))
      (finish-output results))
    (format results "Done.~%")
    (format *debug-io* ";; Done.~%")
    (climi::port-force-output (car climi::*all-ports*))))

(defun help-drawing-benchmark (frame pane)
  (declare (ignore frame))
  (let* ((medium (sheet-medium pane))
         (style (medium-default-text-style medium))
         (text-x (* (text-style-width style medium) 0.5))
         (text-y (* (text-style-ascent style medium) 1.5)))
    (window-clear pane)
    (with-sheet-medium (medium pane)
      (draw-text* pane +help-text+ text-x text-y))))

(define-drawing-benchmark-command (com-quit-drawing-benchmark :menu "Quit") ()
  (frame-exit *application-frame*))

(define-drawing-benchmark-command (com-run-drawing-benchmark :menu "Run") ()
  (execute-benchmark *application-frame*
                     (frame-standard-output *application-frame*)))

(define-drawing-benchmark-command (com-help-drawing-benchmark :menu "Help") ()
  (help-drawing-benchmark *application-frame*
                          (find-pane-named *application-frame* 'results)))

(defun run-drawing-benchmark ()
  (run-frame-top-level (make-application-frame 'drawing-benchmark)))
