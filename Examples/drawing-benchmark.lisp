;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2006 by David Lichteblau <david@lichteblau.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-demo)

(defvar *glider*
  (make-rectangular-tile
   (make-pattern-from-bitmap-file
    (merge-pathnames #p"images/glider.png"
                     (asdf:system-source-directory :clim-examples)))
   100 100))

(defvar *transformed-glider*
  (transform-region (make-rotation-transformation* (/ pi 4)) *glider*))

(define-application-frame drawing-benchmark ()
  ()
  (:panes
   (canvas :application
           :min-width 600
           :incremental-redisplay nil
           :display-time t)
   (mode
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "rectangle" :id :rectangle))
      (make-pane 'toggle-button :label "polygon" :id :polygon)
      (make-pane 'toggle-button :label "ellipse" :id :ellipse)
      (make-pane 'toggle-button :label "text" :id :text)
      (make-pane 'toggle-button :label "text*" :id :text*)))
   (ink
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "random" :id :random))
      (make-pane 'toggle-button :label "red" :id +red+)
      (make-pane 'toggle-button :label "flipping ink" :id +flipping-ink+)
      (make-pane 'toggle-button :label "indirect" :id +foreground-ink+)
      (make-pane 'toggle-button :label "pattern" :id *glider*)
      (make-pane 'toggle-button :label "pattern*" :id *transformed-glider*))))
  (:layouts
   (default
    (vertically ()
      (horizontally ()
        (labelling (:label "Mode") mode)
        (labelling (:label "Ink") ink))
      canvas))))

(defmethod run-drawing-benchmark (frame stream)
  (window-clear stream)
  (let* ((width (rectangle-width (sheet-region stream)))
         (height (rectangle-height (sheet-region stream)))
         (mode (gadget-id (gadget-value (find-pane-named frame 'mode))))
         (ink (gadget-id (gadget-value (find-pane-named frame 'ink))))
         (itups internal-time-units-per-second)
         (n 0)
         (start (get-internal-real-time))
         (stop (+ start (* 5 itups))))
    (do ()
        ((>= (get-internal-real-time) stop))
      (incf n)
      (let ((ink
             (if (eq ink :random)
                 (clim:make-rgb-color (random 1.0d0)
                                      (random 1.0d0)
                                      (random 1.0d0))
                 ink)))
        (with-output-recording-options (stream :record nil)
         (ecase mode
           (:rectangle
            (draw-rectangle* stream
                             10 10 (- width 10) (- height 10)
                             :ink ink
                             :filled t))
           (:polygon
            (draw-polygon* stream (list 10 10
                                        (- width 10) 10
                                        10 (- height 10))
                           :ink ink
                           :closed t))
           (:ellipse
            (let ((w/2 (/ width 2))
                  (h/2 (/ height 2)))
             (draw-ellipse* stream w/2 h/2
                            (- w/2 10) 0
                            0 (- h/2 10)
                            :ink ink)))
           (:text
            (dotimes (x 10)
              (draw-text* stream
                          "Bla blub hastenichgesehen noch viel mehr Text so fuellen wir eine Zeile."
                          0
                          (* x 20)
                          :ink ink)))
           (:text* ;; custom align-x/y triggers path which calls text-size
            (dotimes (x 10)
              (draw-text* stream
                          "Bla blub hastenichgesehen noch viel mehr Text so fuellen wir eine Zeile."
                          150
                          (* x 20)
                          :ink ink
                          :align-x :center
                          :align-y :center)))))))
    (finish-output stream)
    (medium-finish-output (sheet-medium stream))
    (climi::port-force-output (car climi::*all-ports*))
    (setf stop (get-internal-real-time))
    (window-clear stream)
    (format stream "Score: ~A operations/s~%" (float (/ n (/ (- stop start) itups))))
    (format *debug-io* "Score: ~A operations/s~%" (float (/ n (/ (- stop start) itups))))))

(define-drawing-benchmark-command (com-quit-drawing-benchmark :menu "Quit") ()
  (frame-exit *application-frame*))

(define-drawing-benchmark-command (com-run-drawing-benchmark :menu "Run") ()
  (run-drawing-benchmark *application-frame*
                         (frame-standard-output *application-frame*)))
