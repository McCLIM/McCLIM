;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005 Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This is an example of multiprocessing in which a thread -- running a
;;; simulation or responding to external events -- sends events to the
;;; application thread that cause it to refresh the display. The two threads
;;; share state variables protected by a lock; the application thread uses a
;;; condition variable to signal the simulation thread to change its
;;; behavior. One could also envision sending the simulation state in a
;;; message to the application thread and using an event queue to send control
;;; messages back to the simulation thread, eliminating the need for explicit
;;; locks. Perhaps in anothor demo...
;;;
;;; Based on an idea described by Paul Werkowski in the mcclim-devel
;;; mailing list.

(defpackage #:clim-demo.stopwatch
  (:use #:clim #:clim-lisp)
  (:import-from #:alexandria #:when-let)
  (:export #:stopwatch #:run-stopwatch))
(in-package #:clim-demo.stopwatch)

(define-application-frame stopwatch ()
  (;; state of the timer
   (start-time :accessor start-time :initform 0
               :documentation "In internal time units")
   (elapsed-time :accessor elapsed-time :initform 0
                 :documentation "In internal time units")
   (stop-time :accessor stop-time :initform 0
              :documentation "In count-down mode, elapsed time to stop")
   (mode :accessor mode :initform :stopped)
   ;; data displayed by main thread
   (hours :accessor hours :initform 0)
   (minutes :accessor minutes :initform 0)
   (seconds :accessor seconds :initform 0)
   (clock-process :accessor clock-process :initform nil)
   (condition-variable :accessor condition-variable
                       :initform (clim-sys:make-condition-variable))
   (clock-lock :accessor clock-lock :initform (clim-sys:make-lock)))
  (:menu-bar menubar-command-table)
  (:pointer-documentation t)
  (:panes
   (clock clock-pane
          :width 300 :height 200
          :display-function 'draw-clock
          :incremental-redisplay t)
   (commands :interactor :height 100))
  (:layouts
   (default
     (vertically ()
       clock
       commands))))

(defmacro with-locking-bind (var-forms lock &body body)
  "Bind the variables in VAR-FORMS under the protection of LOCK, then
   release the lock for the BODY. Declarations are permitted in BODY."
  (let ((vars (mapcar #'alexandria:ensure-car var-forms))
        (vals (mapcar (lambda (form)
                        (if (consp form)
                            (second form)
                            nil))
                      var-forms)))
    `(multiple-value-bind ,vars
         (clim-sys:with-lock-held (,lock)
           (values ,@vals))
       ,@body)))

(define-presentation-type clock ()
  :inherit-from t)

(defun center-output-record (record pane)
  (with-bounding-rectangle* (min-x min-y max-x max-y) (sheet-region pane)
    (with-bounding-rectangle* (r-minx r-miny r-maxx r-maxy) record
      (let ((width (- r-maxx r-minx))
            (height (- r-maxy r-miny)))
        (setf (output-record-position record)
              (values (+ min-x (/ (- max-x min-x width) 2.0))
                      (+ min-y (/ (- max-y min-y height) 2.0))))))))

(defun draw-clock (frame pane)
  (with-locking-bind ((hours (hours frame))
                      (minutes (minutes frame))
                      (seconds (seconds frame))
                      (elapsed-time (elapsed-time frame)))
      (clock-lock frame)
    ;; First create an output record for the clock face, then center
    ;; it in the pane.
    (let ((record
            (with-output-recording-options (pane :record t :draw nil)
              (with-output-as-presentation (pane elapsed-time 'clock
                                                 :single-box t)
                (with-text-size (pane :huge)
                  ;; Use a table because otherwise its tricky to get the
                  ;; stream state just right for each individual
                  ;; updating-output form.
                  (formatting-table (pane :x-spacing 0)
                    (formatting-row (pane)
                      (formatting-cell (pane)
                        (updating-output (pane :unique-id 'hours
                                               :cache-value hours)
                          (format pane "~D:" hours)))
                      (formatting-cell (pane)
                        (updating-output (pane :unique-id 'minutes
                                               :cache-value minutes)
                          (format pane "~2,'0D:" minutes)))
                      (formatting-cell (pane)
                        (updating-output (pane :unique-id 'seconds
                                               :cache-value seconds)
                          (with-drawing-options (pane :ink +dark-green+)
                            (format pane "~5,2,,,'0F" seconds)))))))))))
      (center-output-record record pane))))

(defclass clock-pane (application-pane) ())

(defmethod note-sheet-region-changed :after ((sheet clock-pane))
  (when-let ((record (first (output-record-children (stream-output-history sheet)))))
    (with-output-recording-options (sheet :record nil :draw t)
      (with-bounding-rectangle* (x1 y1 x2 y2) record
        (draw-rectangle* sheet x1 y1 x2 y2 :ink +background-ink+))
      (center-output-record record sheet)
      (replay record sheet))))

(defclass update-clock-event (device-event)
  ()
  (:default-initargs :modifier-state 0))

(defmethod handle-event ((client clock-pane) (event update-clock-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame client)))

(defun decode-time (internal-time)
  (let ((total-seconds (/ internal-time internal-time-units-per-second)))
    (multiple-value-bind (hours min-rem) (truncate total-seconds 3600.0)
      (multiple-value-bind (minutes secs) (truncate min-rem 60.0)
        (values hours minutes (float secs))))))

;;; The simulation thread function.
(defun update-clock (frame client)
  (let ((tls (frame-top-level-sheet frame))
        (clock-lock (clock-lock frame)))
    (loop (clim-sys:with-lock-held (clock-lock)
            (loop (case (mode frame)
                    (:exit
                     (return-from update-clock nil))
                    (:stopped
                     (clim-sys:condition-wait (condition-variable frame) clock-lock)
                     (setf (start-time frame) (get-internal-real-time)
                           (elapsed-time frame) 0))
                    (:running
                     (let ((new-time (- (get-internal-real-time) (start-time frame))))
                       (setf (elapsed-time frame) new-time)
                       (multiple-value-bind (hours minutes seconds)
                           (decode-time new-time)
                         (when (or (/= hours (hours frame))
                                   (/= minutes (minutes frame))
                                   (/= seconds (seconds frame)))
                           (setf (hours frame) hours
                                 (minutes frame) minutes
                                 (seconds frame) seconds)
                           (queue-event tls (make-instance 'update-clock-event
                                                           :sheet client))))
                       (when (not (clim-sys:condition-wait (condition-variable frame)
                                                           clock-lock
                                                           .01))
                         (return))))))))))

(defmethod run-frame-top-level ((frame stopwatch) &key)
  (let ((clock-pane (find-pane-named frame 'clock)))
    (setf (clock-process frame)
          (clim-sys:make-process (lambda ()
                                   (update-clock frame clock-pane))
                                 :name "ticker")))
  (unwind-protect
       (call-next-method)
    (when-let ((process (clock-process frame)))
      (setf (mode frame) :exit)
      (clim-sys:condition-notify (condition-variable frame))
      (bt:join-thread process)
      (setf (clock-process frame) nil))))

(define-stopwatch-command (com-toggle-watch :name t)
    ()
  (let ((frame *application-frame*))
    (setf (mode frame)
          (if (eq (mode frame) :stopped)
              :running
              :stopped))
    (clim-sys:with-lock-held ((clock-lock frame) "clock lock")
      (clim-sys:condition-notify (condition-variable frame)))))

(defun describe-toggle (mode stream)
  (when-let ((string (case mode
                       (:stopped "Start Watch")
                       (:running "Stop Watch"))))
    (write-string string stream)))

(define-presentation-to-command-translator com-click-stopwatch
    (clock com-toggle-watch stopwatch
     :documentation ((object stream)
                     (describe-toggle (mode *application-frame*) stream))
     :pointer-documentation ((object stream)
                             (describe-toggle (mode *application-frame*) stream)))
    (object)
  '())

(define-stopwatch-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(define-command-table menubar-command-table
  :menu (("Quit" :command com-quit)
         ("Toggle Watch" :command com-toggle-watch)))

(defun run-stopwatch ()
  (run-frame-top-level (make-application-frame 'stopwatch)))
