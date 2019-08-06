;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)
;;;  (c) copyright 2004 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2019 by Daniel Kochmański (daniel@turtleware.eu)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :clim-internals)

(defun default-tracking-handler (&key event &allow-other-keys)
  (handle-event (event-sheet event) event))

(deftype tracking-pointer-clause ()
  `(member :pointer-motion :pointer-button-press :pointer-button-release
           :presentation :presentation-button-press :presentation-button-release
           :keyboard))

(defclass tracking-pointer-state ()
  ((motion-handler
    :reader motion-handler
    :initarg :pointer-motion)
   (button-press-handler
    :reader button-press-handler
    :initarg :pointer-button-press)
   (buttton-release-handler
    :reader button-release-handler
    :initarg :pointer-button-release)
   (presentation-handler
    :reader presentation-handler
    :initarg :presentation)
   (presentation-button-release-handler
    :reader presentation-button-release-handler
    :initarg :presentation-button-release)
   (presentation-button-press-handler
    :reader presentation-button-press-handler
    :initarg :presentation-button-press)
   (keyboard-handler
    :reader keyboard-handler
    :initarg :keyboard)
   (tracked-sheet
    :reader tracked-sheet
    :initarg :sheet)
   (tracked-pointer
    :reader tracked-pointer
    :initarg :pointer)
   (multiple-window
    :reader multiple-window
    :initarg :multiple-window)
   (transformp
    :reader transformp
    :initarg :transformp)
   (context-type
    :reader context-type
    :initarg :context-type)
   (highlight
    :reader highlight
    :initarg :highlight)
   (%highlighted-presentation
    :accessor %highlighted-presentation
    :initform nil))
  (:default-initargs :pointer-motion         #'default-tracking-handler
                     :pointer-button-press   #'default-tracking-handler
                     :pointer-button-release #'default-tracking-handler
                     :keyboard               #'default-tracking-handler
                     ;; Presentation handlers default to NIL so we can
                     ;; resort to their "raw" counterparts when due.
                     :presentation nil
                     :presentation-button-press nil
                     :presentation-button-release nil
                     :multiple-window nil
                     :transformp nil
                     :context-type t
                     :highlight nil))

(defmethod initialize-instance :after
    ((instance tracking-pointer-state)
     &key
       (presentation nil p-p)
       (presentation-button-press nil pbp-p)
       (presentation-button-release nil pbr-p)
       (highlight nil h-p))
  (declare (ignore presentation
                   presentation-button-press
                   presentation-button-release
                   highlight))
  (unless h-p
    (setf (slot-value instance 'highlight)
          (or p-p pbp-p pbr-p))))

(defgeneric sheet-find-presentation (sheet context-type x y)
  (:method (sheet context-type x y)
    nil)
  (:method ((stream output-recording-stream) context-type x y)
    (map-over-output-records-containing-position
     (lambda (record)
       (when (and (presentationp record)
                  (presentation-subtypep (presentation-type record) context-type))
         (return-from sheet-find-presentation record)))
     (stream-output-history stream) x y)))

;;; Function is responsible for handling events in tracking-pointer
;;; macro.
(defgeneric track-event (state event x y)
  (:method ((state tracking-pointer-state) event x y)
    (default-tracking-handler :event event))
  (:method ((state tracking-pointer-state) (event keyboard-event) x y)
    (funcall (keyboard-handler state) :gesture event :event event :x x :y y)))

(macrolet ((frob (event-type presentation-handler normal-handler)
             `(defmethod track-event ((state tracking-pointer-state)
                                      (event ,event-type)
                                      x y)
                (let ((window (event-sheet event)))
                  (when-let ((highlighted (%highlighted-presentation state)))
                    (highlight-output-record highlighted window :unhighlight))
                  (alexandria:when-let*
                      ((context-type (context-type state))
                       (handler (,presentation-handler state))
                       (presentation (sheet-find-presentation window context-type x y)))
                    (when (highlight state)
                      (setf (%highlighted-presentation state) presentation)
                      (highlight-output-record presentation window :highlight))
                    (return-from track-event
                      (funcall handler
                               :presentation presentation
                               :event event :window window :x x :y y)))
                  (funcall (,normal-handler state) :event event :window window :x x :y y)))))
  (frob pointer-motion-event         presentation-handler                motion-handler)
  (frob pointer-button-press-event   presentation-button-press-handler   button-press-handler)
  (frob pointer-button-release-event presentation-button-release-handler button-release-handler))

(defun invoke-tracking-pointer (state
                                &aux
                                  (sheet (tracked-sheet state))
                                  (multiple-window (multiple-window state))
                                  (transformp (transformp state)))
  (flet ((pointer-event-position (event)
           (let ((sheet (event-sheet event)))
             (get-pointer-position (sheet event)
               (if (not transformp)
                   (values x y)
                   (with-sheet-medium (medium sheet)
                     (transform-position (medium-transformation medium) x y)))))))
    (loop
       for event = (event-read sheet)
       do (if (and (not multiple-window)
                   (not (eql sheet (event-sheet event))))
              ;; Event is not intercepted.
              (handle-event (event-sheet event) event)
              (multiple-value-bind (x y)
                  (when (typep event 'pointer-event)
                    (pointer-event-position event))
                (track-event state event x y))))))

(defmacro tracking-pointer
    ((sheet &rest args &key pointer multiple-window transformp context-type highlight)
     &body body)
  (declare (ignore pointer multiple-window transformp context-type highlight))
  (setq sheet (stream-designator-symbol sheet '*standard-output*))
  ;; The Spec specifies the tracking-pointer clause arguments as,
  ;; e.g., (&key presentation event x y), implying that the user must
  ;; write the &key keyword, but real code doesn't do that. Check if
  ;; &key is in the arg list and add it if it is not.
  (flet ((fix-args (name args)
           (let ((aok nil)
                 (args (if (eq (car args) '&key)
                           args
                           (cons '&key args))))
             (dolist (arg (cdr args))
               (cond ((find arg '(window event gesture presentation x y) :test #'string=))
                     ((eq arg '&allow-other-keys)
                      (setf aok t))
                     (t
                      (error "TRACKING-POINTER: ~s is not a valid argument for a clause ~s."
                             arg name))))
             (unless aok
               (setq args (append args '(&allow-other-keys))))
             args)))
    (loop
       for (name arglist . body) in body
       for handler-name = (gensym (symbol-name name))
       do (unless (typep name 'tracking-pointer-clause)
            (error "TRACKING-POINTER: ~s is not a valid clause name." name))
       collect `(,handler-name ,(fix-args name arglist) ,@body) into bindings
       collect `#',handler-name into fn-names
       append  `(,name #',handler-name) into initargs
       finally (return `(flet ,bindings
                          (declare (dynamic-extent ,@fn-names))
                          (invoke-tracking-pointer
                           (make-instance 'tracking-pointer-state
                                          :sheet ,sheet ,@args ,@initargs)))))))


;;; DRAG-OUTPUT-RECORD and DRAGGING-OUTPUT.

;;; XXX Unresolved issues:
;;; multiple-window is completely unsupported.
;;; window-repaint events while dragging.

;;; Fancy double-buffered feedback function
(defun make-buffered-feedback-function (record finish-on-release erase-final)
  (multiple-value-bind (record-x record-y)
      (output-record-position record)
    (lambda (record stream initial-x initial-y x y event)
      (flet ((simple-erase ()
               (when erase-final
                 (when (output-record-parent record)
                   (delete-output-record record (output-record-parent record)))
                 (with-double-buffering
                     ((stream record) (buffer-rectangle))
                   (stream-replay stream buffer-rectangle)))))
        (let ((dx (- record-x initial-x))
              (dy (- record-y initial-y)))
          (typecase event
            (null
             (setf (output-record-position record) (values (+ dx x) (+ dy y)))
             (stream-add-output-record stream record)
             (stream-replay stream record))
            (pointer-motion-event
             ;; Don't do an explicit erase. Instead, update the position of the
             ;; output record and redraw the union of the old and new
             ;; positions.
             (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2)
                 record
               (when (output-record-parent record)
                 (delete-output-record record (output-record-parent record)))
               (setf (output-record-position record)
                     (values (+ dx x) (+  dy y)))
               (stream-add-output-record stream record)
               (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
                   record
                 (multiple-value-bind (area-x1 area-y1 area-x2 area-y2)
                     (values (min old-x1 new-x1) (min old-y1 new-y1)
                             (max old-x2 new-x2) (max old-y2 new-y2))
                   (with-double-buffering
                       ((stream area-x1 area-y1 area-x2 area-y2)
                        (buffer-rectangle))
                     (stream-replay stream buffer-rectangle))))))
            (pointer-button-press-event
             (unless finish-on-release
               (simple-erase)))
            (pointer-button-release-event
             (when finish-on-release
               (simple-erase)))
            (t nil)))))))

;;; If the user supplies a feedback function, create a function to
;;; call it with the simple :draw / :erase arguments.

(defun make-simple-feedback-function
    (record feedback finish-on-release erase-final)
  (declare (ignore record))
  (lambda (record stream initial-x initial-y x y event)
    (typecase event
      (null
       (funcall feedback record stream initial-x initial-y x y :draw))
      (pointer-motion-event
       (funcall feedback record stream initial-x initial-y x y :erase)
       (funcall feedback record stream initial-x initial-y x y :draw))
      (pointer-button-press-event
       (unless finish-on-release
         (when erase-final
           (funcall feedback record stream initial-x initial-y x y :erase))))
      (pointer-button-release-event
       (when (and finish-on-release erase-final)
         (funcall feedback record stream initial-x initial-y x y :erase)))
      (t nil))))

(defmethod drag-output-record
    ((stream output-recording-stream) (record output-record)
     &key (repaint t) (erase #'erase-output-record)
     feedback finish-on-release multiple-window
     feedback-event erase-final)
  (declare (ignore erase repaint))
  (let ((feedback-event-fn
         (cond (feedback-event
                feedback-event)
               (feedback
                (make-simple-feedback-function record
                                               feedback
                                               finish-on-release
                                               erase-final))
               (t (make-buffered-feedback-function record
                                                   finish-on-release
                                                   erase-final)))))
    (setf (stream-current-output-record stream)
          (stream-output-history stream))
    (let* ((pointer (port-pointer (port stream)))
           (pointer-state (pointer-button-state pointer)))
      (multiple-value-bind (x0 y0)
          (stream-pointer-position stream)
        (funcall feedback-event-fn record stream x0 y0 x0 y0 nil)
        (tracking-pointer (stream :multiple-window multiple-window)
         (:pointer-motion (&key event x y)
           ;; XXX What about the sheet?
           (funcall feedback-event-fn record stream x0 y0 x y event)
           (funcall feedback-event-fn record stream x0 y0 x y event))
         (:pointer-button-press (&key event x y)
           (unless finish-on-release
             (funcall feedback-event-fn record stream x0 y0 x y event)
             (return-from drag-output-record (values x y))))
         (:pointer-button-release (&key event x y)
           ;; If the button released was one of those held down on entry to
           ;; drag-output-record, we're done.
           (when (and finish-on-release
                      (not (zerop (logand pointer-state
                                          (pointer-event-button event)))))
             (funcall feedback-event-fn record stream x0 y0 x y event)
             (return-from drag-output-record (values x y)))))))))

(defmacro dragging-output ((&optional (stream '*standard-output*) &rest args
                                      &key (repaint t) finish-on-release multiple-window)
                           &body body)
  (declare (ignore repaint finish-on-release multiple-window))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-gensyms (record)
    `(let ((,record (with-output-to-output-record (,stream)
                      ,@body)))
       (drag-output-record ,stream ,record :erase-final t ,@args))))
