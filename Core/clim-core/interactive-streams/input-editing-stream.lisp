;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2022 by Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the input editing protocol.
;;;

(in-package #:clim-internals)

(with-system-redefinition-allowed
  (when (and (fboundp 'interactive-stream-p)
             (not (typep (fdefinition 'interactive-stream-p)
                         'generic-function)))
    (fmakunbound 'interactive-stream-p))
  (defgeneric interactive-stream-p (stream)
    (:method (stream)
      (cl:interactive-stream-p stream))))

;;; KLUDGE we retain the `class' argument for backward compatibility.
(defgeneric invoke-with-input-editing
    (stream continuation input-sensitizer initial-contents class)
  (:method (stream continuation input-sensitizer initial-contents class)
    (funcall continuation stream)))

;;; The specification of the typeout is somewhat lacking. A typeout is
;;; estabilished to introduce a hint in the input editor, for example:
;;;
;;; - show available commands after pressing [tab] in the interactor
;;; - show the function arglist or its documentation during typing
;;;
;;; When ERASE is NIL, then "old" typeout may be retained, otherwise it must be
;;; removed. To "remove" the typeout just supply an empty body and :ERASE T.
;;; When the typeout shares the output record with the input editor, then erase
;;; may be important
;;;
;;; The specification says that FRESH-LINE should be called before and after the
;;; evaluating the body - that strongly suggests that the input editor should be
;;; redrawn (vertically) below the typeout, however this should be up to the
;;; method - i.e we may want to show a tooltip instead, or use the pointer
;;; documentation pane when available. -- jd 2022-07-20
(defgeneric invoke-with-input-editor-typeout (stream continuation &key erase)
  (:method (stream continuation &key erase)
    (declare (ignore erase))
    (fresh-line stream)
    (unwind-protect (funcall continuation stream)
      (fresh-line stream))))

(defmacro with-input-editing ((stream &rest args
                               &key input-sensitizer initial-contents
                                 (class ''standard-input-editing-stream)
                               &allow-other-keys)
                              &body body)
  (with-stream-designator (stream '*standard-input*)
    (with-keywords-removed (args (:input-sensitizer :initial-contents :class))
      (let ((continuation (gensym)))
        `(flet ((,continuation (,stream) ,@body))
           (declare (dynamic-extent (function ,continuation)))
           (invoke-with-input-editing (make-instance ,class :stream ,stream ,@args)
                                      (function ,continuation)
                                      ,input-sensitizer
                                      ,initial-contents
                                      nil))))))

(defmacro with-input-editor-typeout ((&optional stream &key erase) &body body)
  (with-stream-designator (stream '*standard-input*)
    (let ((continuation (gensym)))
      `(flet ((,continuation (,stream) ,@body))
         (declare (dynamic-extent (function ,continuation)))
         (invoke-with-input-editor-typeout ,stream (function ,continuation) :erase ,erase)))))

(defun clear-typeout (&optional (stream t))
  (with-input-editor-typeout (stream :erase t)
    (declare (ignore stream))))


(defclass standard-input-editing-stream (edward-mixin standard-encapsulating-stream input-editing-stream)
  (;; Editor-related
   (input-editor-buffer :reader stream-input-buffer)
   (insertion-pointer :accessor stream-insertion-pointer :initarg :ip)
   (scan-pointer :accessor stream-scan-pointer :initarg :sp)
   (fill-pointer :accessor stream-fill-pointer :initarg :fp)
   (rescanning-p :accessor stream-rescanning-p :initarg :rescanning-p)
   (rescan-queued-p :accessor stream-rescan-queued-p :initarg :rescan-queued-p)
   ;; Display-related
   (x0 :accessor x0)
   (y0 :accessor y0))
  (:default-initargs :sp 0 :ip 0 :fp 0 :rescanning-p nil :rescan-queued-p nil))

(defmethod initialize-instance :after ((editing-stream standard-input-editing-stream) &key stream &allow-other-keys)
  (setf (values (x0 editing-stream) (y0 editing-stream))
        (stream-cursor-position (encapsulating-stream-stream editing-stream))))

;;; XXX we should maintain a real cursor.
(defmethod stream-cursor-initial-position ((stream standard-input-editing-stream))
  (with-slots (x0 y0) stream
    (values x0 y0)))

(defmethod invoke-with-input-editing
    ((stream standard-input-editing-stream)
     continuation input-sensitizer initial-contents class)
  (declare (ignore class))
  (catch 'activate
    (loop
      (catch 'rescan
        (reset-scan-pointer stream)
        (loop (funcall continuation stream)))))
  (let ((str (edward-buffer-string stream)))
    (format *debug-io* "Stream activated! returnging ~s~%" str)
    str))

(defmethod invoke-with-input-editor-typeout
    ((stream standard-input-editing-stream) continuation &key erase)
  (erase-input-buffer stream)
  (fresh-line stream)
  (if erase
      (surrounding-output-with-border (stream :filled t :ink +background-ink+)
        (funcall continuation stream))
      (funcall continuation stream))
  (fresh-line stream)
  (redraw-input-buffer stream))

;;; FIXME implement noise strings!
(defmethod input-editor-format ((stream standard-extended-output-stream) format-string &rest format-args)
  (apply #'format stream format-string format-args))

;;; "Rescanning" is a fancy name for restarting the continuation, so it may
;;; read gestures up to the current insertion pointer as if they were typed
;;; anew.  This is to give a chance for the client code to exit early for
;;; example to return a completion.
(defmethod reset-scan-pointer
    ((stream standard-encapsulating-stream) &optional (scan-pointer 0))
  (setf (stream-scan-pointer stream) scan-pointer
        (stream-rescanning-p stream) t
        (stream-rescan-queued-p stream) nil))

(defmethod immediate-rescan ((stream standard-encapsulating-stream))
  (throw 'rescan t))

(defmethod queue-rescan ((stream standard-encapsulating-stream))
  (setf (stream-rescan-queued-p stream) t))

(defmethod rescan-if-necessary
    ((stream standard-encapsulating-stream) &optional inhibit-activation)
  (when (stream-rescan-queued-p stream)
    (setf (stream-rescan-queued-p stream) nil)
    (immediate-rescan stream)))

(defmethod erase-input-buffer ((stream standard-encapsulating-stream) &optional (start-position 0))
  (repaint-sheet (encapsulating-stream-stream stream) +everywhere+))

;;; We probably must add an output record in a case of repaint (i.e on scroll).
;;; Otherwise we must somehow know that the input buffer needs to be redrawn.
(defmethod redraw-input-buffer ((stream standard-encapsulating-stream) &optional (start-position 0))
  (setf (stream-cursor-position stream)
        (stream-cursor-initial-position stream))
  (let ((sheet (encapsulating-stream-stream stream)))
    (flet ((thunk (sheet)
             (with-sheet-medium (medium stream)
               (loop with buffer = (input-editor-buffer stream)
                     with cursor = (edward-cursor stream)
                     with cursor-line = (cluffer:line cursor)
                     with cursor-position = (cluffer:cursor-position cursor)
                     with tstyle = (medium-text-style medium)
                     with height = (text-style-height tstyle medium)
                     with sfocus = (port-keyboard-input-focus (port stream))
                     for lineno from 0 below (cluffer:line-count buffer)
                     for line = (cluffer:find-line buffer lineno)
                     for text = (coerce (cluffer:items line) 'string)
                     for cury from 0 by height
                     if (eq line cursor-line) do
                       (stream-write-string sheet text 0 cursor-position)
                       (with-room-for-graphics (sheet :move-cursor nil :first-quadrant nil)
                         (draw-rectangle* sheet 0 0 2 height))
                       (stream-write-string sheet text cursor-position)
                       (terpri sheet)
                     else do
                       (princ text sheet)
                       (terpri sheet)))))
      (if (member (stream-end-of-line-action stream) '(:wrap :wrap*))
          (filling-output (sheet :after-line-break "↳ ")
            (thunk sheet))
          (thunk sheet)))))

(defmethod stream-process-gesture ((stream standard-input-editing-stream) gesture type)
  (when (activation-gesture-p gesture)
    (throw 'activate t))
  (if (handle-editor-event stream gesture)
      (with-output-recording-options (stream :record nil :draw t)
        (erase-input-buffer stream)
        (redraw-input-buffer stream))
      nil)
  #|invoke the command|#)

(defmethod stream-read-gesture ((stream standard-input-editing-stream) &key &allow-other-keys)
  (rescan-if-necessary stream)
  (loop
    (if (< (stream-scan-pointer stream)
           (stream-insertion-pointer stream))
        ;; FIXME this assumes that there is only a single line (scan pointer
        ;; is taken as is).
        (let ((cursor (edward-cursor stream))
              (scan-pointer (stream-scan-pointer stream)))
          (incf (stream-scan-pointer stream))
          (return-from stream-read-gesture
            (cluffer:item-at-position (cluffer:line cursor) scan-pointer)))
        (multiple-value-bind (result reason) (call-next-method)
          (when (null result)
            (return-from stream-read-gesture (values result reason)))
          (when-let ((result* (stream-process-gesture stream result t)))
            (return-from stream-read-gesture result*))))))

(defmethod stream-unread-gesture ((stream standard-input-editing-stream) gesture)
  (assert (> (stream-scan-pointer stream) 0))
  (decf (stream-scan-pointer stream)))


