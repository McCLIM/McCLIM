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
      (let ((continuation (gensym))
            (editing-stream (gensym)))
        `(flet ((,continuation (,stream) ,@body))
           (declare (dynamic-extent (function ,continuation)))
           (let ((,editing-stream (if (input-editing-stream-p ,stream)
                                      ,stream
                                      (make-instance ,class :stream ,stream ,@args))))
             (invoke-with-input-editing ,editing-stream (function ,continuation)
                                        ,input-sensitizer ,initial-contents nil)))))))

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
   (rescanning-p :accessor stream-rescanning-p :initarg :rescanning-p)
   (rescan-queued-p :accessor stream-rescan-queued-p :initarg :rescan-queued-p)
   ;; Display-related
   (x0 :accessor x0)
   (y0 :accessor y0))
  (:default-initargs :rescanning-p nil :rescan-queued-p nil))

(defmethod initialize-instance :after ((editing-stream standard-input-editing-stream) &key stream &allow-other-keys)
  (setf (values (x0 editing-stream) (y0 editing-stream))
        (stream-cursor-position stream)))

(defmethod stream-scan-pointer ((stream standard-input-editing-stream))
  (cursor-linear-position (scan-cursor stream)))

(defmethod (setf stream-scan-pointer) (position (stream standard-input-editing-stream))
  (setf (cursor-linear-position (scan-cursor stream)) position))

(defmethod stream-insertion-pointer ((stream standard-input-editing-stream))
  (cursor-linear-position (edit-cursor stream)))

(defmethod (setf stream-insertion-pointer) (position (stream standard-input-editing-stream))
  (setf (cursor-linear-position (edit-cursor stream)) position))

(defmethod stream-fill-pointer ((stream standard-input-editing-stream))
  (let ((buffer (input-editor-buffer stream)))
    (+ -1
       (cluffer:item-count buffer)
       ;; Each line stands for a "newline".
       (cluffer:line-count buffer))))

;;; XXX Conses a new string each time this is called. We can do better by
;;; utilizing cluffer update protocol (allocate one string with a fill pointer
;;; and update it only when the buffer changes).
(defmethod stream-input-buffer ((stream standard-input-editing-stream))
  (edward-buffer-string stream))

;;; XXX we should maintain a real cursor.
(defmethod stream-cursor-initial-position ((stream standard-input-editing-stream))
  (with-slots (x0 y0) stream
    (values x0 y0)))

;;; FIXME This code is for backward compatibility; but only internally in the
;;; codebase. References to functions i-e-r-l and finalize should be purged.
(defun input-editing-rescan-loop (stream continuation)
  (loop with start-position = (stream-scan-pointer stream) do
    (catch 'rescan
      (reset-scan-pointer stream start-position)
      (return-from input-editing-rescan-loop
        (funcall continuation stream)))))

(defun finalize (stream input-sensitizer)
  (clear-typeout stream)
  (erase-input-buffer stream)
  (if input-sensitizer
      (funcall input-sensitizer stream
               (lambda () (redraw-input-buffer stream)))
      (redraw-input-buffer stream)))

(defmethod (setf cursor-visibility) (visibility (stream standard-input-editing-stream))
  visibility)

;; (defgeneric input-editing-stream-output-record (stream)
;;   (:documentation "Return the output record showing the display of the
;; input-editing stream `stream' values. This function does not
;; appear in the spec but is used by the command processing code for
;; layout and to implement a general with-input-editor-typeout."))

;;; ~

(defmethod invoke-with-input-editing
    ((stream standard-input-editing-stream)
     continuation input-sensitizer initial-contents class)
  (declare (ignore class))
  (when (and initial-contents (not (stream-rescanning-p stream)))
    (if (stringp initial-contents)
        (replace-input stream initial-contents)
        (presentation-replace-input stream
                                    (first initial-contents)
                                    (second initial-contents)
                                    (stream-default-view stream))))
  (unwind-protect (input-editing-rescan-loop stream continuation)
    (format *debug-io* "i-w-i-e: finalizing! result ~s~%" (edward-buffer-string stream))
    (finalize stream input-sensitizer)))

(defmethod replace-input ((stream standard-input-editing-stream) (new-input array)
                          &key (start 0)
                               (end (length new-input))
                               (buffer-start (stream-scan-pointer stream))
                               (rescan nil rescan-supplied-p))
  (let* ((buffer-str (stream-input-buffer stream))
         (buffer-end (stream-scan-pointer stream))
         (mismatch-p (mismatch new-input buffer-str :start1 start :end1 end
                                                    :start2 buffer-start :end2 buffer-end)))
    ;; Cursors are implicitly updated by the underlying buffer. -- jd 2022-08-03
    (edward-replace-input stream new-input start end buffer-start)
    (with-output-recording-options (stream :record nil :draw t)
      (erase-input-buffer stream)
      (redraw-input-buffer stream))
    ;; XXX: This behavior for the :rescan parameter is not mentioned explicitly
    ;; in any CLIM guide, but McCLIM input-editing machinery relies on it.
    (if rescan-supplied-p
        (when rescan
          (queue-rescan stream))
        (when mismatch-p
          (queue-rescan stream))))
  ;; The returned value is "the position in the input buffer" - we assume that
  ;; it is the position from where the next gesture would be read. Mind that a
  ;; queued rescan may change the scan pointer position afterwards.
  (stream-scan-pointer stream))

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
                     with cursor = (edit-cursor stream)
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
  (when (handle-editor-event stream gesture)
    (with-output-recording-options (stream :record nil :draw t)
      (erase-input-buffer stream)
      (redraw-input-buffer stream))))

(defun try-scan-element (stream peek-p)
  (let ((scan (scan-cursor stream)))
    (if (< (stream-scan-pointer stream)
           (stream-fill-pointer stream))
        (if peek-p
            (smooth-peek-item scan)
            (smooth-forward-item scan))
        (setf (stream-rescanning-p stream) nil))))

(defmethod stream-read-gesture ((stream standard-input-editing-stream)
                                &key peek-p &allow-other-keys)
  (rescan-if-necessary stream)
  (loop for elt = (try-scan-element stream peek-p)
        while (null elt) do
          (multiple-value-bind (result reason) (call-next-method)
            (when (null result)
              (return-from stream-read-gesture (values result reason)))
            (stream-process-gesture stream result t))
        finally (return elt)))

(defmethod stream-unread-gesture ((stream standard-input-editing-stream) gesture)
  (decf (stream-scan-pointer stream)))


