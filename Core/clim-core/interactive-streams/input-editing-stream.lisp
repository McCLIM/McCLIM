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
  ((input-editor-buffer :reader stream-input-buffer)
   (insertion-pointer :accessor stream-insertion-pointer :initarg :ip)
   (scan-pointer :accessor stream-scan-pointer :initarg :sp)
   (fill-pointer :accessor stream-fill-pointer :initarg :fp)
   (rescanning-p :accessor stream-rescanning-p :initarg :rescanning-p)
   (rescan-queued-p :accessor stream-rescan-queued-p :initarg :rescan-queued-p))
  (:default-initargs :sp 0 :ip 0 :fp 0 :rescanning-p nil :rescan-queued-p nil))

(defmethod invoke-with-input-editing
    ((stream standard-input-editing-stream) continuation input-sensitizer initial-contents class)
  (declare (ignore class))
  (catch 'rescan
    (reset-scan-pointer stream)
    (loop (funcall continuation stream))))

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

(defmethod input-editor-format ((stream t) format-string &rest format-args)
  (unless (and (typep stream 'string-stream)
               (input-stream-p stream))
    (apply #'format stream format-string format-args)))

(defmethod reset-scan-pointer ((stream standard-encapsulating-stream) &optional (scan-pointer 0)))

(defmethod immediate-rescan ((stream standard-encapsulating-stream)))
(defmethod queue-rescan ((stream standard-encapsulating-stream)))
(defmethod rescan-if-necessary ((stream standard-encapsulating-stream) &optional inhibit-activation))

(defmethod erase-input-buffer ((stream standard-encapsulating-stream) &optional (start-position 0))
  (repaint-sheet (encapsulating-stream-stream stream) +everywhere+))

(defmethod redraw-input-buffer ((sheet standard-encapsulating-stream) &optional (start-position 0))
  (multiple-value-bind (x0 y0) (stream-cursor-position sheet)
    (with-bounding-rectangle* (x1 y1 x2 y2) (stream-page-region (encapsulating-stream-stream sheet))
     (with-sheet-medium (medium sheet)
       (loop with buffer = (input-editor-buffer sheet)
             with cursor = (edward-cursor sheet)
             with cursor-line = (cluffer:line cursor)
             with cursor-position = (cluffer:cursor-position cursor)
             with tstyle = (medium-text-style medium)
             with height = (text-style-height tstyle medium)
             with sfocus = (port-keyboard-input-focus (port sheet))
             for lineno from 0 below (cluffer:line-count buffer)
             for line = (cluffer:find-line buffer lineno)
             for text = (coerce (cluffer:items line) 'string)
             for cury from y0 by height
             do (draw-text* sheet text x0 cury :align-x :left :align-y :top)
             when (eq line cursor-line)
               do (let ((cursor-dx (+ x0 (nth-value 2 (text-size medium text :end cursor-position)))))
                    (draw-rectangle* sheet cursor-dx cury (+ cursor-dx 2) (+ cury height)
                                     :ink +blue+ :filled nil
                                     #+ ()
                                     (if t ;(> cursor-dx x2)
                                         +dark-red+
                                         (if (eq sheet sfocus) +black+ +dark-grey+))))
             do (setf x0 x1))))))

(defmethod stream-process-gesture ((stream standard-input-editing-stream) gesture type)
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
           (stream-fill-pointer stream))
        (incf (stream-scan-pointer stream))
        (multiple-value-bind (result reason) (call-next-method)
          (when (null result)
            (return-from stream-read-gesture (values result reason)))
          (when-let ((result* (stream-process-gesture stream result t)))
            (return-from stream-read-gesture result*))))))

(defmethod stream-unread-gesture ((stream standard-input-editing-stream) gesture)
  (decf (stream-scan-pointer stream)))


