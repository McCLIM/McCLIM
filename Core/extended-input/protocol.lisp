(in-package #:clim-internals)

;;;; Part VI: Extended Stream Input Facilities

;; CLIM Specification says that E-I-S is a subclass of INPUT-STREAM,
;; but it does not say what is it. We infer it is a base class for
;; all CLIM input streams (standard-input-stream included).
(defclass input-stream (fundamental-input-stream) ())

;;; 22.2 Extended Input Streams

(define-protocol-class extended-input-stream (input-stream))

;;; 22.2.1 The Extended Stream Input Protocol

(defgeneric stream-input-buffer (stream))
(defgeneric (setf stream-input-buffer) (buffer stream))
(defgeneric stream-pointer-position (stream &key pointer))
(defgeneric* (setf stream-pointer-position) (x y stream))
(defgeneric stream-set-input-focus (stream))
(defgeneric stream-read-gesture
    (stream &key timeout peek-p input-wait-test
            input-wait-handler pointer-button-press-handler))
(defgeneric stream-input-wait (stream &key timeout input-wait-test))
(defgeneric stream-unread-gesture (stream gesture))

;;; 22.2.2 Extended Input Stream Conditions

(defgeneric abort-gesture-event (condition))
(defgeneric accelerator-gesture-event (condition))
(defgeneric accelerator-gesture-numeric-argument (condition))

;;; McCLIM relies on a text editor class (by default DREI-INPUT-EDITING-MIXIN)
;;; to perform the user interaction and display for input editing. Also, that
;;; class must update the stream buffer and the insertion pointer, cause
;;; rescans to happen, and handle activation gestures.
(defgeneric stream-process-gesture (stream gesture type)
  (:documentation "If gesture is an input editing command,
stream-process-gesture performs the input editing operation on
the input editing stream `stream' and returns NIL. Otherwise, it
returns the two values `gesture' and `type'."))

;;; 22.5 Pointer Tracking

;; tracking-pointer
;; dragging-output

(defgeneric drag-output-record
    (stream output &key repaint erase feedback finish-on-release multiple-window))
