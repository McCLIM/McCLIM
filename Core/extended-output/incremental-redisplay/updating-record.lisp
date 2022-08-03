;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2002 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2002-2004 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) Copyright 2021 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This file contains a definition of the standard-updating-output-record.
;;;
(in-package #:clim-internals)

;;; FIXME: although this inherits from COMPLETE-MEDIUM-STATE, in fact it
;;; needn't, as we only ever call SET-MEDIUM-CURSOR-POSITION on it.  Until
;;; 2006-05-28, we did also use the various medium attributes, but with the
;;; reworking of REPLAY-OUTPUT-RECORD (STANDARD-DISPLAYED-OUTPUT-RECORD) to
;;; use around methods and WITH-DRAWING-OPTIONS, they are no longer necessary.
;;;
;;; FIXME shouldn't we maintain a complete-cursor-state here? The cursor has
;;; width, height, appearance and position. -- jd 2021-11-15
(defclass updating-stream-state (complete-medium-state)
  ((cursor-x :accessor cursor-x :initarg :cursor-x :initform 0)
   (cursor-y :accessor cursor-y :initarg :cursor-y :initform 0)
   (cursor-height :accessor cursor-height :initarg :cursor-height :initform 0)))

(defmethod initialize-instance :after ((obj updating-stream-state)
                                       &key (stream nil))
  (when stream
    (setf (values (slot-value obj 'cursor-x) (slot-value obj 'cursor-y))
          (stream-cursor-position stream))
    (setf (slot-value obj 'cursor-height)
          (stream-cursor-height stream))))

(defmethod match-output-records-1 and ((state updating-stream-state)
                                       &key (cursor-x 0 x-supplied-p)
                                         (cursor-y 0 y-supplied-p)
                                         (cursor-height 0 h-supplied-p))
  (and (or (not x-supplied-p)
           (coordinate= (slot-value state 'cursor-x) cursor-x))
       (or (not y-supplied-p)
           (coordinate= (slot-value state 'cursor-y) cursor-y))
       (or (not h-supplied-p)
           (coordinate= (slot-value state 'cursor-height) cursor-height))))

(defgeneric set-medium-cursor-position (state stream)
  (:method ((state updating-stream-state) (stream updating-output-stream-mixin))
    (setf (stream-cursor-position stream)
          (values (cursor-x state) (cursor-y state)))
    (setf (stream-cursor-height stream)
          (cursor-height state))))

(defmethod medium-graphics-state ((stream updating-output-stream-mixin)
                                  &optional state)
  (if (and state (subtypep state 'updating-stream-state))
      (reinitialize-instance state :stream stream)
      (make-instance 'updating-stream-state :stream stream)))

;;; XXX Add to values we test, obviously.
;;;
;;; Well, maybe not.  The goal is to support output records that have moved
;;; but that are otherwise clean. I.e., some previous part of the output has
;;; changed (lines added or deleted, for example). If the stream cursor
;;; position is different, I'm not sure now that the code for the updating
;;; output record needs to be rerun; I think we could use only the difference
;;; in cursor position to move the record. Any other graphics state change --
;;; like a different foreground color -- should probably be handled by the
;;; programmer forcing all new output.

(defun state-matches-stream-p (record stream)
  (or (output-record-fixed-position record)
      (let ((state (start-graphics-state record))
            (cx (stream-cursor-position stream)))
        ;; Note: We don't match the y coordinate.
        (match-output-records state :cursor-x cx))))

(defclass updating-output-record-mixin (updating-output-map-mixin
                                        standard-sequence-output-record)
  ((unique-id
    :reader output-record-unique-id
    :initarg :unique-id)
   (id-test
    :reader output-record-id-test
    :initarg :id-test
    :initform #'eql)
   (cache-value
    :reader output-record-cache-value
    :initarg :cache-value)
   (cache-test
    :reader output-record-cache-test
    :initarg :cache-test
    :initform #'eql)
   (fixed-position
    :reader output-record-fixed-position
    :initarg :fixed-position :initform nil)
   (displayer
    :accessor output-record-displayer
    :initarg :displayer)
   ;; Start and end cursor
   (start-graphics-state
    :accessor start-graphics-state
    :initarg :start-graphics-state
    :documentation "Graphics state needed to
   render record")
   (end-graphics-state
    :accessor end-graphics-state
    :initarg :end-graphics-state
    :documentation "Graphics state after rendering record; used to render non
                    updating-output-records that follow")
   (old-children
    :accessor old-children
    :documentation "Contains the output record tree for the current display.")
   (output-record-dirty
    :accessor output-record-dirty
    :initform :updating
    :documentation ":updating :updated :clean :moved")
   (parent-cache
    :accessor parent-cache :initarg :parent-cache
    :documentation "The parent cache in which this updating output record is
                    stored.")
   (stream
    :accessor updating-output-stream :initarg :stream :initform nil
    :documentation "Capture the screen in order to restrict update to visible
                    records")
   (parent-updating-output
    :accessor parent-updating-output
    :initarg :parent-updating-output :initform nil
    :documentation "A backlink to the
updating-output-parent above this one in the tree.")
   ;; Results of (setf output-record-position) while updating
   (old-bounds
    :accessor old-bounds
    :initform (make-bounding-rectangle 0.0d0 0.0d0 0.0d0 0.0d0)
    :documentation "Holds the old bounds of an updating output record if that
                    can no longer be determined from the old-children.")
   ;; on-screen state?
   ))

(defgeneric sub-record (record)
  (:method ((record updating-output-record-mixin))
    (let ((children (output-record-children record)))
      (if (zerop (length children))
          nil
          (aref children 0)))))

(defmethod output-record-start-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))

(defmethod output-record-end-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (end-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record updating-output-record-mixin))
  (let ((state (end-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))

;;; Prevent deleted output records from coming back from the dead.
(defmethod delete-output-record :after
    ((child updating-output-record-mixin) record &optional errorp)
  (declare (ignore record errorp))
  (let ((pcache (parent-cache child)))
    (delete-from-map pcache
                     (output-record-unique-id child)
                     (output-record-id-test child))))


(defclass standard-updating-output-record (updating-output-record-mixin
                                           updating-output-record)
  ())

(defmethod print-object ((obj standard-updating-output-record) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-standard-rectangle* (x1 y1 x2 y2) obj
      (format stream "X ~S:~S Y ~S:~S " x1 x2 y1 y2))
    (when (slot-boundp obj 'unique-id)
      (let ((*print-length* 10)
            (*print-level* 3))
        (format stream " ~S" (output-record-unique-id obj))))))

;;; Helper function for visiting updating-output records in a tree

(defgeneric map-over-updating-output (function root use-old-records)
  (:method (function (record standard-updating-output-record) use-old-records)
    (funcall function record)
    (let ((children (cond (use-old-records
                           (when (slot-boundp record 'old-children)
                             (old-children record)))
                          (t (sub-record record)))))
      (when children
        (map-over-updating-output function children use-old-records))))
  (:method (function (record compound-output-record) use-old-records)
    (flet ((mapper (r) (map-over-updating-output function r use-old-records)))
      (declare (dynamic-extent #'mapper))
      (map-over-output-records #'mapper record)))
  (:method (function record use-old-records)
    (declare (ignore function record use-old-records))
    nil))
