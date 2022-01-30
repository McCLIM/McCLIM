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
;;; This file contains the implementation of changes propagation. The function
;;; NOTE-OUTPUT-RECORD-CHILD-CHANGED propagates changes upwards.
;;;
(in-package #:clim-internals)

(defmethod propagate-output-record-changes-p
    (record child mode old-position old-bounding-rectangle)
  (not (null record)))

(defmethod propagate-output-record-changes
    (record child mode &optional old-position old-bounding-rectangle
                                 difference-set check-overlapping)
  (declare (ignore record child mode old-position old-bounding-rectangle))
  (values difference-set check-overlapping))

(defmethod note-output-record-child-changed
    (record child mode old-position old-bounding-rectangle stream
     &key difference-set check-overlapping)
  (if (propagate-output-record-changes-p
       record child mode old-position old-bounding-rectangle)
      (let ((old-bbox (copy-bounding-rectangle record)))
        (multiple-value-bind (difference-set check-overlapping)
            (propagate-output-record-changes
             record child mode old-position old-bounding-rectangle
             difference-set check-overlapping)
          (note-output-record-child-changed
           (output-record-parent record) record mode nil old-bbox stream
           :difference-set difference-set
           :check-overlapping check-overlapping)))
      (destructuring-bind (erases moves draws erases* moves*) difference-set
        (incremental-redisplay stream nil erases moves draws erases* moves*))))
