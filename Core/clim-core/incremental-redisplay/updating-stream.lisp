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
;;; This file contains a definition of a mixin for redisplayable streams.
;;;
(in-package #:clim-internals)

;;; Should this have a more complete CPL, to pull in the fact that it needs a
;;; medium for graphics state?
(defclass updating-output-stream-mixin (updating-output-map-mixin
                                        extended-output-stream)
  ((redisplaying-p
    :initform nil
    :reader stream-redisplaying-p)
   (incremental-redisplay
    :initform nil
    :initarg :incremental-redisplay
    :accessor pane-incremental-redisplay)
   ;; For incremental output, holds the top level updating-output-record.
   (updating-record
    :initform nil
    :initarg :updating-record
    :accessor updating-record)))

(defmacro with-stream-redisplaying ((stream) &body body)
  `(letf (((slot-value ,stream 'redisplaying-p) t)) ,@body))

(defmethod redisplayable-stream-p ((stream updating-output-stream-mixin))
  (declare (ignore stream))
  t)

(defmethod pane-needs-redisplay :around ((pane updating-output-stream-mixin))
  (let ((redisplayp (call-next-method)))
    (values redisplayp (and (not (eq redisplayp :no-clear))
                            (not (pane-incremental-redisplay pane))))))

(defmethod window-clear :after ((pane updating-output-stream-mixin))
  "Get rid of any updating output records stored in the stream; they're gone
  from the screen."
  (clear-map pane))

;;; INCREMENTAL-REDISPLAY takes as input the difference set computed by
;;; COMPUTE-DIFFERENCE-SET and updates the screen. The 5 kinds of updates are
;;; not very well defined in the spec. I understand their semantics thus:
;;;
;;; ERASES, MOVES, and DRAWS refer to records that don't overlap *with other
;;; records that survive in the current rendering*. In other words, they don't
;;; overlap with records that were not considered by COMPUTE-DIFFRENCE-SET,
;;; either because they are children of a clean updating output node or they
;;; are in another part of the output history that is not being redisplayed.
;;;
;;; Another way to think about erases, moves and draws is in terms of a
;;; possible implementation:
;;;
;;; - ERASES regions would be erased
;;; - MOVES regions would be blitted
;;; - DRAWS records would be replayed
;;;
;;; Records in ERASE-OVERLAPPING and MOVE-OVERLAPPING might overlap with any
;;; other record. They need to be implemented by erasing their region on the
;;; screen and then replaying the output history for that region. Thus, any
;;; ordering issues implied by overlapping records is handled correctly. Note
;;; that DRAWS records may be drawn without concern for the history because
;;; they additive. -- jd 2021-12-01
(defmethod incremental-redisplay ((stream updating-output-stream-mixin) position
                                  erases moves draws erase-overlapping move-overlapping)
  (declare (ignore position))
  (flet ((clear-bbox (bbox)
           (with-bounding-rectangle* (x1 y1 x2 y2) bbox
             (medium-clear-area stream x1 y1 x2 y2))))
    (with-output-recording-options (stream :record nil :draw t)
      (loop for (record bbox) in erases
            do (note-output-record-lost-sheet record stream)
               (clear-bbox bbox))
      (loop for (record old-bbox) in moves
            do (clear-bbox old-bbox)
               (replay-output-record record stream))
      (loop for (record bbox) in draws
            do (note-output-record-got-sheet record stream)
               (replay-output-record record stream bbox))
      (when (or erase-overlapping move-overlapping)
        (let ((history (stream-output-history stream))
              (regions +nowhere+))
          (loop for (record bbox) in erase-overlapping
                do (note-output-record-lost-sheet record stream)
                   (setf regions (region-union regions bbox)))
          (loop for (record bbox) in move-overlapping
                do (setf regions (region-union regions bbox)))
          (map-over-region-set-regions #'clear-bbox regions)
          (replay history stream regions))))))

