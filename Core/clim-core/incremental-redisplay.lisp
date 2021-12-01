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
;;; Incremental redisplay.
;;;

(in-package #:clim-internals)

#|
Incremental Redisplay Theory of Operation

Incremental redisplay compares the tree of output records before and after
calling REDISPLAY and updates those parts of the screen that are different.
UPDATING-OUTPUT forms in the code create UPDATING-OUTPUT-RECORDs in the record
tree. These records hold the before and after snapshots of the tree.  When the
display code is first run, the bodies of all the UPDATING-OUTPUT forms are
captured as closures.  Usually only the closure in the top-level output record
will ever get called, but the programmer can call REDISPLAY on any updating
output record, so we have to be prepared for that.

Redisplay proceeds thus:

All the updating output records are visited. Their state is changed to
:UPDATING and the OLD-CHILDREN slot is set to the current children.

The closure of the root updating output record is called. None of the closures
the child updating output records are called because any free variables
captured in the UPDATING-OUTPUT forms need to see the fresh bindings from this
run of the code. As UPDATING-OUTPUT forms are encountered, several things can
happen:

* The cache value of the form compares to the value stored in the record. The
record, and all the updating output records below it, are marked :clean. The
body of UPDATING-OUTPUT isn't run.

* The cache value doesn't compare. The record is marked :UPDATED, and the body
is run.

* There isn't an existing UPDATING-OUTPUT-RECORD for this UPDATING-OUTPUT
form. One is created in state :UPDATED. The body is run.

Before the top level UPDATING-OUTPUT closure returns, various output records
in the history tree might be mutated e.g., be moved. The most common case of
this is in table layout, where the records for each cell are first created and
then assigned a final location based on the dimensions of the table. But these
nodes may be children of an updating output record that has been marked
:CLEAN. Therefore, they have to be treated specially so that the rest of
incremental redisplay will consider them and not leave the screen in a trashed
state. An around method on (SETF OUTPUT-RECORD-POSITION) for display records
checks if incremental redisplay is in progress; if so, it stores the mutated
record in its closest parent UPDATING-OUTPUT record (if any). If that parent
is :CLEAN then it and any other clean parent updating output records are
marked as :UPDATED.

Next, COMPUTE-DIFFERENCE-SET compares the old and new trees. Output records
may be removed, added or moved. Their region union must be erased and then
replayed from the history to ensure the correctness. COMPUTE-DIFFERENCE-SET
compares all display output records that are the record descendants.

Finally, the old tree is walked. All updating output records in state
:UPDATING were not visited at all and thus are deleted from their parent
caches.


Problems / Future work

The complete traversals of the output history tree could be avoided by keeping
a generation number in the updating output record and updating that everytime
the node is visited.

The search for equal display nodes is expensive in part because we have no
spatially organized data structure.

|#

;;; The map from unique values to output records. Unfortunately the :ID-TEST
;;; is specified in the child updating output records, not in the record that
;;; holds the cache! So, the map lookup code jumps through some hoops to use a
;;; hash table if the child id tests allow that and if there enough records in
;;; the map to make that worthwhile.

(defclass updating-output-map-mixin ()
  ((id-map :accessor id-map :initform nil)
   (id-counter :accessor id-counter
               :documentation "The counter used to assign unique ids to
                updating output records without one.")
   (tester-function :accessor tester-function :initform 'none
                    :documentation "The function used to lookup
  updating output records in this map if unique; otherwise, :mismatch.")
   (element-count :accessor element-count :initform 0)))

;;; Complete guess...
(defparameter *updating-map-threshold* 10
  "The limit at which the id map in an updating output record switches to a
  hash table.")

;;; ((eq map-test-func :mismatch)
;;;   nil)
(defun function-matches-p (map func)
  (let ((map-test-func (tester-function map)))
    (cond ((eq map-test-func func)
           t)
          ((and (symbolp map-test-func) (symbolp func)) ; not eq
           nil)
          ((and (symbolp map-test-func) (fboundp map-test-func))
           (eq (symbol-function map-test-func) func))
          ((and (symbolp func) (fboundp func))
           (eq map-test-func (symbol-function func)))
          (t nil))))

(defun ensure-test (map test)
  (unless (function-matches-p map test)
    (explode-map-hash map)
    (setf (tester-function map) :mismatch)))

(defgeneric clear-map (map)
  (:method ((map updating-output-map-mixin))
    (setf (id-map map) nil)
    (setf (id-counter map) 0)
    (setf (element-count map) 0)))

;;; Perhaps these should be generic functions, but in the name of premature
;;; optimization they're not :)
(defun get-from-map (map value test)
  (when (eq (tester-function map) 'none)
    (return-from get-from-map nil))
  (ensure-test map test)
  (let ((map (id-map map)))
    (if (hash-table-p map)
        (gethash value map)
        (cdr (assoc value map :test test)))))


(defun maybe-convert-to-hash (map)
  (let ((test (tester-function map)))
    (when (and (not (eq test :mismatch))
               (> (element-count map) *updating-map-threshold*)
               (or (case test
                     ((eq eql equal equalp) t))
                   (eq test #'eq)
                   (eq test #'eql)
                   (eq test #'equal)
                   (eq test #'equalp)))
      (let ((new-map (make-hash-table :test test)))
        (loop
           for (key . value) in (id-map map)
           do (setf (gethash key new-map) value))
        (setf (id-map map) new-map)))))

(defun explode-map-hash (map)
  (let ((hash-map (id-map map)))
    (when (hash-table-p hash-map)
      (loop
         for key being each hash-key of hash-map using (hash-value record)
         collect (cons key record) into alist
         finally (setf (id-map map) alist)))))

(defun add-to-map (map record value test replace)
  (if (eq (tester-function map) 'none)
      (setf (tester-function map) test)
      (ensure-test map test))
  (let ((val-map (id-map map)))
    (if (hash-table-p val-map)
        (multiple-value-bind (existing-value in-table)
            (if replace
                (gethash value val-map)
                (values nil nil))
          (declare (ignore existing-value))
          (setf (gethash value val-map) record)
          (unless in-table
            (incf (element-count map))))
        (let ((val-cons (if replace
                            (assoc value val-map :test test)
                            nil)))
          (if val-cons
              (setf (cdr val-cons) record)
              (progn
                (setf (id-map map) (acons value record val-map))
                (incf (element-count map))
                (maybe-convert-to-hash map)))))))

(defun delete-from-map (map value test)
  (ensure-test map test)
  (let ((val-map (id-map map))
        (deleted nil))
    (if (hash-table-p val-map)
        (setf deleted (remhash value val-map))
        (setf (values (id-map map) deleted)
              (delete-1 value val-map :test test :key #'car)))
    (when deleted
      (decf (element-count map)))))

;;; Reset the ID counter so that updating output records without explicit IDs
;;; can be assigned one during a run of the code. I'm not sure about using
;;; reinitialize-instance for this...
(defmethod shared-initialize :after ((obj updating-output-map-mixin) slot-names
                                     &key)
  (declare (ignore slot-names))
  (setf (id-counter obj) 0))

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

(defgeneric redisplayable-stream-p (stream)
  (:method ((stream t))
    nil)
  (:method ((stream updating-output-stream-mixin))
    t))

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
(defgeneric incremental-redisplay
    (stream position erases moves draws erase-overlapping move-overlapping)
  (:method ((stream updating-output-stream-mixin) position
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
            (replay history stream regions)))))))

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
  ((unique-id :reader output-record-unique-id :initarg :unique-id)
   (id-test :reader output-record-id-test :initarg :id-test
            :initform #'eql)
   (cache-value :reader output-record-cache-value :initarg :cache-value)
   (cache-test :reader output-record-cache-test :initarg :cache-test
               :initform #'eql)
   (fixed-position :reader output-record-fixed-position
                   :initarg :fixed-position :initform nil)
   (displayer :accessor output-record-displayer :initarg :displayer)
   ;; Start and end cursor
   (start-graphics-state :accessor start-graphics-state
                         :initarg :start-graphics-state
                         :documentation "Graphics state needed to
   render record")
   (end-graphics-state :accessor end-graphics-state
                       :initarg :end-graphics-state
                       :documentation "Graphics state after rendering
   record; used to render non updating-output-records that follow")
   (old-children :accessor old-children
                 :documentation "Contains the output record tree for the
  current display.")
   (output-record-dirty :accessor output-record-dirty :initform :updating
          :documentation
          ":updating
           :updated
           :clean")
   (parent-cache :accessor parent-cache :initarg :parent-cache
                 :documentation "The parent cache in which this updating output
record is stored.")
   (stream :accessor updating-output-stream :initarg :stream :initform nil
           :documentation "Capture the screen in order to restrict update to
                                        visible records")
   (parent-updating-output :accessor parent-updating-output
                           :initarg :parent-updating-output :initform nil
                           :documentation "A backlink to the
updating-output-parent above this one in the tree.")
   ;; Results of (setf output-record-position) while updating
   (old-bounds :accessor old-bounds
               :initform (make-bounding-rectangle 0.0d0 0.0d0 0.0d0 0.0d0)
               :documentation "Holds the old bounds of an updating output
 record if that can no longer be determined from the old-children.")
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

(defvar *current-updating-output* nil)

(defgeneric compute-new-output-records (record stream)
  (:method ((record standard-updating-output-record) stream)
    (with-output-recording-options (stream :record t :draw nil)
      (map-over-updating-output
       #'(lambda (r)
           (let ((sub-record (sub-record r)))
             (when sub-record
               (setf (old-children r) sub-record)
               (setf (output-record-dirty r) :updating)
               (setf (rectangle-edges* (old-bounds r))
                     (rectangle-edges* sub-record)))))
       record
       nil)
      (force-output stream)
      ;; Why is this binding here? We need the "environment" in this call that
      ;; computes the new records of an outer updating output record to resemble
      ;; that when a record's contents are computed in invoke-updating-output.
      (letf (((stream-current-output-record stream)
              (output-record-parent record)))
        (compute-new-output-records-1 record
                                      stream
                                      (output-record-displayer record))))))

;;; Create the sub-record that holds the new contents of the updating output
;;; record.
(defun %invoke-updating (record stream displayer)
  (letf (((stream-current-output-record stream) record))
    (with-new-output-record (stream)
      (funcall displayer stream))))

(defun compute-new-output-records-1 (record stream displayer)
  "Like compute-new-output-records with an explicit displayer function."
  (check-type record standard-updating-output-record)
  (when-let ((sub-record (sub-record record)))
    (delete-output-record sub-record record))
  ;; Don't add this record repeatedly to a parent updating-output-record.
  (unless (eq (output-record-parent record)
              (stream-current-output-record stream))
    (setf (output-record-parent record) nil)
    (add-output-record record (stream-current-output-record stream)))
  (reinitialize-instance record)
  (%invoke-updating record stream displayer)
  (setf (output-record-dirty record) :updated))

#+nyi
(defgeneric find-child-output-record (record use-old-elements record-type
                                      &rest initargs
                                      &key unique-id unique-id-test))

(defconstant +fixnum-bits+ (integer-length most-positive-fixnum))

(declaim (inline hash-coords))
(defun hash-coords (x1 y1 x2 y2)
  (declare (type coordinate x1 y1 x2 y2))
  (let ((hash-val 0))
      (declare (type fixnum hash-val))
      (labels ((rot4 (val)
                 (dpb (ldb (byte 4 0) val)
                      (byte 4 (- +fixnum-bits+ 4 1))
                      (ash val -4)))
               (mix-it-in (val)
               (let ((xval (sxhash val)))
                 (declare (type fixnum xval))
                 (when (minusp val)
                   (setq xval (rot4 xval)))
                 (setq hash-val (logxor (rot4 hash-val) xval)))))
        (declare (inline rot4 mix-it-in))
        (mix-it-in x1)
        (mix-it-in y1)
        (mix-it-in x2)
        (mix-it-in y2)
        hash-val)))

(defgeneric output-record-hash (record)
  (:documentation "Produce a value that can be used to hash the output record
in an equalp hash table")
  (:method  ((record standard-bounding-rectangle))
    (slot-value record 'coordinates))
  (:method ((record output-record))
    (with-bounding-rectangle* (x1 y1 x2 y2) record
      (hash-coords x1 y1 x2 y2))))

(defgeneric compute-difference-set (record &optional check-overlapping
                                             offset-x offset-y
                                             old-offset-x old-offset-y)
  (:method ((record standard-updating-output-record)
            &optional (check-overlapping t)
              offset-x offset-y
              old-offset-x old-offset-y)
    (declare (ignore offset-x offset-y old-offset-x old-offset-y)
             (values list list list list list))
    ;; (declare (values erases moves draws #|erase-overlapping move-overlapping|#))
    (let ((old-table (make-hash-table :test #'equalp))
          (new-table (make-hash-table :test #'equalp))
          (all-table (make-hash-table)))
      (collect (old-records new-records)
        (flet ((collect-1 (record set)
                 (setf (gethash record all-table) t)
                 (ecase set
                   (:old
                    (old-records record)
                    (push record (gethash (output-record-hash record) old-table)))
                   (:new
                    (new-records record)
                    (push record (gethash (output-record-hash record) new-table))))))
          (labels ((gather-records (record set)
                     (typecase record
                       (displayed-output-record
                        (collect-1 record set))
                       (updating-output-record
                        (ecase (output-record-dirty record)
                          ((:clean :moved)
                           (collect-1 record set))
                          ((:updating :updated)
                           (let ((sub (ecase set
                                        (:old (old-children record))
                                        (:new (sub-record record)))))
                             (map-over-output-records #'gather-records sub
                                                      nil nil set)))))
                       (otherwise
                        (map-over-output-records #'gather-records record
                                                 nil nil set)))))
            (gather-records record :old)
            (gather-records record :new)))
        (collect (erases moves draws)
          (flet ((add-record (rec)
                   (if (updating-output-record-p rec)
                       (ecase (output-record-dirty rec)
                         (:moved
                          ;; If we ever use the new position for something
                          ;; then the specification says it stick it here.
                          (moves (list rec (old-bounds rec) #|new-position|#)))
                         (:clean
                          ;; no need to redraw clean records.
                          nil)
                         #+ (or)
                         ((:updating :updated)
                          ;; UPDATING-OUTPUT-RECORDs with the state :UPDATED
                          ;; are not collected (their children are collected).
                          (error "Updated recoreds are not collected!")))
                       (flet ((match-record (r) (output-record-equal rec r)))
                         (let* ((hash (output-record-hash rec))
                                ;; The bounding rectangle is always the same.
                                (entry (list rec (bounding-rectangle rec)))
                                (old-p (some #'match-record (gethash hash old-table)))
                                (new-p (some #'match-record (gethash hash new-table))))
                           (cond ((null new-p) (erases entry))
                                 ((null old-p) (draws entry))
                                 ;; Record siblings might have been reordered
                                 ;; so we need to "move it" in place.
                                 (t (moves entry))))))))
            (alexandria:maphash-keys #'add-record all-table))
          (if (null check-overlapping)
              (values (erases) (moves) (draws)      nil     nil)
              (values      nil     nil (draws) (erases) (moves))))))))

(defvar *trace-updating-output* nil)

(defvar *no-unique-id* (cons nil nil))

(defun move-output-record (record dx dy)
  (assert (not (output-record-fixed-position record)))
  (multiple-value-bind (x y) (output-record-position record)
    (setf (output-record-position record)
          (values (+ x dx) (+ y dy))))
  ;; Cursor positions are only guaranteed to be non-nil for text
  ;; output records (16.2.1 The Basic Output Record Protocol)
  (multiple-value-bind (x y) (output-record-start-cursor-position record)
    (when (and x y)
      (setf (output-record-start-cursor-position record)
            (values (+ x dx) (+ y dy)))))
  (multiple-value-bind (x y) (output-record-end-cursor-position record)
    (when (and x y)
      (setf (output-record-end-cursor-position record)
            (values (+ x dx) (+ y dy))))))

(defmethod invoke-updating-output ((stream updating-output-stream-mixin)
                                   continuation
                                   record-type
                                   unique-id id-test cache-value cache-test
                                   &key (fixed-position nil) (all-new nil)
                                        (parent-cache nil))
  (force-output stream)
  (setf parent-cache (or parent-cache *current-updating-output* stream))
  (when (eq unique-id *no-unique-id*)
    (setq unique-id (incf (id-counter parent-cache))))
  (let ((record (get-from-map parent-cache unique-id id-test)))
    (cond ((or all-new (null record))
           ;; This case covers the outermost updating-output too.
           (with-new-output-record
               (stream record-type *current-updating-output*
                       :unique-id unique-id
                       :id-test id-test
                       :cache-value cache-value
                       :cache-test cache-test
                       :fixed-position fixed-position
                       :displayer continuation
                       :parent-cache parent-cache
                       :stream stream
                       :parent-updating-output *current-updating-output*)
             (setq record *current-updating-output*)
             (when *trace-updating-output*
               (format *trace-output* "Creating ~S~%" record))
             (setf (start-graphics-state record) (medium-graphics-state stream))
             (%invoke-updating record stream continuation)
             (setf (end-graphics-state record) (medium-graphics-state stream))
             (add-to-map parent-cache record  unique-id id-test all-new)))
          ((or (not (state-matches-stream-p record stream))
               (not (funcall cache-test cache-value (output-record-cache-value record))))
           (when *trace-updating-output*
             (format *trace-output* "~:[cache test~;stream state~] ~S~%"
                     (state-matches-stream-p (start-graphics-state record) stream)
                     record))
           (let ((*current-updating-output* record))
             (setf (start-graphics-state record) (medium-graphics-state stream))
             (compute-new-output-records-1 record stream continuation)
             (setf (slot-value record 'cache-value) cache-value)
             (setf (end-graphics-state record) (medium-graphics-state stream))
             (setf (parent-cache record) parent-cache)
             (setf (output-record-displayer record) continuation)))
          ;; It doesn't need to be updated, but it does go into the parent's
          ;; sequence of records.
          ((output-record-fixed-position record)
           (setf (output-record-parent record) nil)
           (map-over-updating-output (lambda (r)
                                       (setf (output-record-dirty r) :clean))
                                     record
                                     nil)
           (add-output-record record (stream-current-output-record stream))
           (setf (parent-cache record) parent-cache)
           (setf (output-record-displayer record) continuation))
          ;; It doesn't need to be updated, but it does go into the parent's
          ;; sequence of records. The record also needs to be moved.
          (t
           (multiple-value-bind (cx cy)
               (stream-cursor-position stream)
             (multiple-value-bind (sx sy)
                 (output-record-start-cursor-position record)
               (let ((dx (- cx sx))
                     (dy (- cy sy)))
                 (unless (zerop dy)
                   (move-output-record record dx dy))
                 (let ((tag (if (= dx dy 0) :clean :moved)))
                   (when *trace-updating-output*
                     (format *trace-output* "~a ~s~%" tag record))
                   (map-over-updating-output
                    (lambda (r)
                      (unless (eq r record)
                        (incf (slot-value (start-graphics-state r) 'cursor-x) dx)
                        (incf (slot-value (start-graphics-state r) 'cursor-y) dy)
                        (incf (slot-value (end-graphics-state r) 'cursor-x) dx)
                        (incf (slot-value (end-graphics-state r) 'cursor-y) dy))
                      (setf (output-record-dirty r) tag))
                    record
                    nil)
                   (setf (output-record-parent record) nil)
                   (add-output-record record (stream-current-output-record stream))
                   (set-medium-cursor-position (end-graphics-state record) stream)
                   (setf (parent-cache record) parent-cache)
                   (setf (output-record-displayer record) continuation)))))))
    record))

;;; The Franz user guide says that updating-output does &allow-other-keys, and
;;; some code I've encountered does mention other magical arguments, so we'll
;;; do the same. -- moore
(defun force-update-cache-test (a b)
  (declare (ignore a b))
  nil)

(defmacro updating-output
    ((stream
      &key (unique-id '*no-unique-id*) (id-test '#'eql)
      (cache-value ''no-cache-value cache-value-supplied-p)
      (cache-test '#'eql)
      (fixed-position nil fixed-position-p)
      (all-new nil all-new-p)
      (parent-cache nil parent-cache-p)
      (record-type ''standard-updating-output-record)
      &allow-other-keys)
     &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (unless cache-value-supplied-p
    (setq cache-test '#'force-update-cache-test))
  (let ((func (gensym "UPDATING-OUTPUT-CONTINUATION")))
    `(flet ((,func (,stream)
              (declare (ignorable ,stream))
              ,@body))
       (invoke-updating-output ,stream (function ,func) ,record-type ,unique-id
                               ,id-test ,cache-value ,cache-test
                               ,@ (and fixed-position-p
                                       `(:fixed-position ,fixed-position))
                               ,@(and all-new-p `(:all-new ,all-new))
                               ,@(and parent-cache-p
                                      `(:parent-cache ,parent-cache))))))

(defun redisplay (record stream &key (check-overlapping t))
  (redisplay-output-record record stream check-overlapping))

;;; Take the spec at its word that the x/y and parent-x/parent-y arguments are
;;; "entirely bogus."

(defvar *dump-updating-output* nil)

(defgeneric redisplay-output-record (record stream &optional check-overlapping)
  (:method ((record updating-output-record)
            (stream updating-output-stream-mixin)
            &optional (check-overlapping t))
    (let ((*current-updating-output* record)
          (current-graphics-state (medium-graphics-state stream)))
      (unwind-protect
           (progn
             (set-medium-cursor-position (start-graphics-state record) stream)
             (with-stream-redisplaying (stream)
               (compute-new-output-records record stream))
             (when *dump-updating-output*
               (dump-updating record :both *trace-output*))
             (multiple-value-bind
                   (erases moves draws erase-overlapping move-overlapping)
                 (compute-difference-set record check-overlapping)
               (when *trace-updating-output*
                 (let ((*print-pretty* t))
                   (format *trace-output*
                           "erases: ~S~%moves: ~S~%draws: ~S~%erase ~
                                    overlapping: ~S~%move overlapping: ~S~%"
                           erases moves draws
                           erase-overlapping move-overlapping)))
               (note-output-record-child-changed
                (output-record-parent record) record :change
                nil nil stream
                erases moves draws erase-overlapping move-overlapping
                :check-overlapping check-overlapping))
             (delete-stale-updating-output record))
        (set-medium-cursor-position current-graphics-state stream)))))

;;; Suppress the got-sheet/lost-sheet notices during redisplay.

(defmethod note-output-record-lost-sheet :around
    (record (sheet updating-output-stream-mixin))
  (declare (ignore record))
  (unless (stream-redisplaying-p sheet)
    (call-next-method)))

(defmethod note-output-record-got-sheet :around
    (record (sheet updating-output-stream-mixin))
  (declare (ignore record))
  (unless (stream-redisplaying-p sheet)
    (call-next-method)))

(defun delete-stale-updating-output (record)
  (map-over-updating-output
   #'(lambda (r)
       (when (eq (output-record-dirty r) :updating)
         (delete-from-map (parent-cache r)
                          (output-record-unique-id r)
                          (output-record-id-test r))))
   record
   t))

(defgeneric propagate-output-record-changes-p
    (record child mode old-position old-bounding-rectangle)
  (:method (record child mode old-position old-bounding-rectangle)
    nil))

(defgeneric propagate-output-record-changes
    (record child mode
     &optional old-position old-bounding-rectangle erases moves draws
       erase-overlapping move-overlapping check-overlapping)
  (:method (record child mode
            &optional old-position old-bounding-rectangle erases moves draws
              erase-overlapping move-overlapping check-overlapping)
    (declare (ignore record child mode
                     old-position old-bounding-rectangle erases moves draws
                     erase-overlapping move-overlapping check-overlapping))
    (error "This is a stub!")))

(defgeneric note-output-record-child-changed
    (record child mode old-position old-bounding-rectangle stream
     &optional erases moves draws erase-overlapping move-overlapping
     &key check-overlapping)
  (:method (record child mode old-position old-bounding-rectangle stream
            &optional erases moves draws erase-overlapping move-overlapping
            &key check-overlapping)
    (if (propagate-output-record-changes-p
         record child mode old-position old-bounding-rectangle)
        (propagate-output-record-changes
         record child mode old-position old-bounding-rectangle
         erases moves draws erase-overlapping move-overlapping
         check-overlapping)
        (incremental-redisplay
         stream nil erases moves draws erase-overlapping move-overlapping))))

;;; Support for explicitly changing output records.
;;; Example where the child of a :CLEAN output record may be moved:
#+ (or)
(formatting-item-list (stream)
  (formatting-cell (stream)
    (draw-rectangle* stream 0 0 *size* *size*))
  (updating-output (stream :cache-value t)
    (formatting-cell (stream)
      (draw-rectangle* stream 0 0 30 30 :ink +red+))))

(defun mark-updating-output-changed (record)
  (when (and (not (eq record *current-updating-output*))
             (eq (output-record-dirty record) :clean))
    (setf (output-record-dirty record) :updated)
    (let ((parent (parent-updating-output record)))
      (assert (not (null parent)) () "parent of ~S null." record)
      (mark-updating-output-changed parent))))

(defgeneric propagate-to-updating-output
    (record child mode old-bounding-rectangle)
  (:method ((record updating-output-record-mixin) child mode old-bbox)
    (when (and (eq mode :move)
               (eq (output-record-dirty record) :clean))
      (mark-updating-output-changed record)))
  (:method ((record output-record) child mode old-bbox)
    (when-let ((parent (output-record-parent record)))
      (propagate-to-updating-output parent child mode old-bbox))))

(defmethod* (setf output-record-position) :around
  (nx ny (record displayed-output-record))
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (multiple-value-prog1 (call-next-method)
      (unless (and (coordinate= x1 nx) (coordinate= y1 ny))
        (when-let* ((stream (and (slot-exists-p record 'stream)
                                 (slot-value  record 'stream)))
                    (parent (output-record-parent record)))
          (when (stream-redisplaying-p stream)
            (propagate-to-updating-output
             parent record :move (make-bounding-rectangle x1 y1 x2 y2))))))))

;;; Debugging hacks
(defun dump-updating (record old-records &optional (stream *standard-output*))
  (let ((*print-circle* t)
        (*print-pretty* t))
    (fresh-line stream)
    (dump-updating-aux record old-records stream)))

(defgeneric dump-updating-aux (record old-records stream)
  (:method ((record standard-updating-output-record) old-records stream)
    (pprint-logical-block (stream nil)
      (print-unreadable-object (record stream :type t)
        (let ((old-printed nil))
          (format stream "~S " (output-record-dirty record))
          (pprint-indent :block 2 stream)
          (pprint-newline :linear stream)
          (when (and (or (eq old-records :old)
                         (eq old-records :both))
                     (slot-boundp record 'old-children))
            (format stream ":old ~@_")
            (dump-updating-aux (old-children record) old-records stream)
            (setq old-printed t))
          (when (or (eq old-records :new)
                    (eq old-records :both)
                    (not old-records))
            (when old-printed
              (pprint-newline :linear stream))
            (format stream ":new ~@_")
            (dump-updating-aux (sub-record record) old-records stream))))))
  (:method ((record compound-output-record) old-records stream)
    (pprint-logical-block (stream nil)
      (print-unreadable-object (record stream :type t)
        (write-char #\Space stream)
        (pprint-newline :linear stream)
        (pprint-indent :block 2 stream)
        (pprint-logical-block (stream nil :prefix "#(" :suffix ")")
          (loop with children = (output-record-children record)
                for i from 1 below (length children)
                for child across children
                do (progn
                     (pprint-pop)
                     (dump-updating-aux child old-records stream)
                     (write-char #\Space stream)
                     (pprint-newline :fill stream))
                finally (when (> (length children) 0)
                          (pprint-pop)
                          (dump-updating-aux (elt children (1- i))
                                             old-records
                                             stream)))))))
  (:method (record old-records stream)
    (declare (ignore old-records))
    (write record :stream stream)))

(defmethod redisplay-frame-pane
    ((frame application-frame) (pane updating-output-stream-mixin) &key force-p)
  (setf (id-counter pane) 0)
  (let ((incremental-redisplay (pane-incremental-redisplay pane)))
    (cond ((not incremental-redisplay)
           (call-next-method))
          ((or (null (updating-record pane))
               force-p)
           (setf (updating-record pane)
                 (updating-output (pane :unique-id 'top-level)
                   (call-next-method frame pane :force-p force-p))))
          ;; Implements the extension to the :incremental-redisplay
          ;; pane argument found in the Franz User Guide.
          (t (let ((record (updating-record pane)))
               (if (consp incremental-redisplay)
                   (apply #'redisplay record pane incremental-redisplay)
                   (redisplay record pane))) ))))
