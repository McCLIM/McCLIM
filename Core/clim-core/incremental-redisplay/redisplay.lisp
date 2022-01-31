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
;;; This file contains the implementation of the redisplay including
;;; computation of the difference set and the macro UPDATING-OUTPUT.
;;;
(in-package #:clim-internals)

;;; Incremental Redisplay Theory of Operation
;;;
;;; Incremental redisplay compares the tree of output records before and after
;;; calling REDISPLAY and updates those parts of the screen that are different.
;;; UPDATING-OUTPUT forms in the code create UPDATING-OUTPUT-RECORDs in the record
;;; tree. These records hold the before and after snapshots of the tree.  When the
;;; display code is first run, the bodies of all the UPDATING-OUTPUT forms are
;;; captured as closures.  Usually only the closure in the top-level output record
;;; will ever get called, but the programmer can call REDISPLAY on any updating
;;; output record, so we have to be prepared for that.
;;;
;;; Redisplay proceeds thus:
;;;
;;; All the updating output records are visited. Their state is changed to
;;; :UPDATING and the OLD-CHILDREN slot is set to the current children.
;;;
;;; The closure of the root updating output record is called. None of the closures
;;; the child updating output records are called because any free variables
;;; captured in the UPDATING-OUTPUT forms need to see the fresh bindings from this
;;; run of the code. As UPDATING-OUTPUT forms are encountered, several things can
;;; happen:
;;;
;;; * The cache value of the form compares to the value stored in the record. The
;;; record, and all the updating output records below it, are marked :clean. The
;;; body of UPDATING-OUTPUT isn't run.
;;;
;;; * The cache value doesn't compare. The record is marked :UPDATED, and the body
;;; is run.
;;;
;;; * There isn't an existing UPDATING-OUTPUT-RECORD for this UPDATING-OUTPUT
;;; form. One is created in state :UPDATED. The body is run.
;;;
;;; Before the top level UPDATING-OUTPUT closure returns, various output records
;;; in the history tree might be mutated e.g., be moved. The most common case of
;;; this is in table layout, where the records for each cell are first created and
;;; then assigned a final location based on the dimensions of the table. But these
;;; nodes may be children of an updating output record that has been marked
;;; :CLEAN. Therefore, they have to be treated specially so that the rest of
;;; incremental redisplay will consider them and not leave the screen in a trashed
;;; state. An around method on (SETF OUTPUT-RECORD-POSITION) for display records
;;; checks if incremental redisplay is in progress; if so, it stores the mutated
;;; record in its closest parent UPDATING-OUTPUT record (if any). If that parent
;;; is :CLEAN then it and any other clean parent updating output records are
;;; marked as :UPDATED.
;;;
;;; Next, COMPUTE-DIFFERENCE-SET compares the old and new trees. Output records
;;; may be removed, added or moved. Their region union must be erased and then
;;; replayed from the history to ensure the correctness. COMPUTE-DIFFERENCE-SET
;;; compares all display output records that are the record descendants.
;;;
;;; Finally, the old tree is walked. All updating output records in state
;;; :UPDATING were not visited at all and thus are deleted from their parent
;;; caches.
;;;
;;;
;;; Problems / Future work
;;;
;;; The complete traversals of the output history tree could be avoided by
;;; keeping a generation number in the updating output record and updating
;;; that everytime the node is visited.
;;;
;;; The search for equal display nodes is expensive in part because we have no
;;; spatially organized data structure.

(defvar *current-updating-output* nil)

(defmethod compute-new-output-records ((record standard-updating-output-record) stream)
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
                                    (output-record-displayer record)))))

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

(defmethod compute-difference-set ((record standard-updating-output-record)
                                   &optional (check-overlapping t))
  (let ((old-table (make-hash-table :test #'equalp))
        (new-table (make-hash-table :test #'equalp))
        (all-table (make-hash-table)))
    (collect (old-records new-records)
      (flet ((collect-1 (record set)
               (setf (gethash record all-table) set)
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
        (flet ((add-record (rec set)
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
                               ;; Siblings might have been reordered so we
                               ;; need to "move it" in place.
                               ;; Don't add the same output record twice  v
                               (t (when (and check-overlapping (eq set :new))
                                    (moves entry)))))))))
          (maphash #'add-record all-table))
        (if (null check-overlapping)
            (list (erases) (moves) (draws)      nil    nil)
            (list      nil     nil (draws) (erases) (moves)))))))

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
             (setf (start-graphics-state record) (medium-graphics-state stream))
             (%invoke-updating record stream continuation)
             (setf (end-graphics-state record) (medium-graphics-state stream))
             (add-to-map parent-cache record  unique-id id-test all-new)))
          ((or (not (state-matches-stream-p record stream))
               (not (funcall cache-test cache-value (output-record-cache-value record))))
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

(defun redisplay (record stream &key (check-overlapping t))
  (redisplay-output-record record stream check-overlapping))

(defmethod redisplay-output-record ((record updating-output-record)
                                    (stream updating-output-stream-mixin)
                                    &optional (check-overlapping t))
  (let ((*current-updating-output* record)
        (current-graphics-state (medium-graphics-state stream)))
    (unwind-protect
         (progn
           (set-medium-cursor-position (start-graphics-state record) stream)
           (with-stream-redisplaying (stream)
             (compute-new-output-records record stream))
           (let ((difference-set (compute-difference-set record check-overlapping)))
             (note-output-record-child-changed
              (output-record-parent record) record :change
              nil (old-bounds record) stream
              :difference-set difference-set
              :check-overlapping check-overlapping))
           (delete-stale-updating-output record))
      (set-medium-cursor-position current-graphics-state stream))))

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
