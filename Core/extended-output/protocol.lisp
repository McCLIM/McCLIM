(in-package #:clim-internals)

;;;; Part V: Extended Stream Output Facilities

;; CLIM Specification says that E-O-S is a subclass of OUTPUT-STREAM,
;; but it does not say what is it. We infer it is a base class for
;; all CLIM output streams (output-recording-stream included).
(defclass output-stream (fundamental-character-output-stream) ())

;;; 15.2 Extended Output Streams
(define-protocol-class extended-output-stream
    (output-stream)
  ())

;;; :foreground :background :text-style :vertical-spacing :text-margin
;;; :end-of-line-action :end-of-page-action :default-view

;;; 15.3 The Text Cursor
(define-protocol-class cursor ())

;;; 15.3.1 Text Cursor Protocol [complete]

;; cursor [protocol class]
;; cursorp object [protocol predicate]
;; :sheet [Initarg for cursor]
;; standard-text-cursor [class]
(defgeneric cursor-sheet (cursor))
(defgeneric cursor-position (cursor))
(defgeneric* (setf cursor-position) (x y cursor))
(defgeneric cursor-active (cursor))
(defgeneric (setf cursor-active) (value cursor))
(defgeneric cursor-state (cursor))
(defgeneric (setf cursor-state) (value cursor))
(defgeneric cursor-focus (cursor))
(defgeneric cursor-visibility (cursor))
(defgeneric (setf cursor-visibility) (visibility cursor))

;;; 15.3.2 Stream Text Cursor Protocol [complete]

(defgeneric stream-text-cursor (stream))
(defgeneric (setf stream-text-cursor) (cursor stream))
(defgeneric stream-cursor-position (stream))
(defgeneric* (setf stream-cursor-position) (x y stream))
(defgeneric stream-set-cursor-position (stream x y)) ; This is actually in 19.3.1 in CLIM 2.2
(defgeneric stream-increment-cursor-position (stream dx dy))

;;; 15.4 Text Protocol [complete]

(defgeneric stream-character-width (stream character &key text-style))
(defgeneric stream-string-width (stream character &key start end text-style))
(defgeneric stream-text-margin (stream))
(defgeneric (setf stream-text-margin) (margin stream))
(defgeneric stream-line-height (stream &key text-style))
(defgeneric stream-line-width (stream)
  (:documentation "McCLIM extension which returns a space between left and right margin for text output."))
(defgeneric stream-vertical-spacing (stream))
(defgeneric stream-baseline (stream))
(defgeneric stream-text-style (stream)
  (:documentation "McCLIM extension returning a default text style of the stream."))


;;; 15.4.1 Mixing Text and Graphics [complete]

(declmacro with-room-for-graphics
           ((&optional stream &key (first-quadrant t) height (move-cursor t) record-type)
            &body body))

(defgeneric invoke-with-room-for-graphics
    (cont stream &key first-quadrant height move-cursor record-type))

(defgeneric output-record-baseline (record))

;;; 15.4.2 Wrapping of Text Lines [complete]

(defgeneric stream-end-of-line-action (stream))
(defgeneric (setf stream-end-of-line-action) (action stream))
;; with-end-of-line-action (stream action) &body body [Macro]
(defgeneric stream-end-of-page-action (stream))
(defgeneric (setf stream-end-of-page-action) (action stream))
;; with-end-of-page-action (stream action) &body body [Macro]

;;; 16.2 Output Records
(define-protocol-class output-record (bounding-rectangle)
  ())

(define-protocol-class displayed-output-record (output-record)
  ())

;;; 16.2.1. The Basic Output Record Protocol
(defgeneric output-record-position (record)
  (:documentation
   "Returns the x and y position of RECORD. The position is the
position of the upper-left corner of its bounding rectangle. The
position is relative to the stream, where (0,0) is (initially) the
upper-left corner of the stream."))

(defgeneric* (setf output-record-position) (x y record)
  (:documentation
   "Changes the x and y position of the RECORD to be X and Y, and
updates the bounding rectangle to reflect the new position (and saved
cursor positions, if the output record stores it). If RECORD has any
children, all of the children (and their descendants as well) will be
moved by the same amount as RECORD was moved. The bounding rectangles
of all of RECORD's ancestors will also be updated to be large enough
to contain RECORD."))

(defgeneric output-record-start-cursor-position (record)
  (:documentation
   "Returns the x and y starting cursor position of RECORD. The
positions are relative to the stream, where (0,0) is (initially) the
upper-left corner of the stream."))

(defgeneric* (setf output-record-start-cursor-position) (x y record))

(defgeneric output-record-end-cursor-position (record)
  (:documentation
   "Returns the x and y ending cursor position of RECORD. The
positions are relative to the stream, where (0,0) is (initially) the
upper-left corner of the stream."))

(defgeneric* (setf output-record-end-cursor-position) (x y record))

(defgeneric output-record-parent (record)
  (:documentation
   "Returns the output record that is the parent of RECORD, or NIL if
RECORD has no parent."))

(defgeneric replay-output-record (record stream
                                  &optional region x-offset y-offset)
  (:documentation "Displays the output captured by RECORD on the
STREAM, exactly as it was originally captured. The current user
transformation, line style, text style, ink and clipping region of
STREAM are all ignored. Instead, these are gotten from the output
record.

Only those records that overlap REGION are displayed."))

(defgeneric output-record-hit-detection-rectangle* (record))

(defgeneric output-record-refined-position-test (record x y))

(defgeneric highlight-output-record (record stream state))

(defgeneric displayed-output-record-ink (displayed-output-record))

;;; 16.2.1. The Basic Output Record Protocol (extras)

(defgeneric (setf output-record-parent) (parent record)
  (:documentation "Additional protocol generic function. PARENT may be
an output record or NIL."))

;;; 16.2.2. Output Record "Database" Protocol

(defgeneric output-record-children (record))

(defgeneric add-output-record (child record)
  (:documentation "Sets RECORD to be the parent of CHILD."))

(defgeneric delete-output-record (child record &optional errorp)
  (:documentation "If CHILD is a child of RECORD, sets the parent of
CHILD to NIL."))

(defgeneric clear-output-record (record)
  (:documentation "Sets the parent of all children of RECORD to NIL."))

(defgeneric output-record-count (record))

(defgeneric map-over-output-records-containing-position
  (function record x y &optional x-offset y-offset &rest function-args)
  (:documentation "Maps over all of the children of RECORD that
contain the point at (X,Y), calling FUNCTION on each one. FUNCTION is
a function of one or more arguments, the first argument being the
record containing the point. FUNCTION is also called with all of
FUNCTION-ARGS as APPLY arguments.

If there are multiple records that contain the point,
MAP-OVER-OUTPUT-RECORDS-CONTAINING-POSITION hits the most recently
inserted record first and the least recently inserted record
last. Otherwise, the order in which the records are traversed is
unspecified."))

(defgeneric map-over-output-records-overlapping-region
  (function record region &optional x-offset y-offset &rest function-args)
  (:documentation "Maps over all of the children of the RECORD that
overlap the REGION, calling FUNCTION on each one. FUNCTION is a
function of one or more arguments, the first argument being the record
overlapping the region. FUNCTION is also called with all of
FUNCTION-ARGS as APPLY arguments.

If there are multiple records that overlap the region and that overlap
each other, MAP-OVER-OUTPUT-RECORDS-OVERLAPPING-REGION hits the least
recently inserted record first and the most recently inserted record
last. Otherwise, the order in which the records are traversed is
unspecified. "))

;;; 16.2.2. Output Record "Database" Protocol (extras)
;;;
;;; From the Franz CLIM user's guide but not in the spec... clearly
;;; necessary.

(defgeneric map-over-output-records-1 (continuation record continuation-args))

(defun map-over-output-records
    (function record &optional (x-offset 0) (y-offset 0) &rest function-args)
  "Call FUNCTION on each of the children of RECORD.
FUNCTION is a function of one or more arguments and called with all of
FUNCTION-ARGS as APPLY arguments."
  (declare (ignore x-offset y-offset))
  (map-over-output-records-1 function record function-args))

;;; These two aren't in the spec, but are needed to make indirect
;;; adding/deleting of GADGET-OUTPUT-RECORDs work:

(defgeneric note-output-record-lost-sheet (record sheet))
(defgeneric note-output-record-got-sheet  (record sheet))


;;; 16.2.3. Output Record Change Notification Protocol

(defgeneric recompute-extent-for-new-child (record child))

(defgeneric recompute-extent-for-changed-child
  (record child old-min-x old-min-y old-max-x old-max-y))

(defgeneric tree-recompute-extent (record))

;;; 16.3.2 Graphics Displayed Output Records
(define-protocol-class graphics-displayed-output-record
    (displayed-output-record)
  ())

;;; 16.3.3 Text Displayed Output Record
(define-protocol-class text-displayed-output-record (displayed-output-record)
  ())

;;; 16.3.3 Text Displayed Output Record

(defgeneric add-character-output-to-text-record
  (text-record character text-style width height baseline))

(defgeneric add-string-output-to-text-record
  (text-record string start end text-style width height baseline))

(defgeneric text-displayed-output-record-string (text-record))

;;; 16.4 Output Recording Streams
(define-protocol-class output-recording-stream (output-stream)
  ())

;;; 16.4.1. The Output Recording Stream Protocol

(defgeneric stream-recording-p (stream))
(defgeneric (setf stream-recording-p) (recording-p stream))
(defgeneric stream-drawing-p (stream))
(defgeneric (setf stream-drawing-p) (drawing-p stream))
(defgeneric stream-output-history (stream))
(defgeneric stream-current-output-record (stream))
(defgeneric (setf stream-current-output-record) (record stream))
(defgeneric stream-add-output-record (stream record))
(defgeneric stream-replay (stream &optional region))
(defgeneric erase-output-record (record stream &optional errorp))

;;; 16.4.3. Text Output Recording
(defgeneric stream-text-output-record (stream text-style))
(defgeneric stream-close-text-output-record (stream))
(defgeneric stream-add-character-output
  (stream character text-style width height baseline))
(defgeneric stream-add-string-output
  (stream string start end text-style width height baseline))

;;; 16.4.4 Output Recording Utilities [complete]

;; with-output-recording-options (stream &key record draw) &body body [Macro]
(defgeneric invoke-with-output-recording-options
    (stream continuation record draw))

;;; with-new-output-record (stream &optional record-type record &rest initargs) &body body [Macro]
(defgeneric invoke-with-new-output-record
    (stream continuation record-type &rest initargs &key parent &allow-other-keys))

;;; with-output-to-output-record (stream &optional record-type record &rest initargs)) &body body [Macro]
(defgeneric invoke-with-output-to-output-record
    (stream continuation record-type &rest initargs))

(defgeneric make-design-from-output-record (record))

;;; 17.3.1 Table Formatting Protocol
(define-protocol-class table-output-record (output-record))

;;; 17.3.2 Row and Column Formatting Protocol
(define-protocol-class row-output-record (output-record))
(define-protocol-class column-output-record (output-record))

;;; 17.3.3 Cell Formatting Protocol
(define-protocol-class cell-output-record (output-record))

;;; 17.3.4 Item List Formatting Protocol
(define-protocol-class item-list-output-record ()
  ())

;;;; 17.3 The Table and Item List Formatting Protocols

(defgeneric map-over-table-elements (function table-record type)
  (:documentation "Applies FUNCTION to all the rows or columns of
TABLE-RECORD that are of type TYPE. TYPE is one of :ROW, :COLUMN or
:ROW-OR-COLUMN. FUNCTION is a function of one argument. The function
skips intervening non-table output record structures."))

(defgeneric map-over-block-cells (function block)
  (:documentation "Applies the FUNCTION to all cells in the BLOCK."))

(defgeneric map-over-row-cells (function row-record)
  (:documentation "Applies FUNCTION to all the cells in the row
ROW-RECORD, skipping intervening non-table output record structures.
FUNCTION is a function of one argument, an output record corresponding
to a table cell within the row."))

(defgeneric map-over-column-cells (function column-record)
  (:documentation "Applies FUNCTION to all the cells in the column
COLUMN-RECORD, skipping intervening non-table output record
structures. FUNCTION is a function of one argument, an output record
corresponding to a table cell within the column."))

(defgeneric map-over-item-list-cells (function item-list-record))

(defgeneric adjust-table-cells (table-record stream))
(defgeneric adjust-multiple-columns (table-record stream))
(defgeneric adjust-item-list-cells (item-list-record stream))

;;; 18.2 The Graph Formatting Protocol
(define-protocol-class graph-output-record (output-record))
(define-protocol-class graph-node-output-record (output-record))

;;; 21.3 Incremental Redisplay Protocol
(define-protocol-class updating-output-record (output-record))

;;;; 21.2
(defgeneric invoke-updating-output
    (stream continuation record-type unique-id id-test cache-value cache-test
     &key fixed-position all-new parent-cache))

(declfun redisplay (record stream &key (check-overlapping t)))

(defgeneric redisplay-output-record (record stream &optional check-overlapping))

;;; 21.3 Incremental Redisplay Protocol.

(defgeneric output-record-unique-id (record))
(defgeneric output-record-cache-value (record))
(defgeneric output-record-fixed-position (record))
(defgeneric output-record-displayer (record))
(defgeneric compute-new-output-records (record stream))
(defgeneric compute-difference-set
    (record &optional check-overlapping))
(defgeneric augment-draw-set (record difference-set))
(defgeneric note-output-record-child-changed
    (record child mode old-position old-bounding-rectangle stream
     &key difference-set check-overlapping))

(defgeneric propagate-output-record-changes-p
    (record child mode old-position old-bounding-rectangle))

(defgeneric propagate-output-record-changes
    (record child mode
     &optional old-position old-bounding-rectangle
       difference-set check-overlapping))

(defgeneric match-output-records (record &rest args))

;;; The following operators are not implemented. -- jd 2021-12-08
(defgeneric find-child-output-record
    (record use-old-elements record-type &key unique-id unique-id-test))
(defgeneric output-record-contents-ok (record))
(defgeneric recompute-contents-ok (record))
(defgeneric cache-output-record (record child unique-id))
(defgeneric decache-output-record (record child use-old-elements))
(defgeneric find-cached-output-record (record use-old-elements record-type
                                       &key unique-id unique-id-test &allow-other-keys ))

;;; 21.4 Incremental Redisplay Stream Protocol
(defgeneric redisplayable-stream-p (stream)
  (:method (stream) nil))

(defgeneric stream-redisplaying-p (stream)
  (:method (stream) nil))

(defgeneric incremental-redisplay
    (stream position erases moves draws erase-overlapping move-overlapping))

;;; 23.6 Views
(define-protocol-class view ())
(defgeneric stream-default-view (stream))
