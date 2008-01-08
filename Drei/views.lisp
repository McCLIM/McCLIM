;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2007 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.
;;;
;;; This file contains definitions for Dreis "views", objects that are
;;; used as the actual content of a Drei instance. Drei displays a
;;; view of something, most commonly a buffer, and this view imposes
;;; its own rules on how it sees the buffer contents, the typical
;;; example being parsing it and displaying it with syntax
;;; highlighting. The special buffer classes used by views are also
;;; defined in this file.

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions.

(define-condition user-condition-mixin ()
  ()
  (:documentation "Conditions of this type are caught by the Drei
command loop and their report displayed to the user in the
minibuffer, instead of being propagated further (and possibly
invoking the debugger)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tabify

(defvar *use-tabs-for-indentation* nil
  "If non-NIL, use tabs when indenting lines. Otherwise, use spaces.")

(defclass tabify-mixin ()
  ((%tab-space-count :initform 8
                     :accessor tab-space-count
                     :initarg :tab-space-count)
   ;; We save the old values for performance. Doesn't take text-style
   ;; into account (for performance!)
   (%space-width :accessor recorded-space-width
                 :initform nil)
   (%tab-width :accessor recorded-tab-width
               :initform nil)
   (%recorded-stream :accessor recorded-stream
                     :initform nil)
   (%use-tabs :accessor use-tabs
              :initform *use-tabs-for-indentation*
              :initarg :use-tabs)))

(defun maybe-update-recordings (stream tabify)
  (with-accessors ((space-width recorded-space-width)
                   (tab-width recorded-tab-width)
                   (recorded-stream recorded-stream)) tabify
    (unless (eq stream recorded-stream)
      ;; Update the recorded values.
      (setf space-width (stream-character-width stream #\Space)
            tab-width (stream-character-width stream #\Tab)
            recorded-stream stream))))

(defgeneric space-width (stream tabify)
  (:documentation "Return the width of a space character on
`stream' in device units (most likely pixels).")
  (:method ((stream extended-output-stream) (tabify tabify-mixin))
    (maybe-update-recordings stream tabify)
    (recorded-space-width tabify)))

(defgeneric tab-width (stream tabify)
  (:documentation "Return the width of a tab character on
`stream' in device units (most likely pixels).")
  (:method ((stream extended-output-stream) (tabify tabify-mixin))
    (if (tab-space-count tabify)
        (* (tab-space-count tabify) (space-width stream tabify))
        (recorded-tab-width tabify))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo

(defgeneric undo-tree (buffer)
  (:documentation "The undo-tree object associated with the
buffer. This usually contains a record of every change that has
been made to the buffer since it was created."))

(defgeneric undo-accumulate (buffer)
  (:documentation "A list of the changes that have been made to
`buffer' since the last time undo was added to the undo tree for
the buffer. The list returned by this function is initially
NIL (the empty list). The :before methods on
`insert-buffer-object', `insert-buffer-sequence', and
`delete-buffer-range' push undo records on to this list."))

(defgeneric performing-undo (buffer)
  (:documentation "If true, the buffer is currently performing an
undo operation. The :before methods on `insert-buffer-object',
`insert-buffer-sequence', and `delete-buffer-range' push undo
records onto the undo accumulator only if `performing-undo' is
false, so that no undo information is added as a result of an
undo operation."))

(defclass undo-mixin ()
  ((tree :initform (make-instance 'standard-undo-tree)
         :reader undo-tree
         :documentation "Returns the undo-tree of the buffer.")
   (undo-accumulate :initform '()
                    :accessor undo-accumulate
                    :documentation "The undo records created
since the start of the undo context.")
   (performing-undo :initform nil
                    :accessor performing-undo
                    :documentation "True if we are currently
performing undo, false otherwise."))
  (:documentation "This is a mixin class that buffer classes can
inherit from. It contains an undo tree, an undo accumulator and a
flag specifyng whether or not it is currently performing
undo. The undo tree and undo accumulators are initially empty."))

(defclass drei-undo-record (standard-undo-record)
  ((buffer :initarg :buffer
           :documentation "The buffer to which the record
belongs."))
  (:documentation "A base class for all output records in
Drei."))

(defclass simple-undo-record (drei-undo-record)
  ((offset :initarg :offset
           :reader undo-offset
           :documentation "The offset that determines the
position at which the undo operation is to be executed."))
  (:documentation "A base class for output records that modify
buffer contents at a specific offset."))

(defclass insert-record (simple-undo-record)
 ((objects :initarg :objects
            :documentation "The sequence of objects that are to
be inserted whenever flip-undo-record is called on an instance of
insert-record."))
  (:documentation "Whenever objects are deleted, the sequence of
objects is stored in an insert record containing a mark."))

(defclass delete-record (simple-undo-record)
  ((length :initarg :length
           :documentation "The length of the sequence of objects
to be deleted whenever `flip-undo-record' is called on an
instance of `delete-record'."))
  (:documentation "Whenever objects are inserted, a
`delete-record' containing a mark is created and added to the
undo tree."))

(defclass compound-record (drei-undo-record)
  ((records :initform '()
            :initarg :records
            :documentation "The undo records contained by this
compound record."))
  (:documentation "This record simply contains a list of other
records."))

(defmethod print-object  ((object delete-record) stream)
  (with-slots (offset length) object
    (format stream "[offset: ~a length: ~a]" offset length)))

(defmethod print-object  ((object insert-record) stream)
  (with-slots (offset objects) object
    (format stream "[offset: ~a objects: ~a]" offset objects)))

(defmethod print-object  ((object compound-record) stream)
  (with-slots (records) object
    (format stream "[records: ~a]" records)))

(defmethod insert-buffer-object :before ((buffer undo-mixin) offset object)
  (declare (ignore object))
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
                         :buffer buffer :offset offset :length 1)
	  (undo-accumulate buffer))))

(defmethod insert-buffer-sequence :before ((buffer undo-mixin) offset sequence)
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
                         :buffer buffer :offset offset :length (length sequence))
	  (undo-accumulate buffer))))

(defmethod delete-buffer-range :before ((buffer undo-mixin) offset n)
  (unless (performing-undo buffer)
    (push (make-instance 'insert-record
                         :buffer buffer :offset offset
                         :objects (buffer-sequence buffer offset (+ offset n)))
	  (undo-accumulate buffer))))

(defmacro with-undo ((get-buffers-exp) &body body)
  "This macro executes the forms of `body', registering changes
made to the list of buffers retrieved by evaluating
`get-buffers-exp'. When `body' has run, for each buffer it will
call `add-undo' with an undo record and the undo tree of the
buffer.  If the changes done by `body' to the buffer has resulted
in only a single undo record, it is passed as is to `add-undo'.
If it contains several undo records, a compound undo record is
constructed out of the list and passed to `add-undo'.  Finally,
if the buffer has no undo records, `add-undo' is not called at
all."
  (with-gensyms (buffer)
    `(progn
       (dolist (,buffer ,get-buffers-exp)
         (setf (undo-accumulate ,buffer) '()))
       (unwind-protect (progn ,@body)
         (dolist (,buffer ,get-buffers-exp)
           (cond ((null (undo-accumulate ,buffer)) nil)
                 ((null (cdr (undo-accumulate ,buffer)))
                  (add-undo (car (undo-accumulate ,buffer))
                            (undo-tree ,buffer)))
                 (t
                  (add-undo (make-instance 'compound-record
                                           :buffer ,buffer
                                           :records (undo-accumulate ,buffer))
                            (undo-tree ,buffer)))))))))

(defmethod flip-undo-record :around ((record drei-undo-record))
  (with-slots (buffer) record
    (let ((performing-undo (performing-undo buffer)))
      (setf (performing-undo buffer) t)
      (unwind-protect (call-next-method)
        (setf (performing-undo buffer) performing-undo)))))

(defmethod flip-undo-record ((record insert-record))
  (with-slots (buffer offset objects) record
    (change-class record 'delete-record
                  :length (length objects))
    (insert-buffer-sequence buffer offset objects)))

(defmethod flip-undo-record ((record delete-record))
  (with-slots (buffer offset length) record
    (change-class record 'insert-record
                  :objects (buffer-sequence buffer offset (+ offset length)))
    (delete-buffer-range buffer offset length)))

(defmethod flip-undo-record ((record compound-record))
  (with-slots (records) record
    (mapc #'flip-undo-record records)
    (setf records (nreverse records))))

(defgeneric clear-undo-history (undo-maintainer)
  (:documentation "Clear the undo history for `undo-maintainer',
preventing the undoing to before the state of whatever
`undo-maintainer' is maintaining undo for."))

(defmethod clear-undo-history ((undo-maintainer undo-mixin))
  (setf (slot-value undo-maintainer 'tree)
        (make-instance 'standard-undo-tree)))

;;; undo-mixin delegation (here because of the package)

(defmethod undo-tree ((buffer delegating-buffer))
  (undo-tree (implementation buffer)))

(defmethod undo-accumulate ((buffer delegating-buffer))
  (undo-accumulate (implementation buffer)))

(defmethod (setf undo-accumulate) (object (buffer delegating-buffer))
  (setf (undo-accumulate (implementation buffer)) object))

(defmethod performing-undo ((buffer delegating-buffer))
  (performing-undo (implementation buffer)))

(defmethod (setf performing-undo) (object (buffer delegating-buffer))
  (setf (performing-undo (implementation buffer)) object))

(defmethod clear-undo-history ((buffer delegating-buffer))
  (clear-undo-history (implementation buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readonly

(defclass read-only-mixin ()
  ((read-only-p :initform nil :accessor read-only-p)))

(define-condition buffer-read-only (user-condition-mixin simple-error)
  ((buffer :reader condition-buffer :initarg :buffer))
  (:report (lambda (condition stream)
	     (format stream "Attempt to change read only buffer: ~a"
		     (condition-buffer condition))))
  (:documentation "This condition is signalled whenever an attempt
is made to alter a buffer which has been set read only."))

(defmethod insert-buffer-object ((buffer read-only-mixin) offset object)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod insert-buffer-sequence ((buffer read-only-mixin) offset sequence)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod delete-buffer-range ((buffer read-only-mixin) offset n)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod (setf buffer-object) (object (buffer read-only-mixin) offset)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod read-only-p ((buffer delegating-buffer))
  (read-only-p (implementation buffer)))

(defmethod (setf read-only-p) (flag (buffer delegating-buffer))
  (setf (read-only-p (implementation buffer)) flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Single-line buffer.

(defclass single-line-mixin ()
  ((%single-line-p :initform nil
                   :accessor single-line-p
                   :initarg :single-line))
  (:documentation "Prevent the insertion of #\Newline characters
into the buffer if `single-line-p' is true."))

(define-condition buffer-single-line (user-condition-mixin simple-error)
  ((buffer :reader condition-buffer :initarg :buffer))
  (:report "Attempt to insert newline into single-line buffer.")
  (:documentation "This condition is signalled whenever an
attempt is made to insert a #\Newline character into a
single-line buffer."))

(defmethod insert-buffer-object :before ((buffer single-line-mixin) offset (object (eql #\Newline)))
  (when (single-line-p buffer)
    (error 'buffer-single-line :buffer buffer)))

(defmethod insert-buffer-sequence :before ((buffer single-line-mixin) offset sequence)
  (when (and (single-line-p buffer)
             (find #\Newline sequence))
    (error 'buffer-single-line :buffer buffer)))

(defmethod (setf buffer-object) :before ((object (eql #\Newline)) (buffer single-line-mixin) offset)
  (when (single-line-p buffer)
    (error 'buffer-single-line :buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Drei buffer.

(defclass extended-standard-buffer (single-line-mixin
                                    read-only-mixin standard-buffer
                                    undo-mixin abbrev-mixin
                                    observable-buffer-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass extended-binseq2-buffer (single-line-mixin
                                   read-only-mixin binseq2-buffer
                                   p-undo-mixin abbrev-mixin
                                   observable-buffer-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass drei-buffer (delegating-buffer esa-buffer-mixin
                                         observable-buffer-mixin)
  ((point :initarg :point :initform nil :accessor point-of))
  (:default-initargs :implementation (make-instance 'extended-standard-buffer)))

(defmethod initialize-instance :after ((buffer drei-buffer) &rest args
                                       &key initial-contents)
  (declare (ignore args))
  (with-accessors ((point point)) buffer
    (when initial-contents
      (check-type initial-contents array)
      (insert-buffer-sequence buffer 0 initial-contents))
    (setf point (make-buffer-mark buffer 0 :right))
    ;; Hack: we need to be told whenever the undo facilities in the
    ;; implementation buffer changes the buffer contents.
    (add-observer (implementation buffer) buffer)))

(defmethod observer-notified ((observer drei-buffer)
                              (observable observable-buffer-mixin)
                              data)
  (notify-observers observer (constantly data)))

(defmethod notify-observers :after ((buffer drei-buffer)
                                    &optional data-fn)
  (declare (ignore data-fn))
  ;; This means that any buffer modification sets the needs-saving
  ;; flag to true. It might be nice if undo back to the last saved
  ;; state would set it to false.
  (setf (needs-saving buffer) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View classes.

(defclass drei-view (tabify-mixin subscriptable-name-mixin modual-mixin)
  ((%active :accessor active
            :initform t
            :initarg :active
            :type boolean
            :documentation "A boolean value indicating whether
the view is \"active\". This should control highlighting when
redisplaying.")
   (%modified-p :accessor modified-p
                :initform nil
                :initarg :modified-p
                :documentation "This value is true if the view
contents have been modified since the last time this value was
set to false.")
   (%no-cursors :accessor no-cursors
                :initarg :no-cursors
                :initform nil
                :documentation "True if the view does not display
cursors.")
   (%full-redisplay-p :accessor full-redisplay-p
                      :initform nil
                      :documentation "True if the view should be
fully redisplayed the next time it is redisplayed.")
   (%use-editor-commands :accessor use-editor-commands-p
                         :initarg :use-editor-commands
                         :initform nil
                         :documentation "If the view is supposed
to support standard editor commands (for inserting objects,
moving cursor, etc), this will be true. If you want your view to
support standard editor commands, you should *not* inherit from
`editor-table' - the command tables containing the editor
commands will be added automatically, as long as this value is
true."))
  (:documentation "The base class for all Drei views. A view
observes some other object and provides a visual representation
for Drei.")
  (:default-initargs :name "*scratch*"))

(defmethod print-object ((view drei-view) stream)
  (print-unreadable-object (view stream :type t :identity t)
    (format stream "name: ~a ~a" (name view) (subscript view))))

(defmethod available-modes append ((modual drei-view))
  *global-modes*)

(defmethod mode-applicable-p or ((modual drei-view) mode-name)
  (mode-applicable-p (syntax modual) mode-name))

(defgeneric synchronize-view (view &key &allow-other-keys)
  (:documentation "Synchronize the view with the object under
observation - what exactly this entails, and what keyword
arguments are supported, is up to the individual view
subclass.")
  (:method ((view drei-view) &key)
    nil))

(defgeneric view-command-tables (view)
  (:documentation "Return a list of command tables containing
commands relevant for `view'.")
  (:method-combination append)
  (:method append ((view drei-view))
    '()))

(defgeneric create-view-cursors (output-stream view)
  (:documentation "Create cursors for `view' that are to be
displayed on `output-stream'.")
  (:method nconc (output-stream (view drei-view))
    '())
  (:method :around (output-stream (view drei-view))
           (unless (no-cursors view)
             (call-next-method)))
  (:method-combination nconc))

(defgeneric clone-view (view &rest initargs)
  (:documentation "Clone the view object `view'. `Initargs' can
be used to supply different values to the initargs of the
class. A default method doing slot-by-slot copying of `view' has
been defined that should be appropriate for most view classes.")
  (:method ((view view) &rest initargs)
    ;; We iterate over the slots of `view', remembering the value and
    ;; initarg for any slot with an initarg, and use this to create
    ;; the new `make-instance' form. We assume that any slot with no
    ;; initarg will get its value from an `initialize-instance' method
    ;; or similar.
    (apply #'make-instance (class-of view)
           (append initargs
                   (loop for slot in (clim-mop:class-slots (class-of view))
                      for slot-initarg = (first (clim-mop:slot-definition-initargs slot))
                      for slot-name = (clim-mop:slot-definition-name slot)
                      for slot-boundp = (slot-boundp view slot-name)
                      when (and slot-initarg slot-boundp)
                      nconc (list slot-initarg (slot-value view slot-name)))))))

(defclass drei-buffer-view (drei-view)
  ((%buffer :accessor buffer
            :initform (make-instance 'drei-buffer)
            :initarg :buffer
            :type drei-buffer
            :accessor buffer)
   (%top :accessor top
         :documentation "The top of the displayed buffer, that
is, the mark indicating the first visible object in the buffer.")
   (%bot :accessor bot
         :documentation "The bottom of the displayed buffer, that
is, the mark indicating the last visible object in the buffer.")
   (%cache-string :reader cache-string
                  :initform (make-array 0 :element-type 'character
                                          :adjustable t
                                          :fill-pointer 0)
                  :documentation "A string used during redisplay
to reduce consing. Instead of consing up a new string every time
we need to pull out a buffer region, we put it in this
string. The fill pointer is automatically set to zero whenever
the string is accessed through the reader.")
   (%displayed-lines :accessor displayed-lines
                     :initform (make-array 0 :element-type 'displayed-line
                                           :initial-element (make-displayed-line))
                     :type array
                     :documentation "An array of the
`displayed-line' objects displayed by the view. Not all of these
are live.")
   (%displayed-lines-count :accessor displayed-lines-count
                           :initform 0
                           :type integer
                           :documentation "The number of lines in
the views `displayed-lines' array that are actually live, that
is, used for display right now."))
  (:documentation "A view that contains a `drei-buffer'
object."))

(defmethod initialize-instance :after ((view drei-buffer-view) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((top top) (bot bot) (buffer buffer)) view
    (setf top (make-buffer-mark buffer 0 :left)
          bot (make-buffer-mark buffer (size buffer) :right))))

(defmethod (setf top) :after (new-value (view drei-buffer-view))
  (invalidate-all-strokes view))

(defmethod (setf bot) :after (new-value (view drei-buffer-view))
  (invalidate-all-strokes view))

(defmethod (setf buffer) :after (new-value (view drei-buffer-view))
  (invalidate-all-strokes view))

(defmethod (setf syntax) :after (new-value (view drei-buffer-view))
  (invalidate-all-strokes view :modified t))

(defmethod cache-string :around ((view drei-buffer-view))
  (let ((string (call-next-method)))
    (setf (fill-pointer string) 0)
    string))

(defmethod observer-notified ((view drei-buffer-view) (buffer drei-buffer)
                              changed-region)
  (dotimes (i (displayed-lines-count view))
    (let ((line (line-information view i)))
      (when (<= (car changed-region) (line-end-offset line))
        (invalidate-line-strokes line :modified t)))))

(defclass drei-syntax-view (drei-buffer-view)
  ((%syntax :accessor syntax)
   (%prefix-size :accessor prefix-size
                 :initform 0
                 :documentation "The number of unchanged objects
at the beginning of the buffer.")
   (%suffix-size :accessor suffix-size
                 :initform 0
                 :documentation  "The number of unchanged objects
at the end of the buffer.")
   (%recorded-buffer-size :accessor buffer-size
                          :initform 0
                          :documentation "The size of the buffer
the last time the view was synchronized."))
  (:documentation "A buffer-view that maintains a parse tree of
the buffer, or otherwise pays attention to the syntax of the
buffer."))

(defmethod initialize-instance :after ((view drei-syntax-view) &rest args
                                       &key (syntax *default-syntax*))
  (declare (ignore args))
  (check-type syntax (or symbol syntax))
  (with-accessors ((view-syntax syntax)
                   (buffer buffer)
                   (suffix-size suffix-size)
                   (prefix-size prefix-size)) view
    (setf view-syntax (if (symbolp syntax)
                          (make-syntax-for-view view syntax)
                          syntax))
    (add-observer buffer view)))

(defmethod (setf buffer) :before ((buffer drei-buffer) (view drei-syntax-view))
  ;; Remove the observation of the old buffer.
  (with-accessors ((old-buffer buffer)) view
    (remove-observer old-buffer view)))

(defmethod (setf buffer) :after ((buffer drei-buffer) (view drei-syntax-view))
  ;; Add observation of the new buffer.
  (add-observer buffer view)
  ;; We need a new syntax object of the same type as the old one, and
  ;; to zero out the unchanged-prefix-values.
  (with-accessors ((view-syntax syntax)
                   (point point) (mark mark)
                   (suffix-size suffix-size)
                   (prefix-size prefix-size)
                   (buffer-size buffer-size)
                   (bot bot) (top top)) view
    (setf point (clone-mark (point buffer))
          mark (clone-mark (point buffer) :right)
          (offset mark) 0
          view-syntax (make-syntax-for-view view (class-of view-syntax))
          prefix-size 0
          suffix-size 0
          buffer-size -1 ; For reparse even if buffer is empty.
          ;; Also set the top and bot marks.
          top (make-buffer-mark buffer 0 :left)
          bot (make-buffer-mark buffer (size buffer) :right))))

(defmethod (setf syntax) :after (syntax (view drei-syntax-view))
  (setf (prefix-size view) 0
        (suffix-size view) 0
        (buffer-size view) -1))

(defmethod mode-enabled-p or ((modual drei-syntax-view) mode-name)
  (mode-enabled-p (syntax modual) mode-name))

(defmethod enable-mode ((modual drei-syntax-view) mode-name &rest initargs)
  (if (mode-applicable-p (syntax modual) mode-name)
      (apply #'enable-mode (syntax modual) mode-name initargs)
      (call-next-method)))

(defmethod disable-mode ((modual drei-syntax-view) mode-name)
  (if (mode-applicable-p (syntax modual) mode-name)
      (disable-mode (syntax modual) mode-name)
      (call-next-method)))

(defmethod observer-notified ((view drei-syntax-view) (buffer drei-buffer)
                              changed-region)
  (with-accessors ((prefix-size prefix-size)
                   (suffix-size suffix-size)) view
    (setf prefix-size (min (car changed-region) prefix-size)
          suffix-size (min (- (size buffer) (cdr changed-region))
                           suffix-size)
          (modified-p view) t))
  (call-next-method))

(defmethod synchronize-view :around ((view drei-syntax-view) &key
                                     force-p (begin 0) (end (size (buffer view))))
  (assert (>= end begin))
  ;; If nothing changed, then don't call the other methods.
  (let ((high-offset (- (size (buffer view)) (suffix-size view))))
    (when (or (and (> begin (prefix-size view))
                   (> high-offset begin))
              (and (> end (prefix-size view))
                   (or (> end high-offset)
                       (>= (prefix-size view) begin)))
              (/= (size (buffer view)) (buffer-size view))
              force-p)
      (call-next-method))))

(defmethod synchronize-view ((view drei-syntax-view)
                             &key (begin 0) (end (size (buffer view))))
  "Synchronize the syntax view with the underlying
buffer. `Begin' and `end' are offsets specifying the region of
the buffer that must be synchronised, defaulting to 0 and the
size of the buffer respectively."
  (let ((prefix-size (prefix-size view))
        (suffix-size (suffix-size view)))
    ;; Set some minimum values here so if `update-syntax' calls
    ;; `update-parse' itself, we won't end with infinite recursion.
    (setf (prefix-size view) (if (> begin prefix-size)
                                 prefix-size
                                 end)
          (suffix-size view) (if (>= end (- (size (buffer view)) suffix-size))
                                 (- (size (buffer view)) (prefix-size view))
                                 suffix-size)
          (buffer-size view) (size (buffer view)))
    (multiple-value-bind (parsed-start parsed-end)
        (update-syntax (syntax view) prefix-size suffix-size begin end)
      (assert (>= parsed-end parsed-start))
      ;; Now set the proper new values for prefix-size and
      ;; suffix-size.
      (setf (prefix-size view) (max (if (>= prefix-size parsed-start)
                                        parsed-end
                                        prefix-size)
                                    prefix-size)
            (suffix-size view) (max (if (>= parsed-end (- (size (buffer view)) suffix-size))
                                        (- (size (buffer view)) parsed-start)
                                        suffix-size)
                                    suffix-size)))
    (call-next-method)))

(defun make-syntax-for-view (view syntax-symbol &rest args)
  (apply #'make-instance syntax-symbol
   :buffer (buffer view)
   :updater-fns (list (lambda (begin end)
                        (synchronize-view view :begin begin :end end)))
   args))

(defgeneric pump-state-for-offset-with-syntax (view syntax offset)
  (:documentation "Return a pump state that will enable pumping
strokes from `offset' in the buffer of `view' as specified by
`syntax' (via `stroke-pump-for-syntax'). The pump state is not
guaranteed to be valid past the next call to
`stroke-pump-for-syntax' or `synchronize-view'. The results are
undefined if `offset' is not at the beginning of a line."))

(defgeneric stroke-pump-with-syntax (view syntax stroke pump-state)
  (:documentation "Put stroke information in `stroke' as
specified by `syntax', returns new pump-state. `Pump-state' must
either be the result of a call to
`pump-state-for-offset-with-syntax' or be the return value of an
earlier call to `stroke-pump-with-syntax'. A pump state is not
guaranteed to be valid past the next call to
`stroke-pump-with-syntax' or `synchronize-view'. It is
permissible for `pump-state' to be destructively modified by this
function."))

(defclass point-mark-view (drei-buffer-view)
  ((%point :initform nil :initarg :point :accessor point-of)
   (%mark :initform nil :initarg :mark :accessor mark-of))
  (:documentation "A view class containing a point and a mark
into its buffer."))

(defmethod initialize-instance :after ((view point-mark-view)
                                       &rest args)
  (declare (ignore args))
  (with-accessors ((point point) (mark mark)
                   (buffer buffer)) view
    (setf point (clone-mark (point buffer)))
    (setf mark (clone-mark (point buffer)))))

(defmethod (setf buffer) :before ((buffer drei-buffer) (view point-mark-view))
  ;; Set the point of the old buffer to the current point of the view,
  ;; so the next time the buffer is revealed, it will remember its
  ;; point.
  (setf (point (buffer view)) (point view)))

(defmethod (setf buffer) :after ((buffer drei-buffer) (view point-mark-view))
  (with-accessors ((point point) (mark mark)) view
    (setf point (clone-mark (point buffer))
          mark (clone-mark (point buffer) :right))))

(defclass textual-drei-syntax-view (drei-syntax-view point-mark-view textual-view)
  ((%auto-fill-mode :initform nil :accessor auto-fill-mode)
   (%auto-fill-column :initform 70 :accessor auto-fill-column)
   (%region-visible-p :initform nil :accessor region-visible-p)
   ;; for next-line and previous-line commands
   (%goal-column :initform nil :accessor goal-column)
   ;; for dynamic abbrev expansion
   (%original-prefix :initform nil :accessor original-prefix)
   (%prefix-start-offset :initform nil :accessor prefix-start-offset)
   (%dabbrev-expansion-mark :initform nil :accessor dabbrev-expansion-mark)
   (%overwrite-mode :initform nil :accessor overwrite-mode))
  (:default-initargs :use-editor-commands t))

(defmethod create-view-cursors nconc ((output-stream extended-output-stream)
                                      (view textual-drei-syntax-view))
  (unless (no-cursors view)
    (list (make-instance 'point-cursor :view view :output-stream output-stream)
          (make-instance 'mark-cursor :view view :output-stream output-stream))))

(defmethod view-command-tables append ((view textual-drei-syntax-view))
  (syntax-command-tables (syntax view)))

(defmethod use-editor-commands-p ((view textual-drei-syntax-view))
  t)
