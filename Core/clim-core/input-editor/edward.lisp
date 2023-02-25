;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020-2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Edward - the input editor (based on Cluffer).
;;;

(in-package #:climi)

;;; Edward operations.

(defclass edward-lsticky-cursor
    (standard-text-cursor cluffer-standard-line:left-sticky-cursor)
  ())

(defclass edward-rsticky-cursor
    (standard-text-cursor cluffer-standard-line:right-sticky-cursor)
  ())

(defclass edward-mixin ()
  ((edward-buffer
    :initarg :input-buffer
    :reader input-editor-buffer
    :initform (make-cluffer))
   (edit-cursor                         ; aka "point"
    :reader edit-cursor
    :initform (make-instance 'edward-rsticky-cursor))
   #+ (or)
   (scan-cursor                         ; for parsing
    :reader scan-cursor
    :initform (make-instance 'edward-lsticky-cursor))
   (selections                          ; aka "regions"
    :reader selections
    :initform (make-hash-table))
   (kill-history
    :allocation :class                  ; banzai! (and yolo)
    :reader input-editor-kill-history
    :initform (nth-value 1 (make-kluffer)))
   (last-command
    :accessor input-editor-last-command
    :initform nil)
   ;; (edward-undo-history :reader edward-undo-history)
   ;; (edward-redo-history :reader edward-redo-history)
   (edward-numarg
    :accessor numeric-argument
    :initform 1)
   (edward-update
    :accessor input-editor-timestamp
    :initform -1)
   (edward-string
    :reader input-editor-string
    ;; reinitializing the instance resets only a fill pointer.
    :initform (make-array 0 :element-type 'character
                            :adjustable t
                            :fill-pointer t))))

(defmethod print-object ((object edward-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((buffer (input-editor-buffer object)))
      (format stream "~s ~d" :lines (cluffer:line-count buffer)))))

(defun ensure-edward-selection (editor name &key start end (data nil data-p))
  (let ((selection (or (gethash name (selections editor))
                       (setf (gethash name (selections editor))
                             (make-buffer-selection nil nil nil)))))
    (move-buffer-selection selection start end)
    (when data-p
      (setf (payload selection) data))
    selection))

(defun delete-edward-selection (editor name)
  (when-let ((selection (gethash name (selections editor))))
    (remhash name (selections editor))
    (let ((c1 (lcursor selection))
          (c2 (rcursor selection)))
      (when (cluffer:cursor-attached-p c1)
        (cluffer:detach-cursor c1))
      (when (cluffer:cursor-attached-p c2)
        (cluffer:detach-cursor c2)))))

(defmacro do-cursors ((cursor editor) &body body)
  (with-gensyms (cont)
    `(flet ((,cont (,cursor) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (,cont (edit-cursor ,editor))
       (dohash ((key val) (selections ,editor))
         (declare (ignore key))
         (,cont (lcursor val))
         (,cont (rcursor val))))))

(defmethod shared-initialize :after ((object edward-mixin) slot-names &key)
  (declare (ignore slot-names))
  (let* ((buffer (input-editor-buffer object))
         (line-0 (cluffer:find-line buffer 0)))
    (do-cursors (cursor object)
      (unless (cluffer:cursor-attached-p cursor)
        (cluffer:attach-cursor cursor line-0))))
  (setf (numeric-argument object) 1
        (fill-pointer (input-editor-string object)) 0))

(defun edward-insert-input (editor string cursor)
  (declare (ignore editor))
  (smooth-insert-input cursor string))

;;; Edward operations.

(defun edward-buffer-dirty-p (editor)
  (let* ((buffer (input-editor-buffer editor))
         (editor-timestamp (input-editor-timestamp editor))
         (buffer-timestamp (buffer-timestamp buffer)))
    (if (= editor-timestamp buffer-timestamp)
        nil
        buffer-timestamp)))

(defun edward-buffer-string (editor)
  (let ((string (input-editor-string editor)))
    (when (edward-buffer-dirty-p editor)
      (let ((buffer (input-editor-buffer editor)))
        (setf (fill-pointer string) 0
              (input-editor-timestamp editor) (buffer-timestamp buffer))
        (buffer-string buffer string)))
    string))

#+ (or)
(defun edward-replace-input
    (editor new-input start end buffer-start)
  (let ((buffer (input-editor-buffer editor))
        (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor))
        (open-cursor-position (if (integerp buffer-start)
                                  buffer-start
                                  (cursor-linear-position buffer-start)))
        (scan-cursor-position (cursor-linear-position (scan-cursor editor))))
    (flet ((thunk ()
             (setf (cursor-linear-position cursor) open-cursor-position)
             ;; The scan cursor is "left sticky" hence the order of operations.
             (loop for i from start below end do
               (cluffer:insert-item cursor (char new-input i)))
             (loop repeat (- scan-cursor-position open-cursor-position) do
               (smooth-delete-item cursor))))
      (cluffer:attach-cursor cursor (cluffer:find-line buffer 0))
      (unwind-protect (thunk)
        (cluffer:detach-cursor cursor)))))

(defun edward-buffer-extent (editor)
  "Computes the space requied to show all text in the buffer."
  (with-sheet-medium (medium editor)
    (let ((maximal-x 0)
          (maximal-y 0)
          (line-height (text-style-height (medium-text-style medium) medium)))
      (flet ((account-for-line (line)
               (incf maximal-y line-height)
               (maxf maximal-x (text-size medium (line-string line)))))
        (declare (dynamic-extent (function account-for-line)))
        (map-over-lines #'account-for-line (input-editor-buffer editor))
        (values maximal-x maximal-y)))))

;;; FIXME implement the soft line wrapping
(defun edward-cursor-position-from-coordinates (editor rel-x rel-y)
  "Computes the cursor position based on the relative pointercoordinates."
  (with-sheet-medium (medium editor)
    (let* ((buffer (input-editor-buffer editor))
           (lin-h (text-style-height (medium-text-style medium) medium))
           (linum (truncate rel-y lin-h))
           (licnt (cluffer:line-count buffer))
           (line (cluffer:find-line buffer (clamp linum 0 (1- licnt)))))
      (when (>= linum licnt)
        (return-from edward-cursor-position-from-coordinates
          (values line (cluffer:item-count line))))
      (let* ((string (line-string line))
             (breaks (line-breaks string (lambda (string start end)
                                           (text-size editor string :start start :end end))
                                  :break-strategy nil
                                  ;; If the initial offset is 0, then the utility
                                  ;; line-breaks breaks the line at least after
                                  ;; the first character. To avoid that we add 1
                                  ;; to both initial-offset and margin and in
                                  ;; consequence "break" may occur at index 0).
                                  :initial-offset 1
                                  :margin (+ rel-x 1)
                                  :count 1)))
        (values line
                (or (car breaks) (length string)))))))

;;; editor function prototype

;; (defgeneric edward-move (cursor unit syntax numeric-argument))
;; ;;; (edward-move cursor :char <text> +1)
;; ;;; (edward-move cursor :word <text> +1)
;; ;;; (edward-move cursor :line <text> -2)

;; (defgeneric edward-search (cursor object syntax numeric-argument))
;; ;;; (edward-move cursor "s/abc/g" <regexp>  1)
;; ;;; (edward-move cursor "s/abc/g" <regexp> -1)
;; ;;; (edward-move cursor "abc"     <search> 1)

;; (defgeneric edward-clear (cursor unit syntax numeric-argument))
;; ;;; (edward-clear cursor :line   <text> 0)
;; ;;; (edward-clear cursor :buffer <text> 0)

;; (defgeneric edward-delete (cursor unit syntax numeric-argument))
;; ;;; (edward-delete cursor :char <text> -1)
;; ;;; (edward-delete cursor :char <text> +1)

;; (defgeneric edward-transpose (cursor unit syntax numeric-argument))
;; ;;; (edward-transpose cursor :char <text> 1)

;; (defgeneric edward-insert (cursor object syntax numeric-argument))
;; ;;; (edward-insert cursor cursor #\newline <text> -1)

(defmethod ie-forward-object
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (smooth-forward-item cursor)))

(defmethod ie-forward-word
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop do (smooth-forward-item cursor)
                 until (or (cluffer:end-of-line-p cursor)
                           (cluffer:end-of-buffer-p cursor)
                           (char= (cluffer:item-after-cursor cursor) #\space)))))

(defmethod ie-backward-object
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (smooth-backward-item cursor)))

(defmethod ie-backward-word
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop do (smooth-backward-item cursor)
                 until (or (cluffer:beginning-of-line-p cursor)
                           (cluffer:beginning-of-buffer-p cursor)
                           (char= (cluffer:item-before-cursor cursor) #\space)))))

(defmethod ie-beginning-of-line
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event numeric-argument))
  (cluffer:beginning-of-line (edit-cursor sheet)))

(defmethod ie-end-of-line
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event numeric-argument))
  (cluffer:end-of-line (edit-cursor sheet)))

(defmethod ie-next-line
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (smooth-move-line buffer (edit-cursor sheet) numeric-argument))

(defmethod ie-previous-line
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (smooth-move-line buffer (edit-cursor sheet) (- numeric-argument)))

(defmethod ie-beginning-of-buffer
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event numeric-argument))
  (smooth-beg-of-buffer buffer (edit-cursor sheet)))

(defmethod ie-end-of-buffer
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event numeric-argument))
  (smooth-end-of-buffer buffer (edit-cursor sheet)))

(defmethod ie-scroll-forward
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-arg)
  (smooth-move-line buffer (edit-cursor sheet) (* 4 numeric-arg)))

(defmethod ie-scroll-backward
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-arg)
  (smooth-move-line buffer (edit-cursor sheet) (- (* 4 numeric-arg))))

(defmethod ie-erase-object
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        for item = (smooth-erase-item cursor)
        finally (return item)))

(defmethod ie-delete-object
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        for item = (smooth-delete-item cursor)
        finally (return item)))

;;; Killing commands

(defmethod ie-erase-word
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop for item = (smooth-erase-item cursor)
                 when item
                   collect item into result
                 until (or (cluffer:beginning-of-line-p cursor)
                           (cluffer:beginning-of-buffer-p cursor)
                           (char= (cluffer:item-before-cursor cursor) #\space))
                 finally (input-editor-kill-object sheet (nreverse result) :front))))

(defmethod ie-delete-word
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (loop with cursor = (edit-cursor sheet)
        repeat numeric-argument
        do (loop for item = (smooth-delete-item cursor)
                 when item
                   collect item into result
                 until (or (cluffer:end-of-line-p cursor)
                           (cluffer:end-of-buffer-p cursor)
                           (char= (cluffer:item-after-cursor cursor) #\space))
                 finally (input-editor-kill-object sheet result :back))))

(defmethod ie-kill-line
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (handler-bind ((cluffer:end-of-buffer
                   (lambda (condition)
                     (declare (ignore condition))
                     (return-from ie-kill-line))))
    (loop with cursor = (edit-cursor sheet)
          repeat numeric-argument
          for line = (smooth-kill-line cursor)
          do (input-editor-kill-object sheet line :back))))

;;; Yank

(defmethod ie-yank-kill-ring
    ((sheet edward-mixin) (buffer cluffer:buffer) event numarg)
  (let ((cursor (edit-cursor sheet)))
    (ensure-edward-selection sheet :yank :start cursor :end cursor)
    (smooth-insert-input cursor (input-editor-yank-kill sheet))))

(defmethod ie-yank-next-item
    ((sheet edward-mixin) (buffer cluffer:buffer) event numarg)
  (when-let ((items (input-editor-yank-next sheet)))
    (smooth-replace-input (ensure-edward-selection sheet :yank) items)))

;;; Editing

(defmethod ie-clear-input-buffer
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event numeric-argument))
  (smooth-clean-buffer buffer (edit-cursor sheet)))

(defmethod ie-insert-newline
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (smooth-insert-item (edit-cursor sheet) #\newline))

(defmethod ie-insert-newline-after-cursor
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore event))
  (let* ((cursor (edit-cursor sheet))
         (initial-line (cluffer:line cursor))
         (initial-position (cluffer:cursor-position cursor)))
    (smooth-insert-item cursor #\newline)
    (smooth-set-position cursor initial-line initial-position)))

(defmethod ie-transpose-objects
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (let ((cursor (edit-cursor sheet)))
    (when (cluffer:beginning-of-buffer-p cursor)
      (return-from ie-transpose-objects))
    (when (cluffer:end-of-line-p cursor)
      (smooth-backward-item cursor))
    (let ((elt (if (cluffer:beginning-of-line-p cursor)
                   #\newline
                   (cluffer:item-before-cursor cursor))))
      (smooth-erase-item cursor)
      (smooth-forward-item cursor)
      (smooth-insert-item cursor elt))))

#+ (or)                                 ; requires more work
(defmethod ie-transpose-words
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (let ((cursor (edit-cursor sheet))
        word-start
        word-end
        word-items)
    (ie-backward-word sheet buffer event 2)
    (ie-forward-word sheet buffer event)
    (setf word-start (cluffer:cursor-position cursor))
    (ie-forward-word sheet buffer event)
    (setf word-end (cluffer:cursor-position cursor))
    (setf word-items (cluffer:items cursor :start word-start :end word-end))
    (ie-backward-word sheet buffer event 2)
    (ie-forward-word sheet buffer event)
    (ie-delete-word sheet buffer event)
    (ie-forward-word sheet buffer event numeric-argument)
    (loop for item across word-items
          do (cluffer:insert-item cursor item))))

(defmethod ie-numeric-argument
    ((sheet edward-mixin) buffer event numeric-argument)
  (declare (ignore buffer event numeric-argument))
  (setf (numeric-argument sheet) 4))

(defmethod ie-insert-object
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (when-let ((ch (typecase event
                   (keyboard-event (event-char event))
                   (character event))))
    (let ((cursor (edit-cursor sheet)))
      (loop repeat numeric-argument
            do (smooth-insert-item cursor ch)))))

(defmethod ie-select-object
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore numeric-argument))
  (if (typep event 'pointer-event)
      (multiple-value-bind (line position)
          (edward-cursor-position-from-coordinates sheet
                                                   (pointer-event-x event)
                                                   (pointer-event-y event))
        (smooth-set-position (edit-cursor sheet) line position))
      (smooth-set-position (edit-cursor sheet) (edit-cursor sheet))))

(defmethod ie-select-region
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-arg)
  (multiple-value-bind (line position)
      (edward-cursor-position-from-coordinates sheet
                                               (pointer-event-x event)
                                               (pointer-event-y event))
    (smooth-set-position (edit-cursor sheet) line position)))

(defmethod ie-context-menu
    ((sheet edward-mixin) (buffer cluffer:buffer) event numeric-arg)
  (menu-choose '("Cut" "Copy" "Paste" "Delete") :label "Stub menu" :scroll-bars nil))

